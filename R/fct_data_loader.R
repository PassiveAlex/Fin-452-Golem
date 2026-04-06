# fct_data_loader.R
# Loads RTL::dflong and reshapes it for use throughout the app.
#
# RTL::dflong structure:
#   date   : Date
#   series : character  — e.g., "CL01", "CL12", "NG03"
#   value  : numeric    — daily settlement price
#
# Contract numbering: {TICKER}{NN} where NN = 01 (front) … 36 (36th contract)
#
# NOTE: BRN and HTT are stored in RTL as differentials to WTI (CL).
#       True price = CL contract price + differential.
#       adjust_differential() performs this addition before any downstream
#       computations so that log returns and all derived quantities reflect
#       the actual traded price.

#' Load the raw RTL::dflong dataset
#' @return data.frame with columns: date, series, value
load_rtl_data <- function() {
  df <- RTL::dflong
  df$date <- as.Date(df$date)
  df
}

#' Extract the integer contract number from a series name
#' "CL01" -> 1, "NG12" -> 12
#' @param series character vector of RTL series names
#' @param prefix character — the ticker prefix (e.g., "CL")
#' @return integer vector
extract_contract_num <- function(series, prefix) {
  as.integer(sub(paste0("^", prefix), "", series))
}

#' Filter RTL long data to a single market and return tidy long format
#'
#' @param df data.frame from load_rtl_data()
#' @param prefix character — rtl_prefix from MARKETS list (e.g., "CL")
#' @param max_contracts integer — maximum contract number to retain (default 36)
#' @return data.frame: date, contract_num (int), price (dbl)
get_market_long <- function(df, prefix, max_contracts = 36L) {
  pattern <- paste0("^", prefix, "\\d{2}$")
  df %>%
    filter(grepl(pattern, series)) %>%
    mutate(contract_num = extract_contract_num(series, prefix)) %>%
    filter(!is.na(contract_num), contract_num >= 1L, contract_num <= max_contracts) %>%
    select(date, contract_num, price = value) %>%
    arrange(date, contract_num)
}

#' Reshape long market data to wide format
#' Each column: {PREFIX}_C{N}, rows: one per date
#'
#' @param long_df data.frame from get_market_long()
#' @param prefix character — used to name columns
#' @return data.frame with date column + one column per contract
get_market_wide <- function(long_df, prefix) {
  long_df %>%
    pivot_wider(
      names_from  = contract_num,
      names_prefix = paste0(prefix, "_C"),
      values_from = price
    ) %>%
    arrange(date)
}

#' Add base market prices to a differential wide price dataframe
#'
#' BRN and HTT are stored in RTL as spreads to CL. This reconstructs
#' true prices by adding the corresponding CL contract price for each
#' contract month: true_price_N = differential_N + base_price_N.
#' Called before log-return computation so all downstream quantities
#' (vol, seasonality, hedge ratios, etc.) reflect actual traded prices.
#'
#' @param diff_wide data.frame — wide prices of the differential market
#' @param base_wide data.frame — wide prices of the base market (CL)
#' @param prefix character — prefix of the differential market (e.g., "BRN")
#' @param base_prefix character — prefix of the base market (e.g., "CL")
#' @return data.frame — wide true prices, same shape as diff_wide
adjust_differential <- function(diff_wide, base_wide, prefix, base_prefix = "CL") {
  diff_cols <- grep(paste0("^", prefix, "_C\\d+$"), names(diff_wide), value = TRUE)

  # Join base prices by date; suffix avoids any collision on shared non-key columns
  joined <- left_join(diff_wide, base_wide, by = "date", suffix = c("", ".base"))

  for (col in diff_cols) {
    n        <- sub(paste0("^", prefix, "_C"), "", col)
    base_col <- paste0(base_prefix, "_C", n)
    if (base_col %in% names(joined)) {
      joined[[col]] <- joined[[col]] + joined[[base_col]]
    }
  }

  # Return only the differential market columns (drop joined base columns)
  joined %>% select(date, all_of(diff_cols))
}

#' Compute daily log returns from a wide price dataframe
#'
#' Adds return columns named ret_{PREFIX}_C{N} for every price column found.
#' Returns are NA on the first observation.
#'
#' @param wide_df data.frame from get_market_wide() or adjust_differential()
#' @param prefix character
#' @return data.frame with date + return columns (ret_*)
compute_log_returns_wide <- function(wide_df, prefix) {
  price_cols <- grep(paste0("^", prefix, "_C\\d+$"), names(wide_df), value = TRUE)

  wide_df %>%
    arrange(date) %>%
    mutate(across(
      all_of(price_cols),
      ~ dplyr::if_else(.x > 0 & lag(.x) > 0, log(.x / lag(.x)), NA_real_),
      .names = "ret_{.col}"
    )) %>%
    select(date, starts_with("ret_"))
}

#' Build the full price and returns data for one market
#'
#' For differential markets (BRN, HTT), pass the CL wide price dataframe as
#' base_wide. The adjustment is applied before log-return computation so all
#' downstream quantities reflect true traded prices, not raw differentials.
#'
#' @param prefix character — rtl_prefix from MARKETS
#' @param max_contracts integer
#' @param base_wide data.frame or NULL — wide CL prices, required for differential markets
#' @param base_prefix character — base market prefix (default "CL")
#' @return list with elements:
#'   $long     : tidy long-format prices (date, contract_num, price)
#'   $wide     : wide true prices (date, {PREFIX}_C1 … {PREFIX}_C{N})
#'   $ret_wide : wide log returns (date, ret_{PREFIX}_C1 …)
build_market_data <- function(prefix, max_contracts = 36L,
                              base_wide = NULL, base_prefix = "CL") {
  raw  <- load_rtl_data()
  long <- get_market_long(raw, prefix, max_contracts)
  wide <- get_market_wide(long, prefix)

  if (!is.null(base_wide)) {
    wide <- adjust_differential(wide, base_wide, prefix, base_prefix)
    # Rebuild long from adjusted wide so $long and $wide remain consistent
    diff_cols <- grep(paste0("^", prefix, "_C\\d+$"), names(wide), value = TRUE)
    long <- wide %>%
      pivot_longer(all_of(diff_cols), names_to = "col", values_to = "price") %>%
      mutate(contract_num = as.integer(sub(paste0("^", prefix, "_C"), "", col))) %>%
      select(date, contract_num, price) %>%
      arrange(date, contract_num)
  }

  ret <- compute_log_returns_wide(wide, prefix)
  list(long = long, wide = wide, ret_wide = ret)
}

#' Filter a wide price dataframe to a date range
#'
#' @param wide_df data.frame with a date column
#' @param date_from Date
#' @param date_to   Date
#' @return filtered data.frame
filter_date_range <- function(wide_df, date_from, date_to) {
  wide_df %>% filter(date >= date_from, date <= date_to)
}
