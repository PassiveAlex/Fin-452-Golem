# fct_eia_data.R
# EIA fundamental data: series definitions, fetching via RTL::eia2tidy,
# feather cache loading, and the 5-year range analytics helper.
#
# Fetch flow:
#   build_eia_fundamentals()  ← called by dev/fetch_eia_data.R (GitHub Actions nightly)
#   load_eia_data()           ← called once at Shiny startup

# ── Series definitions ────────────────────────────────────────────────────────
# Each row defines one EIA series.
# ticker:      EIA API series ID (passed to RTL::eia2tidy via /v2/seriesid/)
# name:        unique short key used to join metadata after the API call
# commodity:   one of "crude", "distillate", "gasoline", "natural_gas"
# series_type: one of "stocks", "production", "imports", "exports", "storage",
#              "consumption"
# area:        short code used for filtering in the Shiny app
# area_label:  display label shown to the user
# units:       display unit string
# frequency:   "weekly" or "monthly"
#
# NOTE: Crude PADD-level stocks now use the W_EPC0_SAX format (the old
# WCRSTP11-51 legacy IDs were deprecated by EIA in their v2 API). The R10-R50
# PADD region codes are the best-available substitutes; verify via the EIA API
# browser at api.eia.gov/v2/seriesid/ if any still return 404.
EIA_TICKERS <- tibble::tribble(
  ~ticker,                               ~name,                      ~commodity,    ~series_type,  ~area,          ~area_label,                        ~units,   ~frequency,
  # ── Crude oil ending stocks (Mbbl) — weekly ──────────────────────────────
  "PET.WCRSTUS1.W",                      "crude_stocks_US",          "crude",       "stocks",      "US",           "U.S.",                             "Mbbl",   "weekly",
  "PET.W_EPC0_SAX_R10_MBBL.W",          "crude_stocks_PADD1",       "crude",       "stocks",      "PADD1",        "PADD 1 (East Coast)",              "Mbbl",   "weekly",
  "PET.W_EPC0_SAX_R20_MBBL.W",          "crude_stocks_PADD2",       "crude",       "stocks",      "PADD2",        "PADD 2 (Midwest)",                 "Mbbl",   "weekly",
  "PET.W_EPC0_SAX_R30_MBBL.W",          "crude_stocks_PADD3",       "crude",       "stocks",      "PADD3",        "PADD 3 (Gulf Coast)",              "Mbbl",   "weekly",
  "PET.W_EPC0_SAX_R40_MBBL.W",          "crude_stocks_PADD4",       "crude",       "stocks",      "PADD4",        "PADD 4 (Rockies)",                 "Mbbl",   "weekly",
  "PET.W_EPC0_SAX_R50_MBBL.W",          "crude_stocks_PADD5",       "crude",       "stocks",      "PADD5",        "PADD 5 (West Coast)",              "Mbbl",   "weekly",
  "PET.W_EPC0_SAX_YCUOK_MBBL.W",        "crude_stocks_Cushing",     "crude",       "stocks",      "Cushing",      "Cushing, OK",                      "Mbbl",   "weekly",
  # Crude ending stocks excl. SPR — weekly
  "PET.WCESTUS1.W",                      "crude_stocks_excl_spr",    "crude",       "stocks",      "US_excl_spr",  "U.S. (Excl. SPR)",                 "Mbbl",   "weekly",
  # ── Crude production / imports / exports — weekly ─────────────────────────
  "PET.WCRFPUS2.W",                      "crude_prod_US",            "crude",       "production",  "US",           "U.S.",                             "Mbbl/d", "weekly",
  "PET.WCRIMUS2.W",                      "crude_imports_US",         "crude",       "imports",     "US",           "U.S.",                             "Mbbl/d", "weekly",
  "PET.WCREXUS2.W",                      "crude_exports_US",         "crude",       "exports",     "US",           "U.S.",                             "Mbbl/d", "weekly",
  # ── Crude imports / exports — monthly ────────────────────────────────────
  "PET.MCRIMUS2.M",                      "crude_imports_US_M",       "crude",       "imports",     "US",           "U.S.",                             "Mbbl/d", "monthly",
  "PET.MCREXUS2.M",                      "crude_exports_US_M",       "crude",       "exports",     "US",           "U.S.",                             "Mbbl/d", "monthly",

  # ── Distillate fuel oil ending stocks (Mbbl) — weekly ────────────────────
  "PET.WDISTUS1.W",                      "dist_stocks_US",           "distillate",  "stocks",      "US",           "U.S.",                             "Mbbl",   "weekly",
  "PET.WDISTP11.W",                      "dist_stocks_PADD1",        "distillate",  "stocks",      "PADD1",        "PADD 1 (East Coast)",              "Mbbl",   "weekly",
  "PET.WDISTP21.W",                      "dist_stocks_PADD2",        "distillate",  "stocks",      "PADD2",        "PADD 2 (Midwest)",                 "Mbbl",   "weekly",
  "PET.WDISTP31.W",                      "dist_stocks_PADD3",        "distillate",  "stocks",      "PADD3",        "PADD 3 (Gulf Coast)",              "Mbbl",   "weekly",
  "PET.WDISTP41.W",                      "dist_stocks_PADD4",        "distillate",  "stocks",      "PADD4",        "PADD 4 (Rockies)",                 "Mbbl",   "weekly",
  "PET.WDISTP51.W",                      "dist_stocks_PADD5",        "distillate",  "stocks",      "PADD5",        "PADD 5 (West Coast)",              "Mbbl",   "weekly",

  # ── Total motor gasoline ending stocks (Mbbl) — weekly ───────────────────
  "PET.WGTSTUS1.W",                      "gas_stocks_US",            "gasoline",    "stocks",      "US",           "U.S.",                             "Mbbl",   "weekly",
  "PET.WGTSTP11.W",                      "gas_stocks_PADD1",         "gasoline",    "stocks",      "PADD1",        "PADD 1 (East Coast)",              "Mbbl",   "weekly",
  "PET.WGTSTP21.W",                      "gas_stocks_PADD2",         "gasoline",    "stocks",      "PADD2",        "PADD 2 (Midwest)",                 "Mbbl",   "weekly",
  "PET.WGTSTP31.W",                      "gas_stocks_PADD3",         "gasoline",    "stocks",      "PADD3",        "PADD 3 (Gulf Coast)",              "Mbbl",   "weekly",
  "PET.WGTSTP41.W",                      "gas_stocks_PADD4",         "gasoline",    "stocks",      "PADD4",        "PADD 4 (Rockies)",                 "Mbbl",   "weekly",
  "PET.WGTSTP51.W",                      "gas_stocks_PADD5",         "gasoline",    "stocks",      "PADD5",        "PADD 5 (West Coast)",              "Mbbl",   "weekly",

  # ── Natural gas working underground storage (Bcf) — weekly ───────────────
  "NG.NW2_EPG0_SWO_R48_BCF.W",          "ng_storage_US",            "natural_gas", "storage",     "US",           "U.S. Lower 48",                    "Bcf",    "weekly",
  "NG.NW2_EPG0_SWO_R31_BCF.W",          "ng_storage_East",          "natural_gas", "storage",     "East",         "East Region",                      "Bcf",    "weekly",
  "NG.NW2_EPG0_SWO_R32_BCF.W",          "ng_storage_Midwest",       "natural_gas", "storage",     "Midwest",      "Midwest Region",                   "Bcf",    "weekly",
  "NG.NW2_EPG0_SWO_R33_BCF.W",          "ng_storage_Mountain",      "natural_gas", "storage",     "Mountain",     "Mountain Region",                  "Bcf",    "weekly",
  "NG.NW2_EPG0_SWO_R34_BCF.W",          "ng_storage_Pacific",       "natural_gas", "storage",     "Pacific",      "Pacific Region",                   "Bcf",    "weekly",
  "NG.NW2_EPG0_SWO_R35_BCF.W",          "ng_storage_SouthCent",     "natural_gas", "storage",     "SouthCentral", "South Central Region",             "Bcf",    "weekly",
  # ── Natural gas production / consumption — monthly ────────────────────────
  "NG.N9050US2.M",                       "ng_prod_US_M",             "natural_gas", "production",  "US",           "U.S.",                             "MMcf",   "monthly",
  "NG.N9102US2.M",                       "ng_cons_industrial_M",     "natural_gas", "consumption", "Industrial",   "U.S. Industrial",                  "MMcf",   "monthly",
  "NG.N9103US2.M",                       "ng_cons_residential_M",    "natural_gas", "consumption", "Residential",  "U.S. Residential",                 "MMcf",   "monthly"
)

# ── Fetch all series ──────────────────────────────────────────────────────────

#' Fetch all EIA fundamental series via RTL::eia2tidy and attach metadata
#'
#' Individual ticker failures are caught and skipped so one bad series does not
#' abort the entire run. Returns a tidy long data frame ready to write to the
#' feather cache.
#'
#' @param api_key character — EIA API key (default: EIA_API_KEY env var)
#' @return data.frame with columns: date, commodity, series_type, area,
#'   area_label, units, frequency, value
build_eia_fundamentals <- function(api_key = Sys.getenv("EIA_API_KEY")) {
  if (nchar(api_key) == 0L) stop("EIA_API_KEY is not set.")

  tickers_input <- dplyr::select(EIA_TICKERS, ticker, name)
  meta          <- dplyr::select(EIA_TICKERS, name, commodity, series_type,
                                 area, area_label, units, frequency)

  # Wrap eia2tidy so a 404 / network error on one ticker skips rather than aborts
  safe_fetch <- purrr::possibly(
    function(ticker, key, name) {
      df <- RTL::eia2tidy(ticker = ticker, key = key, name = name)
      tidyr::pivot_longer(df, -date, names_to = "name", values_to = "value",
                          values_drop_na = TRUE)
    },
    otherwise = NULL
  )

  message("Fetching ", nrow(tickers_input), " EIA series...")
  results <- purrr::pmap(
    list(tickers_input$ticker, api_key, tickers_input$name),
    function(t, k, n) {
      out <- safe_fetch(t, k, n)
      if (is.null(out)) message("  WARN: '", t, "' failed — skipped")
      out
    }
  )

  valid <- Filter(Negate(is.null), results)
  if (length(valid) == 0L) stop("All EIA tickers failed to fetch.")
  message("  Fetched ", length(valid), " / ", nrow(tickers_input), " series successfully.")

  dplyr::bind_rows(valid) %>%
    dplyr::left_join(meta, by = "name") %>%
    dplyr::filter(!is.na(value), !is.na(commodity)) %>%
    dplyr::select(date, commodity, series_type, area, area_label, units, frequency, value) %>%
    dplyr::arrange(commodity, series_type, area, date)
}

# ── App-side loader ───────────────────────────────────────────────────────────

#' Resolve path to the feather cache file
eia_data_path <- function() {
  pkg <- tryCatch(
    system.file("extdata/eia_fundamentals.feather", package = "fin452golem"),
    error = function(e) ""
  )
  if (nchar(pkg) > 0L && file.exists(pkg)) return(pkg)
  dev <- "inst/extdata/eia_fundamentals.feather"
  if (file.exists(dev)) return(dev)
  NULL
}

#' Load cached EIA fundamentals from the feather file written by the nightly fetch
#'
#' Returns a list:
#'   $data         — data.frame, or NULL if the file is missing
#'   $last_updated — character timestamp ("YYYY-MM-DD HH:MM:SS UTC"), or NULL
#'   $is_stale     — TRUE when the file is absent or older than `stale_days`
#'
#' @param stale_days numeric — staleness threshold in days (default 3)
load_eia_data <- function(stale_days = 3) {
  path <- eia_data_path()
  if (is.null(path)) {
    return(list(data = NULL, last_updated = NULL, is_stale = TRUE))
  }

  data <- tryCatch(
    arrow::read_feather(path),
    error = function(e) { warning("Cannot read EIA feather: ", conditionMessage(e)); NULL }
  )

  # Back-fill frequency column for caches written before this column was added
  if (!is.null(data) && !"frequency" %in% names(data)) {
    data$frequency <- "weekly"
  }

  ts_path      <- sub("\\.feather$", "_last_updated.txt", path)
  last_updated <- if (file.exists(ts_path)) readLines(ts_path, warn = FALSE)[1L] else NULL

  is_stale <- tryCatch({
    if (is.null(last_updated)) return(TRUE)
    lu <- as.POSIXct(last_updated, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    as.numeric(difftime(Sys.time(), lu, units = "days")) > stale_days
  }, error = function(e) TRUE)

  list(data = data, last_updated = last_updated, is_stale = is_stale)
}

# ── Analytics helper ──────────────────────────────────────────────────────────

#' Compute 5-year historical range bands for an inventory / flow series
#'
#' Groups by ISO week (weekly series) or calendar month (monthly series).
#' Returns the current year's data joined with prior-5-year min, max, and mean
#' for the same period — the standard EIA inventory chart format.
#'
#' Frequency is detected automatically from a `frequency` column in `df` if
#' present; defaults to "weekly" for backward compatibility with older caches.
#'
#' @param df           data.frame with columns `date` (Date) and `value` (numeric)
#' @param current_year integer — year to treat as "current" (default: latest in data)
#' @return data.frame: date, period, value, range_min, range_max, avg_5yr
compute_5yr_range <- function(df, current_year = NULL) {
  freq <- if ("frequency" %in% names(df) && nrow(df) > 0L) df$frequency[1L] else "weekly"

  df <- dplyr::filter(df, !is.na(value)) %>%
    dplyr::mutate(
      year   = lubridate::year(date),
      period = if (freq == "monthly") lubridate::month(date)
               else                   lubridate::isoweek(date)
    )

  if (is.null(current_year)) current_year <- max(df$year)
  ref_years <- seq(current_year - 5L, current_year - 1L)

  ref <- df %>%
    dplyr::filter(year %in% ref_years) %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(
      range_min = min(value,  na.rm = TRUE),
      range_max = max(value,  na.rm = TRUE),
      avg_5yr   = mean(value, na.rm = TRUE),
      .groups   = "drop"
    )

  df %>%
    dplyr::filter(year == current_year) %>%
    dplyr::select(date, period, value) %>%
    dplyr::left_join(ref, by = "period")
}
