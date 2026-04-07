# fct_fred_data.R
# FRED zero curve data fetch + cubic spline interpolation.
#
# Data source: FRED Constant Maturity Treasury (CMT) par yields (H.15 release).
# These are par yields, not zero rates, but for this application the distinction
# is small and the spline approximation is standard risk-manager practice.
#
# Fetched panel schema:
#   date     : Date
#   maturity : numeric — maturity in years
#   yield    : numeric — yield as a decimal (e.g., 0.045 = 4.5%)

# ── FRED ticker → maturity mapping ──────────────────────────────────────────
.FRED_TICKERS <- c(
  "DGS1MO"  = 1 / 12,
  "DGS3MO"  = 3 / 12,
  "DGS6MO"  = 6 / 12,
  "DGS1"    = 1,
  "DGS2"    = 2,
  "DGS3"    = 3,
  "DGS5"    = 5,
  "DGS7"    = 7,
  "DGS10"   = 10,
  "DGS20"   = 20,
  "DGS30"   = 30
)

#' Fetch U.S. Treasury zero curve panel from FRED
#'
#' Pulls Constant Maturity Treasury yields for all configured DGS tickers
#' using tidyquant::tq_get(), converts percent to decimal, and returns a
#' long-format panel ready for cubic spline interpolation.
#'
#' @param from Date — earliest date to fetch (default "2000-01-01")
#' @param tickers named numeric — ticker-to-maturity map (default .FRED_TICKERS)
#' @return data.frame with columns: date (Date), maturity (numeric, years),
#'   yield (numeric, decimal). Rows with NA yields are dropped.
#' @examples
#' \dontrun{
#'   panel <- fetch_fred_zero_curve(from = "2020-01-01")
#'   spline_fn <- get_date_spline(panel, as.Date("2024-01-15"))
#'   interpolate_zero_yield(spline_fn, c(1.5, 4, 8))
#' }
fetch_fred_zero_curve <- function(from = "2000-01-01",
                                   tickers = .FRED_TICKERS) {
  empty_panel <- data.frame(date     = as.Date(character(0)),
                             maturity = numeric(0),
                             yield    = numeric(0))

  # Fetch each ticker directly via the FRED CSV endpoint using base R url().
  # tidyquant::tq_get / quantmod::getSymbols.FRED use curl::curl() internally,
  # which triggers an HTTP/2 INTERNAL_ERROR from FRED's CDN. Base R url() falls
  # back to HTTP/1.1 and succeeds.
  base_url  <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id="
  from_date <- as.Date(from)

  rows <- purrr::imap(tickers, function(maturity_yrs, ticker) {
    tryCatch({
      df <- read.csv(url(paste0(base_url, ticker)), na.strings = ".",
                     stringsAsFactors = FALSE)
      names(df) <- c("date", "price")
      df$date   <- as.Date(df$date)
      df <- df[!is.na(df$price) & df$date >= from_date, ]
      if (nrow(df) == 0L) return(NULL)
      data.frame(date     = df$date,
                 maturity = maturity_yrs,
                 yield    = df$price / 100,   # FRED reports percent
                 stringsAsFactors = FALSE)
    }, error = function(e) {
      message("FRED fetch failed for ", ticker, ": ", conditionMessage(e))
      NULL
    })
  })

  valid <- Filter(Negate(is.null), rows)
  if (length(valid) == 0L) return(empty_panel)

  dplyr::bind_rows(valid) %>%
    dplyr::arrange(date, maturity)
}

#' Build a natural cubic spline function from a single zero curve
#'
#' @param zero_curve_df data.frame with columns: maturity (numeric, years),
#'   yield (numeric, decimal). Rows need not be sorted.
#' @return A function f(maturity) that returns the interpolated zero yield.
#'   Extrapolation uses the flat end of the spline (natural boundary conditions).
#' @examples
#'   zc <- data.frame(maturity = c(0.25, 0.5, 1, 2, 5, 10, 30),
#'                    yield    = c(0.052, 0.051, 0.049, 0.045, 0.042, 0.040, 0.038))
#'   fn <- build_zero_curve_spline(zc)
#'   fn(3.5)   # interpolated 3.5-year yield
build_zero_curve_spline <- function(zero_curve_df) {
  df <- zero_curve_df[order(zero_curve_df$maturity), ]
  df <- df[!is.na(df$maturity) & !is.na(df$yield), ]

  if (nrow(df) < 3L) {
    stop("build_zero_curve_spline: need >= 3 non-NA knots to fit a cubic spline.")
  }

  splinefun(df$maturity, df$yield, method = "natural")
}

#' Interpolate zero yields at arbitrary maturities
#'
#' @param spline_fn function returned by build_zero_curve_spline()
#' @param maturities numeric vector — maturities in years
#' @return numeric vector of interpolated yields (decimal)
interpolate_zero_yield <- function(spline_fn, maturities) {
  yields <- spline_fn(maturities)
  # Guard against extreme extrapolation producing nonsensical negative yields
  pmax(yields, -0.10)
}

#' Compute continuous-compounding discount factors
#'
#' P(0, T) = exp(-y(T) * T)
#'
#' @param spline_fn function from build_zero_curve_spline()
#' @param maturities numeric vector — maturities in years
#' @return numeric vector of discount factors in (0, 1]
discount_factors <- function(spline_fn, maturities) {
  y <- interpolate_zero_yield(spline_fn, maturities)
  exp(-y * maturities)
}

#' Build a zero curve spline for a single date from a panel
#'
#' @param panel data.frame with columns: date (Date), maturity (numeric), yield (numeric)
#' @param target_date Date
#' @return spline function for that date, or NULL if date not found
get_date_spline <- function(panel, target_date) {
  day <- panel[panel$date == target_date, c("maturity", "yield")]
  if (nrow(day) < 3L) return(NULL)
  build_zero_curve_spline(day)
}

#' Generate a smooth interpolated term structure at regular intervals
#'
#' @param spline_fn function from build_zero_curve_spline()
#' @param from numeric — shortest maturity to interpolate (default 0.25 years)
#' @param to   numeric — longest maturity (default 30 years)
#' @param by   numeric — step size in years (default 0.25)
#' @return data.frame with columns: maturity, yield
interpolated_term_structure <- function(spline_fn, from = 0.25, to = 30, by = 0.25) {
  mats <- seq(from, to, by = by)
  data.frame(
    maturity = mats,
    yield    = interpolate_zero_yield(spline_fn, mats)
  )
}

#' Compute the par yield from a zero curve (for display alongside zero yield)
#'
#' The par yield c satisfies: sum_t( c * P(0,t) ) + P(0,T) = 1
#' Assumes annual coupon payments at maturities 1, 2, ..., floor(T).
#'
#' @param spline_fn function from build_zero_curve_spline()
#' @param maturity  numeric — bond maturity in years
#' @return numeric — par yield as a decimal
par_yield <- function(spline_fn, maturity) {
  coupon_times <- seq(1, floor(maturity), by = 1)
  if (length(coupon_times) == 0L) {
    # Zero-coupon case: par yield = zero yield
    return(interpolate_zero_yield(spline_fn, maturity))
  }
  dfs     <- discount_factors(spline_fn, coupon_times)
  df_mat  <- discount_factors(spline_fn, maturity)
  (1 - df_mat) / sum(dfs)
}

#' Compute forward rates implied by the zero curve
#'
#' Instantaneous forward rate: f(T) = -d/dT [log P(0,T)]
#'                                   = y(T) + T * y'(T)
#'
#' @param spline_fn function from build_zero_curve_spline() — must support deriv=1
#' @param maturities numeric vector
#' @return numeric vector of instantaneous forward rates
forward_rates <- function(spline_fn, maturities) {
  y      <- spline_fn(maturities, deriv = 0)
  dy_dT  <- spline_fn(maturities, deriv = 1)
  y + maturities * dy_dT
}

# ── Feather cache loader (mirrors load_eia_data pattern) ─────────────────────

#' Resolve path to the FRED feather cache file
fred_data_path <- function() {
  pkg <- tryCatch(
    system.file("extdata/fred_zero_curve.feather", package = "fin452golem"),
    error = function(e) ""
  )
  if (nchar(pkg) > 0L && file.exists(pkg)) return(pkg)
  dev <- "inst/extdata/fred_zero_curve.feather"
  if (file.exists(dev)) return(dev)
  NULL
}

#' Load cached FRED zero curve from the nightly-written feather file
#'
#' Returns a list:
#'   $data         — data.frame with columns date/maturity/yield, or NULL
#'   $last_updated — character timestamp, or NULL
#'   $is_stale     — TRUE when file is absent or older than stale_days
#'
#' @param stale_days numeric — staleness threshold in days (default 3)
load_fred_data <- function(stale_days = 3) {
  path <- fred_data_path()
  if (is.null(path)) {
    return(list(data = NULL, last_updated = NULL, is_stale = TRUE))
  }

  data <- tryCatch(
    arrow::read_feather(path),
    error = function(e) {
      warning("Cannot read FRED feather: ", conditionMessage(e))
      NULL
    }
  )

  ts_path      <- sub("\\.feather$", "_last_updated.txt", path)
  last_updated <- if (file.exists(ts_path)) readLines(ts_path, warn = FALSE)[1L] else NULL

  is_stale <- tryCatch({
    if (is.null(last_updated)) return(TRUE)
    lu <- as.POSIXct(last_updated, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    as.numeric(difftime(Sys.time(), lu, units = "days")) > stale_days
  }, error = function(e) TRUE)

  list(data = data, last_updated = last_updated, is_stale = is_stale)
}
