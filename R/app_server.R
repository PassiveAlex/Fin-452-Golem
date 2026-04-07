# app_server.R
# Top-level server: reactive data loading, shared state, module wiring.
#
# Data flow:
#   RTL::dflong  →  build_market_data()  →  mkt_data (reactiveVal)
#                                            passed to all feature modules
#
#   FRED DGS tickers  →  fetch_fred_zero_curve()  →  zero_curve_panel (reactiveVal)
#                                                      passed to mod_yield_curve_server
#
#   inst/extdata/eia_fundamentals.feather  →  load_eia_data()  →  eia_data (reactiveVal)
#                                              passed to mod_forward_curve_server + mod_eia_server

app_server <- function(input, output, session) {

  # ── 1. Load and cache commodity data ───────────────────────────────────────
  # CL is built first because BRN and HTT are differentials relative to WTI.
  # Its wide price table is passed into those builds so adjust_differential()
  # can reconstruct true prices before log returns are computed.
  # All other markets build independently. Computed once at startup.
  mkt_data <- shiny::reactiveVal({
    cl_data <- build_market_data(
      prefix        = MARKETS[["CL"]]$rtl_prefix,
      max_contracts = MARKETS[["CL"]]$max_contracts
    )
    cl_wide <- cl_data$wide

    result <- purrr::map(
      stats::setNames(ENABLED_MARKETS, ENABLED_MARKETS),
      function(mkt) {
        if (mkt == "CL") return(cl_data)
        m <- MARKETS[[mkt]]
        build_market_data(
          prefix        = m$rtl_prefix,
          max_contracts = m$max_contracts,
          base_wide     = if (!is.null(m$base_prefix)) cl_wide else NULL,
          base_prefix   = m$base_prefix
        )
      }
    )
    result
  })

  # ── 2. Load FRED zero curve from feather cache ─────────────────────────────
  # Loaded once at startup from the nightly-refreshed file (same pattern as EIA).
  # Falls back gracefully (data = NULL empty panel) if the file is missing.
  fred_cache <- tryCatch(
    load_fred_data(),
    error = function(e) {
      warning("FRED data load failed: ", conditionMessage(e))
      list(data = NULL, last_updated = NULL, is_stale = TRUE)
    }
  )

  zero_curve_panel <- shiny::reactiveVal(
    if (!is.null(fred_cache$data)) {
      fred_cache$data
    } else {
      data.frame(date = as.Date(character(0)),
                 maturity = numeric(0),
                 yield    = numeric(0))
    }
  )

  # ── 3. Load EIA fundamentals from feather cache ────────────────────────────
  # Loaded once at startup from the nightly-refreshed file. Falls back
  # gracefully (data = NULL, is_stale = TRUE) if the file is missing.
  eia_data <- shiny::reactiveVal(
    tryCatch(load_eia_data(), error = function(e) {
      warning("EIA data load failed: ", conditionMessage(e))
      list(data = NULL, last_updated = NULL, is_stale = TRUE)
    })
  )

  # ── 4. Wire feature modules ─────────────────────────────────────────────────
  mod_market_process_server("process")

  mod_forward_curve_server("fwd",    mkt_data = mkt_data, eia_data = eia_data)
  mod_volatility_server("vol",       mkt_data = mkt_data)
  mod_seasonality_server("season",   mkt_data = mkt_data)
  mod_hedge_ratios_server("hedge",   mkt_data = mkt_data)

  mod_yield_curve_server("yc",       zero_curve_panel = zero_curve_panel)

  mod_correlation_server("corr",     mkt_data = mkt_data)
  mod_eia_server("eia",              eia_data = eia_data)
}
