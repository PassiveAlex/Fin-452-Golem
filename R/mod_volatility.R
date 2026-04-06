# mod_volatility.R
# Volatility: realized vol term structure, rolling vol over time, vol cone.

mod_volatility_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls",
      mod_market_selector_ui(ns("sel"))
    ),
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Vol Term Structure",
        plotly::plotlyOutput(ns("vol_ts"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Vol Over Time",
        plotly::plotlyOutput(ns("vol_history"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Vol Cone",
        plotly::plotlyOutput(ns("vol_cone"), height = "70%", width = "100%")
      )
    )
  )
}

mod_volatility_server <- function(id, mkt_data) {
  moduleServer(id, function(input, output, session) {
    sel <- mod_market_selector_server("sel")

    wide <- reactive({
      m <- sel$market()
      req(m, mkt_data()[[m]])
      filter_date_range(mkt_data()[[m]]$wide,
                        sel$date_from(), sel$date_to())
    }) %>% bindEvent(sel$market(), sel$date_from(), sel$date_to())

    prefix <- reactive(MARKETS[[sel$market()]]$rtl_prefix) # Prefix of the commodity, CL, BRN, HTT, NG, ...
    unit   <- reactive(MARKETS[[sel$market()]]$unit) # Unit conversion of commodity.

    # ── Vol Term Structure snapshot ───────────────────────────────────────────
    output$vol_ts <- plotly::renderPlotly({
      snap_date <- max(wide()$date, na.rm = TRUE)
      vts       <- vol_term_structure_snapshot(wide(), prefix(), snap_date,
                                                window = sel$window())
      req(vts)

      mkt_label <- MARKETS[[sel$market()]]$label

      plotly::plot_ly(vts, x = ~contract, y = ~vol,
        type = "bar",
        marker = list(color = MARKETS[[sel$market()]]$color),
        hovertemplate = paste0(mkt_label, " %{x}: %{y:.1%} ann. vol<extra></extra>")
      ) %>%
        plotly::layout(
          title = list(text = glue::glue("{mkt_label} \u2014 Vol Term Structure")),
          xaxis = list(title = "Contract", dtick = 3),
          yaxis = list(title = glue::glue("{sel$window()}d {mkt_label} Ann. Realised Vol"),
                       tickformat = ".0%")
        )
    }) %>% bindEvent(wide(), sel$window())

    # ── Vol Over Time ─────────────────────────────────────────────────────────
    # All contracts are rendered as traces. C1/C3/C6/C12 (capped to max_contracts)
    # are visible by default; all others start as legendonly so the user can toggle
    # them via plotly's native legend click without any separate input control.
    output$vol_history <- plotly::renderPlotly({
      pfx   <- prefix()
      max_c <- MARKETS[[sel$market()]]$max_contracts
      all_cs <- seq_len(max_c)

      vol_df <- rolling_vol_wide(wide(), pfx, all_cs, window = sel$window())

      # Identify high-vol regime using C1 for background shading
      c1_col <- paste0("vol_", pfx, "_C1")
      regime_flag <- if (c1_col %in% names(vol_df)) {
        vol_regime_high(vol_df[[c1_col]])
      } else rep(FALSE, nrow(vol_df))

      run_start <- which(diff(c(FALSE, regime_flag)) == 1L)
      run_end   <- which(diff(c(regime_flag, FALSE)) == -1L)
      shapes <- purrr::map2(run_start, run_end, function(s, e) {
        list(type = "rect", xref = "x", yref = "paper",
             x0 = vol_df$date[s], x1 = vol_df$date[e],
             y0 = 0, y1 = 1,
             fillcolor = "rgba(255,100,100,0.08)",
             line = list(width = 0))
      })

      vol_cols <- grep(paste0("^vol_", pfx, "_C\\d+$"), names(vol_df), value = TRUE)
      # Default-visible contracts: C1, C3, C6, C12 (capped to what exists)
      default_shown <- intersect(c(1L, 3L, 6L, 12L), all_cs)

      mkt_label <- MARKETS[[sel$market()]]$label

      p <- plotly::plot_ly()
      for (vc in vol_cols) {
        cn_num   <- as.integer(sub(paste0("^vol_", pfx, "_C"), "", vc))
        cn_label <- paste0(pfx, cn_num)
        p <- plotly::add_lines(p,
          data    = vol_df,
          x       = ~date,
          y       = stats::as.formula(paste0("~`", vc, "`")),
          name    = cn_label,
          visible = if (cn_num %in% default_shown) TRUE else "legendonly",
          hovertemplate = paste0("%{x|%Y-%m-%d} ", cn_label, ": %{y:.1%}<extra></extra>")
        )
      }

      p %>%
        plotly::layout(
          shapes    = shapes,
          title     = list(text = glue::glue("{mkt_label} \u2014 Rolling Realised Volatility")),
          xaxis     = list(title = ""),
          yaxis     = list(title = glue::glue("{sel$window()}d Ann. Realised Vol"),
                           tickformat = ".0%"),
          legend    = list(orientation = "h"),
          hovermode = "x unified"
        )
    }) %>% bindEvent(wide(), sel$window())

    # ── Vol Cone ──────────────────────────────────────────────────────────────
    output$vol_cone <- plotly::renderPlotly({
      pfx   <- prefix()
      mkt_label <- MARKETS[[sel$market()]]$label

      cone <- build_vol_cone(wide(), pfx, contract_n = 1L)
      req(cone)

      current_vols <- vol_term_structure_snapshot(
        wide(), pfx, max(wide()$date, na.rm = TRUE),
        window = sel$window()
      )
      c1_current <- if (!is.null(current_vols) && nrow(current_vols) > 0L)
        current_vols$vol[1L] else NA_real_

      mkt_color <- MARKETS[[sel$market()]]$color
      hex       <- sub("^#", "", mkt_color)
      r_val     <- strtoi(substr(hex, 1L, 2L), 16L)
      g_val     <- strtoi(substr(hex, 3L, 4L), 16L)
      b_val     <- strtoi(substr(hex, 5L, 6L), 16L)

      p <- plotly::plot_ly(cone, x = ~horizon) %>%
        plotly::add_ribbons(
          ymin = ~p25, ymax = ~p75,
          name = "25th–75th pct",
          fillcolor = sprintf("rgba(%d,%d,%d,0.20)", r_val, g_val, b_val),
          line = list(width = 0)
        ) %>%
        plotly::add_ribbons(
          ymin = ~p10, ymax = ~p90,
          name = "10th–90th pct",
          fillcolor = sprintf("rgba(%d,%d,%d,0.08)", r_val, g_val, b_val),
          line = list(width = 0)
        ) %>%
        plotly::add_lines(
          y = ~p50, name = "Median",
          line = list(color = mkt_color, width = 2)
        )

      if (!is.na(c1_current)) {
        p <- plotly::add_markers(p,
          x = sel$window(), y = c1_current,
          name = "Current",
          marker = list(color = "red", size = 10, symbol = "diamond")
        )
      }

      p %>%
        plotly::layout(
          title  = list(text = glue::glue("{mkt_label} \u2014 Volatility Cone")),
          xaxis  = list(title = "Lookback window (trading days)"),
          yaxis  = list(title = glue::glue("{mkt_label} Ann. Realised Vol"),
                        tickformat = ".0%"),
          legend = list(orientation = "h")
        )
    }) %>% bindEvent(wide(), sel$window())
  })
}
