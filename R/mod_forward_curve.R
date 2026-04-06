# mod_forward_curve.R
# Forward curve analysis: snapshot, regime classification, roll yield, history.

mod_forward_curve_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls",
      mod_market_selector_ui(ns("sel"), show_window = FALSE, show_contracts = FALSE),
      hr(),
      dateInput(ns("snap_date"), "Snapshot date", value = Sys.Date() - 1L),
      dateInput(ns("comp_date"), "Compare to (optional)", value = NULL),
      numericInput(ns("back_c"), "Back contract (slope)", value = 12L, min = 2L, max = 36L),
      hr(),
      # ── EIA Fundamentals overlay controls ──────────────────────────────────
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          title = "EIA Fundamentals",
          icon  = bsicons::bs_icon("database"),
          selectInput(ns("eia_series"), "Overlay series",
            choices = c(
              "None"       = "none",
              "Stocks"     = "stocks",
              "Production" = "production",
              "Imports"    = "imports",
              "Exports"    = "exports"
            ),
            selected = "none"
          ),
          conditionalPanel(
            condition = paste0("input['", ns("eia_series"), "'] !== 'none'"),
            selectInput(ns("eia_area"), "Region",
              choices = c("U.S." = "US")  # updated dynamically by observer
            )
          )
        )
      )
    ),
    # ── Value boxes (dynamic per selected market) ─────────────────────────────
    uiOutput(ns("value_boxes")),
    # ── Charts ───────────────────────────────────────────────────────────────
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Curve Snapshot",
        plotly::plotlyOutput(ns("curve_snapshot"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Curve History (Fan)",
        plotly::plotlyOutput(ns("fwd_fan"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Regime History",
        plotly::plotlyOutput(ns("regime_history"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Roll Yield",
        plotly::plotlyOutput(ns("roll_yield_plot"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Fundamentals Overlay",
        uiOutput(ns("eia_status")),
        plotly::plotlyOutput(ns("eia_overlay"), height = "70%", width = "100%")
      )
    )
  )
}

mod_forward_curve_server <- function(id, mkt_data, eia_data = NULL) {
  moduleServer(id, function(input, output, session) {
    sel <- mod_market_selector_server("sel")

    # ── Named list of filtered wide data, one entry per selected market ──────
    wides <- reactive({
      mkts <- sel$market()
      req(length(mkts) > 0)
      setNames(lapply(mkts, function(m) {
        req(mkt_data()[[m]])
        filter_date_range(mkt_data()[[m]]$wide, sel$date_from(), sel$date_to())
      }), mkts)
    }) %>% bindEvent(sel$market(), sel$date_from(), sel$date_to())

    # Cap back_c to min max_contracts across selected markets
    observeEvent(sel$market(), {
      mkts <- sel$market()
      if (length(mkts) > 0L) {
        max_c <- min(sapply(mkts, function(m) MARKETS[[m]]$max_contracts))
        updateNumericInput(session, "back_c",
                           max   = max_c,
                           value = min(input$back_c, max_c))
      }
    }, ignoreInit = TRUE)

    # ── Per-market snapshot metrics ───────────────────────────────────────────
    mkt_metrics <- reactive({
      mkts   <- sel$market()
      req(length(mkts) > 0L, isTruthy(input$back_c))
      w_list <- wides()

      lapply(setNames(mkts, mkts), function(m) {
        w   <- w_list[[m]]
        pfx <- MARKETS[[m]]$rtl_prefix
        u   <- MARKETS[[m]]$unit

        dates <- w$date
        d     <- input$snap_date
        sd    <- if (length(dates) == 0L) {
          as.Date(NA)
        } else if (is.na(d)) {
          max(dates)
        } else {
          dates[which.min(abs(as.numeric(dates - d)))]
        }

        sl <- if (!is.na(sd)) compute_curve_slope(w, pfx, sd, back = input$back_c) else NA_real_
        rg <- classify_curve_shape(sl)
        ry_df <- compute_roll_yield(w, pfx)
        ry <- if (!is.null(ry_df) && nrow(ry_df) > 0L) tail(ry_df$roll_yield, 1L) else NA_real_

        list(label = MARKETS[[m]]$label, unit = u, snap_date = sd,
             slope = sl, regime = rg, roll_yield = ry)
      })
    })

    # ── Value boxes: one row of three per selected market ────────────────────
    output$value_boxes <- renderUI({
      metrics <- mkt_metrics()
      req(metrics)

      rows <- lapply(metrics, function(mm) {
        bslib::layout_column_wrap(
          width = 1/3,
          bslib::value_box(
            title    = paste0(mm$label, " \u2014 Curve Regime"),
            value    = mm$regime,
            showcase = bsicons::bs_icon("bar-chart-steps")
          ),
          bslib::value_box(
            title    = paste0(mm$label, " C1\u2013C", input$back_c, " Slope (", mm$unit, ")"),
            value    = if (is.na(mm$slope)) "N/A" else sprintf("%+.2f %s", mm$slope, mm$unit),
            showcase = bsicons::bs_icon("arrows-expand")
          ),
          bslib::value_box(
            title    = paste0(mm$label, " \u2014 Roll Yield (ann.)"),
            value    = if (is.na(mm$roll_yield)) "N/A" else scales::percent(mm$roll_yield, accuracy = 0.1),
            showcase = bsicons::bs_icon("arrow-repeat")
          )
        )
      })
      do.call(tagList, rows)
    })

    # ── Curve Snapshot ────────────────────────────────────────────────────────
    output$curve_snapshot <- plotly::renderPlotly({
      metrics <- mkt_metrics()
      req(metrics)
      w_list <- wides()
      comp_d <- input$comp_date

      p <- plotly::plot_ly()
      for (m in names(metrics)) {
        mm  <- metrics[[m]]
        pfx <- MARKETS[[m]]$rtl_prefix
        col <- MARKETS[[m]]$color
        if (is.na(mm$snap_date)) next

        snap <- build_curve_snapshot(w_list[[m]], pfx, mm$snap_date)
        if (is.null(snap)) next

        p <- plotly::add_lines(p,
          data          = snap, x = ~contract, y = ~price,
          name          = paste0(mm$label, " (", format(mm$snap_date, "%b %d, %Y"), ")"),
          line          = list(color = col, width = 2.5),
          hovertemplate = paste0(mm$label, " C%{x}: %{y:.2f}<extra></extra>")
        )

        if (isTruthy(comp_d)) {
          comp_snap <- build_curve_snapshot(w_list[[m]], pfx, comp_d)
          if (!is.null(comp_snap)) {
            p <- plotly::add_lines(p,
              data          = comp_snap, x = ~contract, y = ~price,
              name          = paste0(mm$label, " (", format(comp_d, "%b %d, %Y"), ")"),
              line          = list(color = col, width = 2, dash = "dash"),
              hovertemplate = paste0(mm$label, " C%{x}: %{y:.2f}<extra></extra>")
            )
          }
        }
      }

      p %>% plotly::layout(
        xaxis     = list(title = "Contract", dtick = 3),
        yaxis     = list(title = "Price"),
        legend    = list(orientation = "h"),
        hovermode = "x unified"
      )
    }) %>% bindEvent(mkt_metrics(), input$comp_date)

    # ── Regime History ────────────────────────────────────────────────────────
    # One stacked bar chart per market, stacked vertically and sharing the x-axis.
    # Traces use legendgroup so clicking a regime hides it across all market panels.
    # Only the first market's traces appear in the legend to avoid duplicates.
    output$regime_history <- plotly::renderPlotly({
      req(isTruthy(input$back_c))
      w_list <- wides()
      mkts   <- names(w_list)

      regime_colors <- c(
        "Steep Backwardation" = "#1a7340",
        "Mild Backwardation"  = "#52a36b",
        "Flat"                = "#adb5bd",
        "Mild Contango"       = "#e07b39",
        "Steep Contango"      = "#c0392b"
      )
      regime_order <- names(regime_colors)

      plots <- list()
      for (i in seq_along(mkts)) {
        m    <- mkts[i]
        hist <- curve_slope_history(w_list[[m]], MARKETS[[m]]$rtl_prefix, back = input$back_c)
        if (is.null(hist)) next
        fracs <- regime_fractions_by_year(hist)
        lbl   <- MARKETS[[m]]$label

        p <- plotly::plot_ly()
        for (rg in regime_order) {
          rg_data <- dplyr::filter(fracs, regime == rg)
          if (nrow(rg_data) == 0L) next
          p <- plotly::add_bars(p,
            data          = rg_data, x = ~year, y = ~fraction,
            name          = rg,
            legendgroup   = rg,
            showlegend    = (i == 1L),   # only first market adds to legend
            marker        = list(color = regime_colors[[rg]]),
            hovertemplate = paste0(lbl, " %{x} %{fullData.name}: %{y:.1%}<extra></extra>")
          )
        }
        p <- plotly::layout(p,
          barmode = "stack",
          yaxis   = list(title = paste0(lbl, " \u2014 Fraction"), tickformat = ".0%")
        )
        plots <- c(plots, list(p))
      }

      req(length(plots) > 0L)
      plotly::subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE) %>%
        plotly::layout(
          xaxis  = list(title = "Year"),
          legend = list(orientation = "h", y = -0.1)
        )
    }) %>% bindEvent(wides(), input$back_c)

    # ── Curve History (Fan) ───────────────────────────────────────────────────
    # One C1 spine + forward curve fan per market, all overlaid on a shared time axis.
    output$fwd_fan <- plotly::renderPlotly({
      w_list <- wides()
      mkts   <- names(w_list)

      p <- plotly::plot_ly()
      for (m in mkts) {
        w   <- w_list[[m]]
        pfx <- MARKETS[[m]]$rtl_prefix
        lbl <- MARKETS[[m]]$label
        if (nrow(w) <= 50L) next

        c1_col <- paste0(pfx, "_C1")
        if (!c1_col %in% names(w)) next

        c1_ts <- dplyr::select(w, date, c1 = dplyr::all_of(c1_col)) %>%
          dplyr::filter(!is.na(c1))
        if (nrow(c1_ts) <= 2L) next

        step       <- max(63L, nrow(c1_ts) %/% 30L)
        sample_idx <- seq(1L, nrow(c1_ts), by = step)
        if (utils::tail(sample_idx, 1L) != nrow(c1_ts))
          sample_idx <- c(sample_idx, nrow(c1_ts))
        sampled_dates <- c1_ts$date[sample_idx]

        fan_df <- purrr::map_dfr(sampled_dates, function(d) {
          snap <- build_curve_snapshot(w, pfx, d, max_c = 12L)
          if (is.null(snap) || nrow(snap) == 0L) return(NULL)
          dplyr::mutate(snap,
            snap_date     = d,
            delivery_date = d + lubridate::days(as.integer(round((contract - 1L) * 30.44)))
          )
        })
        if (is.null(fan_df) || nrow(fan_df) == 0L) next

        mkt_color <- MARKETS[[m]]$color
        hex       <- sub("^#", "", mkt_color)
        r_val     <- strtoi(substr(hex, 1L, 2L), 16L)
        g_val     <- strtoi(substr(hex, 3L, 4L), 16L)
        b_val     <- strtoi(substr(hex, 5L, 6L), 16L)
        rib_rgba  <- sprintf("rgba(%d,%d,%d,0.18)", r_val, g_val, b_val)

        hist_dates <- sampled_dates[-length(sampled_dates)]
        rib_rows   <- purrr::map_dfr(hist_dates, function(d) {
          seg <- dplyr::filter(fan_df, snap_date == d, !is.na(price)) %>%
            dplyr::arrange(contract)
          if (nrow(seg) < 2L) return(NULL)
          dplyr::bind_rows(
            dplyr::select(seg, delivery_date, price),
            data.frame(delivery_date = as.Date(NA), price = NA_real_)
          )
        })

        latest_date  <- max(sampled_dates)
        latest_curve <- dplyr::filter(fan_df, snap_date == latest_date, !is.na(price)) %>%
          dplyr::arrange(contract)

        p <- plotly::add_lines(p,
          data          = c1_ts, x = ~date, y = ~c1,
          name          = paste0(lbl, " front month"),
          line          = list(color = mkt_color, width = 2),
          hovertemplate = paste0(lbl, " %{x|%Y-%m-%d}: %{y:.2f}<extra></extra>")
        )

        if (!is.null(rib_rows) && nrow(rib_rows) > 0L) {
          p <- plotly::add_lines(p,
            data        = rib_rows, x = ~delivery_date, y = ~price,
            name        = paste0(lbl, " forward curves (sampled)"),
            line        = list(color = rib_rgba, width = 1),
            connectgaps = FALSE, hoverinfo = "skip"
          )
        }

        if (nrow(latest_curve) > 0L) {
          p <- plotly::add_lines(p,
            data          = latest_curve, x = ~delivery_date, y = ~price,
            name          = paste0(lbl, " latest curve (", format(latest_date, "%b %d, %Y"), ")"),
            line          = list(color = mkt_color, width = 2.5, dash = "dash"),
            customdata    = ~contract,
            hovertemplate = paste0(lbl, " C%{customdata}: %{y:.2f}<extra></extra>")
          )
        }
      }

      p %>% plotly::layout(
        xaxis     = list(title = "Date / Approximate Delivery Month"),
        yaxis     = list(title = "Price"),
        legend    = list(orientation = "h"),
        hovermode = "x unified"
      )
    }) %>% bindEvent(wides())

    # ── EIA overlay: sync series choices to first selected market ────────────
    # Updates the "Overlay series" dropdown to only show series that make sense
    # for the currently selected commodity (e.g., NG only has Storage).
    observeEvent(sel$market(), {
      mkts <- sel$market()
      if (length(mkts) == 0L) return()
      eia_commodity <- MARKET_TO_EIA[mkts[1L]]

      choices <- switch(eia_commodity,
        crude       = c("None"="none","Stocks"="stocks","Production"="production",
                        "Imports"="imports","Exports"="exports"),
        distillate  = c("None"="none","Stocks"="stocks"),
        gasoline    = c("None"="none","Stocks"="stocks"),
        natural_gas = c("None"="none","Storage"="storage"),
        c("None"="none")
      )
      cur <- input$eia_series
      updateSelectInput(session, "eia_series", choices = choices,
                        selected = if (cur %in% choices) cur else "none")
    }, ignoreInit = TRUE)

    # Updates the "Region" dropdown based on the selected series type.
    observeEvent(list(sel$market(), input$eia_series), {
      req(input$eia_series != "none")
      mkts <- sel$market()
      if (length(mkts) == 0L) return()
      eia_commodity <- MARKET_TO_EIA[mkts[1L]]

      choices <- EIA_AREA_CHOICES[[eia_commodity]][[input$eia_series]]
      if (is.null(choices)) choices <- c("U.S." = "US")
      updateSelectInput(session, "eia_area", choices = choices,
                        selected = if ("US" %in% choices) "US" else choices[[1L]])
    }, ignoreInit = TRUE)

    # ── EIA status banner ─────────────────────────────────────────────────────
    output$eia_status <- renderUI({
      if (is.null(eia_data)) return(NULL)
      ed <- eia_data()
      if (input$eia_series == "none") return(NULL)
      if (is.null(ed$data)) {
        bslib::card(class = "bg-danger text-white mb-2", bslib::card_body(
          bsicons::bs_icon("exclamation-triangle-fill"), " ",
          "EIA data not available. Run Rscript dev/fetch_eia_data.R to populate the cache."
        ))
      } else if (isTRUE(ed$is_stale)) {
        bslib::card(class = "bg-warning mb-2", bslib::card_body(
          bsicons::bs_icon("clock-history"), " ",
          paste0("EIA data may be stale (last updated: ",
                 if (!is.null(ed$last_updated)) ed$last_updated else "unknown", ").")
        ))
      }
    })

    # ── Fundamentals Overlay chart ─────────────────────────────────────────────
    # Left y-axis: EIA fundamental (stocks / production / imports / exports / storage)
    # Right y-axis: Futures C1 front-month price for the first selected market
    output$eia_overlay <- plotly::renderPlotly({
      req(!is.null(eia_data), input$eia_series != "none")
      ed <- eia_data()
      req(!is.null(ed$data))

      mkts <- sel$market()
      req(length(mkts) > 0L)
      m   <- mkts[1L]
      lbl <- MARKETS[[m]]$label
      col <- MARKETS[[m]]$color

      eia_commodity <- MARKET_TO_EIA[m]
      st            <- input$eia_series  # "stocks","storage","production","imports","exports"

      eia_df <- ed$data %>%
        dplyr::filter(commodity   == eia_commodity,
                      series_type == st,
                      area        == input$eia_area,
                      date        >= sel$date_from(),
                      date        <= sel$date_to()) %>%
        dplyr::arrange(date)

      # When both weekly and monthly exist for the same series/area,
      # prefer weekly for the overlay (higher frequency = smoother chart)
      if ("frequency" %in% names(eia_df) && length(unique(eia_df$frequency)) > 1L) {
        eia_df <- dplyr::filter(eia_df, frequency == "weekly")
      }

      req(nrow(eia_df) > 0L)

      # Price: C1 from already-filtered wide data
      w      <- wides()[[m]]
      c1_col <- paste0(MARKETS[[m]]$rtl_prefix, "_C1")
      req(c1_col %in% names(w))
      price_ts <- dplyr::select(w, date, price = dplyr::all_of(c1_col)) %>%
        dplyr::filter(!is.na(price))

      units     <- eia_df$units[1L]
      area_lbl  <- eia_df$area_label[1L]
      eia_color <- "#636efa"

      plotly::plot_ly() %>%
        plotly::add_lines(
          data          = eia_df, x = ~date, y = ~value,
          name          = paste0(area_lbl, " ", tools::toTitleCase(st), " (", units, ")"),
          yaxis         = "y",
          line          = list(color = eia_color, width = 1.5),
          hovertemplate = paste0(area_lbl, " %{x|%Y-%m-%d}: %{y:,.0f} ", units, "<extra></extra>")
        ) %>%
        plotly::add_lines(
          data          = price_ts, x = ~date, y = ~price,
          name          = paste0(lbl, " C1 (", MARKETS[[m]]$unit, ")"),
          yaxis         = "y2",
          line          = list(color = col, width = 1.5),
          hovertemplate = paste0(lbl, " C1 %{x|%Y-%m-%d}: %{y:.2f}<extra></extra>")
        ) %>%
        plotly::layout(
          xaxis     = list(title = ""),
          yaxis     = list(title = paste0(units, " \u2014 ", area_lbl), side = "left"),
          yaxis2    = list(title = paste0(MARKETS[[m]]$unit, " \u2014 ", lbl, " C1"),
                           overlaying = "y", side = "right"),
          legend    = list(orientation = "h"),
          hovermode = "x unified"
        )
    }) %>% bindEvent(wides(), input$eia_series, input$eia_area)

    # ── Roll Yield ─────────────────────────────────────────────────────────────
    output$roll_yield_plot <- plotly::renderPlotly({
      w_list <- wides()
      mkts   <- names(w_list)

      p <- plotly::plot_ly()
      for (m in mkts) {
        ry  <- compute_roll_yield(w_list[[m]], MARKETS[[m]]$rtl_prefix)
        if (is.null(ry)) next
        lbl <- MARKETS[[m]]$label
        col <- MARKETS[[m]]$color
        p <- plotly::add_lines(p,
          data          = ry, x = ~date, y = ~roll_yield,
          name          = lbl,
          line          = list(color = col, width = 1.5),
          hovertemplate = paste0(lbl, " %{x|%Y-%m-%d}: %{y:.1%}<extra></extra>")
        )
      }

      p %>% plotly::layout(
        xaxis  = list(title = ""),
        yaxis  = list(title = "Annualised roll yield", tickformat = ".1%"),
        legend = list(orientation = "h"),
        shapes = list(list(
          type = "line", xref = "paper", x0 = 0, x1 = 1,
          y0 = 0, y1 = 0,
          line = list(color = "#adb5bd", dash = "dot", width = 1)
        ))
      )
    }) %>% bindEvent(wides())
  })
}
