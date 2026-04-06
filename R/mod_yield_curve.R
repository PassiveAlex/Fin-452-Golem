# mod_yield_curve.R
# Simplified yield curve tab using FRED zero curve panel.
# Shows key maturity yields as time series lines and the 2s10s spread.

mod_yield_curve_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls",
      dateRangeInput(
        ns("date_range"), "Date range",
        start = Sys.Date() - 365 * 5,
        end   = Sys.Date()
      ),
      actionButton(
        ns("full_history"), "Full history",
        class = "btn btn-sm btn-outline-secondary w-100 mb-2"
      )
    ),
    uiOutput(ns("no_data_banner")),
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Yield History",
        plotly::plotlyOutput(ns("yield_history"), height = "500px", width = "100%")
      ),
      bslib::nav_panel(
        "2s10s Spread",
        plotly::plotlyOutput(ns("two_ten"), height = "500px", width = "100%")
      )
    )
  )
}

mod_yield_curve_server <- function(id, zero_curve_panel) {
  moduleServer(id, function(input, output, session) {

    has_fred_data <- reactive({
      panel <- zero_curve_panel()
      !is.null(panel) && nrow(panel) > 0L
    })

    # Banner shown when FRED data could not be loaded
    output$no_data_banner <- renderUI({
      if (isTRUE(has_fred_data())) return(NULL)
      bslib::card(
        class = "bg-warning mb-2",
        bslib::card_body(
          bsicons::bs_icon("exclamation-triangle-fill"), " ",
          "Treasury yield data could not be loaded from FRED. ",
          "Charts will be empty until data is available."
        )
      )
    })

    observeEvent(input$full_history, {
      updateDateRangeInput(session, "date_range",
        start = as.Date("2000-01-01"),
        end   = Sys.Date()
      )
    })

    # Panel filtered to selected date range
    panel_ranged <- reactive({
      req(has_fred_data())
      panel <- zero_curve_panel()
      dplyr::filter(panel,
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
    })

    # ── Yield History: lines for 3M, 2Y, 5Y, 10Y, 30Y ───────────────────────
    output$yield_history <- plotly::renderPlotly({
      req(has_fred_data())
      df <- panel_ranged()
      req(nrow(df) > 0L)

      # Key maturities (numeric years) and their display labels
      key_mats   <- c(0.25, 2, 5, 10, 30)
      mat_labels <- c("0.25" = "3M", "2" = "2Y", "5" = "5Y", "10" = "10Y", "30" = "30Y")
      mat_colors <- c("3M" = "#adb5bd", "2Y" = "#ff7f0e", "5Y" = "#2ca02c",
                      "10Y" = "#1f77b4", "30Y" = "#9467bd")

      # Round maturities for matching (floating-point safety)
      df_key <- df %>%
        dplyr::mutate(mat_r = round(maturity, 4)) %>%
        dplyr::filter(mat_r %in% round(key_mats, 4)) %>%
        dplyr::mutate(
          mat_label = mat_labels[as.character(mat_r)]
        ) %>%
        dplyr::filter(!is.na(mat_label))

      req(nrow(df_key) > 0L)

      p <- plotly::plot_ly()
      for (lbl in names(mat_labels)) {
        sub <- dplyr::filter(df_key, mat_label == mat_labels[[lbl]]) %>%
          dplyr::arrange(date)
        if (nrow(sub) == 0L) next
        p <- plotly::add_lines(p,
          data          = sub,
          x             = ~date,
          y             = ~(yield * 100),
          name          = mat_labels[[lbl]],
          line          = list(color = mat_colors[[mat_labels[[lbl]]]], width = 1.5),
          hovertemplate = paste0(mat_labels[[lbl]], " %{x|%Y-%m-%d}: %{y:.2f}%<extra></extra>")
        )
      }

      p %>% plotly::layout(
        title     = list(text = "U.S. Treasury Yields"),
        xaxis     = list(title = ""),
        yaxis     = list(title = "Yield (%)"),
        legend    = list(orientation = "h"),
        hovermode = "x unified"
      )
    })

    # ── 2s10s Spread ──────────────────────────────────────────────────────────
    output$two_ten <- plotly::renderPlotly({
      req(has_fred_data())
      df <- panel_ranged()
      req(nrow(df) > 0L)

      y2_df  <- dplyr::filter(df, maturity == 2)[, c("date", "yield")]
      y10_df <- dplyr::filter(df, maturity == 10)[, c("date", "yield")]
      names(y2_df)[[2]]  <- "y2"
      names(y10_df)[[2]] <- "y10"

      spread_df <- dplyr::inner_join(y2_df, y10_df, by = "date") %>%
        dplyr::mutate(spread = (y10 - y2) * 100) %>%
        dplyr::arrange(date)

      req(nrow(spread_df) > 0L)

      plotly::plot_ly(
        data          = spread_df,
        x             = ~date,
        y             = ~spread,
        type          = "scatter",
        mode          = "lines",
        line          = list(color = "#1f77b4", width = 1.5),
        hovertemplate = "%{x|%Y-%m-%d}: %{y:.2f}%<extra></extra>"
      ) %>%
        plotly::layout(
          title  = list(text = "2s10s Spread (10Y \u2212 2Y)"),
          xaxis  = list(title = ""),
          yaxis  = list(title = "Spread (%)"),
          shapes = list(list(
            type = "line", x0 = 0, x1 = 1, xref = "paper",
            y0   = 0, y1 = 0,
            line = list(color = "#888", dash = "dot", width = 1)
          ))
        )
    })

  })
}
