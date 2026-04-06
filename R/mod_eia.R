# mod_eia.R
# Simplified EIA fundamentals tab.
# Shows U.S.-level weekly stocks (or storage for natural gas) as a line chart.
# Data loaded from inst/extdata/eia_fundamentals.feather (nightly refresh).

EIA_COMMODITY_LABELS <- c(
  crude       = "Crude Oil",
  distillate  = "Distillate / Heating Oil",
  gasoline    = "Motor Gasoline",
  natural_gas = "Natural Gas"
)

# Primary series type per commodity for the US-level chart
.EIA_PRIMARY_TYPE <- c(
  crude       = "stocks",
  distillate  = "stocks",
  gasoline    = "stocks",
  natural_gas = "storage"
)

mod_eia_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls",
      selectInput(ns("commodity"), "Commodity",
        choices  = unname(EIA_COMMODITY_LABELS),
        selected = "crude"
      ),
      dateRangeInput(ns("date_range"), "Date range",
        start = Sys.Date() - 365 * 5,
        end   = Sys.Date()
      ),
      actionButton(ns("full_history"), "Full history",
        class = "btn btn-sm btn-outline-secondary w-100 mb-2"
      )
    ),
    uiOutput(ns("staleness_banner")),
    plotly::plotlyOutput(ns("main_chart"), height = "500px", width = "100%")
  )
}

mod_eia_server <- function(id, eia_data) {
  moduleServer(id, function(input, output, session) {

    # ── Staleness banner ───────────────────────────────────────────────────────
    output$staleness_banner <- renderUI({
      ed <- eia_data()
      if (is.null(ed$data)) {
        bslib::card(
          class = "bg-danger text-white mb-2",
          bslib::card_body(
            bsicons::bs_icon("exclamation-triangle-fill"), " ",
            "EIA data file not found. Run ",
            shiny::tags$code("Rscript dev/fetch_eia_data.R"), " locally."
          )
        )
      } else if (isTRUE(ed$is_stale)) {
        bslib::card(
          class = "bg-warning mb-2",
          bslib::card_body(
            bsicons::bs_icon("clock-history"), " ",
            paste0("Data may be stale (last updated: ", ed$last_updated, ").")
          )
        )
      } else {
        NULL
      }
    })

    observeEvent(input$full_history, {
      updateDateRangeInput(session, "date_range",
        start = as.Date("2000-01-01"), end = Sys.Date()
      )
    })

    # ── Filtered data: US-level primary series, weekly preferred ───────────────
    primary_df <- reactive({
      ed <- eia_data()
      req(!is.null(ed$data))

      stype <- .EIA_PRIMARY_TYPE[[input$commodity]]

      df <- ed$data %>%
        dplyr::filter(
          commodity   == input$commodity,
          series_type == stype,
          area        == "US",
          date        >= input$date_range[1],
          date        <= input$date_range[2]
        )

      # Prefer weekly when both frequencies exist
      if ("frequency" %in% names(df) && "weekly" %in% df$frequency) {
        df <- dplyr::filter(df, frequency == "weekly")
      }

      dplyr::arrange(df, date)
    })

    # ── Main chart: simple line ────────────────────────────────────────────────
    output$main_chart <- plotly::renderPlotly({
      df <- primary_df()
      req(nrow(df) > 0)

      lbl   <- EIA_COMMODITY_LABELS[[input$commodity]]
      units <- df$units[[1]]

      plotly::plot_ly(
        data          = df,
        x             = ~date,
        y             = ~value,
        type          = "scatter",
        mode          = "lines",
        line          = list(color = "#1f77b4", width = 2),
        hovertemplate = paste0("%{x|%Y-%m-%d}: %{y:,.0f} ", units, "<extra></extra>")
      ) %>%
        plotly::layout(
          title  = list(text = paste0(lbl, " \u2014 U.S. Stocks")),
          xaxis  = list(title = ""),
          yaxis  = list(title = units)
        )
    })

  })
}
