library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(dplyr)
library(sf)

# ── Load data ────────────────────────────────────────────────────────────────
df_joined <- sf::read_sf("data/df_joined.gpkg")

us_states <- map_data("state")
western <- us_states |>
  filter(region %in% c("oregon", "california", "nevada", "arizona",
                        "wyoming", "colorado", "utah", "montana", "idaho",
                        "washington", "new mexico"))

western_states <- sort(unique(tools::toTitleCase(western$region)))

# ── Drought classification ────────────────────────────────────────────────────
drought_status <- function(pct) {
  if (is.na(pct)) {
    list(label = "No Data", color = "secondary",
         icon = bs_icon("question-circle"),
         msg  = "No SWE data is available for this watershed.")
  } else if (pct < 25) {
    list(label = "Severe Drought Likely", color = "danger",
         icon = bs_icon("thermometer-sun"),
         msg  = paste0("At ", pct, "% of the 30-year median, snowpack is critically low. ",
                       "Significant drought conditions are probable for the coming season."))
  } else if (pct < 50) {
    list(label = "Drought Possible", color = "warning",
         icon = bs_icon("exclamation-triangle"),
         msg  = paste0("At ", pct, "% of the 30-year median, snowpack is well below normal. ",
                       "Drought conditions are possible if dry weather persists."))
  } else if (pct < 75) {
    list(label = "Below Normal", color = "info",
         icon = bs_icon("cloud-drizzle"),
         msg  = paste0("At ", pct, "% of the 30-year median, snowpack is below normal. ",
                       "Monitor conditions; drought is unlikely but possible in dry years."))
  } else if (pct <= 100) {
    list(label = "Near Normal", color = "success",
         icon = bs_icon("check-circle"),
         msg  = paste0("At ", pct, "% of the 30-year median, snowpack is near normal. ",
                       "Drought conditions are unlikely for the coming season."))
  } else {
    list(label = "Above Normal", color = "primary",
         icon = bs_icon("snow"),
         msg  = paste0("At ", pct, "% of the 30-year median, snowpack is above normal. ",
                       "Water supply should be plentiful; drought is not expected."))
  }
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = "Western US Drought Predictor",
  theme = bs_theme(
    version    = 5
  ),
  fillable = FALSE,

  # ── Map Panel ──────────────────────────────────────────────────────────────
  nav_panel(
    "SWE Map",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "state", "State",
          choices  = c("All Western States", western_states),
          selected = "All Western States"
        ),
        conditionalPanel(
          condition = "input.state !== 'All Western States'",
          selectInput("watershed", "Watershed", choices = NULL)
        ),
        p("SWE = Snow Water Equivalent. Values show current SWE as a percentage",
          " of the 1991–2020 median. Data from NRCS.")
      ),
      # Main content
      card(
        plotOutput("swe_map", height = "520px")
      ),
      # Drought status (shown when a watershed is selected)
      conditionalPanel(
        condition = "input.state !== 'All Western States'",
        uiOutput("value_boxes"),
        card(
          card_body(textOutput("drought_msg"))
        )
      )
    )
  ),

  # ── About Panel ────────────────────────────────────────────────────────────
  nav_panel(
    "About",
    card(
      max_width = "800px",
      card_body(
        h5("What is Snow Water Equivalent (SWE)?"),
        p("Snow Water Equivalent (SWE) is the depth of water that would result if all the snow in a ",
          "snowpack were melted. It is one of the most reliable indicators of coming-season water ",
          "availability across the western United States."),
        h5("How is Drought Predicted?"),
        p("This app compares current SWE measurements at NRCS SNOTEL stations to the 1991–2020 ",
          "30-year median for each HUC4 watershed. Watersheds well below the historical median are ",
          "at elevated drought risk."),
        h5("Drought Classifications"),
        tableOutput("class_table"),
        h5("Data Sources"),
        tags$ul(
          tags$li(tags$a("NRCS Western US SWE Report", href = "https://wcc.sc.egov.usda.gov/reports/UpdateReport.html?report=Western+US", target = "_blank")),
          tags$li("USGS NHDPlus HUC4 watershed boundaries via nhdplusTools")
        ),
        p(class = "text-muted small", "Built with R, Shiny, bslib, ggplot2, and sf.")
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Update watershed choices when state changes
  observeEvent(input$state, {
    req(input$state != "All Western States")
    code <- state.abb[match(input$state, state.name)]
    choices <- df_joined |>
      filter(sapply(strsplit(states, ","), \(s) code %in% s)) |>
      arrange(name) |>
      pull(name)
    updateSelectInput(session, "watershed", choices = choices)
  })

  # Reactive: selected watershed row
  selected_ws <- reactive({
    req(input$state != "All Western States", input$watershed)
    df_joined |> filter(name == input$watershed)
  })

  # ── Map ──────────────────────────────────────────────────────────────────
  output$swe_map <- renderPlot({
    # Determine which basins to highlight
    if (input$state == "All Western States") {
      highlight <- NULL
    } else {
      code <- state.abb[match(input$state, state.name)]
      highlight <- df_joined |>
        filter(sapply(strsplit(states, ","), function(s) code %in% s))
    }

    p <- ggplot() +
      geom_polygon(data = western,
                   aes(x = long, y = lat, group = group),
                   fill = "gray92", color = "white", linewidth = 0.3) +
      geom_sf(data = df_joined, aes(fill = SWE_pct_median),
              color = "white", linewidth = 0.4) +
      scale_fill_gradient2(
        low      = "red3",
        mid      = "white",
        high     = "steelblue",
        midpoint = 100,
        limits   = c(0, 150),
        na.value = "gray75",
        name     = "% Median SWE"
      ) +
      coord_sf(xlim = c(-125, -102), ylim = c(30, 50)) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "right") +
      labs(x = NULL, y = NULL)

    # Outline highlighted state's watersheds
    if (!is.null(highlight) && nrow(highlight) > 0) {
      p <- p +
        geom_sf(data = highlight,
                fill  = NA, color = "black", linewidth = .5)
    }

    # Bold outline on selected watershed
    if (input$state != "All Western States" && !is.null(input$watershed)) {
      sel <- df_joined |> filter(name == input$watershed)
      if (nrow(sel) > 0) {
        p <- p +
          geom_sf(data = sel,
                  fill = NA, color = "yellow2", linewidth = .5)
      }
    }

    p
  }, res = 110)

  # ── Value boxes (rendered dynamically so theme can change) ───────────────
  ds <- reactive({
    ws <- selected_ws()
    drought_status(ws$SWE_pct_median)
  })

  output$value_boxes <- renderUI({
    req(input$watershed)
    ws  <- selected_ws()
    pct <- ws$SWE_pct_median
    d   <- ds()
    pct_label <- if (is.na(pct)) "—" else paste0(pct, "%")
    layout_column_wrap(
      width = 1 / 2,
      fill  = FALSE,
      value_box(
        title    = "% of Median SWE",
        value    = pct_label,
        theme    = d$color,
        showcase = bs_icon("bar-chart-line")
      ),
      value_box(
        title    = "Drought Outlook",
        value    = d$label,
        theme    = d$color,
        showcase = d$icon
      )
    )
  })

  output$drought_msg <- renderText(ds()$msg)

  # ── Classification table (About tab) ─────────────────────────────────────
  output$class_table <- renderTable({
    data.frame(
      `SWE % of Median` = c("≥ 100%", "75 – 99%", "50 – 74%", "25 – 49%", "< 25%", "No data"),
      `Drought Outlook`  = c("Above Normal", "Near Normal", "Below Normal",
                             "Drought Possible", "Severe Drought Likely", "Insufficient data")
    )
  })
}

shinyApp(ui, server)
