library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(dplyr)
library(sf)
library(nhdplusTools)
library(DT)

# load data
df_joined <- sf::read_sf("data/df_joined.gpkg")

western <- map_data("state") |>
  filter(region %in% c("oregon", "california", "nevada", "arizona",
                        "wyoming", "colorado", "utah", "montana", "idaho",
                        "washington", "new mexico"))

state_names <- sort(unique(tools::toTitleCase(western$region)))

# classify snowpack based on % of 30-year median
drought_label <- function(pct) {
  if (is.na(pct)) return(list(label = "No Data",               color = "secondary", msg = "No SWE data available for this watershed."))
  if (pct < 25)   return(list(label = "Severe Drought Likely", color = "danger",    msg = paste0(pct, "% of median — snowpack is critically low. Significant drought is probable.")))
  if (pct < 50)   return(list(label = "Drought Possible",      color = "warning",   msg = paste0(pct, "% of median — well below normal. Drought is possible if dry weather continues.")))
  if (pct < 75)   return(list(label = "Below Normal",          color = "info",      msg = paste0(pct, "% of median — below normal. Drought unlikely but worth monitoring.")))
  if (pct <= 100) return(list(label = "Near Normal",           color = "success",   msg = paste0(pct, "% of median — snowpack is near normal. Drought is unlikely.")))
                         list(label = "Above Normal",          color = "primary",   msg = paste0(pct, "% of median — above normal. Water supply should be plentiful."))
}

ui <- page_navbar(
  title    = "Western US Drought Predictor",
  theme    = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#2c7fb8"
  ),
  fillable = FALSE,
  header   = tags$style(HTML("
    .collapse-toggle { display: none !important; }
    .navbar { border-bottom: 2px solid #2c7fb8; }
    .modal-header { background-color: #2c7fb8; color: white; }
    .modal-header .btn-close { filter: invert(1); }
  ")),

  nav_panel("SWE Map",
    layout_sidebar(
      sidebar = sidebar(
        width = 240,
        selectInput("state", "State",
          choices  = c("All Western States", state_names),
          selected = "All Western States"
        ),
        conditionalPanel(
          condition = "input.state !== 'All Western States'",
          selectInput("watershed", "Watershed", choices = NULL),
          actionButton("show_table", "View State Summary",
            class = "btn-outline-primary btn-sm w-100 mt-1")
        ),
        hr(),
        p(class = "text-muted small",
          "SWE = Snow Water Equivalent, shown as % of the 1991-2020 median. Data from NRCS.")
      ),
      card(plotOutput("swe_map", height = "520px")),
      conditionalPanel(
        condition = "input.state !== 'All Western States'",
        uiOutput("value_boxes"),
        card(card_body(textOutput("drought_msg")))
      )
    )
  ),

  nav_panel("About",
    card(max_width = "800px",
      card_body(
        h5("What is Snow Water Equivalent (SWE)?"),
        p("SWE is the depth of water that would result if the snowpack were fully melted.",
          "It is one of the best indicators of seasonal water availability in the western US."),
        h5("How is Drought Predicted?"),
        p("Current SWE is compared to the 1991-2020 median for each HUC4 watershed.",
          "Watersheds well below the median are at elevated drought risk."),
        h5("Drought Classifications"),
        tableOutput("class_table"),
        h5("Data Sources"),
        tags$ul(
          tags$li(tags$a("NRCS Western US SWE Report",
            href = "https://wcc.sc.egov.usda.gov/reports/UpdateReport.html?report=Western+US",
            target = "_blank")),
          tags$li("USGS NHDPlus HUC4 watershed boundaries via nhdplusTools")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # update watershed dropdown when state changes
  observeEvent(input$state, {
    req(input$state != "All Western States")
    code <- state.abb[match(input$state, state.name)]
    choices <- df_joined |>
      filter(sapply(strsplit(states, ","), \(s) code %in% s)) |>
      arrange(name) |>
      pull(name)
    updateSelectInput(session, "watershed", choices = choices)
  })

  sel_ws <- reactive({
    req(input$state != "All Western States", input$watershed)
    df_joined |> filter(name == input$watershed)
  })

  # state summary table for modal
  output$state_table <- DT::renderDT({
    req(input$state != "All Western States")
    code <- state.abb[match(input$state, state.name)]
    tbl <- df_joined |>
      st_drop_geometry() |>
      filter(sapply(strsplit(states, ","), \(s) code %in% s)) |>
      arrange(name) |>
      mutate(
        `SWE % of Median` = ifelse(is.na(SWE_pct_median), "—", paste0(round(SWE_pct_median), "%")),
        `Drought Outlook`  = sapply(SWE_pct_median, \(p) drought_label(p)$label)
      ) |>
      select(Watershed = name, `SWE % of Median`, `Drought Outlook`)

    DT::datatable(tbl, rownames = FALSE,
      options = list(dom = "t", pageLength = 50, ordering = TRUE)) |>
      DT::formatStyle("Drought Outlook",
        backgroundColor = DT::styleEqual(
          c("Above Normal", "Near Normal", "Below Normal",
            "Drought Possible", "Severe Drought Likely", "No Data"),
          c("#cce5ff",      "#d4edda",    "#d1ecf1",
            "#fff3cd",       "#f8d7da",               "#e2e3e5")
        )
      )
  }, server = FALSE)

  # open modal when button is clicked
  observeEvent(input$show_table, {
    showModal(modalDialog(
      title = paste(input$state, "— Watershed SWE Summary"),
      DT::DTOutput("state_table"),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })

  output$swe_map <- renderPlot({
    highlight <- NULL
    if (input$state != "All Western States") {
      code <- state.abb[match(input$state, state.name)]
      highlight <- df_joined |>
        filter(sapply(strsplit(states, ","), \(s) code %in% s))
    }

    p <- ggplot() +
      geom_polygon(data = western, aes(x = long, y = lat, group = group),
                   fill = "gray92", color = "white", linewidth = 0.3) +
      geom_sf(data = df_joined, aes(fill = SWE_pct_median),
              color = "white", linewidth = 0.4) +
      scale_fill_gradient2(
        low = "red3", mid = "white", high = "steelblue",
        midpoint = 100, limits = c(0, 150),
        na.value = "gray75", name = "% Median SWE"
      ) +
      coord_sf(xlim = c(-125, -102), ylim = c(30, 50)) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "right") +
      labs(x = NULL, y = NULL)

    if (!is.null(highlight) && nrow(highlight) > 0)
      p <- p + geom_sf(data = highlight, fill = NA, color = "black", linewidth = 0.5)

    if (input$state != "All Western States" && !is.null(input$watershed)) {
      sel <- df_joined |> filter(name == input$watershed)
      if (nrow(sel) > 0)
        p <- p + geom_sf(data = sel, fill = NA, color = "yellow2", linewidth = 0.5)
    }

    p
  }, res = 110)

  output$value_boxes <- renderUI({
    req(input$watershed)
    pct <- sel_ws()$SWE_pct_median
    d   <- drought_label(pct)
    pct_label <- if (is.na(pct)) "—" else paste0(pct, "%")
    layout_column_wrap(width = 1/2, fill = FALSE,
      value_box("% of Median SWE", pct_label, theme = d$color,
                showcase = bs_icon("bar-chart-line")),
      value_box("Drought Outlook",  d$label,   theme = d$color,
                showcase = bs_icon("cloud-sun"))
    )
  })

  output$drought_msg <- renderText({
    req(input$watershed)
    drought_label(sel_ws()$SWE_pct_median)$msg
  })

  output$class_table <- renderTable({
    data.frame(
      `SWE % of Median` = c("≥ 100%", "75–99%", "50–74%", "25–49%", "< 25%", "No data"),
      `Drought Outlook`  = c("Above Normal", "Near Normal", "Below Normal",
                             "Drought Possible", "Severe Drought Likely", "Insufficient data"),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
