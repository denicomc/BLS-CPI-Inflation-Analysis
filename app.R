library(shiny)
library(tidyverse)
library(httr)
library(data.table)
library(magrittr)
library(ggridges)
library(lubridate)
library(janitor)
library(viridis)
library(scales)

# ── Standard Cleveland Fed median CPI components ──────────────────────────────
MEDIAN_COMPONENTS <- c(
  "Airline fares", "Alcoholic beverages at home",
  "Alcoholic beverages away from home", "Apparel", "Car and truck rental",
  "Education", "Food away from home", "Food at home", "Fuels and utilities",
  "Household furnishings and operations", "Household operations",
  "Housekeeping supplies", "Infants' and toddlers' apparel",
  "Intracity transportation", "Jewelry and watches",
  "Medical care commodities", "Medical care services",
  "Men's and boys' apparel", "Motor vehicle insurance",
  "Motor vehicle maintenance and repair", "New vehicles", "Other goods",
  "Other personal services", "Other services", "Personal care",
  "Pet services including veterinary", "Public transportation", "Recreation",
  "Rent of primary residence", "Shelter", "Tobacco and smoking products",
  "Used cars and trucks", "Video and audio",
  "Water and sewer and trash collection services", "Women's and girls' apparel"
)

USER_AGENT <- "rortybomb@gmail.com"

# ── Data loading (cached so Binder doesn't re-fetch on every render) ──────────
load_cpi_data <- function() {
  message("Fetching CPI data from BLS…")

  cpi_data <- GET(
    "https://download.bls.gov/pub/time.series/cu/cu.data.0.Current",
    user_agent(USER_AGENT)
  ) %>%
    content(as = "text") %>%
    fread() %>%
    clean_names() %>%
    mutate(
      value = as.numeric(value),
      date  = as.Date(
        paste(substr(period, 2, 3), "01", substr(year, 3, 4), sep = "/"),
        "%m/%d/%y"
      )
    )

  series <- GET(
    "https://download.bls.gov/pub/time.series/cu/cu.series",
    user_agent(USER_AGENT)
  ) %>% content(as = "text") %>% fread() %>% clean_names()

  items <- GET(
    "https://download.bls.gov/pub/time.series/cu/cu.item",
    user_agent(USER_AGENT)
  ) %>% content(as = "text") %>% fread()

  series <- inner_join(series, items, by = "item_code")

  cpi_weights <- GET(
    "https://download.bls.gov/pub/time.series/cu/cu.weights",
    user_agent(USER_AGENT)
  ) %>% content(as = "text") %>% fread() %>% clean_names() %>%
    mutate(series_id = trimws(series_id))

  cpi_data <- cpi_data %>%
    mutate(series_id = trimws(series_id)) %>%
    inner_join(series, by = "series_id") %>%
    left_join(cpi_weights, by = c("series_id", "year"))

  message("BLS data loaded.")
  cpi_data
}

# Cache across sessions within one Binder container lifetime
cpi_env <- new.env()

get_or_load <- function() {
  if (!exists("cpi_data", envir = cpi_env)) {
    cpi_env$cpi_data <- load_cpi_data()
  }
  cpi_env$cpi_data
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
           background: #0d1117; color: #e6edf3; }
    .well { background: #161b22; border: 1px solid #30363d; border-radius: 6px; }
    h2   { color: #f0f6fc; font-weight: 700; letter-spacing: -0.5px; }
    .control-label { color: #8b949e; font-size: 12px; font-weight: 600;
                     text-transform: uppercase; letter-spacing: 0.5px; }
    .irs--shiny .irs-bar { background: #1f6feb; border-color: #1f6feb; }
    .irs--shiny .irs-handle { background: #58a6ff; border-color: #58a6ff; }
    hr  { border-color: #30363d; }
    .btn-primary { background: #238636; border-color: #2ea043; }
    .btn-primary:hover { background: #2ea043; }
    #status_msg { color: #8b949e; font-size: 12px; margin-top: 6px; }
  "))),

  titlePanel(
    div(
      h2("📊 CPI Inflation — Distribution Ridgeline Explorer"),
      tags$p(style = "color:#8b949e; font-size:13px; margin-top:-10px;",
             "Live BLS data · h/t Mike Konczal")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # ── Date range ──────────────────────────────────────────────────────────
      tags$b("DATE RANGE", style = "color:#8b949e; font-size:11px;"),
      hr(style = "margin: 6px 0;"),

      dateInput("start_date", "Chart Start Date",
                value = "2018-06-01", min = "2000-01-01"),

      sliderInput("n_quarters", "Number of Observations",
                  min = 4, max = 24, value = 10, step = 1),

      # ── Distribution trim ───────────────────────────────────────────────────
      tags$b("DISTRIBUTION TRIM", style = "color:#8b949e; font-size:11px; margin-top:16px; display:block;"),
      hr(style = "margin: 6px 0;"),

      sliderInput("trim_pct", "Trim Each Tail (% by weight)",
                  min = 0, max = 40, value = 15, step = 1,
                  post = "%"),

      # ── Appearance ──────────────────────────────────────────────────────────
      tags$b("APPEARANCE", style = "color:#8b949e; font-size:11px; margin-top:16px; display:block;"),
      hr(style = "margin: 6px 0;"),

      selectInput("color_palette", "Color Palette",
                  choices = c(
                    "Turbo (default)"  = "H",
                    "Inferno"          = "B",
                    "Plasma"           = "C",
                    "Viridis"          = "D",
                    "Magma"            = "A",
                    "Cividis"          = "E"
                  ), selected = "H"),

      selectInput("x_axis_range", "X-Axis Range",
                  choices = c(
                    "Auto"        = "auto",
                    "±10%"        = "10",
                    "±20%"        = "20",
                    "±30%"        = "30"
                  ), selected = "auto"),

      checkboxInput("show_zero", "Show zero-line", value = TRUE),

      # ── OER ─────────────────────────────────────────────────────────────────
      tags$b("COMPONENTS", style = "color:#8b949e; font-size:11px; margin-top:16px; display:block;"),
      hr(style = "margin: 6px 0;"),

      checkboxInput("include_oer", "Include OER as single component", value = TRUE),

      hr(),

      actionButton("refresh", "↺  Refresh BLS Data",
                   class = "btn-primary", width = "100%"),
      div(id = "status_msg", textOutput("status"))
    ),

    mainPanel(
      width = 9,
      plotOutput("ridgeline", height = "680px"),
      hr(style = "border-color:#30363d;"),
      fluidRow(
        column(4, verbatimTextOutput("stats_box")),
        column(8,
          tags$p(style = "color:#8b949e; font-size:12px; margin-top:10px;",
            "Tip: Narrowing the trim removes outlier categories and reveals where
             the core of price changes is clustering. A tighter distribution
             (sharper ridge) means more categories are inflating at a similar rate."
          )
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Reactive data load (re-triggers only when Refresh clicked)
  raw_data <- eventReactive(input$refresh, {
    cpi_env$cpi_data <- load_cpi_data()
    cpi_env$cpi_data
  }, ignoreNULL = FALSE)

  # Derive filtered/computed dataset from inputs
  plot_data <- reactive({
    req(raw_data())
    cpi <- raw_data()

    trim_lo <- input$trim_pct / 100
    trim_hi <- 1 - trim_lo

    start_month <- month(max(cpi$date, na.rm = TRUE))
    quarters    <- ((seq(start_month, start_month + input$n_quarters - 1) - 1) %% 12) + 1

    oer_filter <- if (input$include_oer) {
      quote(item_name %in% MEDIAN_COMPONENTS | item_name == "Owners' equivalent rent of residences")
    } else {
      quote(item_name %in% MEDIAN_COMPONENTS)
    }

    df <- cpi %>%
      filter(eval(oer_filter)) %>%
      filter(period != "M13", seasonal == "S") %>%
      arrange(date) %>%
      group_by(item_name) %>%
      mutate(Pchange3 = value / lag(value, 1) - 1) %>%
      ungroup() %>%
      group_by(date) %>%
      mutate(
        normalized = sum(weight, na.rm = TRUE),
        weightN    = weight / normalized
      ) %>%
      arrange(Pchange3) %>%
      mutate(
        cumsum  = cumsum(weight) / 100,
        cumsumN = cumsum(weightN)
      ) %>%
      ungroup() %>%
      mutate(Pchange3a = (1 + Pchange3)^12 - 1) %>%
      filter(cumsumN >= trim_lo, cumsumN <= trim_hi) %>%
      filter(date >= as.Date(input$start_date)) %>%
      filter(month(date) %in% quarters) %>%
      mutate(
        monthC  = format(date, "%B, %Y"),
        monthC  = fct_reorder(monthC, date),
        monthCR = fct_rev(monthC)
      )

    df
  })

  output$ridgeline <- renderPlot({
    df <- plot_data()
    req(nrow(df) > 0)

    trim_lo <- input$trim_pct
    trim_hi <- 100 - trim_lo

    p <- df %>%
      ggplot(aes(x = Pchange3a, y = monthCR, fill = after_stat(x))) +
      geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
      scale_fill_viridis(option = input$color_palette) +
      theme_ridges(grid = TRUE) +
      scale_x_continuous(labels = percent) +
      labs(
        title    = "CPI Inflation Report: Distribution Ridgeline Plot",
        subtitle = sprintf(
          "Trimmed distribution — dropping bottom %d%% and top %d%% by weight",
          trim_lo, trim_lo
        ),
        x       = "One Annualized Month Percent Change",
        y       = "",
        caption = "OER treated as one value · h/t Mike Konczal · Data: BLS"
      ) +
      theme(
        plot.background  = element_rect(fill = "#0d1117", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        plot.title.position = "plot",
        legend.position  = "none",
        plot.title       = element_text(size = 20, color = "#f0f6fc",
                                        margin = margin(0, 0, 5, 0)),
        plot.subtitle    = element_text(size = 13, color = "#8b949e"),
        plot.caption     = element_text(size = 10, face = "italic", color = "#8b949e"),
        axis.text.y      = element_text(size = 12, face = "bold", color = "#e6edf3"),
        axis.text.x      = element_text(size = 12, color = "#e6edf3"),
        panel.grid.major = element_line(color = "#30363d"),
        panel.grid.minor = element_blank()
      )

    if (input$x_axis_range != "auto") {
      lim <- as.numeric(input$x_axis_range) / 100
      p <- p + coord_cartesian(xlim = c(-lim, lim))
    }

    if (input$show_zero) {
      p <- p + geom_vline(xintercept = 0, color = "#f85149",
                          linetype = "dashed", linewidth = 0.7, alpha = 0.8)
    }

    p
  }, bg = "#0d1117")

  # Summary stats box
  output$stats_box <- renderPrint({
    df <- plot_data()
    req(nrow(df) > 0)

    latest <- df %>% filter(date == max(date))
    cat("Latest month:", format(max(df$date), "%B %Y"), "\n")
    cat("Components included:", n_distinct(latest$item_name), "\n")
    cat("Median annualized change: ",
        scales::percent(median(latest$Pchange3a, na.rm = TRUE), accuracy = 0.1), "\n")
    cat("Mean  annualized change: ",
        scales::percent(mean(latest$Pchange3a, na.rm = TRUE),   accuracy = 0.1), "\n")
    cat("IQR:  ",
        scales::percent(IQR(latest$Pchange3a, na.rm = TRUE), accuracy = 0.1), "\n")
  })

  output$status <- renderText({
    if (is.null(raw_data())) "Loading…" else
      paste("Last fetch:", format(Sys.time(), "%H:%M:%S"))
  })
}

shinyApp(ui, server)
