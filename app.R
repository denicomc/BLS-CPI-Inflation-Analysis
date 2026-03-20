# =============================================================================
# CPI Inflation — Distribution Ridgeline Explorer
# Data: U.S. Bureau of Labor Statistics (live, no API key required)
# Method: 30%-trimmed distribution of CPI components, h/t Mike Konczal
# Weights: BLS cu.aspect relative importance, collapsed to item_name + year
#          so seasonally-adjusted series receive the correct weight
# =============================================================================

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

# -----------------------------------------------------------------------------
# Cleveland Fed median CPI component list
# These are the items used to compute the trimmed-mean / median CPI.
# OER is kept separate and optionally included as a single series.
# -----------------------------------------------------------------------------
MEDIAN_COMPONENTS <- c(
  "Airline fares",
  "Alcoholic beverages at home",
  "Alcoholic beverages away from home",
  "Apparel",
  "Car and truck rental",
  "Education",
  "Food away from home",
  "Food at home",
  "Fuels and utilities",
  "Household furnishings and operations",
  "Household operations",
  "Housekeeping supplies",
  "Infants' and toddlers' apparel",
  "Intracity transportation",
  "Jewelry and watches",
  "Medical care commodities",
  "Medical care services",
  "Men's and boys' apparel",
  "Motor vehicle insurance",
  "Motor vehicle maintenance and repair",
  "New vehicles",
  "Other goods",
  "Other personal services",
  "Other services",
  "Personal care",
  "Pet services including veterinary",
  "Public transportation",
  "Recreation",
  "Rent of primary residence",
  "Shelter",
  "Tobacco and smoking products",
  "Used cars and trucks",
  "Video and audio",
  "Water and sewer and trash collection services",
  "Women's and girls' apparel"
)

BLS_EMAIL <- "rortybomb@gmail.com"  # BLS requests a contact email in user-agent

# -----------------------------------------------------------------------------
# Data loading
# Fetches three BLS files and one aspect/weights file, then joins them.
# Result is cached in cpi_env so Binder doesn't re-fetch on every interaction.
# -----------------------------------------------------------------------------
load_cpi_data <- function() {

  fetch_bls <- function(url) {
    GET(url, user_agent(BLS_EMAIL)) %>%
      content(as = "text") %>%
      fread() %>%
      clean_names()
  }

  message("Fetching CPI observations...")
  cpi <- fetch_bls("https://download.bls.gov/pub/time.series/cu/cu.data.0.Current") %>%
    mutate(
      series_id = trimws(series_id),
      value     = suppressWarnings(as.numeric(value)),
      date      = as.Date(
        paste(substr(period, 2, 3), "01", substr(year, 3, 4), sep = "/"),
        "%m/%d/%y"
      )
    )

  message("Fetching series + item metadata...")
  series <- fetch_bls("https://download.bls.gov/pub/time.series/cu/cu.series") %>%
    mutate(series_id = trimws(series_id))

  items <- fetch_bls("https://download.bls.gov/pub/time.series/cu/cu.item")

  # Attach item_name to series via item_code
  series <- left_join(series, items, by = "item_code")

  # ---------------------------------------------------------------------------
  # Weights from cu.aspect (relative importance)
  # aspect_type "I1" = end-of-year relative importance (preferred)
  # Fallback chain: I1 -> R1 -> most-populated type -> equal weights
  #
  # Key insight: BLS only publishes aspect weights for UNADJUSTED series ("U").
  # Seasonally-adjusted series ("S") share the same item_name but have a
  # different series_id, so joining by series_id alone misses them.
  # Solution: collapse weights to item_name + year (mean across variants)
  # so both adjusted and unadjusted series receive the correct weight.
  # ---------------------------------------------------------------------------
  message("Fetching weights from cu.aspect...")
  weights <- tryCatch({

    aspect <- fetch_bls("https://download.bls.gov/pub/time.series/cu/cu.aspect") %>%
      mutate(
        series_id = trimws(series_id),
        value     = suppressWarnings(as.numeric(value))
      )

    # Select best available aspect_type
    w <- aspect %>% filter(aspect_type == "I1")
    if (nrow(w) == 0) w <- aspect %>% filter(aspect_type == "R1")
    if (nrow(w) == 0) {
      best <- aspect %>% filter(!is.na(value)) %>%
        count(aspect_type) %>% slice_max(n, n = 1, with_ties = FALSE) %>%
        pull(aspect_type)
      w <- aspect %>% filter(aspect_type == best)
      message("Used fallback aspect_type: ", best)
    }

    w %>%
      filter(!is.na(value)) %>%
      select(series_id, year, weight = value) %>%
      left_join(series %>% select(series_id, item_name), by = "series_id") %>%
      filter(!is.na(item_name)) %>%
      group_by(item_name, year) %>%
      summarise(weight = mean(weight, na.rm = TRUE), .groups = "drop")

  }, error = function(e) {
    message("Weight fetch failed (", e$message, ") — using equal weights")
    tibble(item_name = character(), year = integer(), weight = numeric())
  })

  # Join everything together
  cpi <- cpi %>%
    inner_join(series,  by = "series_id") %>%
    left_join(weights,  by = c("item_name", "year"))

  # Last-resort fallback: equal weights keep the plot running even if
  # the aspect file changes structure or becomes unavailable
  if (all(is.na(cpi$weight))) {
    message("WARNING: all weights NA — using equal weights (unweighted trim)")
    cpi <- cpi %>% mutate(weight = 1)
  }

  message(sprintf(
    "Load complete: %s rows | %s with weight | %s unique items",
    nrow(cpi), sum(!is.na(cpi$weight)), n_distinct(cpi$item_name)
  ))
  cpi
}

cpi_cache <- new.env(parent = emptyenv())

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(

  tags$head(tags$style(HTML("
    body            { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
                      background: #0d1117; color: #e6edf3; }
    .well           { background: #161b22; border: 1px solid #30363d; border-radius: 6px; }
    h2              { color: #f0f6fc; font-weight: 700; letter-spacing: -0.5px; }
    .control-label  { color: #8b949e; font-size: 12px; font-weight: 600;
                      text-transform: uppercase; letter-spacing: 0.5px; }
    .irs--shiny .irs-bar    { background: #1f6feb; border-color: #1f6feb; }
    .irs--shiny .irs-handle { background: #58a6ff; border-color: #58a6ff; }
    hr              { border-color: #30363d; }
    .btn-primary    { background: #238636; border-color: #2ea043; }
    .btn-primary:hover { background: #2ea043; }
    #status_msg     { color: #8b949e; font-size: 12px; margin-top: 6px; }
    pre             { background: #161b22 !important; color: #8b949e !important;
                      font-size: 11px; border: 1px solid #30363d; }
  "))),

  titlePanel(div(
    h2("CPI Inflation \u2014 Distribution Ridgeline Explorer"),
    tags$p(style = "color:#8b949e; font-size:13px; margin-top:-10px;",
           "Live BLS data \u00b7 h/t Mike Konczal")
  )),

  sidebarLayout(

    sidebarPanel(
      width = 3,

      # -- Date & display -------------------------------------------------------
      tags$b("DATE & DISPLAY", style = "color:#8b949e; font-size:11px;"),
      hr(style = "margin:6px 0;"),
      dateInput("start_date", "Chart Start Date",
                value = "2018-06-01", min = "2000-01-01"),
      sliderInput("n_months", "Months to Show",
                  min = 1, max = 84, value = 24, step = 1),
      sliderInput("plot_height", "Plot Height (px)",
                  min = 300, max = 3000, value = 1800, step = 20),

      # -- Trim -----------------------------------------------------------------
      tags$b("DISTRIBUTION TRIM",
             style = "color:#8b949e; font-size:11px; margin-top:16px; display:block;"),
      hr(style = "margin:6px 0;"),
      sliderInput("trim_pct", "Trim Each Tail (% by weight)",
                  min = 0, max = 49, value = 29, step = 1, post = "%"),

      # -- Appearance -----------------------------------------------------------
      tags$b("APPEARANCE",
             style = "color:#8b949e; font-size:11px; margin-top:16px; display:block;"),
      hr(style = "margin:6px 0;"),
      selectInput("color_palette", "Color Palette",
                  choices = c(
                    "Turbo (default)" = "H",
                    "Inferno"         = "B",
                    "Plasma"          = "C",
                    "Viridis"         = "D",
                    "Magma"           = "A",
                    "Cividis"         = "E"
                  ), selected = "H"),
      selectInput("x_range", "X-Axis Range",
                  choices = c(
                    "Auto"   = "auto",
                    "\u00b110%" = "10",
                    "\u00b120%" = "20",
                    "\u00b130%" = "30"
                  ), selected = "auto"),
      checkboxInput("show_zero", "Show 0% reference line",  value = TRUE),
      checkboxInput("show_2pct", "Show 2% target line",     value = TRUE),

      # -- Components -----------------------------------------------------------
      tags$b("COMPONENTS",
             style = "color:#8b949e; font-size:11px; margin-top:16px; display:block;"),
      hr(style = "margin:6px 0;"),
      checkboxInput("include_oer", "Include OER as single component", value = TRUE),

      hr(),
      actionButton("refresh", "\u21ba  Refresh BLS Data",
                   class = "btn-primary", width = "100%"),
      div(id = "status_msg", textOutput("status")),
      hr(),
      checkboxInput("show_debug", "Show debug panel", value = FALSE)
    ),

    mainPanel(
      width = 9,
      uiOutput("error_banner"),
      uiOutput("plot_container"),
      hr(style = "border-color:#30363d;"),
      fluidRow(
        column(4, verbatimTextOutput("stats_box")),
        column(8, tags$p(
          style = "color:#8b949e; font-size:12px; margin-top:10px;",
          "Tip: Narrowing the trim removes outlier categories and reveals where
           the core of price changes is clustering. A tighter ridge means more
           categories are inflating at a similar rate."
        ))
      ),
      conditionalPanel(
        condition = "input.show_debug == true",
        hr(style = "border-color:#30363d;"),
        tags$b("Debug", style = "color:#8b949e; font-size:11px;"),
        verbatimTextOutput("debug_out")
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  # -- Data load ---------------------------------------------------------------
  raw_data <- eventReactive(input$refresh, {
    withProgress(message = "Fetching BLS data\u2026", value = 0.5, {
      tryCatch(load_cpi_data(), error = function(e) {
        message("Load error: ", e$message)
        NULL
      })
    })
  }, ignoreNULL = FALSE)

  # -- Filtered / computed dataset for plotting --------------------------------
  plot_data <- reactive({
    req(raw_data())
    cpi         <- raw_data()
    trim_lo     <- input$trim_pct / 100
    trim_hi     <- 1 - trim_lo
    include_oer <- input$include_oer

    # Build the set of months to display: n_months counting back from the
    # most recent month in the data
    latest_month <- month(max(cpi$date, na.rm = TRUE))
    show_months  <- ((seq(latest_month, latest_month + input$n_months - 1) - 1) %% 12) + 1

    cpi %>%
      filter(
        item_name %in% MEDIAN_COMPONENTS |
        (include_oer & item_name == "Owners' equivalent rent of residences")
      ) %>%
      filter(period != "M13", trimws(seasonal) == "S") %>%
      arrange(date) %>%
      group_by(item_name) %>%
      mutate(mom_chg = value / lag(value, 1) - 1) %>%   # month-over-month change
      ungroup() %>%
      group_by(date) %>%
      mutate(
        weight_sum = sum(weight, na.rm = TRUE),
        weight_n   = if_else(weight_sum > 0, weight / weight_sum, NA_real_)
      ) %>%
      arrange(mom_chg) %>%
      mutate(cumwt = cumsum(replace_na(weight_n, 0))) %>%
      ungroup() %>%
      mutate(ann_chg = (1 + mom_chg)^12 - 1) %>%        # annualised
      filter(cumwt >= trim_lo, cumwt <= trim_hi) %>%
      filter(date >= as.Date(input$start_date)) %>%
      filter(month(date) %in% show_months) %>%
      filter(!is.na(ann_chg)) %>%
      mutate(
        month_label = fct_rev(fct_reorder(format(date, "%B, %Y"), date))
      )
  })

  # -- Error / empty-data banner -----------------------------------------------
  output$error_banner <- renderUI({
    result <- tryCatch(plot_data(), error = function(e) e)
    if (inherits(result, "error")) {
      div(style = "color:#f85149; padding:12px; background:#2d1b1b;
                   border:1px solid #f85149; border-radius:4px; margin-bottom:12px;",
          strong("Error: "), result$message)
    } else if (nrow(result) == 0) {
      div(style = "color:#e3b341; padding:12px; background:#2d2208;
                   border:1px solid #e3b341; border-radius:4px; margin-bottom:12px;",
          strong("No data to plot. "),
          "Enable the debug panel to diagnose which filter is removing all rows.")
    }
  })

  # -- Dynamic plot height -----------------------------------------------------
  output$plot_container <- renderUI({
    plotOutput("ridgeline", height = paste0(input$plot_height, "px"))
  })

  # -- Main ridgeline plot -----------------------------------------------------
  output$ridgeline <- renderPlot({
    df <- plot_data()
    validate(need(nrow(df) > 0, ""))

    p <- ggplot(df, aes(x = ann_chg, y = month_label, fill = after_stat(x))) +
      geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
      scale_fill_viridis(option = input$color_palette) +
      theme_ridges(grid = TRUE) +
      scale_x_continuous(labels = percent) +
      labs(
        title    = "CPI Inflation Report: Distribution Ridgeline Plot",
        subtitle = sprintf(
          "Trimmed distribution \u2014 dropping bottom %d%% and top %d%% by weight",
          input$trim_pct, input$trim_pct
        ),
        x       = "One-Month Change, Annualised",
        y       = NULL,
        caption = "OER treated as one value \u00b7 h/t Mike Konczal \u00b7 Source: BLS"
      ) +
      theme(
        plot.background     = element_rect(fill = "#0d1117", color = NA),
        panel.background    = element_rect(fill = "#161b22", color = NA),
        plot.title.position = "plot",
        legend.position     = "none",
        plot.title    = element_text(size = 20, color = "#f0f6fc",
                                     margin = margin(0, 0, 5, 0)),
        plot.subtitle = element_text(size = 13, color = "#8b949e"),
        plot.caption  = element_text(size = 10, face = "italic", color = "#8b949e"),
        axis.text.y   = element_text(size = 12, face = "bold",   color = "#e6edf3"),
        axis.text.x   = element_text(size = 12,                  color = "#e6edf3"),
        panel.grid.major = element_line(color = "#30363d"),
        panel.grid.minor = element_blank()
      )

    if (input$x_range != "auto") {
      lim <- as.numeric(input$x_range) / 100
      p   <- p + coord_cartesian(xlim = c(-lim, lim))
    }
    if (input$show_zero) {
      p <- p + geom_vline(xintercept = 0,    color = "#f85149",
                          linetype = "dashed", linewidth = 0.7, alpha = 0.8)
    }
    if (input$show_2pct) {
      p <- p +
        geom_vline(xintercept = 0.02,  color = "#58a6ff",
                   linetype = "dashed", linewidth = 0.7, alpha = 0.8) +
        annotate("text", x = 0.02, y = Inf, label = "2% target",
                 color = "#58a6ff", size = 3.2, hjust = -0.1, vjust = 1.5)
    }
    p
  }, bg = "#0d1117")

  # -- Summary stats -----------------------------------------------------------
  output$stats_box <- renderPrint({
    df <- plot_data()
    req(nrow(df) > 0)
    latest <- df %>% filter(date == max(date))
    cat("Latest month:       ", format(max(df$date), "%B %Y"),      "\n")
    cat("Components in plot: ", n_distinct(latest$item_name),        "\n")
    cat("Median annualised:  ",
        scales::percent(median(latest$ann_chg, na.rm = TRUE), accuracy = 0.1), "\n")
    cat("Mean annualised:    ",
        scales::percent(mean(latest$ann_chg,   na.rm = TRUE), accuracy = 0.1), "\n")
    cat("IQR:                ",
        scales::percent(IQR(latest$ann_chg,    na.rm = TRUE), accuracy = 0.1), "\n")
  })

  # -- Debug panel -------------------------------------------------------------
  output$debug_out <- renderPrint({
    req(raw_data())
    cpi         <- raw_data()
    trim_lo     <- input$trim_pct / 100
    trim_hi     <- 1 - trim_lo
    include_oer <- input$include_oer
    latest_month <- month(max(cpi$date, na.rm = TRUE))
    show_months  <- ((seq(latest_month, latest_month + input$n_months - 1) - 1) %% 12) + 1

    cat("=== FILTER FUNNEL ===\n")

    s1 <- cpi %>% filter(
      item_name %in% MEDIAN_COMPONENTS |
      (include_oer & item_name == "Owners' equivalent rent of residences")
    )
    cat(sprintf("After component filter:   %d rows\n", nrow(s1)))

    s2 <- s1 %>% filter(period != "M13")
    cat(sprintf("After period != M13:      %d rows\n", nrow(s2)))

    cat(sprintf("Seasonal values present:  %s\n",
                paste(unique(trimws(s2$seasonal)), collapse = ", ")))

    s3 <- s2 %>% filter(trimws(seasonal) == "S")
    cat(sprintf("After seasonal == 'S':    %d rows\n", nrow(s3)))

    s4 <- s3 %>%
      arrange(date) %>% group_by(item_name) %>%
      mutate(mom_chg = value / lag(value, 1) - 1) %>% ungroup() %>%
      group_by(date) %>%
      mutate(weight_sum = sum(weight, na.rm = TRUE),
             weight_n   = if_else(weight_sum > 0, weight / weight_sum, NA_real_)) %>%
      arrange(mom_chg) %>%
      mutate(cumwt = cumsum(replace_na(weight_n, 0))) %>% ungroup()
    cat(sprintf("After cumwt calc:         %d rows\n", nrow(s4)))
    cat(sprintf("  cumwt range: [%.3f, %.3f]  trim: [%.2f, %.2f]\n",
                min(s4$cumwt, na.rm=TRUE), max(s4$cumwt, na.rm=TRUE), trim_lo, trim_hi))

    s5 <- s4 %>% filter(cumwt >= trim_lo, cumwt <= trim_hi)
    cat(sprintf("After trim filter:        %d rows\n", nrow(s5)))

    s6 <- s5 %>% filter(date >= as.Date(input$start_date))
    cat(sprintf("After start_date filter:  %d rows\n", nrow(s6)))

    s7 <- s6 %>% filter(month(date) %in% show_months)
    cat(sprintf("After months filter:      %d rows\n", nrow(s7)))
    cat(sprintf("  show_months: %s\n",  paste(sort(show_months), collapse=", ")))
    cat(sprintf("  months in data: %s\n", paste(sort(unique(month(s6$date))), collapse=", ")))

    s8 <- s7 %>% mutate(ann_chg = (1 + mom_chg)^12 - 1) %>% filter(!is.na(ann_chg))
    cat(sprintf("After NA ann_chg drop:    %d rows\n\n", nrow(s8)))

    cat("=== WEIGHT SAMPLE (latest year) ===\n")
    cpi %>%
      filter(item_name %in% MEDIAN_COMPONENTS, !is.na(weight)) %>%
      distinct(item_name, year, weight) %>%
      slice_max(year, n = 10, with_ties = FALSE) %>%
      as.data.frame() %>%
      print()
  })

  # -- Status line -------------------------------------------------------------
  output$status <- renderText({
    if (is.null(raw_data())) "Loading\u2026"
    else paste("Last fetch:", format(Sys.time(), "%H:%M:%S"))
  })
}

shinyApp(ui, server)
