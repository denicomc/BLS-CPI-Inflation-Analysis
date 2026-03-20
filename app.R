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

# ── Cleveland Fed median CPI components ───────────────────────────────────────
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

load_cpi_data <- function() {
  message("Fetching CPI observations...")
  cpi_data <- GET(
    "https://download.bls.gov/pub/time.series/cu/cu.data.0.Current",
    user_agent(USER_AGENT)
  ) %>%
    content(as = "text") %>%
    fread() %>%
    clean_names() %>%
    mutate(
      value     = as.numeric(value),
      series_id = trimws(series_id),
      date      = as.Date(
        paste(substr(period, 2, 3), "01", substr(year, 3, 4), sep = "/"),
        "%m/%d/%y"
      )
    )

  message("Fetching series metadata...")
  series <- GET(
    "https://download.bls.gov/pub/time.series/cu/cu.series",
    user_agent(USER_AGENT)
  ) %>% content(as = "text") %>% fread() %>% clean_names() %>%
    mutate(series_id = trimws(series_id))

  items <- GET(
    "https://download.bls.gov/pub/time.series/cu/cu.item",
    user_agent(USER_AGENT)
  ) %>% content(as = "text") %>% fread() %>% clean_names()

  series <- left_join(series, items, by = "item_code")

  # ── Weights via cu.aspect with full fallback chain ───────────────────────────
  # Strategy 1: aspect_type "I1" (end-of-year relative importance)
  # Strategy 2: aspect_type "R1" (alternate code in some BLS vintages)
  # Strategy 3: whichever aspect_type has the most non-NA numeric rows
  # Strategy 4: equal weights = 1 so the plot always renders
  message("Fetching aspect/weights...")
  cpi_weights <- tryCatch({
    aspect_raw <- GET(
      "https://download.bls.gov/pub/time.series/cu/cu.aspect",
      user_agent(USER_AGENT)
    ) %>% content(as = "text") %>% fread() %>% clean_names() %>%
      mutate(series_id = trimws(series_id),
             value     = suppressWarnings(as.numeric(value)))

    message("Aspect cols:  ", paste(names(aspect_raw), collapse = ", "))
    message("Aspect types: ", paste(unique(aspect_raw$aspect_type), collapse = ", "))

    w <- aspect_raw %>% filter(aspect_type == "I1")
    if (nrow(w) == 0) {
      message("I1 empty - trying R1...")
      w <- aspect_raw %>% filter(aspect_type == "R1")
    }
    if (nrow(w) == 0) {
      best_type <- aspect_raw %>%
        filter(!is.na(value)) %>%
        count(aspect_type) %>%
        slice_max(n, n = 1, with_ties = FALSE) %>%
        pull(aspect_type)
      message("Falling back to aspect_type: ", best_type)
      w <- aspect_raw %>% filter(aspect_type == best_type)
    }
    # Aspect weights live on UNADJUSTED series (seasonal=="U"). Seasonally
    # adjusted series share the same item_name but different series_id, so a
    # series_id join misses them entirely. Collapse to item_name + year so both
    # adjusted and unadjusted series get the same weight.
    w %>%
      select(series_id, year, weight = value) %>%
      filter(!is.na(weight)) %>%
      left_join(series %>% select(series_id, item_name), by = "series_id") %>%
      filter(!is.na(item_name)) %>%
      group_by(item_name, year) %>%
      summarise(weight = mean(weight, na.rm = TRUE), .groups = "drop")
  }, error = function(e) {
    message("Aspect fetch failed: ", e$message)
    tibble(item_name = character(), year = integer(), weight = numeric())
  })

  message("Weight rows after item_name collapse: ", nrow(cpi_weights))

  cpi_data <- cpi_data %>%
    inner_join(series,     by = "series_id") %>%
    left_join(cpi_weights, by = c("item_name", "year"))

  # Final fallback: if all weights are still NA, use equal weights.
  # Trimmed mean becomes an unweighted trim — still valid, just note it.
  if (sum(!is.na(cpi_data$weight)) == 0) {
    message("WARNING: all weights NA - falling back to equal weights (unweighted trim)")
    cpi_data <- cpi_data %>% mutate(weight = 1)
  }

  message("Final rows: ", nrow(cpi_data),
          " | with weight: ", sum(!is.na(cpi_data$weight)),
          " | unique items: ", n_distinct(cpi_data$item_name))
  cpi_data
}

cpi_env <- new.env()

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
    pre { background: #161b22 !important; color: #8b949e !important;
          font-size: 11px; border: 1px solid #30363d; }
  "))),

  titlePanel(div(
    h2("CPI Inflation - Distribution Ridgeline Explorer"),
    tags$p(style = "color:#8b949e; font-size:13px; margin-top:-10px;",
           "Live BLS data - h/t Mike Konczal")
  )),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      tags$b("DATE RANGE", style = "color:#8b949e; font-size:11px;"),
      hr(style = "margin: 6px 0;"),
      dateInput("start_date", "Chart Start Date",
                value = "2018-06-01", min = "2000-01-01"),
      sliderInput("n_quarters", "Number of Observations",
                  min = 1, max = 84, value = 48, step = 1),
      sliderInput("plot_height", "Plot Height (px)",
                  min = 300, max = 3000, value = 1800, step = 20),

      tags$b("DISTRIBUTION TRIM",
             style = "color:#8b949e; font-size:11px; margin-top:16px; display:block;"),
      hr(style = "margin: 6px 0;"),
      sliderInput("trim_pct", "Trim Each Tail (% by weight)",
                  min = 0, max = 49, value = 29, step = 1, post = "%"),

      tags$b("APPEARANCE",
             style = "color:#8b949e; font-size:11px; margin-top:16px; display:block;"),
      hr(style = "margin: 6px 0;"),
      selectInput("color_palette", "Color Palette",
                  choices = c("Turbo (default)" = "H", "Inferno" = "B",
                              "Plasma" = "C", "Viridis" = "D",
                              "Magma" = "A", "Cividis" = "E"),
                  selected = "H"),
      selectInput("x_axis_range", "X-Axis Range",
                  choices = c("Auto" = "auto", "+-10%" = "10",
                              "+-20%" = "20", "+-30%" = "30"),
                  selected = "auto"),
      checkboxInput("show_zero", "Show zero-line", value = TRUE),
      checkboxInput("show_2pct", "Show 2% target line", value = TRUE),

      tags$b("COMPONENTS",
             style = "color:#8b949e; font-size:11px; margin-top:16px; display:block;"),
      hr(style = "margin: 6px 0;"),
      checkboxInput("include_oer", "Include OER as single component", value = TRUE),

      hr(),
      actionButton("refresh", "Refresh BLS Data",
                   class = "btn-primary", width = "100%"),
      div(id = "status_msg", textOutput("status")),
      hr(),
      checkboxInput("show_debug", "Show debug panel", value = FALSE)
    ),

    mainPanel(
      width = 9,
      uiOutput("error_msg"),
      uiOutput("ridgeline_container"),
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

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  raw_data <- eventReactive(input$refresh, {
    withProgress(message = "Fetching BLS data...", value = 0.5, {
      tryCatch(load_cpi_data(), error = function(e) {
        message("Load error: ", e$message); NULL
      })
    })
  }, ignoreNULL = FALSE)

  plot_data <- reactive({
    req(raw_data())
    cpi <- raw_data()

    trim_lo <- input$trim_pct / 100
    trim_hi <- 1 - trim_lo

    start_month <- month(max(cpi$date, na.rm = TRUE))
    quarters    <- ((seq(start_month, start_month + input$n_quarters - 1) - 1) %% 12) + 1

    include_oer <- input$include_oer

    df <- cpi %>%
      filter(
        item_name %in% MEDIAN_COMPONENTS |
        (include_oer & item_name == "Owners' equivalent rent of residences")
      ) %>%
      filter(period != "M13", seasonal == "S") %>%
      arrange(date) %>%
      group_by(item_name) %>%
      mutate(Pchange3 = value / lag(value, 1) - 1) %>%
      ungroup() %>%
      group_by(date) %>%
      mutate(
        normalized = sum(weight, na.rm = TRUE),
        weightN    = if_else(normalized > 0, weight / normalized, NA_real_)
      ) %>%
      arrange(Pchange3) %>%
      mutate(
        cumsumN = cumsum(replace_na(weightN, 0))
      ) %>%
      ungroup() %>%
      mutate(Pchange3a = (1 + Pchange3)^12 - 1) %>%
      filter(cumsumN >= trim_lo, cumsumN <= trim_hi) %>%
      filter(date >= as.Date(input$start_date)) %>%
      filter(month(date) %in% quarters) %>%
      filter(!is.na(Pchange3a)) %>%
      mutate(
        monthC  = format(date, "%B, %Y"),
        monthC  = fct_reorder(monthC, date),
        monthCR = fct_rev(monthC)
      )

    df
  })

  output$error_msg <- renderUI({
    df <- tryCatch(plot_data(), error = function(e) e)
    if (inherits(df, "error")) {
      div(style = "color:#f85149; padding:12px; background:#2d1b1b;
                   border:1px solid #f85149; border-radius:4px; margin-bottom:12px;",
          strong("Plot error: "), df$message)
    } else if (nrow(df) == 0) {
      div(style = "color:#e3b341; padding:12px; background:#2d2208;
                   border:1px solid #e3b341; border-radius:4px; margin-bottom:12px;",
          strong("No data to plot. "),
          "Check the debug panel — weights may all be NA or component names
           may not match BLS item_name values.")
    }
  })

  output$ridgeline_container <- renderUI({
    plotOutput("ridgeline", height = paste0(input$plot_height, "px"))
  })

  output$ridgeline <- renderPlot({
    df <- plot_data()
    validate(need(nrow(df) > 0, ""))

    p <- df %>%
      ggplot(aes(x = Pchange3a, y = monthCR, fill = after_stat(x))) +
      geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
      scale_fill_viridis(option = input$color_palette) +
      theme_ridges(grid = TRUE) +
      scale_x_continuous(labels = percent) +
      labs(
        title    = "CPI Inflation Report: Distribution Ridgeline Plot",
        subtitle = sprintf(
          "Trimmed distribution - dropping bottom %d%% and top %d%% by weight",
          input$trim_pct, input$trim_pct),
        x       = "One Annualized Month Percent Change",
        y       = "",
        caption = "OER treated as one value - h/t Mike Konczal - Data: BLS"
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
        axis.text.y   = element_text(size = 12, face = "bold", color = "#e6edf3"),
        axis.text.x   = element_text(size = 12, color = "#e6edf3"),
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
    if (input$show_2pct) {
      p <- p + geom_vline(xintercept = 0.02, color = "#58a6ff",
                          linetype = "dashed", linewidth = 0.7, alpha = 0.8) +
        annotate("text", x = 0.02, y = Inf, label = "2% target",
                 color = "#58a6ff", size = 3.2, hjust = -0.1, vjust = 1.5)
    }
    p
  }, bg = "#0d1117")

  output$stats_box <- renderPrint({
    df <- plot_data()
    req(nrow(df) > 0)
    latest <- df %>% filter(date == max(date))
    cat("Latest month:         ", format(max(df$date), "%B %Y"), "\n")
    cat("Components in plot:   ", n_distinct(latest$item_name), "\n")
    cat("Median annualized:    ",
        scales::percent(median(latest$Pchange3a, na.rm = TRUE), accuracy = 0.1), "\n")
    cat("Mean annualized:      ",
        scales::percent(mean(latest$Pchange3a,   na.rm = TRUE), accuracy = 0.1), "\n")
    cat("IQR:                  ",
        scales::percent(IQR(latest$Pchange3a,    na.rm = TRUE), accuracy = 0.1), "\n")
  })

  output$debug_out <- renderPrint({
    req(raw_data())
    cpi <- raw_data()

    include_oer <- input$include_oer
    trim_lo     <- input$trim_pct / 100
    trim_hi     <- 1 - trim_lo
    start_month <- month(max(cpi$date, na.rm = TRUE))
    quarters    <- ((seq(start_month, start_month + input$n_quarters - 1) - 1) %% 12) + 1

    cat("=== STEP-BY-STEP FILTER FUNNEL ===\n\n")

    s1 <- cpi %>%
      filter(
        item_name %in% MEDIAN_COMPONENTS |
        (include_oer & item_name == "Owners' equivalent rent of residences")
      )
    cat("After component filter:        ", nrow(s1), "rows\n")

    s2 <- s1 %>% filter(period != "M13")
    cat("After period != M13:           ", nrow(s2), "rows\n")

    cat("Unique 'seasonal' values seen: ",
        paste(unique(trimws(s2$seasonal)), collapse = ", "), "\n")
    s3 <- s2 %>% filter(trimws(seasonal) == "S")
    cat("After seasonal == 'S':         ", nrow(s3), "rows\n")

    s4 <- s3 %>%
      arrange(date) %>%
      group_by(item_name) %>%
      mutate(Pchange3 = value / lag(value, 1) - 1) %>%
      ungroup() %>%
      group_by(date) %>%
      mutate(
        normalized = sum(weight, na.rm = TRUE),
        weightN    = if_else(normalized > 0, weight / normalized, NA_real_)
      ) %>%
      arrange(Pchange3) %>%
      mutate(cumsumN = cumsum(replace_na(weightN, 0))) %>%
      ungroup()

    cat("After Pchange3/cumsumN:        ", nrow(s4), "rows\n")
    cat("  cumsumN range: [",
        round(min(s4$cumsumN, na.rm=TRUE), 3), ",",
        round(max(s4$cumsumN, na.rm=TRUE), 3), "]\n")
    cat("  trim_lo=", trim_lo, " trim_hi=", trim_hi, "\n")

    s5 <- s4 %>% filter(cumsumN >= trim_lo, cumsumN <= trim_hi)
    cat("After cumsumN trim:            ", nrow(s5), "rows\n")

    s6 <- s5 %>% filter(date >= as.Date(input$start_date))
    cat("After date >= start_date:      ", nrow(s6), "rows\n")

    cat("Quarters filter months:        ", paste(sort(quarters), collapse=", "), "\n")
    cat("Months present in data:        ",
        paste(sort(unique(month(s6$date))), collapse=", "), "\n")
    s7 <- s6 %>% filter(month(date) %in% quarters)
    cat("After quarters filter:         ", nrow(s7), "rows\n")

    s8 <- s7 %>%
      mutate(Pchange3a = (1 + Pchange3)^12 - 1) %>%
      filter(!is.na(Pchange3a))
    cat("After removing NA Pchange3a:   ", nrow(s8), "rows\n")

    cat("\n=== WEIGHT SAMPLE (most recent year) ===\n")
    w_sample <- cpi %>%
      filter(item_name %in% MEDIAN_COMPONENTS, !is.na(weight)) %>%
      distinct(item_name, year, weight) %>%
      slice_max(year, n = 10, with_ties = FALSE)
    print(as.data.frame(w_sample))
  })

  output$status <- renderText({
    if (is.null(raw_data())) "Loading..."
    else paste("Last fetch:", format(Sys.time(), "%H:%M:%S"))
  })
}

shinyApp(ui, server)
