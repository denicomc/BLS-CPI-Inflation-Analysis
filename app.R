# =============================================================================
# CPI Inflation — Distribution Ridgeline Explorer  (improved)
# Data: U.S. Bureau of Labor Statistics (live, no API key required)
# Method: 30%-trimmed distribution of CPI components, h/t Mike Konczal
# Weights: BLS cu.aspect relative importance, collapsed to item_name + year
#          so seasonally-adjusted series receive the correct weight
#
# CHANGES FROM ORIGINAL:
#  - ann_chg computed & NA-filtered BEFORE trim (fixes trim accuracy)
#  - cumwt arrange scoped per group via .by_group = TRUE (fixes trim math)
#  - month-window logic replaced with correct date-range cutoff
#  - start_date / n_months conflict resolved: start_date anchors the window
#  - cpi_cache env wired up (4-hour on-disk file cache in tempdir)
#  - plot_height isolated from data pipeline (no unnecessary recomputes)
#  - trim_pct / n_months sliders debounced (400 ms)
#  - debug panel now reads from reactive log, not a duplicated pipeline
#  - Download buttons for chart (PNG) and filtered data (CSV)
#  - Stats panel rendered as styled HTML cards, not raw cat()
#  - Equal-weight fallback shown as a yellow UI banner
#  - BLS contact email read from env var BLS_EMAIL (falls back to placeholder)
#  - magrittr removed (native pipe |> used throughout, requires R >= 4.1)
#  - data.table removed; readr::read_tsv used instead
#  - shinycssloaders spinner on ridgeline plot
# =============================================================================

library(shiny)
library(tidyverse)
library(httr)
library(ggridges)
library(lubridate)
library(janitor)
library(viridis)
library(scales)
library(shinycssloaders)

# -----------------------------------------------------------------------------
# Constants
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

# Read contact email from environment; fall back to a placeholder.
# Set via: Sys.setenv(BLS_EMAIL = "you@example.com") or .Renviron
BLS_EMAIL <- Sys.getenv("BLS_EMAIL", unset = "contact@example.com")

CACHE_FILE  <- file.path(tempdir(), "cpi_cache.rds")
CACHE_HOURS <- 4   # hours before a re-fetch is triggered

# -----------------------------------------------------------------------------
# Data loading  (with 4-hour file cache)
# -----------------------------------------------------------------------------

load_cpi_data <- function() {

  # --- serve from cache if fresh enough ---------------------------------------
  if (file.exists(CACHE_FILE)) {
    age_h <- as.numeric(difftime(Sys.time(), file.mtime(CACHE_FILE), units = "hours"))
    if (age_h < CACHE_HOURS) {
      message(sprintf("Serving from cache (%.1f h old)", age_h))
      return(readRDS(CACHE_FILE))
    }
  }

  # --- fetch helper -----------------------------------------------------------
  fetch_bls <- function(url) {
    GET(url, user_agent(BLS_EMAIL)) |>
      content(as = "text") |>
      readr::read_tsv(show_col_types = FALSE) |>
      clean_names()
  }

  message("Fetching CPI observations...")
  cpi <- fetch_bls("https://download.bls.gov/pub/time.series/cu/cu.data.0.Current") |>
    mutate(
      series_id = trimws(series_id),
      value      = suppressWarnings(as.numeric(value)),
      date       = as.Date(
        paste(substr(period, 2, 3), "01", substr(year, 3, 4), sep = "/"),
        "%m/%d/%y"
      )
    )

  message("Fetching series + item metadata...")
  series <- fetch_bls("https://download.bls.gov/pub/time.series/cu/cu.series") |>
    mutate(series_id = trimws(series_id))

  items <- fetch_bls("https://download.bls.gov/pub/time.series/cu/cu.item")

  series <- left_join(series, items, by = "item_code")

  # --- weights ----------------------------------------------------------------
  # BLS only publishes aspect weights for UNADJUSTED series.  Collapse to
  # item_name + year so both S and U variants get the right weight.
  message("Fetching weights from cu.aspect...")
  weights <- tryCatch({
    aspect <- fetch_bls("https://download.bls.gov/pub/time.series/cu/cu.aspect") |>
      mutate(
        series_id = trimws(series_id),
        value      = suppressWarnings(as.numeric(value))
      )

    w <- aspect |> filter(aspect_type == "I1")
    if (nrow(w) == 0) w <- aspect |> filter(aspect_type == "R1")
    if (nrow(w) == 0) {
      best <- aspect |>
        filter(!is.na(value)) |>
        count(aspect_type) |>
        slice_max(n, n = 1, with_ties = FALSE) |>
        pull(aspect_type)
      w <- aspect |> filter(aspect_type == best)
      message("Used fallback aspect_type: ", best)
    }

    w |>
      filter(!is.na(value)) |>
      select(series_id, year, weight = value) |>
      left_join(series |> select(series_id, item_name), by = "series_id") |>
      filter(!is.na(item_name)) |>
      group_by(item_name, year) |>
      summarise(weight = mean(weight, na.rm = TRUE), .groups = "drop")

  }, error = function(e) {
    message("Weight fetch failed (", e$message, ") — using equal weights")
    tibble(item_name = character(), year = integer(), weight = numeric())
  })

  cpi <- cpi |>
    inner_join(series, by = "series_id") |>
    left_join(weights, by = c("item_name", "year"))

  if (all(is.na(cpi$weight))) {
    message("WARNING: all weights NA — using equal weights (unweighted trim)")
    cpi <- cpi |> mutate(weight = 1)
    attr(cpi, "equal_weights") <- TRUE
  } else {
    attr(cpi, "equal_weights") <- FALSE
  }

  message(sprintf(
    "Load complete: %s rows | %s with weight | %s unique items",
    nrow(cpi), sum(!is.na(cpi$weight)), n_distinct(cpi$item_name)
  ))

  saveRDS(cpi, CACHE_FILE)
  cpi
}

# -----------------------------------------------------------------------------
# Shared trim helper  (single source of truth — used by plot_data & debug)
# -----------------------------------------------------------------------------

apply_trim <- function(cpi, trim_pct, include_oer, n_months, start_date) {
  trim_lo <- trim_pct / 100
  trim_hi <- 1 - trim_lo

  # Determine date window: start_date ... latest, capped to n_months lookback
  max_date    <- max(cpi$date, na.rm = TRUE)
  window_start <- max(as.Date(start_date),
                      max_date %m-% months(n_months - 1))

  log <- list()

  s1 <- cpi |> filter(
    item_name %in% MEDIAN_COMPONENTS |
      (include_oer & item_name == "Owners' equivalent rent of residences")
  )
  log[["after_component"]] <- nrow(s1)

  s2 <- s1 |> filter(period != "M13", trimws(seasonal) == "S")
  log[["after_period_seasonal"]] <- nrow(s2)

  # FIX 1: compute mom_chg and ann_chg, then drop NAs BEFORE trimming
  # so the trim isn't distorted by rows with missing changes.
  s3 <- s2 |>
    arrange(date) |>
    group_by(item_name) |>
    mutate(
      mom_chg = value / lag(value, 1) - 1,
      ann_chg = (1 + mom_chg)^12 - 1
    ) |>
    ungroup() |>
    filter(!is.na(mom_chg))
  log[["after_na_drop"]] <- nrow(s3)

  # FIX 2: arrange inside group via .by_group = TRUE so cumwt is per-date
  s4 <- s3 |>
    group_by(date) |>
    mutate(
      weight_sum = sum(weight, na.rm = TRUE),
      weight_n   = if_else(weight_sum > 0, weight / weight_sum, NA_real_)
    ) |>
    arrange(mom_chg, .by_group = TRUE) |>   # scoped to each date group
    mutate(cumwt = cumsum(replace_na(weight_n, 0))) |>
    ungroup()
  log[["after_cumwt"]] <- nrow(s4)

  s5 <- s4 |> filter(cumwt >= trim_lo, cumwt <= trim_hi)
  log[["after_trim"]] <- nrow(s5)

  # FIX 3: date window is a proper range, not a month-number set
  s6 <- s5 |> filter(date >= window_start, date <= max_date)
  log[["after_date_window"]] <- nrow(s6)

  s7 <- s6 |>
    filter(!is.na(ann_chg)) |>
    mutate(month_label = fct_rev(fct_reorder(format(date, "%B, %Y"), date)))
  log[["final"]] <- nrow(s7)

  list(data = s7, log = log, window_start = window_start, max_date = max_date)
}

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
           background: #0d1117; color: #e6edf3; }
    .well { background: #161b22; border: 1px solid #30363d; border-radius: 6px; }
    h2    { color: #f0f6fc; font-weight: 700; letter-spacing: -0.5px; }
    .control-label { color: #8b949e; font-size: 12px; font-weight: 600;
                     text-transform: uppercase; letter-spacing: 0.5px; }
    .irs--shiny .irs-bar    { background: #1f6feb; border-color: #1f6feb; }
    .irs--shiny .irs-handle { background: #58a6ff; border-color: #58a6ff; }
    hr { border-color: #30363d; }
    .btn-primary          { background: #238636; border-color: #2ea043; }
    .btn-primary:hover    { background: #2ea043; }
    .btn-default          { background: #21262d; border-color: #30363d; color: #e6edf3; }
    .btn-default:hover    { background: #30363d; color: #e6edf3; }
    #status_msg           { color: #8b949e; font-size: 12px; margin-top: 6px; }
    pre { background: #161b22 !important; color: #8b949e !important;
          font-size: 11px; border: 1px solid #30363d; }
    .stat-card {
      background: #161b22; border: 1px solid #30363d; border-radius: 6px;
      padding: 12px 16px; margin-bottom: 10px;
    }
    .stat-label { color: #8b949e; font-size: 11px; text-transform: uppercase;
                  letter-spacing: 0.5px; margin-bottom: 4px; }
    .stat-value { color: #f0f6fc; font-size: 22px; font-weight: 700; }
    .stat-sub   { color: #8b949e; font-size: 11px; margin-top: 2px; }
    .banner-warn {
      color: #e3b341; padding: 10px 14px; background: #2d2208;
      border: 1px solid #e3b341; border-radius: 4px; margin-bottom: 10px;
      font-size: 12px;
    }
    .banner-err {
      color: #f85149; padding: 10px 14px; background: #2d1b1b;
      border: 1px solid #f85149; border-radius: 4px; margin-bottom: 10px;
    }
  "))),

  titlePanel(div(
    h2("CPI Inflation \u2014 Distribution Ridgeline Explorer"),
    tags$p(style = "color:#8b949e; font-size:13px; margin-top:-10px;",
           "Live BLS data \u00b7 h/t Mike Konczal")
  )),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # -- Date & display ------------------------------------------------------
      tags$b("DATE & DISPLAY", style = "color:#8b949e; font-size:11px;"),
      hr(style = "margin:6px 0;"),
      dateInput("start_date", "Window Start Date",
                value = "2018-06-01", min = "2000-01-01"),
      sliderInput("n_months", "Max Months to Show",
                  min = 1, max = 84, value = 24, step = 1),
      tags$p(style = "color:#8b949e; font-size:11px; margin-top:-8px;",
             "Window = start_date \u2192 latest, capped to N months lookback."),

      # -- Trim ----------------------------------------------------------------
      tags$b("DISTRIBUTION TRIM",
             style = "color:#8b949e; font-size:11px; margin-top:16px; display:block;"),
      hr(style = "margin:6px 0;"),
      sliderInput("trim_pct", "Trim Each Tail (% by weight)",
                  min = 0, max = 49, value = 29, step = 1, post = "%"),

      # -- Appearance ----------------------------------------------------------
      tags$b("APPEARANCE",
             style = "color:#8b949e; font-size:11px; margin-top:16px; display:block;"),
      hr(style = "margin:6px 0;"),
      # FIX: plot_height is read directly in renderUI, NOT inside plot_data,
      # so resizing never triggers a data recompute.
      sliderInput("plot_height", "Plot Height (px)",
                  min = 300, max = 3000, value = 1800, step = 20),
      selectInput("color_palette", "Color Palette",
                  choices = c(
                    "Turbo"   = "H",
                    "Inferno" = "B",
                    "Plasma"  = "C",
                    "Viridis" = "D",
                    "Magma"   = "A",
                    "Cividis" = "E"
                  ), selected = "H"),
      tags$p(style = "color:#8b949e; font-size:11px; margin-top:-8px;",
             "Turbo requires viridis \u2265 0.6.0"),
      selectInput("x_range", "X-Axis Range",
                  choices = c(
                    "Auto"   = "auto",
                    "\u00b110%" = "10",
                    "\u00b120%" = "20",
                    "\u00b130%" = "30"
                  ), selected = "auto"),
      checkboxInput("show_zero",  "Show 0% reference line",  value = TRUE),
      checkboxInput("show_2pct", "Show 2% target line",      value = TRUE),

      # -- Components ----------------------------------------------------------
      tags$b("COMPONENTS",
             style = "color:#8b949e; font-size:11px; margin-top:16px; display:block;"),
      hr(style = "margin:6px 0;"),
      checkboxInput("include_oer", "Include OER as single component", value = TRUE),

      hr(),
      actionButton("refresh", "\u21ba Refresh BLS Data",
                   class = "btn-primary", width = "100%"),
      div(id = "status_msg", textOutput("status")),

      hr(),
      # Download buttons
      tags$b("EXPORT", style = "color:#8b949e; font-size:11px;"),
      hr(style = "margin:6px 0;"),
      downloadButton("dl_plot", "Download Chart (PNG)", class = "btn-default",
                     style = "width:100%; margin-bottom:6px;"),
      downloadButton("dl_data", "Download Data (CSV)",  class = "btn-default",
                     style = "width:100%;"),

      hr(),
      checkboxInput("show_debug", "Show debug panel", value = FALSE)
    ),

    mainPanel(
      width = 9,
      uiOutput("weight_banner"),
      uiOutput("error_banner"),

      # FIX: plot container reads input$plot_height in renderUI (UI layer only)
      uiOutput("plot_container"),

      hr(style = "border-color:#30363d;"),
      fluidRow(
        column(8, uiOutput("stats_cards")),
        column(4, tags$p(
          style = "color:#8b949e; font-size:12px; margin-top:10px;",
          "Tip: Narrowing the trim removes outlier categories and reveals where
           the core of price changes is clustering. A tighter ridge means more
           categories are inflating at a similar rate."
        ))
      ),

      conditionalPanel(
        condition = "input.show_debug == true",
        hr(style = "border-color:#30363d;"),
        tags$b("Debug — filter funnel", style = "color:#8b949e; font-size:11px;"),
        verbatimTextOutput("debug_out")
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # -- 1. Raw data (cached) ----------------------------------------------------
  raw_data <- eventReactive(input$refresh, {
    withProgress(message = "Fetching BLS data\u2026", value = 0.5, {
      tryCatch(load_cpi_data(), error = function(e) {
        message("Load error: ", e$message); NULL
      })
    })
  }, ignoreNULL = FALSE)

  # -- 2. Debounce sliders that are expensive to recompute on every tick -------
  trim_d    <- debounce(reactive(input$trim_pct),  400)
  n_months_d <- debounce(reactive(input$n_months), 400)

  # -- 3. Shared computed dataset ----------------------------------------------
  result <- reactive({
    req(raw_data())
    apply_trim(
      cpi         = raw_data(),
      trim_pct    = trim_d(),
      include_oer = input$include_oer,
      n_months    = n_months_d(),
      start_date  = input$start_date
    )
  })

  plot_data <- reactive(result()$data)

  # -- 4. Equal-weight banner --------------------------------------------------
  output$weight_banner <- renderUI({
    req(raw_data())
    if (isTRUE(attr(raw_data(), "equal_weights"))) {
      div(class = "banner-warn",
          tags$b("\u26a0 Equal weights in use. "),
          "The BLS aspect (weights) file could not be loaded. ",
          "All components are being treated as equally weighted. ",
          "Results may differ from official trimmed-mean CPI figures.")
    }
  })

  # -- 5. Error / empty-data banner --------------------------------------------
  output$error_banner <- renderUI({
    res <- tryCatch(result(), error = function(e) e)
    if (inherits(res, "error")) {
      div(class = "banner-err",
          strong("Error: "), res$message)
    } else if (nrow(res$data) == 0) {
      div(class = "banner-warn",
          strong("No data to plot. "),
          "Try widening the date window or enabling the debug panel.")
    }
  })

  # -- 6. Plot height (isolated — does NOT trigger data recompute) -------------
  output$plot_container <- renderUI({
    h <- input$plot_height   # reads only plot_height, not any data reactive
    withSpinner(
      plotOutput("ridgeline", height = paste0(h, "px")),
      color = "#58a6ff", type = 6
    )
  })

  # -- 7. Build the ggplot object (shared by screen render + PNG download) -----
  build_plot <- reactive({
    df <- plot_data()
    validate(need(nrow(df) > 0, ""))

    latest_label <- format(max(df$date), "%B %Y")

    p <- ggplot(df, aes(x = ann_chg, y = month_label, fill = after_stat(x))) +
      geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
      scale_fill_viridis(option = input$color_palette) +
      theme_ridges(grid = TRUE) +
      scale_x_continuous(labels = percent) +
      labs(
        title    = "CPI Inflation Report: Distribution Ridgeline Plot",
        subtitle = sprintf(
          "Trimmed distribution \u2014 dropping bottom %d%% and top %d%% by weight \u00b7 latest: %s",
          input$trim_pct, input$trim_pct, latest_label
        ),
        x       = "One-Month Change, Annualised",
        y       = NULL,
        caption = "OER treated as one value \u00b7 h/t Mike Konczal \u00b7 Source: BLS"
      ) +
      theme(
        plot.background  = element_rect(fill = "#0d1117", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        plot.title.position = "plot",
        legend.position  = "none",
        plot.title    = element_text(size = 20, color = "#f0f6fc",
                                     margin = margin(0, 0, 5, 0)),
        plot.subtitle = element_text(size = 13, color = "#8b949e"),
        plot.caption  = element_text(size = 10, face = "italic", color = "#8b949e"),
        axis.text.y   = element_text(size = 12, face = "bold", color = "#e6edf3"),
        axis.text.x   = element_text(size = 12, color = "#e6edf3"),
        panel.grid.major = element_line(color = "#30363d"),
        panel.grid.minor = element_blank()
      )

    if (input$x_range != "auto") {
      lim <- as.numeric(input$x_range) / 100
      p <- p + coord_cartesian(xlim = c(-lim, lim))
    }
    if (input$show_zero) {
      p <- p + geom_vline(xintercept = 0, color = "#f85149",
                          linetype = "dashed", linewidth = 0.7, alpha = 0.8)
    }
    if (input$show_2pct) {
      p <- p +
        geom_vline(xintercept = 0.02, color = "#58a6ff",
                   linetype = "dashed", linewidth = 0.7, alpha = 0.8) +
        annotate("text", x = 0.02, y = Inf, label = "2% target",
                 color = "#58a6ff", size = 3.2, hjust = -0.1, vjust = 1.5)
    }
    p
  })

  output$ridgeline <- renderPlot(build_plot(), bg = "#0d1117")

  # -- 8. Stats cards (HTML, not cat()) ----------------------------------------
  output$stats_cards <- renderUI({
    df <- plot_data()
    req(nrow(df) > 0)
    latest <- df |> filter(date == max(date))

    stat_card <- function(label, value, sub = NULL) {
      div(class = "stat-card",
          div(class = "stat-label", label),
          div(class = "stat-value", value),
          if (!is.null(sub)) div(class = "stat-sub", sub)
      )
    }

    fluidRow(
      column(4, stat_card(
        "Latest Month", format(max(df$date), "%B %Y")
      )),
      column(4, stat_card(
        "Components in Plot", n_distinct(latest$item_name),
        sub = "after trim"
      )),
      column(4, stat_card(
        "Median (annualised)",
        percent(median(latest$ann_chg, na.rm = TRUE), accuracy = 0.1)
      )),
      column(4, stat_card(
        "Mean (annualised)",
        percent(mean(latest$ann_chg, na.rm = TRUE), accuracy = 0.1)
      )),
      column(4, stat_card(
        "IQR",
        percent(IQR(latest$ann_chg, na.rm = TRUE), accuracy = 0.1)
      )),
      column(4, stat_card(
        "Date Window",
        paste(format(min(df$date), "%b %Y"), "\u2192", format(max(df$date), "%b %Y"))
      ))
    )
  })

  # -- 9. Debug panel (reads from shared result, no duplicated pipeline) -------
  output$debug_out <- renderPrint({
    req(result())
    res <- result()
    cat("=== FILTER FUNNEL ===\n")
    for (nm in names(res$log)) {
      cat(sprintf("  %-28s %d rows\n", nm, res$log[[nm]]))
    }
    cat(sprintf("\nDate window: %s -> %s\n",
                res$window_start, res$max_date))
    cat(sprintf("trim: [%.2f, %.2f]\n",
                input$trim_pct / 100, 1 - input$trim_pct / 100))

    req(raw_data())
    cat("\n=== WEIGHT SAMPLE (latest year) ===\n")
    raw_data() |>
      filter(item_name %in% MEDIAN_COMPONENTS, !is.na(weight)) |>
      distinct(item_name, year, weight) |>
      slice_max(year, n = 10, with_ties = FALSE) |>
      as.data.frame() |>
      print()
  })

  # -- 10. Download: chart (PNG) -----------------------------------------------
  output$dl_plot <- downloadHandler(
    filename = function() {
      paste0("cpi_ridgeline_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      ggsave(file, plot = build_plot(),
             width = 14, height = input$plot_height / 96,
             dpi = 150, bg = "#0d1117")
    }
  )

  # -- 11. Download: filtered data (CSV) ---------------------------------------
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("cpi_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      plot_data() |>
        select(date, item_name, mom_chg, ann_chg, weight, cumwt) |>
        readr::write_csv(file)
    }
  )

  # -- 12. Status line ---------------------------------------------------------
  output$status <- renderText({
    if (is.null(raw_data())) "Loading\u2026"
    else {
      age <- if (file.exists(CACHE_FILE))
        sprintf(" (cached %.0f min ago)",
                as.numeric(difftime(Sys.time(), file.mtime(CACHE_FILE), units = "mins")))
      else ""
      paste0("Last fetch: ", format(Sys.time(), "%H:%M:%S"), age)
    }
  })
}

shinyApp(ui, server)
