library(tidyverse)
library(lubridate)
library(scales)

# Download CPI data with weights merged
source("scripts/01_download_cpi_data.R")

category_lookup <- read_csv("weights/most_prices.csv", show_col_types = FALSE) %>%
  select(item_name, category)

jan_data <- cpi_data %>%
  filter(
    seasonal == "U",
    area_code == "0000",
    period == "M01",
    !is.na(date)
  ) %>%
  transmute(item_name, year = year(date), value)

required_years <- c(2024, 2025, 2026)
missing_years <- setdiff(required_years, unique(jan_data$year))
if (length(missing_years) > 0) {
  stop(
    paste0(
      "Missing January CPI data for year(s): ",
      paste(sort(missing_years), collapse = ", ")
    )
  )
}

acceleration <- jan_data %>%
  filter(year %in% required_years) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "value_") %>%
  mutate(
    jan_2025_vs_2024 = value_2025 / value_2024 - 1,
    jan_2026_vs_2025 = value_2026 / value_2025 - 1,
    abs_acceleration = abs(jan_2026_vs_2025) - abs(jan_2025_vs_2024)
  ) %>%
  filter(!is.na(abs_acceleration), abs_acceleration > 0) %>%
  left_join(category_lookup, by = "item_name") %>%
  mutate(category = replace_na(category, "Uncategorized"))

category_levels <- c("Services", "Goods", "Food", "Energy", "Meta", "Uncategorized")
category_colors <- c(
  "Services" = "#2c3254",
  "Goods" = "#ff8361",
  "Food" = "#70ad8f",
  "Energy" = "#f2b134",
  "Meta" = "#8a8a8a",
  "Uncategorized" = "#5c5c5c"
)

esp_clean_theme <- theme_esp(base_family = "sans") +
  theme(
    plot.title.position = "plot",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey80")
  )

# Graphic 1: Accelerators by category (top 12 per category)
by_category <- acceleration %>%
  filter(category %in% c("Services", "Goods", "Food", "Energy")) %>%
  mutate(category = factor(category, levels = category_levels)) %>%
  group_by(category) %>%
  slice_max(order_by = abs_acceleration, n = 12, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(category, desc(abs_acceleration)) %>%
  mutate(item_name = factor(item_name, levels = rev(unique(item_name))))

ggplot(by_category, aes(item_name, abs_acceleration, fill = category)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~category, scales = "free_y", ncol = 2) +
  esp_clean_theme +
  scale_fill_manual(values = category_colors, drop = FALSE) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "January Price Acceleration by Category",
    subtitle = "Top 12 per category where |Jan 2026 vs Jan 2025| is above |Jan 2025 vs Jan 2024|.",
    x = NULL,
    y = "Absolute acceleration in January inflation rate",
    caption = "BLS CPI, seasonally unadjusted (national). Author's calculations. Mike Konczal."
  ) +
  theme(
    legend.position = "none"
  )

ggsave(
  "graphics/jan_acceleration_by_category.png",
  dpi = "retina",
  width = 12,
  height = 9,
  units = "in"
)

# Graphic 2: Top 20 accelerators overall
top20 <- acceleration %>%
  mutate(category = factor(category, levels = category_levels)) %>%
  slice_max(order_by = abs_acceleration, n = 20, with_ties = FALSE) %>%
  arrange(abs_acceleration) %>%
  mutate(item_name = factor(item_name, levels = item_name))

ggplot(top20, aes(abs_acceleration, item_name, fill = category)) +
  geom_col() +
  geom_vline(xintercept = 0, color = "grey70") +
  esp_clean_theme +
  scale_fill_manual(values = category_colors, drop = FALSE) +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Top 20 January Price Accelerators",
    subtitle = "Largest increases in |Jan YoY| in 2026 compared with the 2025-vs-2024 baseline.",
    x = "Absolute acceleration in January inflation rate",
    y = NULL,
    caption = "BLS CPI, seasonally unadjusted (national). Author's calculations. Mike Konczal."
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

ggsave(
  "graphics/jan_acceleration_top20.png",
  dpi = "retina",
  width = 12,
  height = 8,
  units = "in"
)
