library(tidyverse)
library(lubridate)
library(scales)
library(stringr)

annual_comparison <- c(
  "All items",
  "All items less food and energy",
  "Food",
  "Food at home",
  "Food away from home",
  "Energy",
  "Commodities less food and energy commodities",
  "Services less energy services",
  "Shelter",
  "All items less food, shelter, energy, and used cars and trucks",
  "Services less rent of shelter"
)

# --- Meta grouping ---
meta_lookup <- tibble(
  item_name = annual_comparison,
  meta = case_when(
    item_name %in%
      c(
        "All items",
        "All items less food and energy",
        "All items less food, shelter, energy, and used cars and trucks"
      ) ~ "All items",
    item_name %in%
      c(
        "Services less energy services",
        "Shelter",
        "Services less rent of shelter"
      ) ~ "Services (incl. shelter)",
    item_name %in%
      c(
        "Food",
        "Food at home",
        "Food away from home"
      ) ~ "Food",
    TRUE ~ "Rest"
  )
)

n_months_2025 <- month(max(cpi$date))

annual_comparison_results <- cpi %>%
  filter(item_name %in% annual_comparison) %>%
  group_by(item_name) %>%
  summarize(
    change_2025_annualized = (value[date == max(date)] /
      value[date == as.Date("2024-12-01")])^(12 / n_months_2025) -
      1,
    change_2024 = value[date == as.Date("2024-12-01")] /
      value[date == as.Date("2023-12-01")] -
      1,
    .groups = "drop"
  ) %>%
  left_join(meta_lookup, by = "item_name")

plot_df <- annual_comparison_results %>%
  pivot_longer(
    cols = c(change_2025_annualized, change_2024),
    names_to = "year",
    values_to = "change"
  ) %>%
  mutate(
    year = factor(
      year,
      levels = c("change_2025_annualized", "change_2024"),
      labels = c(
        paste0(
          "2025 (annualized YTD, through ",
          format(max(cpi$date), "%b %Y"),
          ")"
        ),
        "2024"
      )
    )
  )

item_order <- annual_comparison_results %>%
  group_by(meta) %>%
  arrange(desc(change_2025_annualized), .by_group = TRUE) %>%
  mutate(item_rank = row_number()) %>%
  ungroup() %>%
  mutate(item_key = paste(meta, item_name, sep = "___")) %>%
  arrange(meta, item_rank)

plot_df <- plot_df %>%
  mutate(item_key = paste(meta, item_name, sep = "___")) %>%
  mutate(item_key = factor(item_key, levels = rev(item_order$item_key)))

# --- ESP-ish fill colors ---
col_2025 <- "#ff8361" # ESP salmon
col_2024 <- "#2c3254" # ESP navy

fill_vals <- setNames(
  c(col_2025, col_2024),
  levels(plot_df$year)
)

ggplot(plot_df, aes(x = change, y = item_key, fill = year)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = percent(change, accuracy = 0.1)),
    position = position_dodge(width = 0.8),
    hjust = ifelse(plot_df$change >= 0, -0.08, 1.08),
    size = 3
  ) +
  facet_grid(meta ~ ., scales = "free_y", space = "free_y") +
  scale_y_discrete(labels = function(x) {
    stringr::str_wrap(sub("^.*___", "", x), width = 34)
  }) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.20))
  ) +
  scale_fill_manual(values = fill_vals) +
  labs(
    title = "CPI inflation: 2025 (annualized YTD) vs 2024",
    subtitle = paste0(
      "2025 annualized from Dec 2024 \u2192 ",
      format(max(cpi$date), "%b %Y"),
      " (",
      n_months_2025,
      " months)"
    ),
    x = "Percent change",
    y = NULL,
    fill = NULL
  ) +
  theme_esp() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.y = element_blank(),
    strip.background = element_blank(),
    axis.text.y = element_text(lineheight = 0.95),
    legend.position = "top"
  )
