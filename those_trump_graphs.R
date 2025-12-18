cpi %>%
  group_by(item_name) %>%
  reframe(
    change = value[date == max(date)] / value[date == "2025-01-01"] - 1
  ) %>%
  arrange(change) %>%
  head(18)


avg_prices %>%
  group_by(series_title) %>%
  reframe(
    change_2025 = value[date == max(date)] / value[date == "2025-01-01"] - 1
  ) %>%
  arrange(change_2025)
