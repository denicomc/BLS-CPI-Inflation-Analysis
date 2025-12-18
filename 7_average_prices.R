library(tidyverse)
library(tidyusmacro)
library(lubridate)


avg_prices <- getBLSFiles("averageprice", "konczal@gmail.com")


avg_prices %>%
  filter(area_code == "0000") %>% # keep all years for lag
  arrange(item_name, date, .by_group = TRUE) %>%
  group_by(item_name) %>%
  mutate(
    value_12m = lag(value, 12),
    yoyP = value / value_12m - 1,
    yoyA = value - value_12m
  ) %>%
  filter(end_year == 2025) %>% # now restrict to 2025
  slice_max(date, n = 1, with_ties = FALSE) %>% # latest month in 2025
  ungroup() %>%
  select(item_name, date, value_now = value, value_12m, yoyP, yoyA) %>%
  arrange(desc(yoyA)) %>%
  head(20)


price_list <- c(
  "Coffee, 100%, ground roast, all sizes, per lb. (453.6 gm)",
  "Bananas, per lb. (453.6 gm)", # common grocery item with price sensitivity:contentReference[oaicite:2]{index=2}
  "Eggs, Grade A, large, per doz.", # volatile staple with consumer sensitivity:contentReference[oaicite:3]{index=3
  "Gasoline, unleaded regular, per gallon", # key transport expense with high visibility:contentReference[oaicite:5]{index=5}
  "Electricity per KWH", # utility price often noted in household budgets:contentReference[oaicite:7]{index=7}
  "Ground beef, 100% beef, per lb. (453.6 gm)" # frequently highlighted meat price in grocery inflation discussions:contentReference[oaicite:8]{index=8}
)

date_breaks <- generate_dates(avg_prices$date, 10)


avg_prices %>%
  filter(area_code == "0000", end_year == 2025, item_name %in% price_list) %>%
  filter(year >= 2022) %>%
  ggplot(aes(date, value)) +
  geom_line() +
  facet_wrap(~item_name, scale = "free") +
  theme_esp() +
  scale_x_date(date_labels = "%b\n%Y", breaks = date_breaks) +
  geom_vline(xintercept = as.Date("2025-01-01")) +
  scale_y_continuous(label = dollar) +
  labs(
    title = "Price Increases For Important Items in 2025",
    subtitle = "BLS, CPI: Average Price Data. Not Seasonally Adjusted.",
    caption = "Mike Konczal, Economic Security Project."
  )


ggsave(
  "graphics/high_prices.png",
  dpi = "retina",
  width = 12,
  height = 6.75,
  units = "in"
)
