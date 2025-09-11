library(tidyverse)
library(tidyusmacro)
library(lubridate)


avg_prices <- getBLSFiles("averageprice", "konczal@gmail.com")



avg_prices_latest <- avg_prices %>%
  filter(area_code == "0000") %>%                 # keep all years for lag
  arrange(item_name, date, .by_group = TRUE) %>%
  group_by(item_name) %>%
  mutate(
    value_12m = lag(value, 12),
    yoyP = value / value_12m - 1,
    yoyA = value - value_12m
  ) %>%
  filter(end_year == 2025) %>%                    # now restrict to 2025
  slice_max(date, n = 1, with_ties = FALSE) %>%   # latest month in 2025
  ungroup() %>%
  select(item_name, date, value_now = value, value_12m, yoyP, yoyA)


View(avg_prices_latest)

avg_prices %>%
  filter(area_code == "0000", end_year == 2025,
         item_name %in% c("Coffee, 100%, ground roast, all sizes, per lb. (453.6 gm)",
                   "Orange juice, frozen concentrate, 12 oz. can, per 16 oz. (473.2 ml)",
                   "Bananas, per lb. (453.6 gm)",
                   "Lemons, per lb. (453.6 gm)",
                   "Wine, red and white table, all sizes, any origin, per 1 liter (33.8 oz)",
                   "Malt beverages, all types, all sizes, any origin, per 16 oz. (473.2 ml)")
) %>%
  arrange(item_name, date, .by_group = TRUE) %>%
  group_by(item_name) %>%
  mutate(
    value_12m = lag(value, 12),
    yoyP = value / value_12m - 1,
    yoyA = value - value_12m
  ) %>%
  ggplot(aes(date, value)) + geom_line() +
  facet_wrap(~item_name, scale = "free") +
  theme_esp()


avg_prices %>%
  filter(area_code == "0000", year >= 2024,
item_name == "Coffee, 100%, ground roast, all sizes, per lb. (453.6 gm)") %>%
  ggplot(aes(date, value)) + geom_line()