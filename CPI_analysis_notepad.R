#https://www.mikekonczal.com/
#https://github.com/mtkonczal/BLS-CPI-Inflation-Analysis/blob/main/CPI_analysis_notepad.R
#install.packages("magrittr") 
library(tidyverse)
library(httr)
library(data.table)
library(magrittr)
library(ggridges)
library(lubridate)
library(janitor)
library(viridis)
library(scales)

library(dplyr)

cpi_data <- GET("https://download.bls.gov/pub/time.series/cu/cu.data.0.Current", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()
cpi_data <- cpi_data %>% clean_names()
cpi_data$value <- as.numeric(cpi_data$value)
#cpi_data$series_id <- str_trim(cpi_data$series_id)
cpi_data$date <- paste(substr(cpi_data$period, 2,3), "01", substr(cpi_data$year, 3, 4), sep="/")
cpi_data$date <- as.Date(cpi_data$date, "%m/%d/%y")

series <- GET("https://download.bls.gov/pub/time.series/cu/cu.series", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()
series <- series %>% clean_names()
cpi_data$series_id <- trimws(cpi_data$series_id)
#series$series_id <- str_trim(series$series_id)
#series$series_id <- series$series_id

items <- GET("https://download.bls.gov/pub/time.series/cu/cu.item", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread()
series <- inner_join(series, items, by = c("item_code"))
cpi_data <- inner_join(cpi_data, series, by = c("series_id"))

cpi_weights <- read_csv(file = "weights/inflation_weights.csv") %>% select(-year_weight)
#cpi_weights <- read_csv(file = "data/inflation_weights.csv") %>% select(-year_weight)
#cpi_weights <- read_csv(file = "~/Downloads/inflation_weights.csv") %>% select(-year_weight)

cpi_data <- inner_join(cpi_data, cpi_weights, by = c("item_name"))
#cpi_weights <- read_csv(file = "data/inflation_weights_2023.csv") %>% select(item_name, weight_2023 = weight, year = year_weight)
#cpi_weights <- read_csv(file = "~/Downloads/inflation_weights_2023.csv") %>% select(item_name, weight_2023 = weight, year = year_weight)
cpi_weights <- read_csv(file = "~/weights/inflation_weights_2023.csv") %>% select(item_name, weight_2023 = weight, year = year_weight)
cpi_data <- left_join(cpi_data, cpi_weights, by = c("item_name", "year"))

cpi_data$weight <- ifelse(!is.na(cpi_data$weight_2023),cpi_data$weight_2023,cpi_data$weight)

### Make the graphic ###
#median_terms <- read_csv("data/mediancpi_component_table.csv") %>% mutate(item_name = Component)
#median_terms <- read_csv("~/Downloads/mediancpi_component_table.csv") %>% mutate(item_name = Component)
median_terms <- read_csv("~/weights/mediancpi_component_table.csv") %>% mutate(item_name = Component)

median <- cpi_data %>%  filter(item_name %in% median_terms$item_name | item_name == "Owners' equivalent rent of residences") %>%
  filter(period != "M13", seasonal == "S") %>%
  arrange(date) %>%
  group_by(item_name) %>%
  mutate(Pchange3 = (value/lag(value, 3)-1)) %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(normalized = sum(weight)) %>%
  mutate(weightN = weight/normalized) %>%
  arrange(Pchange3) %>%
  mutate(cumsum = cumsum(weight)/100) %>%
  mutate(cumsumN = cumsum(weightN)) %>%
  ungroup() %>%
  mutate(Pchange3a = (1+Pchange3)^4-1)

start_month <- month(max(median$date))
quarters <- ((seq(start_month, start_month + 9, by=3) - 1) %% 12) + 1

#THIS IS THE GRAPHIC - 30 percent-trimmed distribution
median %>% mutate(dateF = as.factor(date)) %>%
  filter(cumsumN <= 0.85 & cumsum >= 0.15) %>%
  mutate(Pchange3a = (1+Pchange3)^4-1) %>%
  filter(date >= "2017-06-01") %>%
# filter(date != "2020-06-01") %>%
#  filter(month(date) %in% quarters) %>%
  mutate(monthC = format(date, "%B, %Y")) %>%
  mutate(monthC = fct_reorder(monthC,date)) %>%
  mutate(monthCR = fct_rev(monthC)) %>%
# ggplot(aes(x = Pchange3a, y = monthCR, fill = stat(x))) +
  ggplot(aes(x = Pchange3a, y = monthCR, fill = after_stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis(option = "H") +
  theme_ridges() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = percent) +
  labs(title="Price Inflation Distribution Returning Back",
       x="Three Month Percent Change", y="", caption="OER is treated as one value.\nMike Konczal, Roosevelt Institute") +
  theme(plot.title.position = "plot", legend.position = "none", legend.title = element_blank(),
        plot.title = element_text(size = 20,margin=margin(0,0,5,0)),
        plot.subtitle = element_text(size=13),
        plot.caption = element_text(size=10, face="italic"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.text.x = element_text(size=12))
