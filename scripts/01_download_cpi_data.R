# This file downloads the CPI files from BLS.gov, formats them,
# and merges them.

# Libraries
library(tidyusmacro)

cpi_data <- getBLSFiles("cpi", "rortybomb@gmail.com")

# Add weight data
# We only have weight data from 2022 onward; 2022 is used prior.
cpi_weights <- read_csv(file = "weights/inflation_weights.csv") %>% filter(!is.na(weight))
base_weight <- cpi_weights %>% filter(year_weight == 2022) %>% select(item_name, base_weight = weight)
cpi_data <- left_join(cpi_data, base_weight, by = c("item_name"))

latest_weight_year <- max(cpi_weights$year_weight, na.rm = TRUE)
cpi_data <- cpi_data %>%
  mutate(weight_year = pmin(year, latest_weight_year))

cpi_weights_later <- cpi_weights %>% select(item_name, later_weight = weight, weight_year = year_weight)
cpi_data <- left_join(cpi_data, cpi_weights_later, by = c("item_name", "weight_year"))

cpi_data$weight <- ifelse(!is.na(cpi_data$later_weight), cpi_data$later_weight, cpi_data$base_weight)
cpi_data <- cpi_data %>% select(-base_weight, -later_weight, -weight_year)
