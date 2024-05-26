library(dplyr)
library(tidyverse)
library(ggplot2)
data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990.csv")
county_data <- data %>%
  filter(state == "WA" & county_name == "King County") %>%
  pivot_longer(cols = c(aapi_prison_pop_rate, black_prison_pop_rate, native_prison_pop_rate, white_prison_pop_rate), 
               names_to = "race", 
               values_to = "prison_pop_rate") %>%
  mutate(race = recode(race, 
                       aapi_prison_pop_rate = "Asian American / Pacific Islander",
                       black_prison_pop_rate = "Black",
                       latinx_prison_pop_rate = "Latinx",
                       native_prison_pop_rate = "Native American",
                       white_prison_pop_rate = "White"))
ggplot(county_data, aes(x = year, y = prison_pop_rate, color = race)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(county_data$year), max(county_data$year), by = 5)) +
  labs(title = "Prison Population Rate in King County, WA",
       x = "Year",
       y = "Prison Population per 100,000 People",
       color = "Race/Ethnicity") +
  theme_minimal()