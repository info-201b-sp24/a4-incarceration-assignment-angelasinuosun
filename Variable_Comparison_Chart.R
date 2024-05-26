library(dplyr)
library(tidyverse)
library(ggplot2)

rate_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990.csv")

highest_prison_race <- function(x){
  races <- c("aapi", "black", "latinx", "native", "white")
  rates <- x
  if (all(is.na(rates))) {
    return(NA_character_)
  } else {
    return(races[which.max(rates)])
  }
}
rate_data <- rate_data %>%
  mutate(race_highest_prison = pmap_chr(
    list(aapi_prison_pop_rate, black_prison_pop_rate, latinx_prison_pop_rate, 
         native_prison_pop_rate, white_prison_pop_rate), 
    ~ highest_prison_race(c(...)))) %>%
  filter(!is.na(race_highest_prison)) %>%
  filter(year >= (max(year) - 20)) %>%
  drop_na()

ggplot(rate_data, aes(x = female_prison_pop_rate, y = male_prison_pop_rate, 
                      color = race_highest_prison)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Male and Female Prison Population Rate") +
  theme_minimal()