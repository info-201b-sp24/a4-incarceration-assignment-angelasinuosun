library(dplyr)
library(tidyverse)
library(purrr)
data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990.csv")
data_population <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-jail-pop.csv")

# How many columns and rows are in this dataset?
data %>%
  summarise(num_rows = nrow(data), num_cols = ncol(data))
data_population %>%
  summarise(num_rows = nrow(data_population), num_cols = ncol(data_population))

data <- data %>%
  mutate(location = paste(county_name, state, sep = ", "))

highest_prison_race <- function(x){
  races <- c("aapi", "black", "latinx", "native", "white")
  rates <- x
  if (all(is.na(rates))) {
    return(NA_character_)
  } else {
    return(races[which.max(rates)])
  }
}
data <- data %>%
  mutate(race_highest_prison = pmap_chr(
    list(aapi_prison_pop_rate, black_prison_pop_rate, latinx_prison_pop_rate,
         native_prison_pop_rate, white_prison_pop_rate),
    ~ highest_prison_race(c(...))))
highest_jail_race <- function(x){
  races <- c("aapi", "black", "latinx", "native", "white")
  rates <- x
  if (all(is.na(rates))) {
    return(NA_character_)
  } else {
    return(races[which.max(rates)])
  }
}
data <- data %>%
  mutate(race_highest_jail = pmap_chr(
    list(aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate, 
         native_jail_pop_rate, white_jail_pop_rate), 
    ~ highest_jail_race(c(...))))

# In King County -- Washington's most populated county
# Which is the highest total prison population rate that has occured in King County, WA?
# What is that rate and what is the year it occurred?
# What is the corresponding race that has the highest prison_pop_rate in King County at that time?
data %>%
  filter(location == "King County, WA") %>%
  filter(total_prison_pop_rate == max(total_prison_pop_rate, na.rm = TRUE)) %>%
  summarise(highest_rate = total_prison_pop_rate, year = year, race = race_highest_prison)

# Gender
# What is the mean, median, and standard deviation of male prison population rate?
data %>%
  summarise(mean = mean(male_prison_pop_rate, na.rm = TRUE), 
            median = median(male_prison_pop_rate, na.rm = TRUE),
            sd = sd(male_prison_pop_rate, na.rm = TRUE))
# What about female?
data %>%
  summarise(mean = mean(female_prison_pop_rate, na.rm = TRUE), 
            median = median(female_prison_pop_rate, na.rm = TRUE),
            sd = sd(female_prison_pop_rate, na.rm = TRUE))
# States with top 3 highest total jail population
top_states <- data_population %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  arrange(desc(total_jail_pop)) %>%
  slice(1:3) %>%
  reframe(state = state)
top_states_rate <- data %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  filter(state %in% top_states$state) %>%
  count(state, race_highest_jail) %>%
  arrange(state, desc(n)) %>%
  group_by(state) %>%
  slice(1) %>%
  ungroup() %>%
  rename(race_highest_frequent = race_highest_jail, count = n) %>%
  select(state, race_highest_frequent)