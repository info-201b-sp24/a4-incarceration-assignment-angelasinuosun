library(dplyr)
library(tidyverse)
library(purrr)
data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990.csv")
# Only use data for the summary part.

# How many columns and rows are in this dataset?
data %>%
  summarise(num_rows = nrow(data), num_cols = ncol(data))
 
# Location with 

# Create a new column called "location" in the form "County, State"
data <- data %>%
  mutate(location = paste(county_name, state, sep = ", "))
# Which location has the greatest total jail pop rate? and what that rate is?
data %>%
  filter(total_jail_pop_rate == max(total_jail_pop_rate, na.rm = TRUE)) %>%
  summarise(highest_jail = location)
# Which location has the greatest total prison pop rate? and what that rate is?
data %>%
  filter(total_prison_pop_rate == max(total_prison_pop_rate, na.rm = TRUE)) %>%
  summarise(highest_prison = location)

# ADD: in King County WA: 



# Create a function to determine the race with highest jail rate
highest_jail_race <- function(x){
  races <- c("aapi", "black", "latinx", "native", "white")
  rates <- x
  if (all(is.na(rates))) {
    return(NA_character_)
  } else {
    return(races[which.max(rates)])
  }
}
# Create a new column
data <- data %>%
  mutate(race_highest_jail = pmap_chr(
    list(aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate, 
         native_jail_pop_rate, white_jail_pop_rate), 
    ~ highest_jail_race(c(...))))
# Which race has the highest jail population rate in each county most frequently
# (considered all years)? 
data %>%
  group_by(county_name) %>%
  filter(!is.na(race_highest_jail)) %>%
  count(race_highest_jail, sort = TRUE) %>%
  slice(1) %>%
  summarise(most_frequent_race = race_highest_jail) %>%
  count(most_frequent_race, sort = TRUE)

# Prison
highest_prison_race <- function(x){
  races <- c("aapi", "black", "latinx", "native", "white")
  rates <- x
  if (all(is.na(rates))) {
    return(NA_character_)
  } else {
    return(races[which.max(rates)])
  }
}
# Create a new column
data <- data %>%
  mutate(race_highest_prison = pmap_chr(
    list(aapi_prison_pop_rate, black_prison_pop_rate, latinx_prison_pop_rate,
         native_prison_pop_rate, white_prison_pop_rate),
    ~ highest_prison_race(c(...))))
# Which race appears most frequently as the race with the highest prison 
# population rate among all counties?
data %>%
  group_by(county_name) %>%
  filter(!is.na(race_highest_prison)) %>%
  count(race_highest_prison, sort = TRUE) %>%
  slice(1) %>%
  summarise(most_frequent_race = race_highest_prison) %>%
  count(most_frequent_race, sort = TRUE)

# Which urbanicity category has the highest total prison population rate? 
data %>%
  filter(!is.na(total_prison_pop_rate)) %>%
  filter(total_prison_pop_rate == max(total_prison_pop_rate)) %>%
  summarise(urbanicity_highest_prison = urbanicity)

# Which urbanicity category has the highest total jail population rate? 
data %>%
  filter(!is.na(total_jail_pop_rate)) %>%
  filter(total_jail_pop_rate == max(total_jail_pop_rate)) %>%
  summarise(urbanicity_highest_jail = urbanicity)

# In each kind of urbanicity above, which race has appeared the most frequent as 
# the highest? 



# Gender MAY BE DELETED: turn to calculate median and mean
# Calculate the difference between male_jail_pop_rate and female_jail_pop_rate. 
# Calculate the difference between male_prison_pop_rate and female_prison_pop_rate
# Make two new columns.
data <- data %>%
  mutate(dif_jail_gender = male_jail_pop_rate - female_jail_pop_rate, 
         dif_prison_gender = male_prison_pop_rate - female_prison_pop_rate)
# How many rows have positive number in dif_jail_gender?
data %>%
  filter(dif_jail_gender > 0) %>%
  summarise(count = n())
# How many rows have positive number in dif_prison_gender?
data %>%
  filter(dif_prison_gender > 0) %>%
  summarise(count = n())