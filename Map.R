library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(scales)

rate_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990.csv")
pop_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-jail-pop.csv")

highest_jail_race <- function(x){
  races <- c("aapi", "black", "latinx", "native", "white")
  rates <- x
  if (all(is.na(rates))) {
    return(NA_character_)
  } else {
    return(races[which.max(rates)])
  }
}
rate_data <- rate_data %>%
  mutate(race_highest_jail = pmap_chr(
    list(aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate, 
         native_jail_pop_rate, white_jail_pop_rate), 
    ~ highest_jail_race(c(...))))

most_frequent_race <- rate_data %>%
  filter(year == 2018) %>%
  count(state, race_highest_jail) %>%
  arrange(state, desc(n)) %>%
  group_by(state) %>%
  slice(1) %>%
  ungroup() %>%
  rename(race_highest_frequent = race_highest_jail, count = n) %>%
  select(state, race_highest_frequent)
jail_pop_state <- pop_data %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
state_jail_data <- left_join(most_frequent_race, jail_pop_state, by = "state")

# Create the map:
state_shape <- map_data("state")
state_abbr <- data.frame(
  state = tolower(state.abb),
  region = tolower(state.name)
)
state_jail_data <- state_jail_data %>%
  mutate(state = tolower(state)) %>%
  left_join(state_abbr, by = c("state" = "state"))
state_shape <- state_shape %>%
  left_join(state_jail_data, by = "region")

state_shape$total_jail_pop_norm <- rescale(state_shape$total_jail_pop, 
                                           to = c(0.5, 1), na.rm = TRUE)
race_names <- c("aapi" = "Asian American / Pacific Islander",
                "black" = "Black",
                "latinx" = "Latinx",
                "native" = "Native American",
                "white" = "White")
ggplot(state_shape, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = race_highest_frequent, alpha = total_jail_pop_norm), color = "black") +
  scale_alpha(range = c(0.1, 1), na.value = 0.5) +
  coord_fixed(1.3) +
  labs(title = "2018 US Jail Population", 
       subtitle = "An Analysis of Intensity and Race", 
       fill = "Most Frequent Race/Ethnicity with the Highest Jail Population Rate", 
       alpha = "Total Jail Population Intensity") +
  scale_fill_discrete(labels = race_names) +
  theme_minimal() +
  theme_void() +
  theme(legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.box = "vertical") +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = 0.5),
    alpha = guide_legend(order = 0, position = "right"))