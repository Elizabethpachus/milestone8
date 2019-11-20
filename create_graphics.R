library(ggplot2)
library(maps)
library(ggthemes)
library(janitor)
library(openintro)
library(scales)
library(infer)
library(broom)
library(tidyverse)


# READING DATA INTO OBJECTS

final_data <- read_csv("raw-data/final_data.csv")
final_data_map <- read_csv("raw-data/final_data_map.csv")

# us_states <- map_data("state") %>% 
#   mutate(region = state2abbr(region)) %>% 
#   mutate(state_name = abbr2state(region)) %>% 
#   select(-region, -subregion)
# 
# 
# final_data_map <- full_join(final_data, us_states)
# 
# write_csv(final_data_map, "raw-data/final_data_map.csv")
# 
# suicide_data <- read_csv("raw-data/suicide_by_state.csv") %>% 
#   clean_names() %>% 
#   mutate(suicide_rate = rate) %>% 
#   mutate(suicide_deaths = deaths) %>% 
#   mutate(state_name = abbr2state(state)) %>% 
#   select(-state, -url, -deaths, -rate)
# 
# final_data <- full_join(final_data, suicide_data)
# 
# write_csv(final_data, "raw-data/final_data.csv")

final_data_map <- final_data_map %>% 
  group_by(state_name, year)


# Creating Map of Firearm Death Rates per State

final_data_2017 <- final_data_map %>% 
  filter(year == 2017)

firearm_graphic <- ggplot(data = final_data_2017,
                          mapping = aes(x = long, y = lat, group = group, fill = rate_per_1000)) + 
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

# Creating the graphic and adding titles and legends and changing the colors for a better looking graph

firearm_graphic <- firearm_graphic + labs(title = "Firearm Mortality by State") + theme_map() + labs(fill = "Death Rate per 100,000") + scale_fill_gradient(low = "#ffcccb", high = "#CB454A") +
  labs(title = "2017 Firearm Mortality by State") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

firearm_graphic

# Making another graphic

death_graphic <- final_data %>%
  filter(year == 2017) %>% 
  filter(cause == "All Intents Firearm") %>% 
  ggplot(aes(x = reorder(state_name, -deaths_year), y = deaths_year)) + 
  geom_col(width = 0.5) +
  theme(
    axis.text.x = element_text(angle = 50, hjust = .6, vjust = .6)
  ) +
  labs(
    title = "Number of Deaths By Guns By State",
    x = "State",
    y = "Number of Deaths"
  )

death_graphic


# Making a statistical model of the data

stat_model <- final_data %>% 
  filter(cause == "All Intents Firearm") %>%
  group_by(state_name) %>% 
  nest() %>%
  mutate(mod = map(data, ~ lm(deaths_year ~ suicide_rate, data = .x))) %>%
  mutate(coefficients = map(mod, ~ tidy(.x))) %>%
  mutate(sum_stats = map(mod, ~ glance(.x))) %>%
  unnest(coefficients) %>%
  select(-std.error, -statistic, -p.value) %>%
  unnest(sum_stats) 
  
stat_model

# Scatter Plot

final_data_suicide <- final_data %>% 
  group_by(state_name, year)

final_data %>% 
  group_by(state_name, year) %>% 
  ggplot(aes(x = suicide_rate, y = deaths_year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Deaths per Year by Guns by Suicide Rate of State",
    x = "Suicide Rate per 1000",
    y = "Deaths per Year"
  )
  
  



