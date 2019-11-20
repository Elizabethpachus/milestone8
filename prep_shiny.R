library(tidyverse)
library(sf)
library(fs)
library(ggplot2)
library(maps)
library(ggthemes)
library(janitor)
library(openintro)
library(scales)
library(infer)
library(broom)
library(ggthemes)
library(tidyverse)


# Reading the data into objects

final_data <- read_csv("raw-data/final_data.csv")
final_data_map <- read_csv("raw-data/final_data_map.csv")

### Map Graphic Prep

# Grouping data frame
final_data_map <- final_data_map %>% 
  group_by(state_name, year)

# Actually creating the graphic

firearm_graphic <- ggplot(data = final_data_2017,
                          mapping = aes(x = long, y = lat, group = group, fill = rate_per_1000)) + 
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

# Creating the graphic and adding titles and legends and changing the colors for a better looking graph

firearm_graphic <- firearm_graphic + labs(title = "Firearm Mortality by State") +
  theme_map() + labs(fill = "Death Rate per 100,000") +
  scale_fill_gradient(low = "#ffcccb", high = "#CB454A") +
  labs(title = "2017 Firearm Mortality by State") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))



### Statistical Analysis

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



# Scatter Plot

final_data_suicide <- final_data %>% 
  group_by(state_name, year)

suicide_graphi <- final_data %>% 
  group_by(state_name, year) %>% 
  ggplot(aes(x = suicide_rate, y = deaths_year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Deaths per Year by Guns by Suicide Rate of State",
    x = "Suicide Rate per 1000",
    y = "Deaths per Year"
  )


# Writing objects out into rds files

write_rds(final_data_suicide, "milestone8_shiny/final_data_s.rds")

write_rds(final_data_map, "milestone8_shiny/final_data_m.rds")
