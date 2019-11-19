library(ggplot2)
library(maps)
library(ggthemes)
library(janitor)
library(openintro)
library(tidyverse)


# READING DATA INTO OBJECTS

final_data <- read_csv("raw-data/final_data.csv")

us_states <- map_data("state") %>% 
  mutate(region = state2abbr(region)) %>% 
  mutate(state_name = abbr2state(region)) %>% 
  select(-region, -subregion)


final_data_map <- full_join(final_data, us_states)

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
  ggplot(aes(x = state_name, y = total_deaths)) + geom_col()

death_graphic


