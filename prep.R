library(sf)
library(fs)
library(shiny)
library(gganimate)
library(gifski)
library(lubridate)
library(transformr)
library(dplyr)
library(tidyverse)

# Creating two new directorties

dir.create("raw-data")
dir.create("clean-data")
dir.create('graphics')

# Downloading necessary files

download.file("https://stacks.stanford.edu/file/druid:hp256wp2687/hp256wp2687_ok_oklahoma_city_2019_08_13.rds",
              destfile = "raw-data/oklahoma_city.rds",
              mode = "wb")

download.file("https://stacks.stanford.edu/file/druid:hp256wp2687/hp256wp2687_ok_oklahoma_city_shapefiles_2019_08_13.tgz",
              destfile = "raw-data/oklahoma_city.tgz",
              mode = "wb")

# # Untarring

untar('raw-data/oklahoma_city.tgz')

# Saving data into R objects

oklahoma_data <- read_rds("raw-data/oklahoma_city.rds")

oklahoma_shape <- st_read("raw-data/ok_oklahoma_city_shapefiles/City_Boundaries.shp", quiet = TRUE)

# Writing out into an R object

write_rds(oklahoma_shape, "clean-data/clean_oklahoma_shape.shp")

# Cleaning the data

oklahoma_clean <- oklahoma_data %>% 
  drop_na() %>% 
  filter(division %in% c("Santa Fe","Springlake"))

# Writing out into clean-data 

locations <- st_as_sf(oklahoma_clean, coords = c("lng","lat"), crs = 4326)

write_rds(locations, "clean-data/locations.rds")
  
# Filtering a subset of the locations data to use in my animation
# I chose to add in a column of month so I can annimate over that later
# also chose to group by race to plot that by color
# selected subset of the data so that the animation wouldn't take a super long time to load

plot_locations <- locations %>% 
  arrange(date) %>% 
  mutate(month = as.integer(format(date, "%m"))) %>%
  mutate(month_name = month.name[month]) %>% 
  group_by(subject_race) %>% 
  slice(1:100)

plot_locations

# Creating a plot which has the points colored by race of subject, and the animation is
# based on the month of the police stop

graph <- ggplot() +
  geom_sf(data = oklahoma_shape) +
  geom_sf(data = plot_locations, aes(color = subject_race)) +
  labs(title = "Police Stops by Race 2011-2012",
       fill = "Race of Subject") +
  scale_fill_discrete(name = "Subject Race") +
  transition_states(month_name,
                    transition_length = 1,
                    state_length = 1) +
  ggtitle("Police Stops in Hartford, CT from 2011-2012",
          subtitle = 'Month: {closest_state}')

graph

# Saving animation as a gif

anim_save("graphics/myanim.gif", graph)

# Deleting raw files

file_delete("raw-data")



