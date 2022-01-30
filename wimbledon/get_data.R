
# loads Wimbledon data from https://github.com/JeffSackmann/tennis_slam_pointbypoint/

#--------------------------------------

library(dplyr)
library(feather)
library(readr)
library(purrr)
library(glue)
library(janitor)
library(here)

#--------------------------------------

years <-
  c(seq(2015, 2019, 1), 2021)

get_path <- 
  function(year, type){
    glue('https://raw.githubusercontent.com/JeffSackmann/tennis_slam_pointbypoint/master/{year}-wimbledon-{type}.csv')
  }

get_points <- 
  function(path){
    read_csv({{path}}) %>%
      as_tibble() %>%
      mutate(PointNumber = as.numeric(PointNumber)) %>%
      clean_names()
  }

get_matches <- 
  function(path){
    readr::read_csv({{path}}) %>%
      as_tibble() %>%
      janitor::clean_names()
  }

#--------------------------------------

points <-
  map(years, ~get_path(.x, type = 'points')) %>%
  map_dfr(., ~get_points(.x)) 

matches <-
  map(years, ~get_path(.x, type = 'matches')) %>%
  map_dfr(., ~get_matches(.x)) 

#--------------------------------------

write_feather(points, here("wimbledon", "data", "raw", "points.feather"))
write_feather(matches, here("wimbledon", "data", "raw" "matches.feather"))
