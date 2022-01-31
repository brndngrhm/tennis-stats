
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

# no wimbledon in 2020
years <-
  c(seq(2015, 2019, 1), 2021)

get_url <- 
  function(year, type){
    
    if(!type %in% c('points', 'matches')) stop("Type must be one of 'points' or 'maches'")
    
    glue('https://raw.githubusercontent.com/JeffSackmann/tennis_slam_pointbypoint/master/{year}-wimbledon-{type}.csv')
    
  }

get_data <- 
  function(url){
    
    #points datasets needs an extra step in order to use map_dfr
    data <- 
      if (str_detect(url, "points")){
        read_csv({{url}}) %>%
          as_tibble() %>%
          # this step needed for map_dfr(); in some datasets point number is numeric, others its a character
          mutate(PointNumber = as.numeric(PointNumber)) %>%
          clean_names()
      } else {
        
        readr::read_csv({{url}}) %>%
          as_tibble() %>%
          janitor::clean_names()
      }
    
    data
    
  }

#--------------------------------------

points <-
  map(years, ~get_url(.x, type = 'points')) %>%
  map_dfr(., ~get_data(.x)) 

matches <-
  map(years, ~get_url(.x, type = 'matches')) %>%
  map_dfr(., ~get_data(.x)) 

#--------------------------------------

write_feather(points, here("wimbledon", "data", "raw", "points.feather"))
write_feather(matches, here("wimbledon", "data", "raw" "matches.feather"))
