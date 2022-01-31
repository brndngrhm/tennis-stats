
# formats Wimbledon points and matches data sets

#--------------------------------------

library(dplyr)
library(feather)
library(readr)
library(purrr)
library(glue)
library(janitor)
library(here)
library(tidyr)
library(stringr)

#--------------------------------------

points <-
  read_feather(here("wimbledon", "data", "raw", "points.feather"))

matches <-
  read_feather(here("wimbledon", "data", "raw", "matches.feather"))

#--------------------------------------

format_matches <- 
  function(){
    matches %>%
      select(match_id:player2) %>%
      mutate(category = ifelse(str_sub(match_num, 1, 1) == 1, "men", "women"),
             round = str_sub(match_num, 2, 2),
             match_no = as.numeric(str_sub(match_num, 3, 4)))
  }

get_match_winner <- 
  function(){
    
    points %>%
      filter(set_winner > 0) %>%
      select(match_id, p1games_won, p2games_won) %>%
      group_by(match_id) %>%
      summarise(tot_p1games_won = sum(p1games_won),
                tot_p2games_won = sum(p2games_won)) %>%
      mutate(match_winner = ifelse(tot_p1games_won > tot_p2games_won, "player1", "player2")) %>%
      select(match_id, match_winner)
  }

matches <- 
  format_matches() %>%
  left_join(., get_match_winner()) %>%
  mutate(match_winner = ifelse(match_winner == "player1", player1, player2))

write_feather(matches, here("wimbledon", "data", "formatted", "matches.feather"))

#--------------------------------------

format_points <- 
  function(){

    time <- 
      points %>%
      filter(point_number > 0) %>%
      select(match_id, set_no, elapsed_time) %>%
      group_by(match_id, set_no) %>%
      filter(elapsed_time == max(elapsed_time)) %>%
      ungroup() %>%
      group_by(match_id) %>%
      mutate(
        set_length_mins = case_when(
          set_no == 1 ~ as.numeric(elapsed_time)/60,
          TRUE ~ (as.numeric(elapsed_time) - lag(as.numeric(elapsed_time)))/60),
        game_length_mins = sum(set_length_mins)
      )
    
    points_formatted <- 
      points %>%
      filter(point_number > 0) %>%
      group_by(match_id) %>%
      mutate(match_seq = dplyr::row_number()) %>%
      ungroup() %>%
      group_by(match_id, set_no) %>%
      mutate(set_seq = dplyr::row_number()) %>%
      ungroup() %>%
      group_by(match_id, set_no, game_no) %>%
      mutate(game_seq = dplyr::row_number()) %>%
      ungroup() %>% 
      inner_join(., time %>% select(-elapsed_time), by = c("match_id", "set_no")) %>%
      select(match_id, set_no, match_seq:game_length_mins, everything()) %>%
      mutate(p1score = as.numeric(p1score),
             p2score = as.numeric(p2score))
    
    points_formatted
    
  }

points <-
  format_points()

write_feather(points, here("wimbledon", "data", "formatted", "points.feather"))

