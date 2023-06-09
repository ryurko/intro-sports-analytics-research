# PURPOSE: Prepare example NHL dataset to use for expected goals modeling

library(tidyverse)

# Read in the NHL shot data from SCORE ------------------------------------

nhl_shot_data <- 
  read_csv("https://data.scorenetwork.org/data/nhl-shots-2021.csv.gz")

# Perform some preprocessing ----------------------------------------------

model_nhl_shot_data <- nhl_shot_data %>%
  # Remove shots where distance is missing
  filter(!is.na(shot_distance), 
         # Only consider even-strength
         strength_code == "EV",
         # Remove empty-net
         (is.na(empty_net) | empty_net == FALSE)) %>%
  # Create numeric indicator for goals:
  mutate(is_goal = as.numeric(shot_outcome == "GOAL"),
         # Get the goalie team:
         goalie_team = ifelse(event_team == home_name,
                              away_name, home_name)) %>%
  # Just grab a subset of columns:
  dplyr::select(event_player_1_name, event_team, event_goalie_name,
                goalie_team, x_fixed, y_fixed, shot_distance,
                shot_angle, is_goal) %>%
  rename(shooting_player = event_player_1_name,
         shooting_team = event_team, 
         goalie_name = event_goalie_name)


# Save data ---------------------------------------------------------------

write_csv(model_nhl_shot_data,
          "expected_goals/data/model_nhl_shot_data.csv")


