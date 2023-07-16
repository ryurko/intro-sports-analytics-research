# PURPOSE: Initialize passing play dataset for most recent NFL season

library(tidyverse)

# Use nflreadr to load pbp data for 2022 season ---------------------------

nfl_2022_data <- nflreadr::load_pbp(2022, file_type = "rds")

# Construct passing play dataset for modeling -----------------------------

nfl_passing_plays <- nfl_2022_data %>%
  # Regular season only
  filter(play_type == "pass", season_type == "REG", 
         !is.na(epa), !is.na(posteam), posteam != "") %>%
  select(# Player info attempting the pass:
    passer_player_name, passer_player_id, posteam, 
    # Info about the pass:
    complete_pass, interception, yards_gained, touchdown, 
    pass_location, pass_length, air_yards, yards_after_catch, epa, wpa,
    shotgun, no_huddle, qb_dropback, qb_hit, sack,
    # Context about the receiver:
    receiver_player_name, receiver_player_id,
    # Team context:
    posteam, defteam, posteam_type,
    # Play and game context:
    play_id, yardline_100, side_of_field, down, qtr, play_clock,
    penalty, half_seconds_remaining, game_half, game_id,
    home_team, away_team, home_score, away_score,
    # Description of play
    desc)

# Save this file:
write_csv(nfl_passing_plays, 
          "slides/multilevel-model-example/nfl_passing_plays_2022.csv")

