# PURPOSE: Bootstrap player effects in lmer model by resampling periods for
#          each team across the season, refitting the model, and extracting
#          player coefficients

library(tidyverse)

# Load the data -----------------------------------------------------------

model_nhl_shot_data <-
  read_csv("expected_goals/data/model_nhl_shot_data.csv")


# Create a table with game ids to resample periods ------------------------

game_table <- model_nhl_shot_data %>%
  dplyr::select(game_id) %>%
  distinct()

# Now repeat 3 times:
game_periods <- map_dfr(1:3,
                        function(period_i) {
                          game_table %>%
                            mutate(period = period_i)
                        })

# Create nested version of shot data:
game_period_shots <- game_periods %>%
  left_join(dplyr::select(model_nhl_shot_data,
                          game_id, period, shooting_player, goalie_name,
                          shot_distance, is_goal),
            by = c("game_id", "period")) %>%
  group_by(game_id, period) %>%
  nest() %>%
  ungroup()


# Bootstrap game periods to generate distribution of player effects -------

N_BOOTS <- 100

bootstrap_player_effects <-
  map_dfr(1:N_BOOTS,
          function(boot_i) {
            
            # First resample periods within games:
            boot_game_periods <- game_period_shots %>%
              group_by(game_id) %>%
              sample_n(3, replace = TRUE) %>%
              # ungroup and unnest
              ungroup() %>%
              unnest(cols = data)
            
            # Now fit the model
            xg_lmer <- glmer(is_goal ~ shot_distance + (1|shooting_player) + (1|goalie_name), 
                             data = boot_game_periods, family = "binomial")
            
            # Extract the varying intercepts for each player
            xg_player_results <- ranef(xg_lmer)
            
            # Create the shooting table:
            shooting_re_results <- xg_player_results$shooting_player %>%
              as_tibble() %>%
              mutate(player = rownames(xg_player_results$shooting_player),
                     type = "shooter") %>%
              rename(intercept = `(Intercept)`)
            
            # Create the goalie table:
            goalie_re_results <- xg_player_results$goalie_name %>%
              as_tibble() %>%
              mutate(player = rownames(xg_player_results$goalie_name),
                     type = "goalie") %>%
              rename(intercept = `(Intercept)`)
            
            # Join together and return:
            shooting_re_results %>%
              bind_rows(goalie_re_results) %>%
              mutate(boot = boot_i) %>%
              return()
            
          })

  
  
