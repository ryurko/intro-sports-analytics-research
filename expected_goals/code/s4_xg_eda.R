# PURPOSE: Generate some basic expected goals analysis without fitting any models

library(tidyverse)

# Load the data -----------------------------------------------------------

model_nhl_shot_data <-
  read_csv("expected_goals/data/model_nhl_shot_data.csv")

# Fit model based on shot distance ----------------------------------------

init_logit <- glm(is_goal ~ shot_distance, data = model_nhl_shot_data,
                  family = "binomial")

# Add the expected goals column to the data -------------------------------

model_nhl_shot_data <- model_nhl_shot_data %>%
  mutate(xg = init_logit$fitted.values)

# Accumulate expected goals by shooting and goalie teams ------------------

team_shooting_xg <- model_nhl_shot_data %>%
  group_by(shooting_team) %>%
  summarize(total_xg_for = sum(xg, na.rm = TRUE),
            ave_xg_for = mean(xg, na.rm = TRUE))

team_goalie_xg <- model_nhl_shot_data %>%
  group_by(goalie_team) %>%
  summarize(total_xg_against = sum(xg, na.rm = TRUE),
            ave_xg_against = mean(xg, na.rm = TRUE))

# Join the tables together:
team_xg_summary <- team_shooting_xg %>%
  inner_join(team_goalie_xg, by = c("shooting_team" = "goalie_team"))


# Make a plot of team performance for vs against --------------------------

# Use ggrepel to make team names easier to read:
library(ggrepel)

# Make total plot
total_xg_plot <- team_xg_summary %>%
  ggplot(aes(x = total_xg_for, y = total_xg_against)) +
  geom_point() +
  geom_label_repel(aes(label = shooting_team), min.segment.length = 0.1) +
  #coord_fixed() +
  theme_bw() +
  labs(x = "Total expected goals on shots taken",
       y = "Total expected goals on shots allowed")

cowplot::save_plot("expected_goals/figures/team_total_xg_plot.png",
                   total_xg_plot, base_height = 6)

# Make average plot
ave_xg_plot <- team_xg_summary %>%
  ggplot(aes(x = ave_xg_for, y = ave_xg_against)) +
  geom_point() +
  geom_label_repel(aes(label = shooting_team), min.segment.length = 0.1) +  
  #coord_fixed() +
  theme_bw()  +
  labs(x = "Average expected goals on shots taken",
       y = "Average expected goals on shots allowed")

cowplot::save_plot("expected_goals/figures/team_ave_xg_plot.png",
                   ave_xg_plot, base_height = 6)


# Who are the top players in terms of expected goals ----------------------

model_nhl_shot_data %>%
  group_by(shooting_player) %>%
  summarize(total_xg = sum(xg, na.rm = TRUE),
            ave_xg = mean(xg, na.rm = TRUE),
            total_g = sum(is_goal, na.rm = TRUE)) %>%
  slice_max(order_by = total_xg, n = 5)


# Perform basic residual analysis -----------------------------------------

# Which players have the most / least goals above expectation

model_nhl_shot_data %>%
  group_by(shooting_player) %>%
  summarize(total_xg = sum(xg, na.rm = TRUE),
            total_g = sum(is_goal, na.rm = TRUE)) %>%
  mutate(g_xg_diff = total_g - total_xg) %>%
  slice_max(order_by = g_xg_diff, n = 5)

# A tibble: 5 × 4
# shooting_player total_xg total_g g_xg_diff
# <chr>              <dbl>   <dbl>     <dbl>
# 1 Steven.Stamkos      16.3      36      19.7
# 2 Auston.Matthews     29.9      46      16.1
# 3 Filip.Forsberg      17.5      30      12.5
# 4 Ryan.Hartman        16.4      28      11.6
# 5 Kirill.Kaprizov     19.5      31      11.5

model_nhl_shot_data %>%
  group_by(shooting_player) %>%
  summarize(total_xg = sum(xg, na.rm = TRUE),
            total_g = sum(is_goal, na.rm = TRUE)) %>%
  mutate(g_xg_diff = total_g - total_xg) %>%
  slice_min(order_by = g_xg_diff, n = 5)

# A tibble: 5 × 4
# shooting_player   total_xg total_g g_xg_diff
# <chr>                <dbl>   <dbl>     <dbl>
#   1 Brendan.Gallagher     11.6       1    -10.6 
# 2 Pierre.Luc.Dubois     19.9      10     -9.90
# 3 Phil.Kessel           13.1       4     -9.10
# 4 Evan.Rodrigues        18.5      10     -8.54
# 5 Trevor.Moore          18.4      10     -8.42

# For goalies:
model_nhl_shot_data %>%
  group_by(goalie_name) %>%
  summarize(total_xg = sum(xg, na.rm = TRUE),
            total_g = sum(is_goal, na.rm = TRUE)) %>%
  mutate(g_xg_diff = total_g - total_xg) %>%
  slice_min(order_by = g_xg_diff, n = 5)

# A tibble: 5 × 4
#     goalie_name        total_xg total_g g_xg_diff
#     <chr>                 <dbl>   <dbl>     <dbl>
#   1 Igor.Shesterkin        173.     126     -46.8
#   2 Ilya.Sorokin           116.      92     -24.3
#   3 Andrei.Vasilevskiy     179.     155     -24.2
#   4 Thatcher.Demko         137.     115     -22.0
#   5 Juuse.Saros            136.     119     -17.5


