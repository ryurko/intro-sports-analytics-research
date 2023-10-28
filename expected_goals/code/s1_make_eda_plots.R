# PURPOSE: Create some basic EDA figures related to shot outcomes

library(tidyverse)

# Load the data -----------------------------------------------------------

model_nhl_shot_data <-
  read_csv("expected_goals/data/model_nhl_shot_data.csv")

# Plot all shots ----------------------------------------------------------

# Raw locations
shot_scatter_plot <- model_nhl_shot_data %>%
  ggplot(aes(x = x_fixed, y = y_fixed, color = as.factor(is_goal))) +
  geom_point(alpha = .25) +
  labs(x = "x coordinate", y = "y coordinate", 
       color = "Shot outcome") +
  ggthemes::scale_color_colorblind(labels = c("Save", "Goal")) +
  theme_bw() +
  theme(legend.position = "bottom")


cowplot::save_plot("expected_goals/figures/shot_scatter.png",
                   shot_scatter_plot)

# Distance histogram by outcome
shot_dist_plot <- model_nhl_shot_data %>%
  ggplot(aes(x = shot_distance, fill = as.factor(is_goal))) +
  geom_histogram() + 
  labs(x = "Shot distance (in feet)",
       y = "Count", fill = "Shot outcome") +
  ggthemes::scale_fill_colorblind(labels = c("Save", "Goal")) +
  theme_bw() +
  theme(legend.position = "bottom")

cowplot::save_plot("expected_goals/figures/distance_hist.png",
                   shot_dist_plot)

# Angle histogram by outcome
model_nhl_shot_data %>%
  ggplot(aes(x = shot_angle, fill = as.factor(is_goal))) +
  geom_histogram() + 
  labs(x = "Shot angle (in degrees)",
       y = "Count", fill = "Shot outcome") +
  ggthemes::scale_fill_colorblind(labels = c("Save", "Goal")) +
  theme_bw() +
  theme(legend.position = "bottom")


