# PURPOSE: Create some basic EDA figures related to shot outcomes

library(tidyverse)

# Load the data -----------------------------------------------------------

model_nhl_shot_data <-
  read_csv("expected_goals/data/model_nhl_shot_data.csv")

# Plot all shots ----------------------------------------------------------

# Raw locations
model_nhl_shot_data %>%
  ggplot(aes(x = x_fixed, y = y_fixed, color = as.factor(is_goal))) +
  geom_point(alpha = .25) +
  theme_bw()

# Distance histogram by outcome
model_nhl_shot_data %>%
  ggplot(aes(x = shot_distance, fill = as.factor(is_goal))) +
  geom_histogram() + 
  labs(x = "Shot distance (in feet)",
       y = "Count", fill = "Shot outcome") +
  ggthemes::scale_fill_colorblind(labels = c("Save", "Goal")) +
  theme_bw() +
  theme(legend.position = "bottom")

# Angle histogram by outcome
model_nhl_shot_data %>%
  ggplot(aes(x = shot_angle, fill = as.factor(is_goal))) +
  geom_histogram() + 
  labs(x = "Shot angle (in degrees)",
       y = "Count", fill = "Shot outcome") +
  ggthemes::scale_fill_colorblind(labels = c("Save", "Goal")) +
  theme_bw() +
  theme(legend.position = "bottom")


