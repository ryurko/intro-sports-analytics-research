# PURPOSE: Evaluate logistic regression model out-of-sample

library(tidyverse)

# Load the data -----------------------------------------------------------

model_nhl_shot_data <-
  read_csv("expected_goals/data/model_nhl_shot_data.csv")


# Create train and test folds based on games ------------------------------

set.seed(1991)
game_fold_table <- tibble(game_id = unique(model_nhl_shot_data$game_id)) %>%
  mutate(game_fold = sample(rep(1:5, length.out = n()), n()))

table(game_fold_table$game_fold)
#   1   2   3   4   5 
# 281 280 280 280 280 

model_nhl_shot_data <- model_nhl_shot_data %>% 
  left_join(game_fold_table, by = "game_id")

table(model_nhl_shot_data$game_fold)
#     1     2     3     4     5 
# 14418 14052 14272 14098 14249 

# Generate out-of-sample predictions --------------------------------------

logit_cv_preds <- 
  map_dfr(unique(model_nhl_shot_data$game_fold), 
          function(test_fold) {
            
            # Separate test and training data:
            test_data <- model_nhl_shot_data %>%
              filter(game_fold == test_fold)
            train_data <- model_nhl_shot_data %>%
              filter(game_fold != test_fold)
            
            # Train model:
            logit_model <- glm(is_goal ~ shot_distance, 
                               data = train_data,family = "binomial")
            
            # Return tibble of holdout results:
            tibble(test_pred_probs = predict(logit_model, newdata = test_data,
                                             type = "response"),
                   test_actual = test_data$is_goal,
                   game_fold = test_fold) 
          })


# View out-of-sample calibration ------------------------------------------

calibration_plot <- logit_cv_preds %>%
  mutate(bin_pred_prob = round(test_pred_probs / 0.05) * .05) %>%
  # Group by bin_pred_prob:
  group_by(bin_pred_prob) %>%
  # Calculate the calibration results:
  summarize(n_shots = n(),
            bin_actual_prob = mean(test_actual),
            bin_se = sqrt((bin_actual_prob * (1 - bin_actual_prob)) / n_shots),
            .groups = "drop") %>%
  mutate(bin_upper = pmin(bin_actual_prob + 2 * bin_se, 1),
         bin_lower = pmax(bin_actual_prob - 2 * bin_se, 0)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_point() +
  geom_point(aes(size = n_shots)) +
  geom_errorbar(aes(ymin = bin_lower, ymax = bin_upper)) + 
  #geom_smooth(method = "loess", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, 
              color = "black", linetype = "dashed") +
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(size = "Number of shot attempts",
       x = "Estimated goal probability",
       y = "Observed goal frequency") + 
  theme_bw() +
  theme(legend.position = "bottom")

cowplot::save_plot("expected_goals/figures/calibration_plot.png",
                   calibration_plot, base_height = 6)