# PURPOSE: Fit initial logistic regression model and generate calibration plot
#          only on training data

library(tidyverse)

# Load the data -----------------------------------------------------------

model_nhl_shot_data <-
  read_csv("expected_goals/data/model_nhl_shot_data.csv")

# Fit model based on shot distance ----------------------------------------

init_logit <- glm(is_goal ~ shot_distance, data = model_nhl_shot_data,
                  family = "binomial")

# View summary output:
summary(init_logit)

# Call:
#   glm(formula = is_goal ~ shot_distance, family = "binomial", data = model_nhl_shot_data)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.7563  -0.4989  -0.3353  -0.2191   4.2235  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -1.057338   0.031326  -33.75   <2e-16 ***
#   shot_distance -0.047936   0.001181  -40.60   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 26329  on 45690  degrees of freedom
# Residual deviance: 24111  on 45689  degrees of freedom
# AIC: 24115
# 
# Number of Fisher Scoring iterations: 6

# View the predicted probability / expected goals relationship
model_nhl_shot_data <- model_nhl_shot_data %>%
  mutate(xg = init_logit$fitted.values)

shot_dist_prob_plot <- model_nhl_shot_data %>%
  ggplot(aes(x = shot_distance)) +
  geom_line(aes(y = xg), 
            color = "blue") +
  geom_point(aes(y = is_goal), 
             size = .5, alpha = 0.15,
             color = "darkorange") +
  labs(x = "Shot distance (in feet)",
       y = "Predicted probability of goal (aka expected goals)") +
  theme_bw()

cowplot::save_plot("expected_goals/figures/init_dist_prob.png",
                   shot_dist_prob_plot)


# Generate calibration plot -----------------------------------------------

model_nhl_shot_data %>%
  mutate(bin_pred_prob = round(xg / 0.05) * .05) %>%
  # Group by bin_pred_prob:
  group_by(bin_pred_prob) %>%
  # Calculate the calibration results:
  summarize(n_shots = n(),
            bin_actual_prob = mean(is_goal),
            bin_se = sqrt((bin_actual_prob * (1 - bin_actual_prob)) / n_shots),
            .groups = "drop") %>%
  mutate(bin_upper = pmin(bin_actual_prob + 2 * bin_se, 1),
         bin_lower = pmax(bin_actual_prob - 2 * bin_se, 0)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_point() +
  #geom_point(aes(size = n_shots)) +
  geom_errorbar(aes(ymin = bin_lower, ymax = bin_upper)) + 
  geom_smooth(method = "loess", se = FALSE) +
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

