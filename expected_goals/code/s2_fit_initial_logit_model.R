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
# -0.7296  -0.5063  -0.3497  -0.2319   4.2716  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -1.0863098  0.0249260  -43.58   <2e-16 ***
#   shot_distance -0.0460831  0.0009384  -49.11   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 41818  on 71088  degrees of freedom
# Residual deviance: 38587  on 71087  degrees of freedom
# AIC: 38591
# 
# Number of Fisher Scoring iterations: 6

# View the predicted probability / expected goals relationship
model_nhl_shot_data <- model_nhl_shot_data %>%
  mutate(xg = init_logit$fitted.values)

shot_dist_prob_plot <- model_nhl_shot_data %>%
  ggplot(aes(x = shot_distance)) +
  geom_jitter(aes(y = is_goal), 
              width = 0, height = .01,
             size = .5, alpha = 0.15,
             color = "darkorange") +
  geom_line(aes(y = xg), 
            color = "blue") +
  labs(x = "Shot distance (in feet)",
       y = "Predicted probability of goal (aka expected goals)") +
  theme_bw()

cowplot::save_plot("expected_goals/figures/init_dist_prob.png",
                   shot_dist_prob_plot)


