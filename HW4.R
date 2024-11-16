library(haven)
library(tidyverse)
library(emmeans)
dental <- read_sas("dental.sas7bdat")

library(lme4)
model_null <- lmer(distance ~ age + (1 | id), data = dental)
model_best <- lmer(distance ~ gender * age + (1 | id), data = dental)
RI_f <- lmer(distance ~ gender*age + gender * I(age^2) + (age | id), data = dental)
anova(RI_f, model_best, test = "F")

RI_d <- lmer(distance ~ gender*age + (1 | id), data = dental)
new_data <- dental |>
  group_by(gender, age) |>
  summarize(predicted_distance = predict(RI_d, newdata = data.frame(gender = gender, age = age), 
                                         re.form = NA), .groups = 'drop')

new_data <- new_data |>
  mutate(se = sqrt(diag(vcov(RI_d))[2] * (gender == "Boy") + diag(vcov(RI_d))[3]
                   * (gender == "Girl")))

ggplot(new_data, aes(x = age, y = predicted_distance, color = gender, 
                     group = gender)) +
  geom_line(stat = "identity") +  # Plot the mean estimates as lines
  geom_errorbar(aes(ymin = predicted_distance - se, ymax = predicted_distance + se), 
                width = 0.2) +  # Add error bars for SE
  labs(title = "Inference Plot", x = "Age", y = "Estimated Average Distance") +
  theme_minimal()

