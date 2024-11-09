library(haven)
library(tidyverse)
library(ggplot2)
library(GGally)

data1 <- read_sas("./H22G_HP.sas7bdat")
dental <- read_sas("dental.sas7bdat")
ggplot(dental, aes(x = age, y = distance, group = id, color = gender)) +
  geom_line() +
  labs(title = "Profile Plot of Distance vs Age", x = "Age", y = "Distance") +
  theme_minimal()

distance_by_age <- dental |>
  pivot_wider(names_from = age, values_from = distance, names_prefix = "distance_age_")

distance_column <- distance_by_age |>
  select(starts_with("distance_age_"))
cor_matrix <- cor(distance_column, use = "complete.obs")


ggpairs(distance_column, 
        aes(color = distance_by_age$gender, shape = distance_by_age$gender),
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = "points")) +
  theme_minimal() +
  labs(title = "Scatterplot Matrix of Distances by Age with Gender Differentiation")


ggplot(dental, aes(x = age, y = distance)) +
  stat_summary(fun = "mean", geom = "line", color = "blue") +
  labs(title = "Empirical Summary Plot (All Individuals)", 
       x = "Age", y = "Average Distance") +
  theme_minimal()

ggplot(dental, aes(x = age, y = distance, group = gender, color = gender)) +
  stat_summary(fun = "mean", geom = "line") +
  labs(title = "Empirical Summary Plot", x = "Age", y = "Average Distance") +
  theme_minimal()

dental_residuals <- numeric(nrow(dental))
for (ids in unique(dental$id)) {
  individual_data <- dental[dental$id == ids, ]
  mean_distance <- mean(individual_data$distance)
  dental_residuals[dental$id == ids] <- individual_data$distance - mean_distance
}
dental$residual <- dental_residuals

ggplot(dental, aes(x = age, y = residual, group = id, color = gender)) +
  geom_line() +
  labs(title = "Profile Plot of Within-Subject Residuals", x = "Age", y = "Residual Distance") +
  theme_minimal()
