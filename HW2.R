library(haven)
library(tidyverse)
library(ggplot2)
weight1 <- read_sas("weight1.sas7bdat") |>
  na.omit()

id_total <- weight1 |>
  distinct(id) |>
  nrow()

slope <- numeric(id_total)

for (i in 1:id_total) {
  data <- weight1 |>
    filter(id == i)
  model <- lm(weight ~ week, data = data)
  slope[i] <- coef(model)[2]
}

slope_data <- tibble(id = 1:id_total, slope = slope)

ggplot(slope_data, aes(x = slope)) +
  geom_histogram(binwidth = 0.1, fill = "salmon", color = "black") +
  labs(title = "Distribution of Slopes", x = "Slope", y = "Frequency")

average_slope <- mean(slope_data$slope)
std_dev_slope <- sd(slope_data$slope)

t_test_result <- t.test(slope_data$slope, mu = 0)
print(t_test_result)

id_count <- weight1 |>
  group_by(id) |>
  summarise(measurements = n())

ids <- c(37, 31, 15, 13, 3)
slope <- numeric(length(ids))
std_error <- numeric(length(ids))
t_values <- numeric(length(ids))
p_values <- numeric(length(ids))
for (i in seq_along(ids)) {
  data <- weight1 |>
    filter(id == ids[i])
  model <- lm(weight ~ week, data = data)
  slope[i] <- coef(model)[2]
  std_error[i] <- summary(model)$coefficients[2, 2]
  t_values[i] <- slope[i] / std_error[i]
  n <- nrow(filter(weight1, id == ids[i]))
  p_values[i] <- 2 * pt(-abs(t_values[i]), df = n - 2)
}

t.test(slope, mu = 0)



ozone1 <- read_sas("newozone1.sas7bdat")






            