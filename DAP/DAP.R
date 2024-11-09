library(tidyverse)
library(ggplot2)
library(lme4)
GCC <- read.table("GCC data set.txt", header = TRUE) |>
  mutate(month = time*12) |>
  mutate(d_GCC = GCC - mean(GCC))
  

observation_counts <- GCC |>
  group_by(id) |>
  summarise(measurement_count = n())

# (b)
ggplot(data = GCC, aes(x = month, y = GCC, group = factor(id)))+
  geom_line() +
  geom_point() +
  geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
  theme_bw() +
  labs(x = "Month",
       y = "GCC(µm)",
       title = 'Profile Plot of GCC Data') +
  theme(plot.title = element_text(hjust =0.5))

# ggplot(data = GCC, aes(x = month, y = GCC, group = factor(id))) +
#   geom_line() + 
#   geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
#   theme_bw() +
#   labs(x = 'Month',
#        y = 'Residual GCC(µm)',
#        title = 'Profile Plot of within-subject residuals for the GCC data') +
#   theme(plot.title = element_text(hjust = 0.5))

summary_stats <- GCC |>
  group_by(id) |>
  summarise(
    mean_GCC = mean(GCC),
    median_GCC = median(GCC),
    sd_GCC = sd(GCC)
  )

ggplot(summary_stats, aes(x = "", y = mean_GCC)) +
  geom_violin(fill = "lightblue", color = "black") +
  geom_jitter(width = 0.1, color = "darkblue", alpha = 0.7) +
  labs(title = "Violin Plot of Mean GCC",
       x = "",
       y = "Mean GCC (µm)") +
  theme_minimal()


# (c)
model_RI <- lmer(GCC ~ month + (1 | id), data = GCC)
model_RS <- lmer(GCC ~ month + (0 + month | id), data = GCC)
model_RR <- lmer(GCC ~ month + (1 + month | id), data = GCC) # win
model_RIASQ <- lmer(GCC ~ month + (1 + month + I(month^2) | id), data = GCC)
anova(model_RI, model_RS, model_RR, model_RIASQ)
library(nlme)


model_cs <- lme(GCC ~ month, random = ~ 1 | id, data = GCC, 
                correlation = corCompSymm(form = ~ month | id))

model_ar1 <- lme(GCC ~ month, random = ~ 1 | id, data = GCC, 
                 correlation = corCAR1(form = ~ month | id)) # win

model_arma11 <- lme(GCC ~ month, random = ~ 1 | id, data = GCC, 
                          correlation = corARMA(form = ~ month | id, p = 1, q = 1))

model_un <- lme(GCC ~ month, random = ~ 1 | id, data = GCC)

anova(model_cs, model_ar1, model_arma11, model_un)

# (d)
model_RIAS <- lmer(GCC ~ month + (1 + month | id), data = GCC)
summary(model_RIAS)
random_effects <- ranef(model_RIAS)$id
random_effects_slope <- random_effects[, "month"]
ggplot(data.frame(Slope = random_effects_slope), aes(x = "", y = Slope)) +
  geom_violin(color = "salmon") +
  geom_jitter(width = 0.1, color = "salmon", alpha = 0.7) +
  labs(title = "Distribution of Individual Rates of Macular Thickness Loss",
       x = "Rate of GCC Thickness Loss (µm/year)",
       y = "Density") +
  theme_minimal()

# (e)
GCC <- GCC |>
  group_by(id) |>
  mutate(initial_GCC = first(GCC)) |>
  ungroup()

model_interaction <- lmer(GCC ~ month * initial_GCC + (1 + month | id), data = GCC)
GCC$predicted_GCC <- predict(model_interaction)

# ggplot(GCC, aes(x = month, y = predicted_GCC, group = factor(id))) +
#   geom_line(aes(group = id), alpha = 0.6) +
#   geom_smooth(aes(group = 1), method = "lm", color = "salmon", se = FALSE) +
#   labs(title = "Effect of Initial GCC Thickness on the Rate of GCC Change",
#        x = "Time (years)",
#        y = "Predicted GCC Thickness (µm)") +
#   theme_minimal()

library(broom)
slopes_per_id <- GCC |>
  group_by(id) |>
  do(tidy(lm(GCC ~ month, data = .))) |>
  filter(term == "month") |>
  select(id, estimate) |>
  rename(slope = estimate) |>
  left_join(GCC, by = "id") |>
  select(id, initial_GCC, slope) |>
  distinct()

ggplot(slopes_per_id, aes(x = initial_GCC, y = slope)) +
  geom_point(color = "salmon") +
  geom_smooth(method = "lm", color = "salmon", se = TRUE) +
  labs(title = "Relationship Between Initial GCC and Slope of GCC Change",
       x = "Initial GCC (µm)",
       y = "Slope of GCC Change (µm/year)") +
  theme_minimal()


# Q2

