library(tidyverse)
library(ggplot2)
library(lme4)
library(nlme)
GCC <- read.table("GCC data set.txt", header = TRUE) |>
  mutate(month = time*12) |>
  mutate(facmonth = round(month / 6) * 6) |>
  mutate(facmonth = factor(facmonth))
# round up the month

ggplot(GCC) +
  geom_histogram(aes(x = GCC), binwidth = 6, fill = "salmon", color = "black") +
  labs(x = "GCC(µm)",
       y = "Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust =0.5))

observation_counts <- GCC |>
  group_by(id) |>
  summarise(measurement_count = n())

# (b)
ggplot(data = GCC, aes(x = month, y = GCC, group = factor(id)))+
  geom_line(alpha = 0.8) +
  geom_point(size = 0.8) +
  geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
  theme_bw() +
  labs(x = "Month",
       y = "GCC(µm)") +
  theme(plot.title = element_text(hjust =0.5))

GCC_residuals <- GCC |>
  group_by(id) |>
  dplyr::mutate(mean_GCC = mean(GCC), 
                d_GCC = GCC - mean_GCC)

ggplot(data = GCC_residuals, aes(x = month, y = d_GCC, group = factor(id))) +
  geom_line(alpha = 0.6) +
  geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
  theme_bw() +
  labs(x = 'Month',
       y = 'Residual GCC(µm)') +
  theme(plot.title = element_text(hjust = 0.5))


sum_stat = summarySE(GCC, 
                     measurevar = 'GCC',
                     groupvars = c('month'))

ggplot(sum_stat, aes(x = month, y = GCC)) +
  geom_line(color = "salmon") + 
  geom_errorbar(aes(ymin = GCC - 2 * se,
                    ymax = GCC + 2 * se), color = "salmon") +
  theme_bw() +
  labs(x = "Month", y = "GCC(µm)") +
  theme(plot.title = element_text(hjust = 0.5))

# ggplot(data = GCC)+
#   geom_boxplot(aes(x= month, group = month, y = GCC)) +
#   theme_bw() +
#   labs(x = "Days",
#        y = "Weight(cg)",
#        title = 'Boxplots of weights by day of BIG MICE Data') +
#   theme(plot.title = element_text(hjust =0.5))
# 
# ggplot(summary_stats, aes(x = "", y = mean_GCC)) +
#   geom_violin(fill = "lightblue", color = "black") +
#   geom_jitter(width = 0.1, color = "darkblue", alpha = 0.7) +
#   labs(title = "Violin Plot of Mean GCC",
#        x = "",
#        y = "Mean GCC (µm)") +
#   theme_minimal()

sum_stat1 = summarySE(GCC_residuals, 
                     measurevar = 'd_GCC',
                     groupvars = c('month'))

ggplot(sum_stat1, aes(x = month, y = d_GCC)) +
  geom_line(color = "salmon") + 
  geom_errorbar(aes(ymin = d_GCC - 2 * se,
                    ymax = d_GCC + 2 * se), color = "salmon") +
  theme_bw() +
  labs(x = "Month", y = "Residual GCC(µm)") +
  theme(plot.title = element_text(hjust = 0.5))

# (c)
model_constant <- lmer(GCC ~ 1 + (1 | id), data = GCC)
model_linear <- lmer(GCC ~ month + (1 | id), data = GCC)
model_quar <- lmer(GCC ~ month + I(month^2) + (1 | id), data = GCC)
model_cubic <- lmer(GCC ~ month + I(month^2) + I(month^3) + (1 | id), data = GCC)
anova(model_constant, model_linear, model_quar, model_cubic)


model_RI <- lmer(GCC ~ month + I(month^2) + (1 | id), data = GCC)
model_RS <- lmer(GCC ~ month + I(month^2) + (0 + month | id), data = GCC)
model_RIAS <- lmer(GCC ~ month + I(month^2) + (1 + month | id), data = GCC) # win
model_RIASQ <- lmer(GCC ~ month + I(month^2) + (1 + month + I(month^2) | id), data = GCC)
anova(model_RI, model_RS, model_RIAS, model_RIASQ)

# library(nlme)
# library(glmmTMB)
# 
# model_cs <- lme(GCC ~ month + I(month^2), random = ~ month | id, data = GCC, 
#                 correlation = corCAR1(form = ~ 1 | id))
# 
# model_ar1 <- lme(GCC ~ time + I(time^2), random = ~ time | id, data = GCC, 
#                  correlation = corCAR1(form = ~ time | id)) # win
# 
# model_arma11 <- lme(GCC ~ month + I(month^2), random = ~ month | id, data = GCC, 
#                           correlation = corARMA(form = ~ month | id, p = 1, q = 1))
# 
# model_un <- lme(GCC ~ month + I(month^2), random = ~ month | id, data = GCC)
# # Independence model
# IND <- glmmTMB(GCC ~ month + I(month^2), 
#                data = GCC, 
#                REML = TRUE)
# # Unstructured model
# UN <- glmmTMB(GCC ~ month + I(month^2) + us(facmonth + 0 | id), 
#               dispformula = ~0, 
#               data = GCC, 
#               REML = TRUE)
# # Compound symmetry (Random Intercept) model
# RI <- glmmTMB(GCC ~ month + I(month^2) + (1 | id), 
#               dispformula = ~1, 
#               data = GCC, 
#               REML = TRUE)
# # Random slope model
# RS <- glmmTMB(GCC ~ month + I(month^2) + (0 + month | id), 
#               dispformula = ~1, 
#               data = GCC, 
#               REML = TRUE)
# # Random intercept and slope model
# RIAS <- glmmTMB(GCC ~ month + I(month^2) + (1 + month | id), 
#                 dispformula = ~1, 
#                 data = GCC, 
#                 REML = TRUE)
# # AR(1) model
# AR1 <- glmmTMB(GCC ~ month + I(month^2) + ar1(facmonth + 0 | id), 
#                dispformula = ~0, 
#                data = GCC, 
#                REML = TRUE)
# # ARMA(1,1) model
# ARMA11 <- glmmTMB(GCC ~ month + I(month^2) + ar1(facmonth + 0 | id), 
#                   dispformula = ~1, 
#                   data = GCC, 
#                   REML = TRUE)
# # Heterogeneous compound symmetry model
# HCS <- glmmTMB(GCC ~ month + I(month^2) + cs(facmonth + 0 | id), 
#                dispformula = ~0, 
#                data = GCC, 
#                REML = TRUE)
# summary(HCS)
# 
# # Non-constant variance with independent observations
# NCV <- glmmTMB(GCC ~ month + I(month^2) + diag(facmonth + 0 | id), 
#                dispformula = ~0, 
#                data = GCC, 
#                REML = TRUE)
# summary(NCV)
# 
# # Toeplitz with non-constant variance
# TOE <- glmmTMB(GCC ~ month + I(month^2) + toep(facmonth + 0 | id), 
#                dispformula = ~0, 
#                data = GCC, 
#                REML = TRUE)
# 
# anova(IND, UN, RI, RS, RIAS, AR1, ARMA11, HCS, NCV, TOE)

# (d)
GCC$predicted <- predict(model_RIAS)
rates_per_id <- GCC |>
  group_by(id) |>
  do(tidy(lm(predicted ~ month, data = .))) |>
  filter(term == "month") |>
  select(id, estimate) |>
  dplyr::rename(slope = estimate) |>
  left_join(GCC, by = "id") |>
  select(id, slope) |>
  distinct()

ggplot(rates_per_id, aes(x = "", y = slope)) +
  geom_violin(color = "salmon") +
  geom_jitter(width = 0.1, color = "salmon", alpha = 0.8) +
  labs(x = NULL,  # No x-axis label
       y = "Loss Rates of GCC Thickness Loss (µm/month)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
# summary(ARMA11)
# random_effects <- ranef(ARMA11)$cond$id
# summary(random_effects)
# 
# GCC$predicted <- predict(ARMA11)
# 
# # Plot observed and predicted trends
# ggplot(GCC, aes(x = month, y = GCC, group = as.factor(id))) +
#   geom_line(alpha = 0.3) +  # Individual trajectories
#   geom_line(aes(y = predicted), color = "salmon") +  # Population trend
#   labs(x = "Month",
#        y = "GCC Thickness (µm)") +
#   theme_minimal()

# random_effects <- ranef(model_RIAS)$id
# random_effects_slope <- random_effects[, "month"]
# ggplot(data.frame(Slope = random_effects_slope), aes(x = "", y = Slope)) +
#   geom_violin(color = "salmon") +
#   geom_jitter(width = 0.1, color = "salmon", alpha = 0.8) +
#   labs(x = NULL,  # No x-axis label
#        y = "Random Slopes of GCC Thickness Loss (µm/month)") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5))
# (e)
GCC <- GCC |>
  group_by(id) |>
  dplyr::mutate(baseline_GCC = first(GCC)) |>
  ungroup()

model_interaction <- lme(GCC ~ month * baseline_GCC + I(month^2) * baseline_GCC, random =  
                          ~month | id, data = GCC)
                             
summary(model_interaction)

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
  dplyr::rename(slope = estimate) |>
  left_join(GCC, by = "id") |>
  select(id, baseline_GCC, slope) |>
  distinct()

slope_baselineGCC <- lm(slope ~ baseline_GCC, data = slopes_per_id)
ggplot(slopes_per_id, aes(x = initial_GCC, y = slope)) +
  geom_point(color = "salmon") +
  geom_smooth(method = "lm", color = "salmon", se = TRUE) +
  labs(x = "Baseline GCC (µm)",
       y = "Slope of GCC Change (µm/month)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
# title = "Relationship Between Initial GCC and Slope of GCC Change",

# Q2
model_no_age <- lmer(GCC ~ month + (1 + month | id), data = GCC)
model_age <- lmer(GCC ~ month + base_age + (1 + month | id), data = GCC)
model_age_quar <- lmer(GCC ~ month + base_age + I(base_age^2) + (1 + month | id), data = GCC)
model_age_interaction <- lmer(GCC ~ month * base_age + (1 + month | id), data = GCC)
model_age_quar_interaction <- lmer(GCC ~ month * base_age + month*I(base_age^2) 
                                   + (1 + month | id), data = GCC)
anova(model_no_age, model_age, model_age_quar, model_age_interaction, model_age_quar_interaction)



ggplot(GCC, aes(x = month, y = GCC, group = factor(id))) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = fitted(model_age_quar)), color = "salmon") +
  labs(title = "Observed vs. Fitted Values with Age Effect",
       x = "Time (years)",
       y = "GCC Thickness (µm)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

library(interactions)
interact_plot(model_age_quar, pred = base_age, modx = month,
              plot.points = TRUE)
mean_gcc_by_id <- GCC %>%
  group_by(id) %>%
  summarize(mean_GCC = mean(GCC, na.rm = TRUE),
            age = first(base_age))
ggplot(mean_gcc_by_id, aes(x = age, y = mean_GCC)) +
  geom_point(color = "salmon", size = 3, alpha = 0.8) +
  labs(
    x = "Age (years)",
    y = "Mean GCC Thickness (µm)",
    title = "Scatter Plot of Mean GCC Thickness vs. Age"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

plot(model_RIAS, color = "salmon")
qqnorm(residuals(model_RIAS), main="")
residuals_age_model <- resid(model_age_quar)
qqnorm(residuals_age_model, main = "Q-Q Plot of Residuals for Age Interaction Model")

# 看一下确实没有interaction age，确实需要age的二次项，两道题的做个比较，age确实不行
