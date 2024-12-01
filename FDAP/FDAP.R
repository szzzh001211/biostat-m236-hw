library(tidyverse)
library(ggplot2)
library(lme4)

ADQS_raw <- read_csv("ADQS.csv")
ADQS_PACC <- ADQS_raw %>%
  filter(MITTFL== 1) %>%
  filter(EPOCH == "BLINDED TREATMENT" | AVISIT == "006") %>%
  filter(QSTESTCD == "PACC") %>%
  rename(PACC = QSSTRESN) %>%
  select(BID, ASEQNCS, TX, ADURW, TX, AGEYR,
         AAPOEGNPRSNFLG, EDCCNTU, SUVRCER, PACC) %>%
  mutate(TX = factor(TX, levels = c("Placebo", "Solanezumab"))) %>%
  na.omit()

ADQS_PACC <- ADQS_PACC |>
  rename(weeks = ADURW, treatment = TX) |>
  rename(id = BID) |>
  mutate(facweek = round(weeks / 24) * 24)

ggplot(ADQS_PACC) +
  geom_histogram(aes(x = PACC, fill = treatment), binwidth = 6, color = "black") +
  labs(x = "PACC Score",
       y = "Frequency") +
  theme_bw() +
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.2)) +
  theme(plot.title = element_text(hjust =0.5))

ggplot(data = ADQS_PACC, aes(x = facweek, y = PACC, color = treatment,
                             group = factor(id))) +
  geom_line(alpha = 0.6) +
  geom_point(size = 0.6) +
  geom_smooth(aes(group = treatment), method = "lm", se = FALSE, size = 0.8, alpha = 0.8, color = "purple") + 
  scale_x_continuous(breaks = seq(0, max(ADQS_PACC$facweek), by = 24)) + 
  theme_bw() +
  labs(x = "Weeks",
       y = "PACC Score") +
  #theme(legend.position = "inside", legend.position.inside = c(0.2, 0.2)) +
  theme(plot.title = element_text(hjust =0.5)) +
  facet_wrap(~ treatment, ncol = 1)



PACC_residuals <- ADQS_PACC |>
  group_by(id) |>
  dplyr::mutate(mean_PACC = mean(PACC), 
                d_PACC = PACC - mean_PACC)

ggplot(data = PACC_residuals, aes(x = facweek, y = d_PACC, color = treatment, 
                                  group = factor(id))) +
  geom_line(alpha = 0.6) +
  geom_point(size = 0.6) +
  geom_smooth(aes(group = treatment), method = "lm", se = FALSE, size = 0.8, alpha = 0.8, color = "purple") + 
  scale_x_continuous(breaks = seq(0, max(ADQS_PACC$facweek), by = 24)) + 
  theme_bw() +
  labs(x = 'Weeks',
       y = 'Residual PACC Score') +
  #theme(legend.position = "inside", legend.position.inside = c(0.2, 0.2)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~ treatment, ncol = 1)

library(Rmisc)
sum_stat = summarySE(ADQS_PACC, 
                     measurevar = 'PACC',
                     groupvars = c('facweek', 'treatment'))

ggplot(sum_stat, aes(x = facweek, y = PACC, color = treatment, group = treatment)) + 
  geom_line() + 
  geom_errorbar(aes(ymin = PACC - 2 * se,
                    ymax = PACC + 2 * se)) +
  scale_x_continuous(breaks = seq(0, max(ADQS_PACC$facweek), by = 24)) +
  theme_bw() +
  labs(x = "Weeks", y = "PACC Score") +
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.2)) +
  theme(plot.title = element_text(hjust = 0.5))


sum_stat1 = summarySE(PACC_residuals, 
                      measurevar = 'd_PACC',
                      groupvars = c('facweek', 'treatment'))

ggplot(sum_stat1, aes(x = facweek, y = d_PACC, color = treatment, group = treatment)) +
  geom_line() + 
  geom_errorbar(aes(ymin = d_PACC - 2 * se,
                    ymax = d_PACC + 2 * se)) +
  theme_bw() +
  labs(x = "Weeks", y = "Residual PACC Score") +
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.2)) +
  theme(plot.title = element_text(hjust = 0.5))

model_constant <- lmer(PACC ~ 1 + (1 | id), data = ADQS_PACC)
model_linear <- lmer(PACC ~ weeks + (1 | id), data = ADQS_PACC)
model_quar <- lmer(PACC ~ weeks + I(weeks^2) + (1 | id), data = ADQS_PACC)
model_cubic <- lmer(PACC ~ weeks + I(weeks^2) + I(weeks^3) + (1 | id), data = ADQS_PACC)
anova(model_constant, model_linear, model_quar, model_cubic)

model_RI <- lmer(PACC ~ weeks + I(weeks^2) + (1 | id), data = ADQS_PACC)
model_RS <- lmer(PACC ~ weeks + I(weeks^2) + (0 + weeks | id), data = ADQS_PACC)
model_RIAS <- lme(PACC ~ weeks + I(weeks^2), random = ~ weeks | id, data = ADQS_PACC) # win
model_RIASQ <- lmer(PACC ~ weeks + I(weeks^2) + (1 + weeks + I(weeks^2) | id), data = ADQS_PACC)
anova(model_RI, model_RS, model_RIAS, model_RIASQ)

ADQS_PACC_placebo <- filter(ADQS_PACC, treatment == "Placebo")
ADQS_PACC_solanezumab <- filter(ADQS_PACC, treatment == "Solanezumab")

model_RIAS_placebo <- lmer(PACC ~ weeks + I(weeks^2) + (1 + weeks | id), data = ADQS_PACC_placebo)
model_RIAS_solanezumab <- lmer(PACC ~ weeks + I(weeks^2) + (1 + weeks | id), data = ADQS_PACC_solanezumab)

model_RIAS_treatment <- lmer(PACC ~ weeks + I(weeks^2) + treatment + (1 + weeks | id), data = ADQS_PACC)
model_RIAS_treatment_interact1 <- lmer(PACC ~ weeks * treatment + I(weeks^2) + (1 + weeks | id), data = ADQS_PACC)
model_RIAS_treatment_interact2 <- lmer(PACC ~ weeks * treatment + I(weeks^2) * treatment + (1 + weeks | id), data = ADQS_PACC)
anova(model_RIAS_treatment, model_RIAS_treatment_interact1, model_RIAS_treatment_interact2)

library(interactions)
interact_plot(model_RIAS_treatment_interact1, pred = weeks, modx = treatment,
              plot.points = TRUE)
# observation_counts <- ADQS_PACC |>
#   group_by(AAPOEGNPRSNFLG) |>
#   summarise(count = n_distinct(BID))
# 
# treatment_1_count <- cd4 |>
#   filter(treatment == 1) |>
#   summarise(patient_count = n_distinct(id))
# 
# model <- glmer(response ~ treatment + month + visit+ treatment:month + (1|id), 
#                data = toenail, family = binomial)
# 
# mean_response <- toenail |>
#   group_by(id) |>
#   summarise(mean_response = mean(response))
# 
# ggplot(mean_response, aes(x = mean_response)) +
#   geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
#   labs(
#     title = "Histogram of Mean Response per Subject",
#     x = "Mean Response",
#     y = "Frequency"
#   ) +
#   theme_minimal()
# 
# ggplot(toenail, aes(x = month, y = length, group = id, color = treatment)) + 
#   geom_line() +
#   facet_wrap(~treatment) +
#   labs(x = "Month", y = "Length (mm)")


