library(tidyverse)
library(ggplot2)
library(lme4)
library(Rmisc)
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
  geom_smooth(aes(color = treatment, group = treatment), method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, max(ADQS_PACC$facweek), by = 24)) + 
  theme_bw() +
  labs(x = "Weeks",
       y = "PACC Score") +
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.2)) +
  theme(plot.title = element_text(hjust =0.5))



PACC_residuals <- ADQS_PACC |>
  group_by(id) |>
  dplyr::mutate(mean_PACC = mean(PACC), 
                d_PACC = PACC - mean_PACC)

ggplot(data = PACC_residuals, aes(x = facweek, y = d_PACC, color = treatment, 
                                  group = factor(id))) +
  geom_line(alpha = 0.6) +
  geom_point(size = 0.6) +
  geom_smooth(aes(color = treatment, group = treatment), method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, max(ADQS_PACC$facweek), by = 24)) + 
  theme_bw() +
  labs(x = 'Weeks',
       y = 'Residual PACC Score') +
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.2)) +
  theme(plot.title = element_text(hjust = 0.5))


sum_stat = summarySE(ADQS_PACC, 
                     measurevar = 'PACC',
                     groupvars = c('facweek', 'treatment'))

ggplot(sum_stat, aes(x = facweek, y = PACC, color = treatment, group = treatment)) + 
  geom_line() + 
  geom_errorbar(aes(ymin = PACC - 2 * se,
                    ymax = PACC + 2 * se)) +
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


