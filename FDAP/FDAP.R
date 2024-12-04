library(tidyverse)
library(ggplot2)
library(lme4)
# PACC_raw <- read_csv("PACC.csv") |>
#   filter(VISCODE == "006")
ADQS_raw <- read_csv("ADQS.csv")
ADQS_PACC <- ADQS_raw %>%
  filter(MITTFL== 1) %>%
  filter(EPOCH == "BLINDED TREATMENT" | AVISIT == "006") %>%
  filter(QSTESTCD == "PACC") %>%
  rename(PACC = QSSTRESN) %>%
  select(BID, ASEQNCS, TX, ADURW, TX, AGEYR,
         AAPOEGNPRSNFLG, EDCCNTU, SUVRCER, PACC) %>%
  dplyr::mutate(TX = factor(TX, levels = c("Placebo", "Solanezumab"))) %>%
  na.omit()

ADQS_PACC <- ADQS_PACC |>
  rename(weeks = ADURW, treatment = TX) |>
  rename(id = BID) |>
  mutate(year = weeks / 48) |>
  mutate(facweek = round(year / 0.5) * 0.5) |> 
  filter(!(facweek %in% c(5.5, 6, 6.5)))
#data_subset <- subset(ADQS_PACC, facweek %in% c(3.5))


ggplot(ADQS_PACC) +
  geom_histogram(aes(x = PACC, fill = treatment), binwidth = 6, color = "black") +
  labs(x = "PACC Score",
       y = "Frequency") +
  theme_bw() +
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.2)) +
  theme(
    axis.title = element_text(size = 14),    # Axis title font size
    axis.text = element_text(size = 12),     # Axis text (ticks) font size
    strip.text = element_text(size = 14),    # Facet label font size
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.text = element_text(size = 12),      # Legend text size
    legend.title = element_text(size = 14)# Plot title font size and centering
  )

ggplot(data = ADQS_PACC, aes(x = facweek, y = PACC,
                             group = factor(id))) +
  geom_line(alpha = 0.6) +
  geom_point(size = 0.6) +
  geom_smooth(aes(group = treatment), method = "lm", se = FALSE, size = 0.8, alpha = 0.8, color = "red") + 
  scale_x_continuous(breaks = seq(0, max(ADQS_PACC$facweek), by = 0.5)) + 
  theme_bw() +
  labs(x = "Years",
       y = "PACC Score") +
  #theme(legend.position = "inside", legend.position.inside = c(0.2, 0.2)) +
  theme(plot.title = element_text(hjust =0.5)) +
  theme(
    axis.title = element_text(size = 14),    # Axis title font size
    axis.text = element_text(size = 12),     # Axis text (ticks) font size
    strip.text = element_text(size = 14),    # Facet label font size
    plot.title = element_text(hjust = 0.5, size = 16)  # Plot title font size and centering
  ) + 
  facet_wrap(~ treatment, ncol = 2)



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
                     measurevar = 'AGEYR',
                     groupvars = c('facweek', 'treatment'))

ggplot(sum_stat, aes(x = facweek, y = PACC, color = treatment, group = treatment)) + 
  geom_line() + 
  geom_errorbar(aes(ymin = PACC - 2 * se,
                    ymax = PACC + 2 * se)) +
  scale_x_continuous(breaks = seq(0, max(ADQS_PACC$facweek), by = 0.5)) +
  theme_bw() +
  labs(x = "Years", y = "PACC Score") +
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.2)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(
    axis.title = element_text(size = 14),    # Axis title font size
    axis.text = element_text(size = 12),     # Axis text (ticks) font size
    strip.text = element_text(size = 14),    # Facet label font size
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.text = element_text(size = 12),      # Legend text size
    legend.title = element_text(size = 14)# Plot title font size and centering
  )


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
model_linear <- lmer(PACC ~ year + (1 | id), data = ADQS_PACC)
model_quar <- lmer(PACC ~ year + I(year^2) + (1 | id), data = ADQS_PACC)
model_cubic <- lmer(PACC ~ year + I(year^2) + I(year^3) + (1 | id), data = ADQS_PACC)
anova(model_constant, model_linear, model_quar, model_cubic)

library(nlme)
model_RI <- lme(PACC ~ year + I(year^2), random = ~ 1 | id, data = ADQS_PACC)
model_RS <- lme(PACC ~ year + I(year^2), random = ~ -1 + year | id, data = ADQS_PACC)
model_AR1 <- lme(PACC ~ year + I(year^2), random = ~ 1 | id, correlation = corAR1(form = ~ ASEQNCS | id), data = ADQS_PACC)
model_RIAS <- lme(PACC ~ year + I(year^2), random = ~ 1 + year | id, data = ADQS_PACC)
model_ARMA <- lme(PACC ~ year + I(year^2), random = ~ 1 | id, correlation = corARMA(form = ~ ASEQNCS | id, p = 1, q = 1), data = ADQS_PACC)
model_RIASQ <- lme(PACC ~ year + I(year^2), random = ~ 1 + year + I(year^2) | id, data = ADQS_PACC) # win
#model_RIASQ <- lmer(PACC ~ year + I(year^2) + (1 + year + I(year^2) | id), data = ADQS_PACC)
anova(model_RI, model_RS, model_AR1, model_RIAS, model_ARMA, model_RIASQ)

ADQS_PACC_placebo <- filter(ADQS_PACC, treatment == "Placebo")
ADQS_PACC_solanezumab <- filter(ADQS_PACC, treatment == "Solanezumab")

model_RIAS_placebo <- lmer(PACC ~ weeks + I(weeks^2) + (1 + weeks | id), data = ADQS_PACC_placebo)
model_RIAS_solanezumab <- lmer(PACC ~ weeks + I(weeks^2) + (1 + weeks | id), data = ADQS_PACC_solanezumab)

model_RIAS_treatment <- lme(PACC ~ year + I(year^2) + treatment, random = ~ 1 + year + I(year^2) | id, data = ADQS_PACC)
#model_RIAS_treatment <- lmer(PACC ~ year + I(year^2) + treatment + (1 + year + I(year^2) | id), data = ADQS_PACC)
model_RIAS_treatment_interact1 <- lme(PACC ~ year * treatment + I(year^2), random = ~ 1 + year + I(year^2) | id, data = ADQS_PACC)
model_RIAS_treatment_interact2 <- lme(PACC ~ year * treatment + I(year^2) * treatment, random = ~ 1 + year + I(year^2) | id, data = ADQS_PACC)
anova(model_RIAS_treatment, model_RIAS_treatment_interact1, model_RIAS_treatment_interact2)

anova(model_RIAS_treatment, model_RIASQ)
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


ggplot(ADQS_PACC, aes(x = facweek, y = resid(model_RIASQ), group = id)) +
  geom_line(alpha = 0.4) +                       # Connect residuals for each subject
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Reference line at 0
  labs(x = "Weeks", y = "Residuals") +
  theme_minimal()

