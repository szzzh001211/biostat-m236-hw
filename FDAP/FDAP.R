library(tidyverse)
library(ggplot2)
library(lme4)
# Repeated CD4 counts data from AIDS clinical trial.
# 
# Source: AIDS Clinical Trial Group 193A Study. 
# Data courtesy of Dr. Keith Henry.
# 
# Reference: Henry, K., Erice, A., Tierney, C., Balfour, H.H. Jr, Fischl, M.A.,
# Kmack, A., Liou, S.H., Kenton, A., Hirsch, M.S., Phair, J., Martinez, A. 
# and Kahn J.O. for the AIDS Clinical Trial Group 193A Study Team (1998). 
# A randomized, controlled, double-blind study comparing the survival benefit of 
# four different reverse transcriptase inhibitor therapies (three-drug, two-drug, 
# and alternating drug) for the treatment of advanced AIDS. Journal of Acquired 
# Immune Deficiency Syndromes and Human Retrovirology, 19, 339-349.
# 
# 
# Description:
#   
# The data are from a randomized, double-blind, study of AIDS patients with 
# advanced immune suppression (CD4 counts of less than or equal to 50 cells/mm^3).
# Patients in AIDS Clinical Trial Group (ACTG) Study 193A were randomized to 
# dual or triple combinations of HIV-1 reverse transcriptase inhibitors. 
# Specifically, patients were randomized to one of four daily regimens containing
# 600mg of zidovudine: zidovudine alternating monthly with 400mg didanosine;
# zidovudine plus 2.25mg of zalcitabine; zidovudine plus 400mg of didanosine; 
# or zidovudine plus 400mg of didanosine plus 400mg of nevirapine (triple therapy). 
# Measurements of CD4 counts were scheduled to be collected at baseline and at 
# 8-week intervals during follow-up. However, the CD4 count data are unbalanced 
# due to mistimed measurements and missing data that resulted from skipped visits
# and dropout. The number of measurements of CD4 counts during the first 40 
# weeks of follow-up varied from 1 to 9, with a median of 4. 

# The response variable is the log transformed CD4 counts, log(CD4 counts + 1), 
# available on 1309 patients.

# The categorical variable Treatment is coded 1 = zidovudine alternating monthly 
# with 400mg didanosine, 2 = zidovudine plus 2.25mg of zalcitabine, 3 = zidovudine
# plus 400mg of didanosine, and 4 = zidovudine plus 400mg of didanosine plus 
# 400mg of nevirapine. The variable Week represents time since baseline (in weeks).
# Variable List: Subject ID, Treatment, Age (years), Gender (1=M, 0=F), Week, log(CD4 count + 1).


cd4 <- read.table("cd4-data.txt", header = FALSE)

colnames(cd4) <- c("id", "treatment", "age", "gender", "week", "counts")

observation_counts <- cd4 |>
  group_by(treatment) |>
  summarise(percentage = n_distinct(id))

treatment_1_count <- cd4 |>
  filter(treatment == 1) |>
  summarise(patient_count = n_distinct(id))

model <- glmer(response ~ treatment + month + visit+ treatment:month + (1|id), 
               data = toenail, family = binomial)

mean_response <- toenail |>
  group_by(id) |>
  summarise(mean_response = mean(response))

ggplot(mean_response, aes(x = mean_response)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Mean Response per Subject",
    x = "Mean Response",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(toenail, aes(x = month, y = length, group = id, color = treatment)) + 
  geom_line() +
  facet_wrap(~treatment) +
  labs(x = "Month", y = "Length (mm)")


