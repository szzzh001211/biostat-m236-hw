library(haven)
library(tidyverse)
library(emmeans)
library(glmmTMB)
dental <- read_sas("dental.sas7bdat")
boys <- dental |>
  filter(gender == "Boy")
girls <- dental |>
  filter(gender == "Girl")

boys_RI <- glmmTMB(distance ~ age + (1 | id),
                   dispformula = ~ 1, data = boys, REML = TRUE)

model <- lme(fixed = distance ~ age, random = ~ 1 | id, data = boys)
girls_RI <- glmmTMB(distance ~ age + (1 | id), 
                    dispformula = ~ 1, data = girls, REML = TRUE)
summary(boys_RI)
summary(girls_RI)