setwd("C:/Users/wiedmann4/Documents/Aid and corruption")
library(tidyverse)
library(dplyr)
#library(plyr)
library(countrycode)
library(countries)
library(tidyr)
library(stringr)
library(readr)
library(data.table)

###############
# Regressions #
###############

DubaiData <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Out/DubaiData.csv")

DubaiDataOA <- fread("C:/Users/wiedmann4/Documents/Aid and corruption/Out/DubaiDataOA.csv")

lm <- lm(SandDummy~comm, data = SLG)
summary(lm)

comm_model <- glm(SandDummy~log_total.comm.CN, family = binomial, data = SLG)
summary(comm_model)
# odds ratio for aid (comm)
exp(coef(comm_model)["log_comm"]) # result: 1.24394; A 1-unit increase in log(comm) 
# multiplies the odds of being in SC by 1.24, or when e.g. aid doubles ($10M->$20M)
# change in log(comm)=log(20)-log(10)=log(20/10)=log(2)=0.693
# Therefore, change in log-odds=0.21828x0.693=0.151, and multiplier for odds=exp(0.151)=1.163 or about 16.3% increase in odds

disb_model <- glm(SandDummy~log_disb, family = binomial, data = SLG)
summary(disb_model)

tenure_model <- glm(SandDummy~tenure, family = binomial, data = SLG)
summary(tenure_model)

# Fixed effects

library(bife)
library(fixest)
fe_model <- bife(SandDummy ~ log_comm | LeadersCountry, 
                 data = SLG,
                 model = "logit",
                 bias_corr = "ana")
summary(fe_model)
fe_model2 <- feols(SandDummy ~ log_comm | LeadersCountry, 
                   data = SLG)
summary(fe_model2)

countries_with_variation <- SLG %>%
  group_by(LeadersCountry) %>%
  summarise(
    n_leaders = n(),
    n_dubai = sum(SandDummy),
    pct_dubai = mean(SandDummy) * 100
  ) %>%
  filter(n_dubai > 0 & n_dubai < n_leaders) %>%  # Countries with variation
  arrange(desc(pct_dubai))
