####################################################################################
# DS 5559: Public Interest Data Lab 
# 1. Explore Cville child population estimates
# 2. Generate disproportionality and disparity measures
# Michele Claibourn (mclaibourn@virginia.edu)
# Updated: March 1, 2018
####################################################################################

rm(list=ls())
setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/acsdata/")
library(tidyverse)
load("cville_acs.Rdata")

# Number of children in Cville by race
childpop %>%  
  ggplot(aes(x = estimate, y = reorder(race, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Estimated number of children in Charlottesville by race",
       subtitle = "2012-2016 American Community Survey",
       x = "Estimate",
       y = "Race")

# Referrals vs. Population by Race (race4)
cwspopacs %>% filter(race != "Total") %>% arrange(race) %>% 
  ggplot(aes(y = number, x = refyear)) +
  geom_bar(stat = "identity", aes(fill = race), position = "fill") +
  labs(title = "Population Proportions and Referral Proportions by Race",
       subtitle = "Population proportions from 2012-2016 American Community Survey",
       y = "Proportion by Race",
       x = "Population vs. Referrals in 2015-2017")
# add proportion values to column; reorder fill values; change colors

# Referrals vs. Population by Race (race2)
cwspopacs2 %>% filter(race != "Total") %>% arrange(race) %>% 
  ggplot(aes(y = number, x = refyear)) +
  geom_bar(stat = "identity", aes(fill = race), position = "fill") +
  labs(title = "Population Proportions and Referral Proportions by Race",
       subtitle = "Population proportions from 2012-2016 American Community Survey",
       y = "Proportion by Race",
       x = "Population vs. Referrals in 2015-2017")
# add proportion values to column; reorder fill values; change colors

### NEXT
# 1. Calculate disproportionality of referrals (race4, race2) by year
# Disproportionality: 
#   ratio of % of [race of] children referred to CWS to % of [race of] children in population
#   e.g., % referred to CWS who are white/% children in Cville who are white
#   calculate based on estimated proportion and lower/upper bound of estimated proportion
# 2. Add screened in proportions and repeat figures and disproportionality calculation
# 3. Time permitting: read in tract data with tidycensus 
#   and explore visually by tract referrals (count, percent, ratios?)