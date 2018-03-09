####################################################################################
# DS 5559: Public Interest Data Lab 
# 1. Visualize of racial proportions: referrals vs. population
# 2. Generate disproportionality and disparity measures: referrals
# Michele Claibourn (mclaibourn@virginia.edu)
# Updated: March 9, 2018 
####################################################################################

rm(list=ls())
setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/acsdata/")
library(tidyverse)
library(RColorBrewer)
load("cville_acs.Rdata")


####################################################################################
# 1. Visualize of racial proportions: referrals vs. population
####################################################################################
# by race4
cwspopacs4 %>% filter(race != "Total") %>% arrange(race) %>% 
  ggplot(aes(y = number, x = refyear)) +
  geom_bar(stat = "identity", aes(fill = race), position = "fill") +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,6,7,8)]) +
  labs(title = "Population Proportions and Referral Proportions by Race",
       subtitle = "Population proportions from 2012-2016 American Community Survey",
       y = "Proportion by Race",
       x = "Population vs. Referrals in 2015-2017") + 
  annotate("text", x = 1, y = c(.1, .31, .38, .85), label = c(".27", ".08", ".06", ".59"), color = "white") +
  annotate("text", x = 2, y = c(.1, .6, .73, .85), label = c(".54", ".17", ".02", ".27"), color = "white") +
  annotate("text", x = 3, y = c(.1, .61, .725, .85), label = c(".56", ".15", ".02", ".27"), color = "white") +
  annotate("text", x = 4, y = c(.1, .6, .68, .85), label = c(".56", ".10", ".03", ".31"), color = "white") +
  annotate("text", x = c(1,2,3,4), y = 1.05, label = c("7084","410", "536", "790"))

# by race2
cwspopacs2 %>% filter(race != "Total") %>% arrange(race) %>% 
  ggplot(aes(y = number, x = refyear)) +
  geom_bar(stat = "identity", aes(fill = race), position = "fill") +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,8)]) +
  labs(title = "Population Proportions and Referral Proportions by Race",
       subtitle = "Population proportions from 2012-2016 American Community Survey",
       y = "Proportion by Race",
       x = "Population vs. Referrals in 2015-2017") +
  annotate("text", x = 1, y = c(.3, .85), label = c(".41", ".59"), color = "white") +
  annotate("text", x = 2, y = c(.3, .85), label = c(".73", ".27"), color = "white") +
  annotate("text", x = 3, y = c(.3, .85), label = c(".73", ".27"), color = "white") +
  annotate("text", x = 4, y = c(.3, .85), label = c(".69", ".31"), color = "white") +
  annotate("text", x = c(1,2,3,4), y = 1.05, label = c("7084", "410", "536", "790"))


####################################################################################
# 2. Generate disproportionality and disparity measures for referrals (by year)
####################################################################################
# Disproportionality: 
#   ratio of % of [race of] children referred to CWS to % of [race of] children in population
#   e.g., % referred to CWS who are white/% children in Cville who are white
#   calculate based on estimated proportion and lower/upper bound of estimated proportion

# by race4
acspopcws4 <- acspopcws4 %>% 
  mutate(rd_2015_lo = prop15/(prop - pmoe),
         rd_2015_mi = prop15/prop,
         rd_2015_hi = prop15/(prop + pmoe),
         rd_2016_lo = prop16/(prop - pmoe),
         rd_2016_mi = prop16/prop,
         rd_2016_hi = prop16/(prop + pmoe),
         rd_2017_lo = prop17/(prop - pmoe),
         rd_2017_mi = prop17/prop,
         rd_2017_hi = prop17/(prop + pmoe))

# by race2
acspopcws2 <- acspopcws2 %>% 
  mutate(rd_2015_lo = prop15/(prop - pmoe),
         rd_2015_mi = prop15/prop,
         rd_2015_hi = prop15/(prop + pmoe),
         rd_2016_lo = prop16/(prop - pmoe),
         rd_2016_mi = prop16/prop,
         rd_2016_hi = prop16/(prop + pmoe),
         rd_2017_lo = prop17/(prop - pmoe),
         rd_2017_mi = prop17/prop,
         rd_2017_hi = prop17/(prop + pmoe))

# visualization
# gather the data frame
acspopcws4_long <- acspopcws4 %>% 
  filter(race != "Total") %>% select(race, rd_2015_lo:rd_2017_hi) %>% 
  gather(key = "year", value = "disp", -race) %>% 
  separate(year, into = c("meas", "year", "int"), sep = "_") %>% 
  select(-meas) %>% 
  spread(key = int, value = disp) 
# and plot by year
ggplot(acspopcws4_long, aes(x = mi, y = race)) +
  geom_errorbarh(aes(xmin = hi, xmax = lo)) +
  geom_point() +
  facet_wrap(~year) +
  labs(title = "Estimated Racial Disproportionality in Child Welfare Referrals",
       subtitle = "Population estimates based on 2012-2016 American Community Survey",
       x = "Disportionality (1 = Proportionate)",
       y = "Race")

  
### NEXT
# 2. Add screened in proportions and repeat figures and disproportionality calculation
# 3. Time permitting: read in tract data with tidycensus 
#   and explore visually by tract referrals (count, percent, ratios?)

# Number of children in Cville by race
acspopcws4 %>%  
  ggplot(aes(x = estimate, y = reorder(race, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Estimated number of children in Charlottesville by race",
       subtitle = "2012-2016 American Community Survey",
       x = "Estimate",
       y = "Race")

