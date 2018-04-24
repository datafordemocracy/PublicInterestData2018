####################################################################################
# DS 5559: Public Interest Data Lab 
# 1. Racial compositions: referrals for 2015, 2016, 2017 vs. population
# 2. Racial disproportionality index for referrals for 2015, 2016, 2017
# 3. Racial compositions: referrals 2017, active 2017, foster 2017 vs. population
# 4. Racial disproportionality index for 2017 referrals, active cases, foster cases
# Michele Claibourn (mclaibourn@virginia.edu)
# Updated: March 17, 2018 
####################################################################################

rm(list=ls())
setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/acsdata/")
library(tidyverse)
library(RColorBrewer)
library(scales)
load("cville_acs.Rdata")


####################################################################################
# 1. Racial compositions: referrals for 2015, 2016, 2017 vs. population
####################################################################################
# by race4
cwspopacs4 %>% filter(source %in% c("ref15", "ref16", "ref17", "acs16")) %>% arrange(race) %>% 
  ggplot(aes(y = number, x = source)) +
  geom_bar(stat = "identity", aes(fill = race), position = "fill") +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,6,7,8)]) +
  labs(title = "Population Proportions and Referral Proportions by Race",
       subtitle = "Population proportions from 2012-2016 American Community Survey",
       y = "", x = "", fill = "Race") + 
  scale_x_discrete(labels=c("acs16" = "2016 Population", "ref15" = "2015 Referrals",
                                "ref16" = "2016 Referrals", "ref17" = "2017 Referrals")) +
  scale_y_continuous(labels = percent) +
  annotate("text", x = 1, y = c(.1, .31, .38, .85), label = c("27.1 (± 3.0)", "8.0 (±- 2.4)", "5.9 (± 2.1)", "59.0 (± 0.7)"), color = "white") +
  annotate("text", x = 2, y = c(.1, .6, .73, .85), label = c("54.4", "17.1", "1.7", "26.8"), color = "white") +
  annotate("text", x = 3, y = c(.1, .61, .725, .85), label = c("56.3", "14.9", "1.9", "26.9"), color = "white") +
  annotate("text", x = 4, y = c(.1, .6, .68, .85), label = c("55.6", "10.1", "3.3", "31.1"), color = "white") +
  annotate("text", x = c(1,2,3,4), y = 1.05, label = c("7084 (± 314)","410", "536", "790"))
ggsave("referral_race4.pdf", width=9, height=6, units="in") # can be .png, .pdf, .eps, .ps, .tiff, .bmp, .svg, .wfm

# by race2
cwspopacs2 %>% filter(source %in% c("ref15", "ref16", "ref17", "acs16")) %>% arrange(race) %>% 
  ggplot(aes(y = number, x = source)) +
  geom_bar(stat = "identity", aes(fill = race), position = "fill") +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,8)]) +
  labs(title = "Population Proportions and Referral Proportions by Race",
       subtitle = "Population proportions from 2012-2016 American Community Survey",
       y = "", x = "", fill = "Race") +
  scale_x_discrete(labels=c("acs16" = "2016 Population", "ref15" = "2015 Referrals",
                            "ref16" = "2016 Referrals", "ref17" = "2017 Referrals")) +
  scale_y_continuous(labels = percent) +
  annotate("text", x = 1, y = c(.3, .85), label = c("41.0 (± 4.2)", "59.0 (± 0.7"), color = "white") +
  annotate("text", x = 2, y = c(.3, .85), label = c("73.2", "26.8"), color = "white") +
  annotate("text", x = 3, y = c(.3, .85), label = c("73.1", "26.9"), color = "white") +
  annotate("text", x = 4, y = c(.3, .85), label = c("68.9", "31.1"), color = "white") +
  annotate("text", x = c(1,2,3,4), y = 1.05, label = c("7084 (± 314)", "410", "536", "790"))
ggsave("referral_race2.pdf", width=9, height=6, units="in") 


####################################################################################
# 2. Racial disproportionality index for referrals for 2015, 2016, 2017
####################################################################################
# Disproportionality: 
#   ratio of % of [race of] children referred to CWS to % of [race of] children in population
#   e.g., % referred to CWS who are white/% children in Cville who are white
#   calculate based on estimated proportion and lower/upper bound of estimated proportion

### a. generate intervals ###
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

### b. visualize ###
# by race4
# gather the data frame
acspopcws4_ref <- acspopcws4 %>% 
  filter(race != "Total") %>% select(race, rd_2015_lo:rd_2017_hi) %>% 
  gather(key = "year", value = "disp", -race) %>% 
  separate(year, into = c("meas", "year", "int"), sep = "_") %>% 
  select(-meas) %>% 
  spread(key = int, value = disp) 
# plot by year
ggplot(acspopcws4_ref, aes(x=year, y=mi, fill=race)) + 
  geom_bar(position = "dodge", stat="identity")  +
  geom_errorbar(aes(ymin = hi, ymax = lo), width = .2, position = position_dodge(0.9)) +
  geom_hline(yintercept = 1, color = "black") +
  scale_y_continuous(name = "Disproportionality Index (90% Confidence Intervals)", 
                     trans = "log", # log transformation
                     breaks = c(0.25, 0.33, 0.5, 0.67, 1, 1.5, 2, 3, 4), 
                     labels = c("0.25", "0.33", "0.5", "0.67", "1", "1.5", "2", "3", "4")) +
                     scale_fill_manual(values = brewer.pal(9, "Blues")[c(8,7,6,4)]) +
  expand_limits(y = 4) +
  labs(title = "Racial Disproportionality Index in Referrals",
       subtitle = "Based on population proportions from 2012-2016 American Community Survey",
       y = "", x = "", fill = "Race") +
  annotate("text", x = c(0.7, 0.9, 1.1, 1.3), y = c(1.1, 1.1, .9, .9), label = c("2.01", "2.13", "0.29", "0.46"), color = "white") +
  annotate("text", x = c(1.7, 1.9, 2.1, 2.3), y = c(1.1, 1.1, .9, .9), label = c("2.08", "1.86", "0.31", "0.46"), color = "white") +
  annotate("text", x = c(2.7, 2.9, 3.1, 3.3), y = c(1.1, 1.1, .9, .9), label = c("2.05", "1.24", "0.55", "0.53"), color = "white") +
  coord_flip()
ggsave("referral_rdi_race4.pdf", width=9, height=6, units="in") 

# by race2
# gather the data frame
acspopcws2_ref <- acspopcws2 %>% 
  filter(race != "Total") %>% select(race, rd_2015_lo:rd_2017_hi) %>% 
  gather(key = "year", value = "disp", -race) %>% 
  separate(year, into = c("meas", "year", "int"), sep = "_") %>% 
  select(-meas) %>% 
  spread(key = int, value = disp) 
# plot by year
ggplot(acspopcws2_ref, aes(x=year, y=mi, fill=race)) + 
  geom_bar(position = "dodge", stat="identity")  +
  geom_errorbar(aes(ymin = hi, ymax = lo), width = .2, position = position_dodge(0.9)) +
  geom_hline(yintercept = 1, color = "black") +
  scale_y_continuous(name = "Disproportionality Index (90% Confidence Intervals)", 
                     trans = "log", # log transformation
                     breaks = c(0.33, 0.5, 0.67, 1, 1.5, 2, 3), 
                     labels = c("0.33", "0.5", "0.67", "1", "1.5", "2", "3")) +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(8,4)]) +
  labs(title = "Racial Disproportionality Index in Referrals",
       subtitle = "Based on population proportions from 2012-2016 American Community Survey",
       y = "", x = "", fill = "Race") +
  annotate("text", x = c(0.8, 1.2), y = c(1.07, .92), label = c("1.78", "0.46"), color = "white") +
  annotate("text", x = c(1.8, 2.2), y = c(1.07, .92), label = c("1.78", "0.46"), color = "white") +
  annotate("text", x = c(2.8, 3.2), y = c(1.07, .92), label = c("1.68", "0.53"), color = "white") +
  coord_flip()
ggsave("referral_rdi_race2.pdf", width=9, height=6, units="in") 


####################################################################################
# 3. Racial compositions: referrals 2017, active 2017, foster 2017 vs. population
####################################################################################
# by race4
bar_order <- c("acs16", "ref", "act17", "fos17") # define sequence of bars, used in scale_x_discrete
cwspopacs4 %>% 
  filter(source %in% c("acs16", "ref", "act17", "fos17")) %>% arrange(race) %>% 
  ggplot(aes(y = number, x = source)) +
  geom_bar(stat = "identity", aes(fill = race), position = "fill") +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,6,7,8)]) +
  labs(title = "Population Proportions and CPS Client Proportions by Race",
       subtitle = "New CPS cases from July 1, 2014 to June 30, 2017",
       y = "", x = "", fill = "Race") + 
  scale_x_discrete(limits = bar_order, 
                   labels=c("acs16" = "2016 Population", "ref" = "Referrals",
                            "act17" = "Active Cases", "fos17" = "Foster Care Cases")) +
  scale_y_continuous(labels = percent) +
  annotate("text", x = 1, y = c(.1, .31, .38, .87), label = c("27.1 (± 3.0)", "8.0 (±- 2.4)", "5.9 (± 2.1)", "59.0 (± 0.7)"), color = "white") +
  annotate("text", x = 2, y = c(.1, .6, .675, .87), label = c("54.6", "11.3", "2.9", "31.1"), color = "white") +
  annotate("text", x = 3, y = c(.1, .735, .79, .87), label = c("69.3", "8.4", "2.5", "19.8"), color = "white") +
  annotate("text", x = 4, y = c(.1, .62, .77, .87), label = c("44.0", "30.5", "2.5", "22.0"), color = "white") +
  annotate("text", x = c(1,2,3,4), y = 1.05, label = c("7084 (± 314)","1325", "323", "118"))
ggsave("ref_act_fos_race4.pdf", width=9, height=6, units="in") 

# by race2
cwspopacs2 %>% 
  filter(source %in% c("acs16", "ref", "act17", "fos17")) %>% arrange(race) %>% 
  ggplot(aes(y = number, x = source)) +
  geom_bar(stat = "identity", aes(fill = race), position = "fill") +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,8)]) +
  labs(title = "Population Proportions and CPS Client Proportions by Race",
       subtitle = "New CPS cases from July 1, 2014 to June 30, 2017",
       y = "", x = "", fill = "Race") + 
  scale_x_discrete(limits = bar_order, 
                   labels=c("acs16" = "2016 Population", "ref" = "Referrals",
                            "act17" = "Active Cases", "fos17" = "Foster Care")) +
  scale_y_continuous(labels = percent) +
  annotate("text", x = 1, y = c(.35, .87), label = c("41.0 (± 4.2)", "59.0 (± 0.7)"), color = "white") +
  annotate("text", x = 2, y = c(.35, .87), label = c("68.9", "31.1"), color = "white") +
  annotate("text", x = 3, y = c(.35, .87), label = c("80.2", "19.8"), color = "white") +
  annotate("text", x = 4, y = c(.35, .87), label = c("78.0", "22.0"), color = "white") +
  annotate("text", x = c(1,2,3,4), y = 1.05, label = c("7084 (± 314)","1325", "323", "118"))
ggsave("ref_act_fos_race2.pdf", width=9, height=6, units="in") 


######################################################################################
# 4. Racial disproportionality index for 2017 referrals, active cases, foster cases
######################################################################################
### a. generate intervals ###
# by race4
acspopcws4 <- acspopcws4 %>% 
  mutate(ref_2017_lo = refprop/(prop - pmoe),
         ref_2017_mi = refprop/prop,
         ref_2017_hi = refprop/(prop + pmoe),
         act_2017_lo = actprop/(prop - pmoe),
         act_2017_mi = actprop/prop,
         act_2017_hi = actprop/(prop + pmoe),
         fos_2017_lo = fosprop/(prop - pmoe),
         fos_2017_mi = fosprop/prop,
         fos_2017_hi = fosprop/(prop + pmoe))

# by race2
acspopcws2 <- acspopcws2 %>% 
  mutate(ref_2017_lo = refprop/(prop - pmoe),
         ref_2017_mi = refprop/prop,
         ref_2017_hi = refprop/(prop + pmoe),
         act_2017_lo = actprop/(prop - pmoe),
         act_2017_mi = actprop/prop,
         act_2017_hi = actprop/(prop + pmoe),
         fos_2017_lo = fosprop/(prop - pmoe),
         fos_2017_mi = fosprop/prop,
         fos_2017_hi = fosprop/(prop + pmoe))

### b. visualize ###
# by race4
# gather the data frame
acspopcws4_all <- acspopcws4 %>% 
  filter(race != "Total") %>% select(race, ref_2017_lo:fos_2017_hi) %>% 
  gather(key = "year", value = "disp", -race) %>% 
  separate(year, into = c("meas", "year", "int"), sep = "_") %>% 
  spread(key = int, value = disp) 
# and plot by year
bar_order <- c("fos", "act", "ref") # define sequence of bars, used in scale_x_discrete
ggplot(acspopcws4_all, aes(x=meas, y=mi, fill=race)) + 
  geom_bar(position = "dodge", stat="identity")  +
  geom_errorbar(aes(ymin = hi, ymax = lo), width = .2, position = position_dodge(0.9)) +
  geom_hline(yintercept = 1, color = "black") +
  scale_x_discrete(limits = bar_order,
                   labels=c("ref" = "Referrals", "act" = "Active",
                            "fos" = "Foster Care")) +
  scale_y_continuous(name = "Disproportionality Index (90% Confidence Intervals)", 
                     trans = "log", # log transformation
                     limits = c(0.2,5.5),
                     breaks = c(0.2, 0.25, 0.33, 0.5, 0.67, 1, 1.5, 2, 3, 4, 5), 
                     labels = c("0.20", "0.25", "0.33", "0.5", "0.67", "1", "1.5", "2", "3", "4", "5")) +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(8,7,6,4)]) +
  expand_limits(y = 4) +
  labs(title = "Racial Disproportionality Index for CPS Clients",
       subtitle = "New CPS cases from July 1, 2014 to June 30, 2017",
       y = "", x = "", fill = "Race") +
  annotate("text", x = c(0.7, 0.9, 1.1, 1.3), y = c(1.15, 1.15, .875, .875), label = c("1.66", "3.80", "0.43", "0.37"), color = "white") +
  annotate("text", x = c(1.7, 1.9, 2.1, 2.3), y = c(1.15, 1.15, .875, .875), label = c("2.56", "1.04", "0.42", "0.37"), color = "white") +
  annotate("text", x = c(2.7, 2.9, 3.1, 3.3), y = c(1.15, 1.15, .875, .875), label = c("2.02", "1.41", "0.50", "0.53"), color = "white") +
  coord_flip()
ggsave("ref_act_fos_rdi_race4.pdf", width=9, height=6, units="in") 

# by race2
# gather the data frame
acspopcws2_all <- acspopcws2 %>% 
  filter(race != "Total") %>% select(race, ref_2017_lo:fos_2017_hi) %>% 
  gather(key = "year", value = "disp", -race) %>% 
  separate(year, into = c("meas", "year", "int"), sep = "_") %>% 
  spread(key = int, value = disp) 
# and plot by year
ggplot(acspopcws2_all, aes(x=meas, y=mi, fill=race)) + 
  geom_bar(position = "dodge", stat="identity")  +
  geom_errorbar(aes(ymin = hi, ymax = lo), width = .2, position = position_dodge(0.9)) +
  geom_hline(yintercept = 1, color = "black") +
  scale_x_discrete(limits = bar_order,
                   labels=c("ref" = "Referrals", "act" = "Active",
                            "fos" = "Foster Care")) +
  scale_y_continuous(name = "Disproportionality Index (90% Confidence Intervals)", 
                     trans = "log", # log transformation
                     limits = c(0.3, 3.3),
                     breaks = c(0.33, 0.5, 0.67, 1, 1.5, 2, 3), 
                     labels = c("0.33", "0.5", "0.67", "1", "1.5", "2", "3")) +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(8,4)]) +
  labs(title = "Racial Disproportionality Index for CPS Clients",
       subtitle = "New CPS cases from July 1, 2014 to June 30, 2017",
       y = "", x = "", fill = "Race") +
  annotate("text", x = c(0.8, 1.2), y = c(1.125, .88), label = c("1.90", "0.37"), color = "white") +
  annotate("text", x = c(1.8, 2.2), y = c(1.125, .88), label = c("1.95", "0.34"), color = "white") +
  annotate("text", x = c(2.8, 3.2), y = c(1.125, .88), label = c("1.68", "0.53"), color = "white") +
  coord_flip()
ggsave("ref_act_fos_rdi_race2.pdf", width=9, height=6, units="in") 


### save work
save.image("cville_acs_analysis.Rdata")
# load("cville_acs_analysis.Rdata")

# Number of children in Cville by race
acspopcws4 %>%  
  ggplot(aes(x = estimate, y = reorder(race, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Estimated number of children in Charlottesville by race",
       subtitle = "2012-2016 American Community Survey",
       x = "Estimate",
       y = "Race")


