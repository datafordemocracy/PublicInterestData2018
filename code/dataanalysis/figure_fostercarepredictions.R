######################################################################################
# DS 5559: Public Interest Data Lab 
# Visualize effects of race on foster care outcomes
# Based on racial disparity models in pidl_analyze.R
# 1. Generate coefficient plot from foster care models
# Michele Claibourn (mclaibourn@virginia.edu)
# Updated: May 10, 2018 
######################################################################################

rm(list = ls())
library(tidyverse)
library(RColorBrewer)
library(dotwhisker)

setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/Group 2 Code/") # save predictions here
load("predictions.Rdata")


######################################################################################
# 1. Generate coefficient plot from foster care models
######################################################################################
# Reduce to coefficients on race
m_ff <- coef_ff %>% 
  filter(term %in% c("race_black")) %>% 
  mutate(sample = "Initial Placement",
         model = "Foster Family")
m_kc <- coef_kc %>% 
  filter(term %in% c("race_black")) %>% 
  mutate(sample = "Initial Placement",
         model = "Kinship Care")
m_rc <- coef_rc %>% 
  filter(term %in% c("race_black")) %>% 
  mutate(sample = "Initial Placement",
         model = "Residential (CRF)")
m_aff <- coef_aff %>% 
  filter(term %in% c("race_black")) %>% 
  mutate(sample = "All Placements",
         model = "Foster Family")
m_akc <- coef_akc %>% 
  filter(term %in% c("race_black")) %>% 
  mutate(sample = "All Placements",
         model = "Kinship Care")
m_arc <- coef_arc %>% 
  filter(term %in% c("race_black")) %>% 
  mutate(sample = "All Placements",
         model = "Residential (CRF)")

# Combine
place_models1 <- rbind(m_ff, m_kc, m_rc, m_aff, m_akc, m_arc)
place_models1 <- place_models1 %>% 
  mutate(sample = factor(sample, levels = c("Initial Placement", "All Placements")), # get right order of facets
         model = factor(model, levels = c("Foster Family", "Kinship Care", "Residential (CRF)")),
         term = recode(term, "race_black" = "Black"))

# set colors  
col0 <- brewer.pal(9, "Blues")[2]
col1 <- brewer.pal(9, "Blues")[4]
coll <- brewer.pal(9, "Blues")[6]
col2 <- brewer.pal(9, "Blues")[8]

# Graph (with model as character, forced alphabetical with numeric name)
dwplot(place_models1, conf.level = 0.90, dot_args = list(aes(colour = model), size = 0.75)) + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  facet_wrap(~ sample, scales = "free_y", drop = TRUE, ncol = 1) +
  scale_color_manual(values = c(col2, coll, col1)) +
  labs(x = "Coefficient Estimate", y = "",
       title = "Effect of Race on Foster Care Placements",
       subtitle = "Coefficient for Black Children Relative to White Children",
       color = "Outcome") +
  theme(legend.position="bottom")
ggsave("fostercoefplace.pdf", width=9, height=6, units="in")  


# Reduce to coefficients on race
m_nump <- coef_nump %>% 
  filter(term %in% c("race_black")) %>% 
  mutate(model = "Number Placements") %>% 
  select(term, estimate, std.error, statistic, p.value, model)
m_time <- coef_time %>% 
  filter(term %in% c("race_black")) %>% 
  mutate(model = "Time in Placement") %>% 
  select(term, estimate, std.error, statistic, p.value, model)
m_dur <- coef_dur %>% 
  filter(term %in% c("race_black")) %>% 
  mutate(model = "Time in Foster Care") %>% 
  select(term, estimate, std.error, statistic, p.value, model)

# Combine
foster_models1 <- rbind(m_nump, m_time, m_dur)
foster_models1 <- foster_models1 %>% 
  mutate(model = factor(model, levels = c("Number Placements", "Time in Placement", "Time in Foster Care")), # get right order of facets
         term = recode(term, "race_black" = "Black"))

# Graph (with model as factor)
dwplot(foster_models1, conf.level = 0.90, dot_args = list(colour = coll), size = 0.75) + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  facet_wrap(~ model, scales = "free_y", drop = TRUE, ncol = 1) +
  labs(x = "Coefficient Estimate", y = "",
       title = "Effect of Race on Foster Care Outcomes",
       subtitle = "Coefficient for Black Children Relative to White Children") +
  theme(legend.position = "none")
ggsave("fostercoefout.pdf", width=9, height=6, units="in")  


######################################################################################
save.image("fostercare_prediction.RData")
