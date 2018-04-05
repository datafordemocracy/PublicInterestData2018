######################################################################################
# DS 5559: Public Interest Data Lab 
# 1. Acquire 2012-2016 5-year populations estimates for Cville census tracts
# 2. Load referral data: generate counts by tract, add to pop
# 3. Create maps
# Michele Claibourn (mclaibourn@virginia.edu)
# Updated: March 31, 2018 
######################################################################################

rm(list=ls())
setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/acsdata/")
library(tidyverse)
library(readr)
library(RColorBrewer)
library(tidycensus)
options(tigris_use_cache = TRUE)


######################################################################################
# 1. Acquire 2012-2016 5-year populations estimates for Cville census tracts
######################################################################################
# request a key: https://api.census.gov/data/key_signup.html
# add it inside the quotes in the following command, if you use install = TRUE you shouldn't have to run it again

# census_api_key("", install = TRUE)
data(fips_codes) # built in dataset for looking up state and county
fips_codes %>% filter(state == "VA") # find county code for CVille

### a. get data: under 18 population ###
cville_acs_pop <- get_acs(geography = "tract", table = "B09001", 
                          year = 2016, state = "VA", county = "540", survey = "acs5",
                          geometry = TRUE, cache_table = TRUE)
tract_totpop <- cville_acs_pop %>% filter(variable == "B09001_001")

### b. get data: poverty estimates ###
cville_acs_pov <- get_acs(geography = "tract", table = "B17001",
                          year = 2016, state = "VA", county = "540", survey = "acs5",
                          geometry = TRUE, cache_table = TRUE)
tract_pov <- cville_acs_pov %>% filter(variable %in% c("B17001_001", "B17001_002"))

# fix formatting for calculations
tract_pov2 <- tract_pov %>%  
  gather(estimate:moe, key = "type", value = "count") %>%
  unite(temp, variable, type) %>%
  spread(temp, count)

# generate pov proportions
tract_pov2 <- tract_pov2 %>% 
  mutate(povprop = B17001_002_estimate/B17001_001_estimate, 
         povmoe = moe_prop(B17001_002_estimate, B17001_001_estimate, B17001_002_moe, B17001_001_moe),
         povrate = povprop*100,
         porvatemoe = povmoe*100)


######################################################################################
# 2. Load referral data: generate counts by tract, add to pop
######################################################################################
setwd("/Volumes/NO NAME") 

### a. referral data ###
load("referral.RData") # created in explore_referraldata.R
tract_ref <- referral %>% filter(!(is.na(tract))) %>% count(tract)

### b. add to pop, generate ratios ###
tracts <- cbind(tract_totpop, tract_ref)
tracts <- tracts %>% 
  mutate(refrate = (n/estimate)*100)

### c. add pov rates/moe to tracts
tracts <- cbind(tracts, tract_pov2[,8:11])
rm(referral)


######################################################################################
# 3. Create maps
######################################################################################
setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/acsdata/")
# needed development versions
# devtools::install_github("tidyverse/rlang")
# devtools::install_github("tidyverse/ggplot2")
# see https://walkerke.github.io/tidycensus/articles/spatial-data.html for example

# map theme: tweaked from https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

## Get center coordinates for labeling
library(sf)
tracts <- tracts %>%
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]),
         tractid = as.numeric(tract),
         tractid = paste0("(",tractid,")"))

# Number of children by tract
tracts %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = "white") +
  theme_map() +
  scale_fill_gradient(name = "Child Pop", low = "#9ecae1", high = "#08306b") +
  labs(x = NULL, 
       y = NULL, 
       title = "Charlottesville's Child Population by Census Tract", 
       subtitle = "Population Estimates from 2012-2016 American Community Survey") +
  geom_text(aes(x=lon, y=lat, label=tractid), size = 3, color = "white",
            position = position_nudge(y = -0.001)) +
  geom_text(aes(x=lon, y=lat, label=estimate), size = 3, color = "white",
            position = position_nudge(y = 0.001))
ggsave("childpopmap.pdf")

# Referral rate by tract
tracts %>%
  ggplot(aes(fill = refrate)) + 
  geom_sf(color = "white") +
  theme_map() +
  scale_fill_gradient(name = "Referral Rate", low = "#9ecae1", high = "#08306b") +
  labs(x = NULL, 
       y = NULL, 
       title = "Charlottesville's Child Population by Census Tract", 
       subtitle = "Population Estimates from 2012-2016 American Community Survey") +
  geom_text(aes(x=lon, y=lat, label=tractid), size = 3, color = "white",
            position = position_nudge(y = -0.001)) +
  geom_text(aes(x=lon, y=lat, label=round(refrate,1)), size = 3, color = "white",
            position = position_nudge(y = 0.001))
ggsave("refratemap.pdf")

# Poverty rate by tract
tracts %>%
  ggplot(aes(fill = povrate)) + 
  geom_sf(color = "white") +
  theme_map() +
  scale_fill_gradient(name = "Poverty Rate", low = "#9ecae1", high = "#08306b") +
  labs(x = NULL, 
       y = NULL, 
       title = "Charlottesville's Child Population by Census Tract", 
       subtitle = "Population Estimates from 2012-2016 American Community Survey") +
  geom_text(aes(x=lon, y=lat, label=tractid), size = 3, color = "white",
            position = position_nudge(y = -0.001)) +
  geom_text(aes(x=lon, y=lat, label=round(povrate,1)), size = 3, color = "white",
            position = position_nudge(y = 0.001))
ggsave("povratmap.pdf")

save.image("cville_acs_tracts.Rdata")
# load("cville_acs_tracts.Rdata")
# see also https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/