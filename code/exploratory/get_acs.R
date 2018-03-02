#####################################
# DS 5559: Public Interest Data Lab 
# Get 5-year ACS estimates for Cville
# Michele Claibourn
# Updated: February 22, 2018
#####################################

library(tidyverse)

##################################
# Can I use the variance replicates to get population estimates? 
##################################
setwd("~/Box Sync/mpc/datafordemocracy/pidl/codeR")
# Checking the variance replicates
rep <- read_csv("B01001.csv")  # from https://www2.census.gov/programs-surveys/acs/replicate_estimates/2016/data/5-year/050/
cvrep <- rep %>% filter(NAME =="Charlottesville city, Virginia")
# Only for the top level sex by age table, not by subtables by race
rm(rep, cvrep)


##################################
# Explore the acs package 
##################################
setwd("~/Box Sync/mpc/datafordemocracy/pidl/PIDL2018/code/acsdata/")
# install.packages("acs")
library(acs)
# request a key: https://api.census.gov/data/key_signup.html
# add it inside the quotes in the following command, you shouldn't have to run it again
# api.key.install(key="")

# specify a geography
geo.lookup(state = "VA", county = "Char")
cville <- geo.make(state=51, county = 540)
cville # just checking

# find data (2016 doesn't appear to be available yet)
acs.lookup(endyear = 2016, table.number="B01001")
acs.lookup(endyear = 2015, table.number="B01001")

# get data
# acs.fetch(endyear = 2015, geo = cville, table.name = "B01001", col.names = "pretty")
rm(cville)


##################################
# Explore the tidycensus package
##################################
# install.packages("tidycensus")
library(tidycensus)
# request a key: https://api.census.gov/data/key_signup.html
# add it inside the quotes in the following command, if you use install = TRUE you shouldn't have to run it again
# census_api_key("", install = TRUE)

data(fips_codes) # built in dataset for looking up state and county
fips_codes %>% filter(state == "VA")

# get data
cville_acs_all <- get_acs(geography = "county", table = "B01001", 
                          year = 2016, state = "VA", county = "540", survey = "acs5")

# plot number of children
child <- c("B01001_003", "B01001_004", "B01001_005", "B01001_006", "B01001_027", "B01001_028", "B01001_029", "B01001_030")
cville_acs_all %>% filter(variable %in% child) %>% 
  ggplot(aes(x = estimate, y = variable)) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3)

# calculate number of children in cville with moe
cville_acs_all %>% filter(variable %in% child) %>% 
  summarize(total = sum(estimate), moe = moe_sum(moe, estimate))


# save work so far
save.image("cville_acs.Rdata")
load("cville_acs.Rdata")
