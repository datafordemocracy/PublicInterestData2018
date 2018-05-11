######################################################################################
# DS 5559: Public Interest Data Lab 
# Data Cleaning Script: process and prepare foster care and placement history data
# 1. Read in, clean placement data and create variables for analysis
# 2. Read in and clean foster care data and create variables for analysis
# 3. Join placement data to foster care data and create addditional variables
# 4. Subset data: for cases entering on or afer July 1, 2014, for race
# Created by C. McClintock
# Contact: Michele Claibourn (mclaibourn@virginia.edu)
# Updated: May 10, 2018 (mpc)
######################################################################################

rm(list=ls()) # clear workspace
library(tidyverse)
library(readxl)
library(lubridate)

setwd("/Volumes/NO NAME") # point to encrypted file
# setwd("/Volumes/encrypted_data") # encrypted file on different computer


######################################################################################
# 1. Read in, clean placement data and create variables for analysis
######################################################################################
# read the data into R
ph <- read_excel("cville_dss_data_2017.xlsx", sheet = 4)

# clean up data: rename the variables, change characters into factors
names(ph) <- c("case_id", "client_id", "place_entry_date", "place_exit_date", "place_exit_why",
           "place_care_type", "place_resource_type", "place_resource_cat", "awol_begin", "awol_end")

ph <- ph %>%
   mutate(
     place_exit_why = factor(place_exit_why),
     place_care_type = factor(place_care_type),
     place_resource_type = factor(place_resource_type),
     place_resource_cat = factor(place_resource_cat),
     case_id = factor(case_id),
     client_id = factor(client_id)
    )

# check number of distinct values 
n_distinct(ph$case_id) ## 133
n_distinct(ph$client_id) ## 223

# create new ariables
# time spent in each foster care placement
ph <- ph %>% 
  mutate(time_weeks = as.numeric(difftime(place_exit_date, place_entry_date, unit = "weeks")))
         
# number of clients in each case
clients_per_case <- ph %>% 
  group_by(case_id) %>% 
  summarise(clients_by_case = n_distinct(client_id))
# add the original dataset using left_join
ph <- left_join(ph, clients_per_case, by="case_id")

# number of placements for each child
num_place <- ph %>% 
  group_by(case_id, client_id) %>% 
  summarise(num_placements_client = n())
# add the original dataset using left_join
ph <- left_join(ph, num_place, by=c("case_id", "client_id"))

# number of placements by category
num_place_bycat <- ph %>% 
  group_by(case_id, client_id, place_resource_cat) %>% 
  summarise(num_placements_cat_client = n())
# add the original dataset using left_join
ph <- left_join(ph, num_place_bycat, by=c("case_id", "client_id", "place_resource_cat"))

# total time in out of home care, does not include time in ongoing placement
#   might not give proper values for clients in care at present and have more than 1 placements
#   because it won't add the present time at placement, so ongoing cases are excluded
tic_client <- ph %>% 
  group_by(case_id, client_id)
time_total <- summarise(tic_client, time_in_care = sum(time_weeks))


######################################################################################
# 2. Read in and clean foster care data and create variables for analysis
######################################################################################
# read the data into R
fc <- read_excel("cville_dss_data_2017.xlsx", sheet = 3)

# rename the variables 
names(fc) <- c("locality", "case_id", "client_id", "ssn", "sys_entry", "sys_exit", 
                              "age_atcustody", "custody_start", "custody_end", "place_type", "dis_date", 
                              "dis_why", "gender", "race", "race_amind", "race_asian", "race_black", 
                              "race_hawpac", "race_white", "race_utd", "race_dec", "race_unknown",
                              "ftf_count", "ftf_homecount")


# create a new data frame with foster care clients and basic data on placement history
placement_basic <- left_join(num_place, time_total, 
                             by=c("case_id" = "case_id", "client_id"="client_id"))
fc <- merge(placement_basic, fc)

# clean up objects
rm(clients_per_case, num_place, num_place_bycat, tic_client, time_total, placement_basic)

# binary indicator for whether the child is still in the system
fc$sys_status <- ifelse(is.na(fc$sys_exit), 0, 1) # 0 if still in the system 
fc <- select(fc, case_id, client_id, time_in_care, num_placements_client, 
                         sys_status, everything())

# dummy retrieval date to generate duration/time in care for clients still in care
dummy_date_retrieval <- "2017-08-30 23:27:05 UTC"
fc$duration <- ifelse((fc$sys_status==0), 
                      difftime(dummy_date_retrieval, fc$sys_entry, units="weeks"), 
                      difftime(fc$sys_exit, fc$sys_entry, units="weeks"))

# generate multiple categorizations of race
fc <- fc %>% 
  mutate(
    # race2: white children, children of color (unknown if present)
    race2 = race, 
    race2 = recode(race, "Asian" = "Children of Color", "Unknown" = "Children of Color",
                   "Multi-Race" = "Children of Color", "Black" = "Children of Color"),
    # race3: white (only) children, black and multiracial black children 
    race3 = race, 
    race3 = recode(race,  "Black" = "Black", 
                   "Multi-Race" = "Black", 
                   "Asian" = "Other", "Unknown" = "Other"), 
    # race4: white children, black children, multiracial children, other children
    race4 = race, 
    race4 = recode(race, "Asian" = "Other", "Unknown" = "Other", "Multi-Race" = "Multiracial"))

fc$race2 <- as.factor(fc$race2)
fc$race3 <- as.factor(fc$race3)
fc$race4 <- as.factor(fc$race4)

# reorder levels
fc <- mutate(fc, race3=fct_relevel(race3, "White", "Black", "Other"))
fc <- mutate(fc, race4=fct_relevel(race4, "White", "Black", "Multiracial", "Other"))

# change characters into factors
fc <- fc %>%
  mutate(
    place_type = factor(place_type),
    dis_why = factor(dis_why),
    gender = factor(gender),
    race = factor(race),
    entry_year = year(sys_entry),
    exit_year = year(sys_exit))

# race
fc$white_alone <- ifelse((fc$race3=="White"), 1, 0)

# gender
fc$female <- ifelse((fc$gender=="Female"), 1, 0)
fc$male <- ifelse((fc$gender=="Male"), 1, 0)

# for system exits
fc$dis_why <- as.factor(fc$dis_why)
fc$reunification <- ifelse((fc$dis_why=="Reunification"), 1, 0)
fc$emancipation <- ifelse((fc$dis_why=="Emancipation"), 1, 0)
fc$transfer_agency <- ifelse((fc$dis_why=="Transfer Other Agency"), 1, 0)
fc$transfer_relative <- ifelse((fc$dis_why=="Transfer Other Relative"), 1, 0)
fc$adoption <- ifelse((fc$dis_why=="Adoption"), 1, 0)

# check number of distinct values 
n_distinct(fc$ssn)
n_distinct(fc$client_id)

# clean up objects
rm(dummy_date_retrieval)


######################################################################################
# 3. Join placement data to foster care data and create addditional variables
######################################################################################
# combining the foster care and placement history data sets completely
fcph <- left_join(ph, fc, by=c("case_id", "client_id"))

# place4: Foster, Kinship, Other, Residential
fcph <- fcph %>% 
  mutate(place4 = as.character(place_resource_cat),
         place4 = ifelse(place_resource_type == "FFC/Kinship/Relative" | place_resource_type == "Kinship/Relative Non-Paid", "Kinship Care", place4))

# binary indicators 
fcph$foster_fam <- ifelse(fcph$place4 == "Foster Family", 1, 0)
fcph$kinship <- ifelse(fcph$place4 == "Kinship Care", 1, 0)
fcph$residential <- ifelse(fcph$place4=="Residential (CRF)", 1, 0)

# get initial foster care placement (from placement history) and join to foster care data
initialplacement <- fcph %>%
  arrange(client_id, place_entry_date) %>% # make sure first placement is listed first
  group_by(client_id) %>%
  slice(1) %>%
  select(case_id, client_id, init_entry = place_entry_date, init_caretype = place_care_type, 
         init_resourcetype = place_resource_type, init_resourcecat = place_resource_cat, 
         init_place_time = time_weeks) 
# rename variables when selecting, as similar names exist in foster care data

# Add initialplacement variables to foster care data
fc <- left_join(fc, initialplacement, by = c("case_id","client_id")) # three place_entry_date values before 2014, why?

# place4: Foster, Kinship, Other, Residential
fc <- fc %>% 
  mutate(iplace4 = as.character(init_resourcecat),
         iplace4 = ifelse(init_resourcetype == "FFC/Kinship/Relative", "Kinship Care", iplace4))

# binary indicators 
fc$foster_fam <- ifelse(fc$iplace4 == "Foster Family", 1, 0)
fc$kinship <- ifelse(fc$iplace4 == "Kinship Care", 1, 0)
fc$residential <- ifelse(fc$iplace4 == "Residential (CRF)", 1, 0)

# clean up
rm(initialplacement)


######################################################################################
# 4. Subset data: for cases entering on or afer July 1, 2014, for race
######################################################################################
# for cases entering on or afer July 1, 2014
# in order to match with referral and exclude long ongoing cases and remove bias
fc <- subset(fc, sys_entry>"2014-07-01")
fcph <- subset(fcph, sys_entry>"2014-07-01")

# distribution of race
fc %>% count(race4)
# only three children, cannot draw conclusions so we need to drop them from analysis
# all three children who are not black or white are still in the system
fc4 <- fc # save for figures
fc <- subset(fc, !race3=="Other")
fcph4 <- fcph # save for figures
fcph <- subset(fcph, !race3=="Other")
fc$race3 <- as.factor(fc$race3) # remove unused factor
fcph$race3 <- as.factor(fcph$race3)


######################################################################################
# list of the important objects
######################################################################################

# clean_placement # a cleaned version of the placement history data
# clean_foster_care # a cleaned version of the foster care data
# fc # merge of clean_placement and clean_foster_care, 115 rows
# fcph # full join of foster care and placement history sheets, 262 rows 


# saving an image to use for analysis
save.image("outofhome.Rdata")
