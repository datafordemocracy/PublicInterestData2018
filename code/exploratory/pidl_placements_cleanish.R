
## DS 5559: Public Interest Data Lab
## C. McClintock, B. Aluri
## Data Cleaning Script: Placement History

# ...........................................................................................................................

# set up: wd, retrieve encrypted data
rm(list=ls())
getwd()
# /Users/charmed33
setwd("/Volumes/NO NAME")
# "/Volumes/NO NAME"

# set up: libraries
library(dplyr)
library(forcats)
library(tidyverse)
library(readxl)
library(tidyr)
library(lubridate)

# ...........................................................................................................................

# STARTING OUT

# read the data into R
placement <- read_excel("cville_dss_data_2017.xlsx", sheet = 4)
foster_care <- read_excel("cville_dss_data_2017.xlsx", sheet = 3)

# rename the variables 
names(placement)
names(placement) <- c("case_id", "client_id", "entry_date", "exit_date", "exit_why",
           "care_type", "resource_type", "resource_cat", "awol_begin", "awol_end")

names(foster_care)
names(foster_care) <- c("locality", "case_id", "client_id", "ssn", "sys_entry", "sys_exit", 
                        "age_atcustody", "custody_start", "custody_end", "place_type", "dis_date", 
                        "dis_why", "gender", "race", "race_amind", "race_asian", "race_black", 
                        "race_hawpac", "race_white", "race_utd", "race_dec", "race_unknown",
                        "ftf_count", "ftf_homecount")

# change characters into factors and including a end date based on when the data
# was retrieved the  for children still in the system
# and added a new variable for time spent in each foster care placement in weeks and rounded it off
# to make it whole weeks.
placement <- placement %>%
   mutate(
     exit_why = factor(exit_why),
     care_type = factor(care_type),
     resource_type = factor(resource_type),
     resource_cat = factor(resource_cat),
     case_id = factor(case_id),
     client_id = factor(client_id),
     exit_date = if_else(is.na(exit_date), ymd_hms("2017-12-30 23:27:05", tz = "UTC"),exit_date),
     time_weeks = round(as.numeric(difftime(placement$exit_date, placement$entry_date), units="weeks"))
    )

# reorder columns, put time_spent near entry and exit date
placement <-placement[c("case_id", "client_id","entry_date", "exit_date", "time_weeks", "exit_why", 
                        "care_type", "resource_type", "resource_cat", "awol_begin", "awol_end")]

# look at a data summary
summary(placement)
str(placement) ## 17 levels for exit_why, 24 levels for care_type, 12 levels for resource_type

# group together similar case ids 
arrange(placement, case_id)

# check number of distinct values 
n_distinct(placement$case_id) ## 133
n_distinct(placement$client_id) ## 223

# number of incomplete cases
sum(!complete.cases(subset(placement,select= -c(awol_begin, awol_end)))) ## 102 cases excluding awol columns
# 99 missing exit dates(might be still in placements mentioned) and 3 missing values for care_type.

summary(placement)


# ...........................................................................................................................

# BUILDING USEFUL OBJECTS

# number of clients in each case
clients_per_case <- placement %>% 
  group_by(case_id) %>% 
  summarise(clients_by_case = n_distinct(client_id))

# number of placements for each child
num_place <- placement %>% 
  group_by(case_id, client_id) %>% 
  summarise(num_placements_client = n())


# number of placements by resource category
num_place_bycat <- placement %>% 
  group_by(case_id, client_id, resource_cat) %>% 
  summarise(num_placements_cat_client = n())

# total time in out of home care, does include time in ongoing placement
time_total <- placement %>% 
  group_by(case_id, client_id) %>% 
  summarise(time_in_care = sum(time_weeks))
  

# combining all the summarised numbers to the origina dataset using left_join
placement <- left_join(placement,clients_per_case, by=c("case_id" = "case_id"))
placement <- left_join(placement,num_place, by=c("case_id" = "case_id", "client_id"="client_id"))
placement <- left_join(placement,num_place_bycat, by=c("case_id" = "case_id", "client_id"="client_id", 
                                                       "resource_cat"="resource_cat"))

# ...........................................................................................................................

# FOSTER CARE DATA WITH BASIC PLACEMENT INFORMATION, 223 OBSERVATIONS

# create a new data frame with foster care clients and basic data on placement history

placement_basic <- left_join(num_place,time_total, by=c("case_id" = "case_id", "client_id"="client_id"))
care_placement <- merge(placement_basic, foster_care)

# binary indicator for whether the child is still in the system
care_placement$sys_status <- ifelse(is.na(care_placement$sys_exit), 1, 0)
care_placement <- select(care_placement, case_id, client_id, time_in_care, num_placements_client, 
                         sys_status, everything())

# explore this data a bit

# age of children in foster care
summary(care_placement$age_atcustody)

# number of children of each race in the placement history & foster care data
num_children_byrace <- care_placement %>% 
  group_by(race) %>% 
  summarize(byrace = n())
# 5 asian children
# 113 black children
# 51 multi-racial children
# 52 white children
# 2 children with race unknown

# time spent in care by race, excluding cases where the child is still in care
# might not be enough data to draw comparisons, just exploratory
time_byrace <- as.data.frame(care_placement %>% group_by(race) %>% 
                               summarise(mean(time_in_care, na.rm = TRUE), 
                                         median(time_in_care, na.rm = TRUE))) 
names(time_byrace) <-  c("race", "mean(weeks)","median(weeks)")
## sample size is too small for asian children, unknown

# number of foster care placements by race
## might not be an ideal way of looking since large number of placements for 
## few clients can bump up the mean, but median might be a good measure.
placements_byrace <- as.data.frame(care_placement %>% group_by(race) %>% 
                            summarise(mean(num_placements_client), 
                                      median(num_placements_client)))
names(placements_byrace) <- c("race", "mean(placements)","median(placements")

# look at some tables by race, for categorical variables 
with(care_placement, table(race, dis_why))
with(care_placement, table(race, place_type))

# ..........................................................................................................................

# FOSTER CARE AND PLACEMENT HISTORY TOGETHER, 971 OBSERVATIONS

# combining the foster care and placement history data sets completely
fcare_placehist <- left_join(placement, care_placement, by=c("case_id" = "case_id", 
                                                             "client_id"="client_id", 
                                                             "num_placements_client" = "num_placements_client"))

#getting rid of irrelevant columns
fcare_placehist$locality <- NULL
# all localities are charlottesville
fcare_placehist$ssn <- NULL
# one less ssn than client ids, why?

# could remove custody start date 
# fcare_placehist$custody_start <- NULL
# arb <- fcare_placehist$sys_entry - fcare_placehist$sys_custody_start, all 0

# binary indicator for whether the child is still in the system
fcare_placehist$sys_status <- ifelse(is.na(fcare_placehist$sys_exit), 1, 0)

# create a dummy data retrieval date
dummy_data_retrieval <- "2017-12-30 23:27:05 UTC"
# create a duration value to include ongoing cases, and end them when the data was retrieved
fcare_placehist$duration <- ifelse(is.na(fcare_placehist$sys_exit), (as.numeric(difftime(dummy_data_retrieval,
                fcare_placehist$sys_entry), units="weeks")), as.numeric(difftime(fcare_placehist$sys_exit, 
                fcare_placehist$sys_entry), units="weeks", na.rm = TRUE))

# moving the variables for clarity
fcare_placehist <- fcare_placehist[c("case_id", "client_id", "entry_date","exit_date", "sys_entry",
                                    "sys_exit", "custody_start", "custody_end","dis_date","sys_status",  "duration",
                                    "time_weeks", "time_in_care", "exit_why", "care_type", "resource_type", "resource_cat",
                                    "place_type", "clients_by_case","num_placements_client", "num_placements_cat_client",    
                                    "dis_why", "age_atcustody", "gender", "race", "race_amind", "race_asian", 
                                    "race_black", "race_hawpac", "race_white","race_utd", "race_dec", "race_unknown",
                                    "ftf_count", "ftf_homecount", "awol_begin", "awol_end")]

# renaming the variables for clarity          
names(fcare_placehist) <- c("case_id", "client_id", "place_entry_date","place_exit_date", "sys_entry",
                            "sys_exit", "sys_custody_start","sys_custody_end","sys_dis_date","sys_status",  
                            "duration", "time_placement", "time_atexit", "place_exit_why", "place_care_type", 
                            "place_resource_type", "place_resource_cat", "sys_place_type", "clients_by_case",
                            "num_placements_client", "num_placements_cat_client", "sys_dis_why", "age_atcustody", 
                            "gender", "race", "race_amind", "race_asian", "race_black", "race_hawpac", "race_white",
                            "race_utd", "race_dec", "race_unknown", "ftf_count", "ftf_homecount", 
                            "awol_begin", "awol_end")

# ...........................................................................................................................

# DATA VISUALIZATION

# dismissal from the system by race
p1 <- ggplot(data =care_placement, aes(x = fct_infreq(dis_why)))
p1 + geom_bar(aes(fill = race)) + coord_flip()
p1 + geom_bar(aes(fill = race), position = "fill") + coord_flip() 

# placement category by race 
p2 <- ggplot(data = fcare_placehist, aes(x = fct_infreq(place_resource_cat)))
p2 + geom_bar(aes(fill = race)) + coord_flip()
p2 + geom_bar(aes(fill = race), position = "fill") + coord_flip() 

# ...........................................................................................................................


# list of the important objects

# placement # a cleaned version of the placement history data
# foster_care # a cleaned version of the placement history data
# care_placement # merge of foster_care and placement_basic, 223 rows
# fcare_placehist # merge of foster care and placement history sheets, 971 rows 

# saving an image
save.image("placement_history_clean.Rdata")




