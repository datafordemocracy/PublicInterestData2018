######################################################################################
# DS 5559: Public Interest Data Lab 
# Generate baseline figures for report: referral data
# 1. Racial composition and reporter relation 
# 2. Racial disparity in accepted cases among referred
# 3. Racial disparity in assignment to investigation vs. assessment among accepted
# 4. Racial disparity in substantiation of investigated cases
# 5. Racial disparity in time to contact (or number of contacts from active)
# 6. Racial disparity in number of contacts from active cases or duration of services
# Michele Claibourn (mclaibourn@virginia.edu)
# Created by: Hannah Sullivan, Michael Woon, Melissa Wu, Naifei Pan, James Mekavibul,
#   and Michele Claibourn
# Updated: April 2, 2018 
######################################################################################
# NOTES: complete 5,6?

rm(list=ls()) # clear workspace
library(tidyverse)
library(RColorBrewer)
library(scales)

setwd("/Volumes/NO NAME") # point to encrypted file
load("postreferral.RData")
setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/Group 1 Code/") # save figures here

######################################################################################
# 1. Racial composition and reporter relation 
######################################################################################
# update for revised relation categories, ordering, additional annotation
# prior vis code: qplot(referral1$general_relation,fill=race,xlab='First Reporter Relation', ylab='Frequency', main='First Reporter Relation & Race')+ coord_flip()+scale_fill_manual(values = brewer.pal(9,"Blues")[c(9,7,5,3)])
with(referral, prop.table(table(relation6, race4), 1)) # for annotation

referral %>% filter(!(is.na(relation6))) %>% 
  ggplot(aes(x = fct_rev(fct_infreq(relation6)))) + 
  geom_bar(aes(fill = fct_rev(race4))) +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,6,7,8)]) +
  labs(title = "Reporter Relation to Referred Children",
       subtitle = "With Percent of Children by Race",
       y = "Frequency", x = "", fill = "Race") +
  annotate("text", x = 1, y = c(75, 161, 190, 230), label = c("52", "18", "1", "29"), color = "white") +
  annotate("text", x = 2, y = c(75, 185, 230, 280), label = c("48", "18", "3", "31"), color = "white") +
  annotate("text", x = 3, y = c(75, 255, 295, 340), label = c("59", "19", "1", "21"), color = "white") +
  annotate("text", x = 4, y = c(75, 255, 295, 350), label = c("56", "15", "1", "28"), color = "white") +
  annotate("text", x = 5, y = c(75, 325, 375, 435), label = c("59", "16", "2", "23"), color = "white") +
  annotate("text", x = 6, y = c(75, 525, 590, 700), label = c("59", "12", "2", "27"), color = "white") +
  coord_flip()
ggsave("reporterRelation.pdf", width=9, height=6, units="in") 


######################################################################################
# 2. Racial disparity in accepted cases among referred
######################################################################################
# update to proportional stacked bar chart (match section 3)
# prior vis code: qplot(referral_final$accept,fill=race,xlab='Accepted (Yes/No)', ylab='Frequency', main='Number of Children Accepted by Race')+ coord_flip()+scale_fill_manual(values = brewer.pal(9,"Blues")[c(9,7,5,3)])
with(referral, prop.table(table(race4, accept), 1))
with(referral, prop.table(table(race4, accept), 2)) # for annotation

ggplot(referral, aes(x = accept)) + 
  geom_bar(aes(fill = fct_rev(race4)), position = "fill") +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,6,7,8)]) +
  labs(title = "Children Accepted for Investigation/Assessment",
       subtitle = "Among 2,706 children referred to CPS",
       y = "", x = "", fill = "Race") +
  scale_x_discrete(labels=c("Y" = "Accepted/Screened In", "N" = "Screened Out")) +  
  scale_y_continuous(labels = percent) +
  annotate("text", x = c(1,2), y = 1.05, label = c("1,378","1,328")) +
  annotate("text", x = 1, y = c(.1, .63, .72, .85), label = c("56.3", "14.7", "2.0", "26.9"), color = "white") +
  annotate("text", x = 2, y = c(.1, .63, .74, .85), label = c("56.9", "15.9", "1.7", "25.5"), color = "white")
ggsave("figures/accepted_race4.pdf", width=9, height=6, units="in")  


######################################################################################
# 3. Racial disparity in assignment to investigation vs. assessment among accepted
######################################################################################
# update, combine out of family and in home investigations, proportional stacked bar
# prior vis code to update: can't locate 
with(referral, prop.table(table(race4, invest2), 1))
with(referral, prop.table(table(race4, invest2), 2)) # for annotation

referral %>% filter(!(is.na(invest2))) %>% 
  ggplot(aes(x = invest2)) + 
  geom_bar(aes(fill = fct_rev(race4)), position = "fill") +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,6,7,8)]) +
  labs(title = "Case Assigned to Investigation or Assessment",
       subtitle = "Among 1,378 screened-in referrals",
       y = "", x = "", fill = "Race") +
  scale_y_continuous(labels = percent) +
  annotate("text", x = c(1,2), y = 1.05, label = c("1,046","332")) +
  annotate("text", x = 1, y = c(.1, .63, .71, .85), label = c("56.2", "13.1", "2.4", "28.3"), color = "white") +
  annotate("text", x = 2, y = c(.1, .63, .77, .85), label = c("56.6", "19.9", "1.0", "22.6"), color = "white")
ggsave("investigated_race4.pdf", width=9, height=6, units="in")   


######################################################################################
# 4. Racial disparity in substantiation of investigated cases
######################################################################################
# update, remove pending and appealed, add unable to complete to unfounded, proportional stacked bar
# prior vis code to update: qplot(nodrs$disposition,fill=race,xlab='Substantiation of abuse', ylab='Frequency', main='Substantiation of abuse & Race')+ coord_flip()
with(referral, prop.table(table(race2, disp4), 1))
with(referral, prop.table(table(race2, disp4), 2)) # for annotation

referral %>% filter(!(is.na(disp4))) %>% 
  ggplot(aes(x = disp4)) + 
  geom_bar(aes(fill = fct_rev(race2)), position = "fill") +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,8)]) +
  labs(title = "Substantiation of Investigated Cases",
       subtitle = "Among 305 non-pending cases assigned to investigation",
       y = "", x = "", fill = "Race") +
  scale_y_continuous(labels = percent) +
  annotate("text", x = c(1,2,3,4), y = 1.05, label = c("122","20", "78", "85")) +
  annotate("text", x = 1, y = c(.1, .85), label = c("76.2", "23.8"), color = "white") +
  annotate("text", x = 2, y = c(.1, .85), label = c("75.0", "25.0"), color = "white") +
  annotate("text", x = 3, y = c(.1, .85), label = c("74.4", "25.6"), color = "white") +
  annotate("text", x = 4, y = c(.1, .85), label = c("77.6", "22.4"), color = "white")
ggsave("substantiation_race2.pdf", width=9, height=6, units="in")    


######################################################################################
# 5. Racial disparity in time to contact (or number of contacts from active)
######################################################################################
# prior vis code to update
# #Creating a "Times" variable so we are able to see the time of first contact and referral date.
# Times <- referral %>% 
#   filter(!is.na(first_contact) & !is.na(ref_date))
# Times <- Times %>% mutate(avgtime = difftime(first_contact, ref_date, units = "days"))
# 
# #Filtering for definition of race, we are splitting race into four categories.
# Times = Times %>% mutate (race_class= ifelse (`ethnicity`=='White',"White",ifelse(`ethnicity`=='Black',"Black",ifelse(`ethnicity`=='Multi-Race',"Multi-Race","Other"))))
# #Setting as a factor in order to allow for visualization. This will also allow for manipulation of race factor in future work.
# Race=as.factor(Times$race_class)
# 
# qplot(Times$avgtime,fill=Race)+ labs(y='Frequency', x='Time Spent (Days)', subtitle="Referral Cases from July 1st, 2014 to August 30th, 2017", title='Time Between First Contact and Referrral Date & Race') + scale_fill_manual(values = brewer.pal(9,"Blues")[c(9,7,5,3)])+coord_cartesian(xlim=c(0, 10))


######################################################################################
# 6. Racial disparity in number of contacts from active cases or duration of services
######################################################################################
# no current vis
active %>% group_by(race4) %>% summarize(mean(contact_count, na.rm = T))
