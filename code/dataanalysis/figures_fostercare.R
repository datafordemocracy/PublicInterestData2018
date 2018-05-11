######################################################################################
# DS 5559: Public Interest Data Lab 
# Generate baseline figures for report: foster care data
# 1. Foster care clients by race
# 2. Initial out of home placement by race
# 3. All out of home placements by race
# 4. Exit/discharge from foster care system by race
# 5. Number of placements by race
# 6. Time in each placement by race
# 7. Duration in foster care by race
# Created by C. McClintock
# Contact: Michele Claibourn (mclaibourn@virginia.edu)
# Updated: May 10, 2018 mpc
######################################################################################

rm(list=ls())
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(scales)

setwd("/Volumes/NO NAME") # point to encrypted file
# setwd("/Volumes/encrypted_data") # encrypted file on different computer
load("outofhome.Rdata")
setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/Group 2 Code/") # save figures here


######################################################################################
# 1. Foster care clients by race
######################################################################################
# By race4
table(fc4$race4) # numbers for figure
# reorder for figure
fc4 <- fc4 %>% 
  mutate(race4 = factor(race4, levels = c("Black", "Multiracial", "White", "Other")))

foster1 <- ggplot(data = fc4, aes(x = race4)) + 
  geom_bar(aes(fill = race4), position = "dodge") + 
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(9,8,6,4,3)]) + 
  labs(title = "Number of Foster Care Clients by Race",
       subtitle = "Foster Care Cases from July 1, 2014 to June 30, 2017",
       fill = "Race", x = "", y = "") +
      annotate("text", x = c(1,2,3,4), y = c(55,38,28,5), 
             label = c("53","36", "26", "3")) +
  theme(text = element_text(size=12)) + guides(fill=FALSE)
ggsave("fc_race.pdf", plot = foster1, width=9, height=6, units="in")


######################################################################################
# 2. Initial out of home placement by race
######################################################################################
# By race3
with(fc, prop.table(table(race3, iplace4), 1)) # for annotation
iplace <- ggplot(data = fc, aes(x = race3)) + 
  geom_bar(aes(fill = iplace4), position = "fill") + 
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,5,7,8)]) + 
  labs(title = "Initial Placement Category by Race",     
       subtitle = "Foster Care Cases from July 1, 2014 to June 30, 2017",
       fill = "Placement", x = "", y = "" ) +
  scale_y_continuous(labels = percent) +
  annotate("text", x = 1, y = c(.1, .213, .29, .6), label = c("19.2", "3.8", "11.5", "65.4"), color = "white") +
  annotate("text", x = 2, y = c(.05, .12, .19, .6), label = c("9.0", "5.6", "7.9", "77.5"), color = "white") + 
  annotate("text", x = c(1,2), y = 1.05, label = c("26","89")) +
  theme(text = element_text(size=10)) 
ggsave("fc_init_place.pdf", plot = iplace, width=9, height=6, units="in")


######################################################################################
# 3. All out of home placements by race
######################################################################################
# By race3
with(fcph, prop.table(table(race3, place4), 1)) # for annotation
place <- ggplot(data = fcph, aes(x = race3)) + 
  geom_bar(aes(fill = place4), position = "fill") + 
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,5,7,8)]) + 
  labs(title = "Placement Category by Race",
       subtitle = "Foster Care Cases from July 1, 2014 to June 30, 2017",
       fill = "Placement", x = "", y = "" ) +
  scale_y_continuous(labels = percent) +
  annotate("text", x = 1, y = c(.08, .23, .36, .75), label = c("18.8", "12.5", "10.4", "58.3"), color = "white") +
  annotate("text", x = 2, y = c(.08, .23, .38, .75), label = c("16.8", "14.0", "15.4", "53.7"), color = "white") + 
  annotate("text", x = c(1,2), y = 1.05, label = c("48","214")) +
  theme(text = element_text(size=10)) 
ggsave("fc_all_place.pdf", plot = place, width=9, height=6, units="in")

  
######################################################################################
# 4. Exit/discharge from foster care system by race
######################################################################################
# By race2
fc_exit <- fc %>% filter(sys_status == 1)
# reorder for figure
fc_exit <- fc_exit %>% 
  mutate(race2 = as.character(race2),
           race2 = recode(race2, "White" = "White", "Children of Color" = "Black"),
           race2 = factor(race2, levels = c("White", "Black")))

fc_exit <- fc_exit %>% 
  mutate(dis_why2 = as.character(dis_why),
         dis_why2 = ifelse(is.na(dis_why), "Not Provided", dis_why2),
         dis_why2 = factor(dis_why2, levels = c("Not Provided", "Emancipation", "Transfer Other Relative", "Adoption", "Reunification")))

with(fc_exit, prop.table(table(race2, dis_why2), 1)) # for annotation
exit <- ggplot(fc_exit, aes(x = race2)) +
  geom_bar(aes(fill = dis_why2), position = "fill") +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,5,6,7,8)]) +
  scale_y_continuous(labels = percent) +
  labs(title = "Nature of Exit from Foster Care by Race",
       subtitle = "Foster Care Cases from July 1, 2014 to June 30, 2017",
       fill = "Reason for Exit", x = "", y = "") +
  annotate("text", x = 1, y = c(.25, .89), label = c("80.0", "20.0"), color = "white") +
  annotate("text", x = 2, y = c(.25, .48, .63, .8, .89), label = 
             c("47.5", "2.5", "27.5", "5.0", "17.5"), color = "white") + 
  annotate("text", x = c(1,2), y = 1.05, label = c("10","40")) +
  theme(text = element_text(size = 10))
ggsave("fc_dis_why.pdf", plot = exit, width=9, height=6, units="in")


######################################################################################
# 5. Number of placements by race
######################################################################################
fc %>% group_by(race2) %>% summarize(mean(num_placements_client)) # 1.85 W, 2.4 B
t.test(num_placements_client ~ race2, data = fc)

numplace <- ggplot(fc, aes(x=num_placements_client, fill=race2)) + geom_density(alpha=.5) + 
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(3,7)]) + 
  labs(title="Number of Placements By Race", 
       subtitle = "Foster Care Cases from July 1, 2014 to June 30, 2017",
       x = "Placements",  y = "", fill="Race") +
  geom_vline(xintercept = 1.9, col=brewer.pal(9, "Blues")[c(9)], linetype=2) +
  geom_vline(xintercept=2.4, col=brewer.pal(9, "Blues")[c(9)], linetype=1) +
  annotate("text", x=1.6, y=.05, label = "White",
           angle = 90, color="black") +
  annotate("text", x=2.7, y=.05, label = "Black",
           angle = 90, color="black") + scale_alpha(guide = "none")
ggsave("fc_place.pdf", plot = numplace, width=9, height=6, units="in")


######################################################################################
# 6. Time in each placement by race
######################################################################################
fcph %>% group_by(race2) %>% summarize(mean(time_weeks, na.rm=T)) # 26.5 W, 19.7 B
t.test(time_weeks ~ race2, data = fcph)

timeplace<- ggplot(fcph, aes(x=time_weeks, fill=race3)) + geom_density(alpha=.5) + 
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(3,7)]) + 
  labs(title="Time in Each Placement By Race", 
       subtitle = "Foster Care Cases from July 1, 2014 to June 30, 2017",
       x = "Weeks",  y = "", fill="Race") +
  geom_vline(xintercept = 19.7, col=brewer.pal(9, "Blues")[c(9)], linetype=1) +
  geom_vline(xintercept=26.5, col=brewer.pal(9, "Blues")[c(9)], linetype=2) +
  annotate("text", x=29.5, y=.0055, label = "White Children",
           angle = 90, color="black") +
  annotate("text", x=16.7, y=.0055, label = "Black Children",
           angle = 90, color="black") + scale_alpha(guide = "none")
ggsave("fc_time.pdf", plot = timeplace, width=9, height=6, units="in")


######################################################################################
# 7. Duration in foster care by race
######################################################################################
fc %>% group_by(race2) %>% summarize(mean(duration)) # looks like 53.6 W and 56.3 B?
t.test(duration ~ race2, data = fcph)

duration <- ggplot(fc, aes(x=duration, fill=race3)) + geom_density(alpha=.5) + 
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(3,7)]) + 
  labs(title="Duration of Care By Race (All Clients)", 
       subtitle = "Foster Care Cases from July 1, 2014 to June 30, 2017",
       x = "Weeks",  y = "", fill="Race") +
  geom_vline(xintercept = 53.6, col=brewer.pal(9, "Blues")[c(9)], linetype=2) +
  geom_vline(xintercept=56.3, col=brewer.pal(9, "Blues")[c(9)], linetype=1) + 
  annotate("text", x = 49.6, y = .0025, label = "White Children",
           angle = 90, color = "black") +
  annotate("text", x = 60.1, y = .0025, label = "Black Children",
           angle = 90, color = "black")
ggsave("fc_dur.pdf", plot = duration, width=9, height=6, units="in")


# clean up and save image to use for analysis
rm(fc4, fcph4, duration, exit, foster1, iplace,  numplace, place, timeplace)
setwd("/Volumes/NO NAME") # point to encrypted file
save.image("visualizations.Rdata")
