######################################################################################
# DS 5559: Public Interest Data Lab 
# Estimate racial disparity models
# 1. Generate graph of predicted probabilities/counts from post-referral models
# Michele Claibourn (mclaibourn@virginia.edu)
# Updated: April 2, 2018 
######################################################################################

rm(list = ls())
library(tidyverse)
# install.packages("ggalt")
library(ggalt)
library(RColorBrewer)

setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/Group 1 Code/githubcode/")
predict <- read_csv("predictions.csv")


######################################################################################
# 1. Generate graph of predicted probabilities/counts from post-referral models
######################################################################################
# make racecat (y) and dv (facet) factors and set order/levels for figure; make pval indicator
predict <- predict %>% 
  mutate(pval3 = ifelse(pval<.1, 3, ifelse(pval<.2, 2, 1)),
         racecat = factor(racecat, levels=c("other", "multi", "black")),
         dv_f = factor(dv, levels=c("ref", "inv", "fin", "sev", "ftf"))) # get right order of facets

# set labels to use for facet/panels
dv_names <- c(
    `ref` = "Accepted",
    `inv` = "Investigated",
    `fin` = "Substantiated",
    `sev` = "Level 3"
)

# set colors  
display.brewer.pal(9, "Blues")
col1 <- brewer.pal(9, "Blues")[4]
coll <- brewer.pal(9, "Blues")[6]
col2 <- brewer.pal(9, "Blues")[8]

# getting the legending/color scales right...
# see example: https://stackoverflow.com/questions/44653597/adding-a-traditional-legend-to-dumbbell-plot-in-ggaltgeom-dumbbell-in-r
# make the dataframe long
predict2 <- predict %>% filter(dv != "ftf") %>% 
    select(dv_f, racecat, wprob, cprob, pval3) %>% 
    gather(group, value, -c(racecat, dv_f, pval3))

# solution for annotating one facet: https://stackoverflow.com/questions/45214725/use-ggplots-annotate-only-within-one-facet
# add df for annotating figure
ann1 <- data.frame(value = 0.29, racecat = "black", lab = "p < 0.2", 
                   dv_f = factor("inv", levels = c("ref", "inv", "fin", "sev")))
ann2 <- data.frame(value = 0.33, racecat = "multi", lab = "p < 0.1", 
                   dv_f = factor("inv", levels = c("ref", "inv", "fin", "sev")))

# graph
ggplot(predict[predict$dv != "ftf",], aes(y = racecat)) + 
    geom_point(data = predict2, aes(x = value, color = group), size = 3) +
    geom_dumbbell(aes(x = wprob, xend = cprob, size = pval3), color=coll, 
                  colour_x = col1, colour_xend = col2, show.legend=FALSE) +
    facet_grid(dv_f ~ ., scales = "free_y", labeller = as_labeller(dv_names)) +
    scale_color_manual(name = "Race", values = c(col2, col1), 
                       labels = c("cprob" = "Minority", "wprob" = "White")) +
    scale_y_discrete(labels=c("other" = "Other Minority", "multi" = "Multi-Racial",
                              "black" = "Black")) +
    labs(x = "Probability Difference", y = "",
         title = "Difference in Predicted Probability of Post-Referral Outcomes",
         subtitle = "Between White Children and Children in Racial Minorities") +
    geom_text(data = ann1, aes(x=value, y=racecat), label = "p<0.2", color="white") +
    geom_text(data = ann2, aes(x=value, y=racecat), label="p<0.1", color="white")
ggsave("postrefpred.pdf") 


######################################################################################
save.image("postreferral_prediciton.RData")

#####################################
# same graph with facet_wrap -- doesn't work (plots geom_point on each panel)
# facet_wrap might be preferable, as facet label would appear above each panel (could be longer)
ggplot(predict[predict$dv != "ftf",], aes(y = racecat)) + 
    geom_point(data = predict2, aes(x = value, color = group), size = 3) +
    geom_dumbbell(aes(x = wprob, xend = cprob, size = Confidence), color=coll, 
                  colour_x = col1, colour_xend = col2) +
    facet_wrap(~ dv, ncol=1, scales = "free_y", labeller = as_labeller(dv_names)) +
    scale_color_manual(name = "Race", values = c(col1, col2)) +
    labs(x="Probability Difference", 
         y="From White to Racial Minority", 
         title="Difference in Predicted Probability of Post-Referral Outcomes", 
         subtitle="Between White Children and Children in Racial Minorities", 
         caption="Source: Predicted Probabilities from Models in Appendix") 



#####################################
# see also: https://rud.is/b/2016/04/17/ggplot2-exercising-with-ggalt-dumbbells/
# adding labels and text as alternative

#####################################
# initial dumbbell plot attempts
predict %>% filter(dv != "ftf") %>% 
    ggplot(aes(x=wprob, xend=cprob, y=racecat, group=racecat)) + 
    geom_dumbbell(aes(size=pval3), colour = coll, colour_x = col1, colour_xend = col2, show.legend=FALSE) +
    facet_wrap(~ dv_f, ncol=1) +
    labs(x="Probability Difference",
         y="White to Racial Minority",
         title="Difference in Predicted Probability of Post-Referral Outcomes",
         subtitle="Between White Children and Children in Racial Minorities",
         caption="Source: Predicted Probabilities from Models in Appendix")
# adding legends
predict %>% filter(dv != "ftf") %>% 
    ggplot(aes(x=wprob, xend=cprob, y=racecat, group=racecat)) + 
    geom_dumbbell(aes(size=pval3, colour=racecat), colour_x = col1) +
    scale_fill_manual(values = brewer.pal(9, "Blues")[c(6,7,8)]) +
    facet_grid(dv_f ~ .) +
    labs(x="Probability Difference", 
         y="From White to Racial Minority", 
         title="Difference in Predicted Probability of Post-Referral Outcomes", 
         subtitle="Between White Children and Children in Racial Minorities", 
         caption="Source: Predicted Probabilities from Models in Appendix") 
# need to control the colors?