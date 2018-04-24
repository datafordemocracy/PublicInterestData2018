######################################################################################
# DS 5559: Public Interest Data Lab 
# Estimate racial disparity models
# 1. Generate coefficient plot from post-referral models
# 2. Generate predicted probabilities/effect size graph from post-referral models
# Michele Claibourn (mclaibourn@virginia.edu)
# Updated: April 2, 2018 
######################################################################################

rm(list = ls())
library(tidyverse)
library(ggalt)
library(grid)
library(RColorBrewer)
library(dotwhisker)

setwd("~/Box Sync/mpc/dataForDemocracy/pidl/PIDL2018/code/Group 1 Code/githubcode/")
load("predictions.RData")


######################################################################################
# 1. Generate coefficient plot from post-referral models
######################################################################################
# Reduce to coefficients on race
m_ref <- coef_ref %>% 
  filter(term %in% c("race4b=Black", "race4b=Multiracial", "race4b=Other")) %>% 
  mutate(model = "1Accepted",
         modelf = "Accepted")
m_inv <- coef_inv %>% 
  filter(term %in% c("race4b=Black", "race4b=Multiracial", "race4b=Other")) %>% 
  mutate(model = "2Investigated",
         modelf = "Investigated")
m_fin <- coef_fin %>% 
  filter(term == "race2b=Children of Color") %>% 
  mutate(model = "3Substantiated",
         modelf = "Substantiated")
m_sev <- coef_sev %>% 
  filter(term == "race2b=Children of Color") %>% 
  mutate(model = "4Level 3",
         modelf = "Level 3")

# set colors  
col0 <- brewer.pal(9, "Blues")[2]
col1 <- brewer.pal(9, "Blues")[4]
coll <- brewer.pal(9, "Blues")[6]
col2 <- brewer.pal(9, "Blues")[8]

# Combine
post_models <- rbind(m_ref, m_inv, m_fin, m_sev)
post_models <- post_models %>% 
  mutate(modelf = factor(modelf, levels = c("Accepted", "Investigated", "Substantiated", "Level 3")), # get right order of facets
         term = recode(term, "race4b=Black" = "Black",
                       "race4b=Multiracial" = "Multiracial", 
                       "race4b=Other" = "Other Minority", 
                       "race2b=Children of Color" = "Black"))
         
# Graph (with model as character, forced alphabetical with numeric name)
dwplot(post_models, conf.level = 0.90, dot_args = list(aes(colour = term), size = 0.7)) + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  facet_grid(model ~ ., scales = "free_y", drop = TRUE) +
  scale_color_manual(values = c(col2, coll, col1)) +
  geom_rect(data = subset(post_models, model == "2Investigated"),
            aes(fill = "2Investigated"), xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, fill = col0, alpha = 0.1, inherit.aes = FALSE) +
  labs(x = "Coefficient Estimate", y = "",
       title = "Effect of Race on Post-Referral Outcomes",
       subtitle = "Coefficient for Racial Minority Children Relative to White Children") +
  theme(legend.position = "none")
ggsave("../figures/postrefcoef.pdf", width=9, height=6, units="in")  

# Graph (with model as factor)
dwplot(post_models, conf.level = 0.90, dot_args = list(aes(colour = term), size = 0.6)) + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  facet_grid(modelf ~ ., scales = "free_y", drop = TRUE) +
  scale_color_manual(values = c(col2, coll, col1)) +
  geom_rect(data = subset(post_models, model == "Investigated"),
            aes(fill = "Investigated"), xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, fill = col0, alpha = 0.1, inherit.aes = FALSE) +
  labs(x = "Coefficient Estimate", y = "",
       title = "Effect of Race on Post-Referral Outcomes",
       subtitle = "Coefficient for Racial Minority Children Relative to White Children") +
  theme(legend.position = "none")
# ggsave("../figures/postrefcoef.pdf") 
### AAARGH -- when I use modelf to get the panels in proper sequence
###   it adds an empty NA panel at the bottom!!!
###   the factor version allows me to leave off the y labels that aren't used, but 
###   the empty NA panel seems like a non-starter

# Troubleshooting: https://stackoverflow.com/questions/40807252/ggplot2-deleting-facets-of-unused-factor-level-combinations-from-a-plot-facet
g <- ggplotGrob(dwgg)
g$layout
# identify problematic pieces (grobs)
# pos <- grep(pattern = "5$", g$layout$name)
# one at a time
pos1 <- grep(pattern = "panel-1-5", g$layout$name)
pos2 <- grep(pattern = "axis-l-5", g$layout$name)
pos3 <- grep(pattern = "axis-r-5", g$layout$name)
pos4 <- grep(pattern = "strip-r-5", g$layout$name)
# replace problematic pieces (grobs) with nullGrob
g$grobs[[pos1]] <- nullGrob()
g$grobs[[pos2]] <- nullGrob()
g$grobs[[pos3]] <- nullGrob()
g$grobs[[pos4]] <- nullGrob()
# redraw the figure
grid.newpage()
grid.draw(g)
# still leaves space for the panel!


######################################################################################
# 2. Generate predicted probabilities/effect size graph from post-referral models
######################################################################################
# make racecat (y) and dv (facet) factors and set order/levels for figure; make pval indicator
predict <- predict %>% 
  mutate(pval3 = ifelse(pval<.1, 3, ifelse(pval<.2, 2, 1)),
         racecat = factor(racecat, levels=c("other", "multi", "black")),
         dv_f = factor(dv, levels=c("ref", "inv", "fin", "sev"))) # get right order of facets

# set labels to use for facet/panels
dv_names <- c(
    `ref` = "Accepted",
    `inv` = "Investigated",
    `fin` = "Substantiated",
    `sev` = "Level 3"
)

# set colors  
display.brewer.pal(9, "Blues")
col0 <- brewer.pal(9, "Blues")[2]
col1 <- brewer.pal(9, "Blues")[4]
coll <- brewer.pal(9, "Blues")[6]
col2 <- brewer.pal(9, "Blues")[8]

# getting the legending/color scales right...
# see example: https://stackoverflow.com/questions/44653597/adding-a-traditional-legend-to-dumbbell-plot-in-ggaltgeom-dumbbell-in-r
# make the dataframe long
predict2 <- predict %>% 
    select(dv_f, racecat, wprob, cprob, pval3) %>% 
    gather(group, value, -c(racecat, dv_f, pval3))

# solution for annotating one facet: https://stackoverflow.com/questions/45214725/use-ggplots-annotate-only-within-one-facet
# add df for annotating figure
ann1 <- data.frame(value = 0.29, racecat = "black", lab = "p < 0.2", 
                   dv_f = factor("inv", levels = c("ref", "inv", "fin", "sev")))
ann2 <- data.frame(value = 0.33, racecat = "multi", lab = "p < 0.1", 
                   dv_f = factor("inv", levels = c("ref", "inv", "fin", "sev")))

# graph: effect size
ggplot(predict, aes(y = racecat)) + 
    geom_point(data = predict2, aes(x = value, color = group), size = 3) +
    geom_rect(data = subset(predict, dv_f == "inv"),
              aes(fill = "inv"), xmin = -Inf, xmax = Inf,
              ymin = -Inf, ymax = Inf, fill = col0, alpha = 0.2) +
    geom_dumbbell(aes(x = wprob, xend = cprob, size = pval3), color=coll, 
                  colour_x = col1, colour_xend = col2, show.legend=FALSE) +
    facet_grid(dv_f ~ ., scales = "free_y", labeller = as_labeller(dv_names)) +
    scale_color_manual(name = "Race", values = c(col2, col1), 
                       labels = c("cprob" = "Minority", "wprob" = "White")) +
    scale_y_discrete(labels=c("other" = "Other Minority", "multi" = "Multiracial",
                              "black" = "Black")) +
    labs(x = "Predicted Probability of Outcome", y = "",
         title = "Difference in Predicted Probability of Post-Referral Outcomes",
         subtitle = "Between White Children and Children in Racial Minorities") +
    geom_text(data = ann1, aes(x=value, y=racecat), label = "p<0.2", color="white") +
    geom_text(data = ann2, aes(x=value, y=racecat), label="p<0.1", color="white")
ggsave("../figures/postrefpred.pdf", width=9, height=6, units="in")   


######################################################################################
save.image("postreferral_prediction.RData")
# load("postreferral_prediction.RData")

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