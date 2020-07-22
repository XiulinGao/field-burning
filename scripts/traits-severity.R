#traits-severity.R
##script for determining influence of both pre-fire tree traits 
# and fire behavior on post-fire severity 

library(dplyr)

source("./treatment-fire-behavior.R")

#pre-fire traits
traits <- read.csv("../data/tree-traits.csv", stringsAsFactors = FALSE,
                   na.strings = c(" ")) 

#convert height into cm
traits <- traits %>% mutate(height = 2.54*height)


severity <- read.csv("../data/post-fire-severity.csv", stringsAsFactors = FALSE,
                     na.strings = c(" "))

ave_weather <- weather %>% group_by(unit) %>% summarize_at(c('RH', 'temp', 'ave_ws'),
                                              list(~mean(., na.rm = TRUE))) %>% 
  mutate_at(c('RH', 'temp', 'ave_ws'), list(~round(., digits = 1)))

#combine the two with fire beahvior to make a dataset with all measurements that
#can be used to explore how both traits and fire behavior influence severity
#measurements at different locations may matter for different severity measurements

firel <- treatment_fire %>% filter(location == "l") #crown heating
fires <- treatment_fire %>% filter(location == "s") #soil heating
  
fireseve_eunits <- traits %>% left_join(severity, by = "tree_ID") %>% 
  left_join(treatment_fire, by = "tree_ID") %>% filter(unit %in% c("SE", "NE")) %>% 
  left_join(eunit_wea, by = "unit")

fireseve_l <- traits %>% left_join(severity, by = "tree_ID") %>% 
  left_join(firel, by = "tree_ID") %>% left_join(ave_weather, by = "unit")
fireseve_s <- traits %>% left_join(severity, by = "tree_ID") %>% 
  left_join(fires, by = "tree_ID") %>% left_join(ave_weather, by = "unit")

#standardize variables
zscore <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm = TRUE) 

fireseve_l <- fireseve_l %>% mutate_at(c("peak.temp", "height", "degsec", "dur", 
                                         "temp", "ave_ws"),funs(s = zscore))
fireseve_s <- fireseve_s %>% mutate_at(c("degsec", "dbh", "peak.temp", "height", 
                                         "temp", "ave_ws"), funs(s = zscore))
                                       
fireseve_eunits <- fireseve_eunits %>% mutate_at(c("height", "dbh", "temp", "ave_ws"), 
                                                 funs(s = zscore))

#fuel treatment and crown volume scorched
fireseve_leunits <- filter(fireseve_eunits, location == "l")
fuelseve_lmod <- lm(Crown_VS ~ fuel*h_ratio*height + unit, 
                    fireseve_leunits)
summary(fuelseve_lmod)
Anova(fuelseve_lmod)

ggplot(fireseve_leunits, aes(fuel, Crown_VS)) + geom_boxplot() +
  facet_grid(.~h_ratio, labeller = labeller(h_ratio = ratiolabel)) +
  xlab("Fuel load treatment") + ylab("Crown volume scorched (%)") +
  pubtheme.nogridlines
ggsave("../results/crown_treatment.pdf", width = col1, height = 0.8*col1,
       units = "cm")

ggplot(fireseve_leunits, aes(unit, Crown_VS)) + geom_boxplot() +
  xlab("Burn unit") + ylab("Crown volume scorched (%)") +
  pubtheme.nogridlines
ggsave("../results/crown_unit.pdf", width = col1, height = 0.8*col1,
       units = "cm")

#model for Crown_VS using canopy heating measurement, specifically 
# peak temperature as it relates to flame height, dataset fireseve_l
crown_lmmod <- lme4::lmer(Crown_VS ~ peak.temp_s*height_s*degsec_s - 
                            peak.temp_s:height_s:degsec_s + (1|unit), 
                          fireseve_l, REML = TRUE)
plot(crown_lmmod)
summary(crown_lmmod)
Anova(crown_lmmod, test.statistic = "F")

sjPlot::plot_model(crown_lmmod, type="pred", terms = c("peak.temp_s", "degsec_s"), 
                   se= FALSE, #axis.lim = c(0, 100),
                   color = schwilkcolors, title = "", 
                   legend.title = "Standardized degsec") +
  xlab("Standardized peak temperature at 100cm") +
  ylab("Percentage crown volume scorched (%)") +
  pubtheme.nogridlines
ggsave("../results/crown_seve.pdf", width = col1, height= 0.8*col1, 
       units="cm")
