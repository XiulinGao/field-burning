#treatment-fire-behavior.R
##script for determining how fuel treatment influence fire behavior

library(dplyr)
library(car)
library(ggplot2)

source("./hobo-summary.R")
#fuel treatments
treatments <- read.csv("../data/fuel-treatments.csv", stringsAsFactors = FALSE,
                       na.strings = c(" "))
treatment_fire <- treatments %>% left_join(tempsec.sum, by = "tree_ID") %>% 
  filter(!is.na(dur)) #throw away data without hobo summary
treatment_fire <- treatment_fire %>% mutate(treatment = paste(fuel, h_ratio,
                                                              sep = ""))
#weather, using average temperature 
eunit_wea <- weather %>% filter(unit %in% c("SE", "NE")) %>%  
  mutate(temp = (temp-32)*5/9) %>% 
  group_by(unit) %>% summarise_at(c("temp", "RH", "ave_ws"), mean, na.rm = TRUE) %>% 
  mutate_at(c("temp", "RH", "ave_ws"), round, digit = 1)

#analysis only with data from SE and NE units
eunits_s <- treatment_fire %>% filter(unit %in% c("SE", "NE")) %>% 
  filter(location == "s") %>% left_join(eunit_wea, by = "unit")

eunits_l <- treatment_fire %>% filter(unit %in% c("SE", "NE")) %>% 
  filter(location == "l") %>% left_join(eunit_wea, by = "unit")


#modles for degsec at 2 locations, as the power is low, we'll just use general linear
#model for this
degsecs_mod <- lm(degsec ~ h_ratio*fuel + temp, eunits_s)
Anova(degsecs_mod)
summary(degsecs_mod)

#plot
ratiolabel <- c("70% fuel above ground", "70% fuel on ground")
names(ratiolabel) <- c("H", "L")

ggplot(eunits_s, aes(fuel, degsec)) + geom_boxplot() +
  facet_grid(.~h_ratio, labeller = labeller(h_ratio = ratiolabel)) + 
  xlab("Fuel load treatment") + ylab("Integrated temperature at soil surface") +
  pubtheme.nogridlines 

ggsave("../results/fuel-0.pdf", width = col1, height= 0.8*col1, 
       units="cm")

degsecl_mod <- lm(degsec ~ fuel*h_ratio + temp, eunits_l)
Anova(degsecl_mod)
summary(degsecl_mod)

ggplot(eunits_l, aes(fuel, degsec)) + geom_boxplot() +
  facet_grid(.~h_ratio, labeller = labeller(h_ratio = ratiolabel)) + 
  xlab("Fuel load treatment") + ylab("Integrated temperature at 100cm") +
  pubtheme.nogridlines 

ggsave("../results/fuel-100.pdf", width = col1, height= 0.8*col1, 
       units="cm")

#models for peak.temp at 2 locations
peaks_mod <- lm(peak.temp ~ fuel*h_ratio + temp, eunits_s)
Anova(peaks_mod)

peakl_mod <- lm(peak.temp ~ fuel*h_ratio + temp, eunits_l)
Anova(peakl_mod)

#only fuel load influence degsec and peak temperatu 
#at the soil surface not at 100cm location

