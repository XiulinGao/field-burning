#treatment-fire-behavior.R
##script for determining how fuel treatment influence fire behavior

library(dplyr)
library(car)

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
degsecs_mod <- lm(degsec ~ fuel*h_raio + temp, eunits_s)
Anova(degsecs_mod)
summary(degsecs_mod)


degsecl_mod <- lm(degsec ~ fuel*h_ratio + temp, eunits_l)
Anova(degsecl_mod)
summary(degsecl_mod)

#models for peak.temp at 2 locations
peaks_mod <- lm(peak.temp ~ fuel*h_ratio + temp, eunits_s)
Anova(peaks_mod)

peakl_mod <- lm(peak.temp ~ fuel*h_ratio + temp, eunits_l)
Anova(peakl_mod)

#only fuel load influence degsec and peak temperatu 
#at the soil surface not at 100cm location

