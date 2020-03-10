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

#post fire severity
severity <- read.csv("../data/post-fire-severity.csv", stringsAsFactors = FALSE,
                     na.strings = c(" "))

#combine the two with fire beahvior to make a dataset with all measurements that
#can be used to explore how both traits and fire behavior influence severity
#measurements at different locations may matter for different severity measurements

firel <- treatment_fire %>% filter(location == "l") #crown heating
fires <- treatment_fire %>% filter(location == "s") #soil heating
  
  
fireseve_l <- traits %>% left_join(severity, by = "tree_ID") %>% 
  left_join(firel, by = "tree_ID") 
fireseve_s <- traits %>% left_join(severity, by = "tree_ID") %>% 
  left_join(fires, by = "tree_ID")

#standardize variables
zscore <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm = TRUE) 

fireseve_l <- fireseve_l %>% mutate_at(c("peak.temp", "height"),
                                       funs(s = zscore))
fireseve_s <- fireseve_s %>% mutate_at(c("degsec", "dbh", "peak.temp", "height"),
                                       funs(s = zscore))

#model for bole_CS30 with using soil heating measurement, dataset fireseve_s
surface_lmmod <- lme4::lmer(Bole_CS30 ~ degsec_s*dbh_s + (1|unit),
                            fireseve_s, REML = TRUE)

summary(surface_lmmod)
Anova(surface_lmmod, test.statistic = "F")

#model for Crown_VS using canopy heating measurement, specifically 
# peak temperature as it relates to flame height, dataset fireseve_l
crown_lmmod <- lme4::lmer(Crown_VS ~ peak.temp_s*height_s + (1|unit), 
                          fireseve_l, REML = TRUE)
summary(crown_lmmod)
Anova(crown_lmmod, test.statistic = "F")
