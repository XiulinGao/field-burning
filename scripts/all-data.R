## all-data.R
## to read in all data and combine into a big dataset (in wide) 
## needed for further analysis

## load all packages 
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(car)
library(ggplot2)


source("./hobo-summary.R") #read in hobo data for fire behavior
source("./crownv-estimation.R") #read in %crown loss or survived data for fire severity 


###### read in all measurements to make a big datasets including all measurements for analysis ######

## read in fuel treatments that matched to each tree
treatments <- read.csv("../data/fuel-treatments.csv", stringsAsFactors = FALSE,
                       na.strings = c(" "))

## read in pre-fire trait measurements and convert height (in inch) to cm
traits <- read.csv("../data/tree-traits.csv", stringsAsFactors = FALSE,
                   na.strings = c(" ")) 
traits <- traits %>% mutate(height = 2.54*height)

## read in postfire severity (visual assessments) and mortality measurements 
## and combint it with image-analyzed severity measurements
severity <- read.csv("../data/post-fire-severity.csv", stringsAsFactors = FALSE,
                     na.strings = c(" "))
mortality <- read.csv("../data/tree-mortality.csv", stringsAsFactors = FALSE, 
                      na.strings = c(" "))
postfire_measures <- severity %>% left_join(percnt_crwn, by = "tree.id") %>% 
  left_join(mortality, by = "tree.id") 
#renmae visual assess for %crown survied to visual.live to distinguish from image-analyzed data
postfire_measures <- postfire_measures %>% 
  rename(visual.live = crown.alive, crown.scorch = crown.vs)

###### combine all measurements ######

## combine fuel treatment with fire behavior by tree.id
treatment_fire <- treatments %>% left_join(tempsec.wide, by = "tree.id")
treatment_fire <- treatment_fire %>% mutate(treatment = paste(fuel, h_ratio,
                                                              sep = ""))
## summarize weather to use average 
ave_weather <- weather %>% #filter(unit %in% c("SE", "NE")) %>%  
  mutate(temp = (temp-32)*5/9) %>% 
  group_by(unit) %>% summarise_at(c("temp", "RH", "ave_ws"), mean, na.rm = TRUE) %>% 
  mutate_at(c("temp", "RH", "ave_ws"), round, digit = 1)


#combine fire beahvior, postfire measures, and plant traits
# to make a dataset with all measurements for later analysis

alldata <- traits %>% left_join(treatment_fire, by = "tree.id") %>% 
  left_join(postfire_measures, by = "tree.id") %>% #filter(unit %in% c("SE", "NE")) %>% 
  left_join(ave_weather, by = "unit")

 