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
library(lme4)
library(pcaMethods)
library(AICcmodavg) #for model selection 
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
# need to
# convert all measures to decimal, some are in unit % while others are in 100%
postfire_measures <- postfire_measures %>% mutate(bole.ch = bole.ch/100,
                                                  bole.cs30 = bole.cs30/100,
                                                  crown.scorch = crown.scorch/100,
                                                  visual.live = visual.live/100)

#As for trees of which images were not taken is because of there was not crown loss
# or survived crown is less than 5% (so impossible to detect from image analysis, we did
# not bother to take image), those will be direcly assigned from the visual assessment. So
# 'NA' is not truely 'NA'. need to o replace 'NA' with 0 for crown.loss and visual assessment
# for crown.live 
# code source see https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-several-columns-on-a-subset-of-rows

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

#assign 0 to crown.loss where crown.loss is NA
postfire_measures <- mutate_cond(postfire_measures, is.na(crown.loss),
                                 crown.loss = 0)

#replace crown.live with visual.live where crown.live is NA, before that 

  
postfire_measures <- mutate_cond(postfire_measures, is.na(crown.live),
                                  crown.live = visual.live) 

#pca of post-fire crown loss and scorch measurements w/o imputation
seve_noimpu <- postfire_measures %>% 
  select(crown.scorch, crown.loss) %>% 
  pca(method = "svd", nPcs = 2, scale = "none")
summary(seve_noimpu) #good, PC1 explains 97% of total variance
ca_injury <- as.data.frame(scores(seve_noimpu))
ca_injury <- ca_injury %>% select(PC1)
postfire_measures$canopy.inj <- ca_injury$PC1 #extrac PC1 as canopy injury index

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

#log transform dependend variables
alldata <- alldata %>% mutate_at( c("s_peak.temp", "l_peak.temp", "s_degsec", "l_degsec",
                                    "s_degsec", "l_degsec", "s_dur", "l_dur"),
                                           list(log = log10))

#clean env
rm("ave_weather", "severity", "mortality", "time_id", "poly.df", "all_hobo",
   "ca_injury")

 