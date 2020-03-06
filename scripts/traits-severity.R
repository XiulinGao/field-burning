#traits-severity.R
##script for determining influence of both pre-fire tree traits 
# and fire behavior on post-fire severity 

library(dplyr)

source("./hobo-summary.R")

#pre-fire traits
traits <- read.csv("../data/tree-traits.csv", stringsAsFactors = FALSE,
                   na.strings = c(" "))

#post fire severity
severity <- read.csv("../data/post-fire-severity.csv", stringsAsFactors = FALSE,
                     na.strings = c(" "))

#combine the two with tempsec.sum to make a dataset with all measurements that
#can be used to explore how both traits and fire behavior influence severity

fire_severity <- traits %>% left_join(severity, by = "tree_ID") %>% 
  left_join(tempsec.sum, by = "tree_ID") 
