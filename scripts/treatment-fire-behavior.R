#treatment-fire-behavior.R
##script for determining how fuel treatment influence fire behavior
library(car)
library(dplyr)

source("./hobo-summary.R")
#fuel treatments
treatments <- read.csv("../data/fuel-treatments.csv", stringsAsFactors = FALSE,
                       na.strings = c(" "))
treatment_fire <- treatments %>% left_join(tempsec.sum, by = "tree_ID")
