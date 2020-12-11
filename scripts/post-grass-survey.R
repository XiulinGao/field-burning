#post-grass-survey.R
#scaript to analyze post-fire grass species composition
#and fuel load

sp.compo <- read.csv("../data/postfire-vegetation.csv", stringsAsFactors = FALSE,
                     na.strings = " ") #grass species composition
fueload <- read.csv("../data/fuel-load.csv", stringsAsFactors = FALSE,
                    na.strings = " ") #grass fuel load 
treatments <- read.csv("../data/fuel-treatments.csv", stringsAsFactors = FALSE,
                       na.strings = " ")

##mean post-fire fuel load surrounding each tree
fueload <- fueload %>% group_by(tree.id) %>% summarise(mean.fuel = mean(weight, na.rm= TRUE)) %>% 
  mutate(mean.fuel = round(mean.fuel, digits = 1))
fueload <- fueload %>% left_join(treatments, by = "tree.id") 
unit.fuel <- fueload %>% group_by(unit) %>% summarise(unit.mean = mean(mean.fuel,
                                                                       na.rm = TRUE))



