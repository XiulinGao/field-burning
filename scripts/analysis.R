#analysis.R

## load all data required for analysis
source("./all-data.R")

############### fuel treatment effects on mortality ################

#standardize variables
zscore <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm = TRUE) 

#filter out NW unit that did not burn
units3df <- filter(alldata, unit!="NW")

units3df <- units3df %>% mutate_at(c("s_peak.temp", "l_peak.temp", "s_degsec", "l_degsec",
                                   "height","temp", "ave_ws", "canopy.inj", "bole.cs30",
                                   "bole.ch"),
                                 list(zs = zscore))


#H0: mortality is burn unit and tree size effect 
mortality_mod0 <- glm(mortality ~ unit + height, 
                    units3df, family = binomial(link = "logit"))
AICc(mortality_mod0, second.ord = TRUE)

#H1: mortality is affected by fuel load
mortality_mod1 <- glm(mortality ~ fuel + height + unit, 
                      units3df, family = binomial(link = "logit"))
AICc(mortality_mod1, second.ord = TRUE)

#H2: mortality is affected by fuel structure 
mortality_mod2 <- glm(mortality ~  h_ratio + height + unit,
                      units3df, family = binomial(link = "logit"))
AICc(mortality_mod2, second.ord = TRUE)

#h3: mortality is determined by both fuel load and structure w/o interacting effect
mortality_mod3 <- glm(mortality ~ fuel*h_ratio - fuel:h_ratio + height + unit, 
                      units3df, family = binomial(link = "logit"))
AICc(mortality_mod3, second.ord = TRUE)

#h4: mortality is determined by both fuel load and structure w/ interacting effect
mortality_mod4 <- glm(mortality ~ fuel*h_ratio + height + unit, 
                      units3df, family = binomial(link = "logit"))
AICc(mortality_mod4,  second.ord = TRUE)

##model selection table

#set up candidate models
cand.models1 <- list()
cand.models1[[1]] <- mortality_mod0
cand.models1[[2]] <- mortality_mod1
cand.models1[[3]] <- mortality_mod2
cand.models1[[4]] <- mortality_mod3
cand.models1[[5]] <- mortality_mod4

#create a vector of names to track each model
mod_names1 <- paste("mod", 1:length(cand.models1), sep = '')
mortality_aics <- aictab(cand.set = cand.models1, modnames = mod_names1, second.ord = TRUE, 
       sort = TRUE)
mortality_aics

#model 0 is the best model
summary(mortality_mod0)
mortality_coef <- summary(mortality_mod0)$coefficients
mortality_aov <- Anova(mortality_mod0, test.statistic = "F")
mortality_aov


#### need to calculate persudo R^2 (McFadden's R squared) 
### how to calculate McFadden's R squared for logistic regression
### see: https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/

nullmod <- glm(mortality ~ 1, units3df, family = binomial)
persudor2_mod0 <- 1 - logLik(nullmod)/logLik(mortality_mod0)
persudor2_mod0

############# canopy injury and mortality #############
injmota_mod <- glm(mortality ~ canopy.inj_zs*bole.cs30_zs + height_zs + unit,
                   family = binomial(link = "logit"), units3df)
summary(injmota_mod)
injmota_coef <- summary(injmota_mod)$coefficients
injmota_aov <- Anova(injmota_mod, test.statistic = "F")
injmota_aov

############# fuel treatment effects on crown injury #############
#H0: crown injury is only unit and tree size effect
fuelcrown_mod0 <- lm(canopy.inj ~ unit + height, units3df)
AICc(fuelcrown_mod0, second.ord = TRUE)

#H1: crown injury is fuel load effect
fuelcrown_mod1 <- lm(canopy.inj ~ fuel + height + unit, units3df)
AICc(fuelcrown_mod1, second.ord = TRUE)

#H2: crown injury is fuel structure effect
fuelcrown_mod2 <- lm(canopy.inj ~ h_ratio + height + unit, units3df)
AICc(fuelcrown_mod2, second.ord = TRUE)

#H3: crown injury is both fuel load and structure effect w/o interaction
fuelcrown_mod3 <- lm(canopy.inj ~ fuel*h_ratio - fuel:h_ratio + height + unit, units3df)
AICc(fuelcrown_mod3, second.ord = TRUE)

#H4: crown injury is both fuel load and structure effect w/ interaction
fuelcrown_mod4 <- lm(canopy.inj ~ fuel*h_ratio + height + unit, units3df)
AICc(fuelcrown_mod4, second.ord = TRUE)

## create model selection tables
# set up candidate models

cand.models <- list()
cand.models[[1]] <- fuelcrown_mod0
cand.models[[2]] <- fuelcrown_mod1
cand.models[[3]] <- fuelcrown_mod2
cand.models[[4]] <- fuelcrown_mod3
cand.models[[5]] <- fuelcrown_mod4

#create a vector of names to track each model
mod_names <- paste("mod", 1:length(cand.models), sep = '')
cainj_aics <- aictab(cand.set = cand.models, modnames = mod_names, second.ord = TRUE, 
       sort = TRUE)
cainj_aics
#mod 0 is the best model

summary(fuelcrown_mod0)
cainj_coef <- summary(fuelcrown_mod0)$coefficients
cainj_aov <- Anova(fuelcrown_mod0, test.statistic = "F")
cainj_aov

#model for canopy.inj using canopy heating measurement, specifically 
# peak temperature as it relates to flame height

## as there are only fire behavior measurements for the 2 east units, 
## filter out east unit only data
#eunits_measure <- alldata %>% filter(unit %in% c("SE", "NE"))
#firecrown_mod <- lm(canopy.inj ~ l_peak.temp_zs*height_zs*l_degsec_zs + unit,
                    #eunits_measure)
#plot(firecrown_mod)
#summary(firecrown_mod)
#Anova(firecrown_mod, test.statistic = "F") #peak.temp positively influence crown.loss