#analysis.R

## load all data required for analysis
source("./all-data.R")

############### fuel treatment effects on mortality ################

#standardize variables
zscore <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm = TRUE) 

alldata <- alldata %>% mutate_at(c("s_peak.temp", "l_peak.temp", "s_degsec", "l_degsec",
                                   "height","temp", "ave_ws", "canopy.inj", "bole.cs30"),
                                 list(zs = zscore))

#filter out NW unit as it burned cold
units3df <- filter(alldata, unit!="NW")

#H0: mortality is burn unit and tree size effect 
mortality_mod0 <- glm(mortality ~ unit + height, 
                    units3df, family = binomial(link = "logit"))
AICc(mortality_mod0, second.ord = TRUE)

#H1: mortality is affected by fuel load
mortality_mod1 <- glm(mortality ~ fuel*height + unit, 
                      units3df, family = binomial(link = "logit"))
AICc(mortality_mod1, second.ord = TRUE)

#H2: mortality is affected by fuel structure 
mortality_mod2 <- glm(mortality ~  h_ratio*height + unit,
                      units3df, family = binomial(link = "logit"))
AICc(mortality_mod2, second.ord = TRUE)

#h3: mortality is determined by both fuel load and structure w/o interacting effect
mortality_mod3 <- glm(mortality ~ fuel*h_ratio*height - fuel:h_ratio - 
                        fuel:h_ratio:height + unit, 
                      units3df, family = binomial(link = "logit"))
AICc(mortality_mod3, second.ord = TRUE)

#h4: mortality is determined by both fuel load and structure w/ interacting effect
mortality_mod4 <- glm(mortality ~ fuel*h_ratio*height + unit, 
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
aictab(cand.set = cand.models1, modnames = mod_names1, second.ord = TRUE, 
       sort = TRUE)


#model 0&2? it is determined by canopy injury
summary(mortality_mod0)
summary(mortality_mod2)

Anova(mortality_mod0)
Anova(mortality_mod2)


#### need to calculate persudo R^2 (McFadden's R squared) 
### how to calculate McFadden's R squared for logistic regression
### see: https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/

nullmod <- glm(mortality ~ 1, units3df, family = binomial)
persudor2_mod0 <- 1 - logLik(nullmod)/logLik(mortality_mod0)
persudor2_mod0

persudor2_mod2 <- 1 - logLik(nullmod)/logLik(mortality_mod2)
persudor2_mod2


############# fuel treatment effects on crown injury #############
#H0: crown injury is only unit and tree size effect
fuelcrown_mod0 <- lm(canopy.inj ~ unit + height_zs , units3df)
AICc(fuelcrown_mod0, second.ord = TRUE)

#H1: crown injury is fuel load effect
fuelcrown_mod1 <- lm(canopy.inj ~ fuel*height_zs + unit, units3df)
AICc(fuelcrown_mod1, second.ord = TRUE)

#H2: crown injury is fuel structure effect
fuelcrown_mod2 <- lm(canopy.inj ~ h_ratio*height_zs + unit, units3df)
AICc(fuelcrown_mod2, second.ord = TRUE)

#H3: crown injury is both fuel load and structure effect w/o interaction
fuelcrown_mod3 <- lm(canopy.inj ~ fuel*h_ratio*height_zs - fuel:h_ratio - 
                       fuel:h_ratio:height_zs + unit, units3df)
AICc(fuelcrown_mod3, second.ord = TRUE)

#H4: crown injury is both fuel load and structure effect w/ interaction
fuelcrown_mod4 <- lm(canopy.inj ~ fuel*h_ratio*height_zs + unit, units3df)
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
aictab(cand.set = cand.models, modnames = mod_names, second.ord = TRUE, 
       sort = TRUE)
#mod 0 & 1

summary(fuelcrown_mod0)
summary(fuelcrown_mod1)

Anova(fuelcrown_mod0)
Anova(fuelcrown_mod1)


#############  fuel treatment effects on stem injury #############
#H0: stem injury is only unit effect
fuelstem_mod0 <- lm(bole.cs30 ~ unit, units3df)

#H1: stem injury is fuel load effect
fuelstem_mod1 <- lm(bole.cs30 ~ fuel + unit, units3df)

#H2: stem injury is fuel structure effect
fuelstem_mod2 <- lm(bole.cs30 ~ h_ratio + unit, units3df)

#H3: stem injury is both fuel load and structure effect w/o interaction
fuelstem_mod3 <- lm(bole.cs30 ~ fuel + h_ratio + unit, units3df)

#H4: stem injury is both fuel load and structure effect w/ interaction
fuelstem_mod4 <- lm(bole.cs30 ~ fuel*h_ratio + unit, units3df)

## create model selection tables
# set up candidate models

stem.models <- list()
stem.models[[1]] <- fuelstem_mod0
stem.models[[2]] <- fuelstem_mod1
stem.models[[3]] <- fuelstem_mod2
stem.models[[4]] <- fuelstem_mod3
stem.models[[5]] <- fuelstem_mod4

#create a vector of names to track each model
stemmod_names <- paste("mod", 1:length(stem.models), sep = '')
aictab(cand.set = stem.models, modnames = stemmod_names, second.ord = TRUE, 
       sort = TRUE)
#mod 0-2

summary(fuelstem_mod0)
summary(fuelstem_mod1)
summary(fuelstem_mod2)

Anova(fuelstem_mod0)
Anova(fuelstem_mod1)
Anova(fuelstem_mod2)


#model for canopy.inj using canopy heating measurement, specifically 
# peak temperature as it relates to flame height

## as there are only fire behavior measurements for the 2 east units, 
## filter out east unit only data
eunits_measure <- alldata %>% filter(unit %in% c("SE", "NE"))
firecrown_mod <- lm(canopy.inj ~ l_peak.temp_zs*height_zs*l_degsec_zs + unit,
                    eunits_measure)
#plot(firecrown_mod)
summary(firecrown_mod)
Anova(firecrown_mod, test.statistic = "F") #peak.temp positively influence crown.loss

sjPlot::plot_model(crown_lmmod, type="pred", terms = c("l_peak.temp_zs"), 
                   se= FALSE) + #axis.lim = c(0, 100),
  #color = schwilkcolors, title = "") +
  xlab("Standardized peak temperature at 100cm") +
  ylab("Percentage crown volume loss (%)")

ggplot(alldata, aes(l_peak.temp, crown.loss)) + geom_point()



#plot
ratiolabel <- c("70% fuel above ground", "70% fuel on ground")
names(ratiolabel) <- c("H", "L")

ggplot(eunits_measure, aes(fuel, s_degsec)) + geom_boxplot() +
  facet_grid(.~h_ratio, labeller = labeller(h_ratio = ratiolabel)) + 
  xlab("Fuel load treatment") + ylab("Integrated temperature at soil surface") 

degsecl_mod <- lm(l_degsec ~ fuel*h_ratio + temp + ave_ws, eunits_measure)
Anova(degsecl_mod)
summary(degsecl_mod)
#plot(degsecl_mod)

ggplot(eunits_measure, aes(fuel, l_degsec)) + geom_boxplot() +
  facet_grid(.~h_ratio, labeller = labeller(h_ratio = ratiolabel)) + 
  xlab("Fuel load treatment") + ylab("Integrated temperature at 100cm") 

