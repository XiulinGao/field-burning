#analysis.R

## load all data required for analysis
source("./all-data.R")

############### fire severity effects on mortality ################

#standardize variables
zscore <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm = TRUE) 

alldata <- alldata %>% mutate_at(c("s_peak.temp", "l_peak.temp", "s_degsec", "l_degsec",
                                   "height","temp", "ave_ws", "canopy.inj", "bole.cs30"),
                                 list(zs = zscore))

#H0: mortality is burn unit effect only
mortality_mod0 <- glm(mortality ~ unit + height_zs, 
                    filter(alldata, unit!= "NW"), family = binomial)
AICc(mortality_mod0, second.ord = TRUE)

#H1: mortality is affected by canopy injury
mortality_mod1 <- glm(mortality ~ canopy.inj_zs + unit + height_zs, 
                      filter(alldata, unit !="NW"), family = binomial)
AICc(mortality_mod1, second.ord = TRUE)

#H2: mortality is determined by stem injury
mortality_mod2 <- glm(mortality ~ bole.cs30_zs + unit + height_zs, 
                      filter(alldata, unit !="NW"), family = binomial)
AICc(mortality_mod2, second.ord = TRUE)

#h3: mortality is determined by both canopy and stem injuries w/o interacting effect
mortality_mod3 <- glm(mortality ~ bole.cs30_zs + canopy.inj_zs + unit + height_zs, 
                      filter(alldata, unit !="NW"), family = binomial)
AICc(mortality_mod3, second.ord = TRUE)

#h4: mortality is determined by both canopy and stem injuries w/ interacting effect
mortality_mod4 <- glm(mortality ~ bole.cs30_zs*canopy.inj_zs + unit + height_zs, 
                      filter(alldata, unit !="NW"), family = binomial)
AICc(mortality_mod4,  second.ord = TRUE)

#model 1? it is determined by canopy injury
summary(mortality_mod1)

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

############# fuel treatment effects on crown injury #############
#H0: crown injury is only unit effect
fuelcrown_mod0 <- lm(canopy.inj ~ unit, filter(alldata, unit !="NW"))
AICc(fuelcrown_mod0, second.ord = TRUE)

#H1: crown injury is fuel load effect
fuelcrown_mod1 <- lm(canopy.inj ~ fuel + unit, filter(alldata, unit !="NW"))
AICc(fuelcrown_mod1, second.ord = TRUE)

#H2: crown injury is fuel structure effect
fuelcrown_mod2 <- lm(canopy.inj ~ h_ratio + unit, filter(alldata, unit !="NW"))
AICc(fuelcrown_mod2, second.ord = TRUE)

#H3: crown injury is both fuel load and structure effect w/o interaction
fuelcrown_mod3 <- lm(canopy.inj ~ fuel + h_ratio + unit, filter(alldata, unit !="NW"))
AICc(fuelcrown_mod3, second.ord = TRUE)

#H4: crown injury is both fuel load and structure effect w/ interaction
fuelcrown_mod4 <- lm(canopy.inj ~ fuel*h_ratio + unit, filter(alldata, unit !="NW"))
AICc(fuelcrown_mod4, second.ord = TRUE)

summary(fuelcrown_mod0)
summary(fuelcrown_mod1)
summary(fuelcrown_mod4)


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

#############  how fire behavior influence crown injury #############

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




########### fuel treatment effects on fire behavior #############

## as fire behavior measurements only available for
## SE and NE units, filter out those two units 


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

#models for peak.temp at 2 locations
peaks_mod <- lm(s_peak.temp ~ fuel*h_ratio + temp + ave_ws, eunits_measure)
Anova(peaks_mod)
summary(peaks_mod)

peakl_mod <- lm(l_peak.temp ~ fuel*h_ratio + temp + ave_ws, eunits_measure)
Anova(peakl_mod)
summary(peakl_mod)
#plot(peakl_mod)

#only fuel load influence degsec and peak temperatu 
#at the soil surface not at 100cm location

