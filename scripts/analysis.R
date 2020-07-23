#analysis.R

## load all data required for analysis
source("./all-data.R")

########### fuel treatment effects on fire behavior #############

## as fire behavior measurements only available for
## SE and NE units, filter out those two units 
eunits_measure <- alldata %>% filter(unit %in% c("SE", "NE"))

#modles for degsec at 2 locations, as rep is low, we'll just use general linear
#model
degsecs_mod <- lm(s_degsec ~ h_ratio*fuel + temp, eunits_measure)
Anova(degsecs_mod)
summary(degsecs_mod)

#plot
ratiolabel <- c("70% fuel above ground", "70% fuel on ground")
names(ratiolabel) <- c("H", "L")

ggplot(eunits_measure, aes(fuel, s_degsec)) + geom_boxplot() +
  facet_grid(.~h_ratio, labeller = labeller(h_ratio = ratiolabel)) + 
  xlab("Fuel load treatment") + ylab("Integrated temperature at soil surface") 

degsecl_mod <- lm(l_degsec ~ fuel*h_ratio + temp, eunits_measure)
Anova(degsecl_mod)
summary(degsecl_mod)

ggplot(eunits_measure, aes(fuel, l_degsec)) + geom_boxplot() +
  facet_grid(.~h_ratio, labeller = labeller(h_ratio = ratiolabel)) + 
  xlab("Fuel load treatment") + ylab("Integrated temperature at 100cm") 

#models for peak.temp at 2 locations
peaks_mod <- lm(s_peak.temp ~ fuel*h_ratio + temp, eunits_measure)
Anova(peaks_mod)

peakl_mod <- lm(l_peak.temp ~ fuel*h_ratio + temp, eunits_measure)
Anova(peakl_mod)

#only fuel load influence degsec and peak temperatu 
#at the soil surface not at 100cm location

############### plant traits, fuel treatment effects on fire damage ################

#standardize variables
zscore <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm = TRUE) 

alldata <- alldata %>% mutate_at(c("s_peak.temp", "l_peak.temp", "s_degsec", "l_degsec",
                                   "height", "s_degsec", "l_degsec", "s_dur", "l_dur",
                                         "temp", "ave_ws", "height", "dbh"),
                                 list(zs = zscore))

#fuel treatment and crown volume scorched
fuelseve_mod <- lm(crown.scorch ~ fuel*h_ratio*height*dbh - fuel:h_ratio:height:dbh + unit, 
                    alldata)
summary(fuelseve_mod)
Anova(fuelseve_mod)
#plot(fuelseve_mod)

ggplot(alldata, aes(fuel, crown.scorch)) + geom_boxplot() +
  facet_grid(.~h_ratio, labeller = labeller(h_ratio = ratiolabel)) +
  xlab("Fuel load treatment") + ylab("Crown volume scorched (%)") 

ggplot(alldata, aes(unit, crown.scorch)) + geom_boxplot() +
  xlab("Burn unit") + ylab("Crown volume scorched (%)") 


#model for crown.scorch using canopy heating measurement, specifically 
# peak temperature as it relates to flame height
crown_lmmod <- lme4::lmer(crown.scorch ~ l_peak.temp_zs*height_zs*l_degsec_zs + (1|unit), 
                          alldata, REML = TRUE)
#plot(crown_lmmod)
summary(crown_lmmod)
Anova(crown_lmmod, test.statistic = "F")

sjPlot::plot_model(crown_lmmod, type="pred", terms = c("l_peak.temp_zs", "l_degsec_zs"), 
                   se= FALSE, #axis.lim = c(0, 100),
                   #color = schwilkcolors, title = "", 
                   legend.title = "Standardized degsec") +
  xlab("Standardized peak temperature at 100cm") +
  ylab("Percentage crown volume scorched (%)") 


