#analysis.R

## load all data required for analysis
source("./all-data.R")

########### fuel treatment effects on fire behavior #############

## as fire behavior measurements only available for
## SE and NE units, filter out those two units 
eunits_measure <- alldata %>% filter(unit %in% c("SE", "NE"))




#modles for degsec at 2 locations, as rep is low, we'll just use general linear
#model
degsecs_mod <- lm(s_degsec ~ h_ratio*fuel + temp + ave_ws, eunits_measure)
Anova(degsecs_mod)
summary(degsecs_mod)
#plot(degsecs_mod)

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

############### plant height, fuel treatment effects on mortality ################

#standardize variables
zscore <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm = TRUE) 

alldata <- alldata %>% mutate_at(c("s_peak.temp", "l_peak.temp", "s_degsec", "l_degsec",
                                   "height","temp", "ave_ws", "crown.loss", "bole.cs30"),
                                 list(zs = zscore))


fueltree_mod <- glm(mortality ~ fuel*h_ratio*height_zs + temp_zs + ave_ws_zs, 
                   filter(alldata, unit!= "NW"), family = binomial)
summary(fueltree_mod)
Anova(fueltree_mod) #tree mortality didn't vary among treatments

#analysis only with NE & SE units
fueltree_eunitmod <- glm(mortality ~ fuel*h_ratio*height_zs + temp_zs + ave_ws_zs,
                         filter(alldata, unit %in% c("NE", "SE")), family = binomial)
summary(fueltree_eunitmod)
Anova(fueltree_eunitmod) #tree mortality didn't vary among treatments


############## plant height, crown.loss effects on mortality ###############

mortality_mod <- glm(mortality ~ height_zs*crown.loss_zs,
                     filter(alldata, unit != "NW"), family = binomial)
summary(mortality_mod)
Anova(mortality_mod)

sjPlot::plot_model(mortality_mod, type = "pred", terms = c("height_zs", "crown.loss_zs"),
                   se=FALSE)

#############  fire behavior effects on crown.loss #############
#model for crown.loss using canopy heating measurement, specifically 
# peak temperature as it relates to flame height

crown_lmmod <- lme4::lmer(crown.loss ~ l_peak.temp_zs*height_zs*l_degsec_zs + 
                            temp_zs + ave_ws_zs + (1|unit), alldata, REML = TRUE)
#plot(crown_lmmod)
summary(crown_lmmod)
Anova(crown_lmmod, test.statistic = "F") #peak.temp positively influence crown.loss

sjPlot::plot_model(crown_lmmod, type="pred", terms = c("l_peak.temp_zs"), 
                   se= FALSE) + #axis.lim = c(0, 100),
                   #color = schwilkcolors, title = "") +
  xlab("Standardized peak temperature at 100cm") +
  ylab("Percentage crown volume loss (%)")

ggplot(alldata, aes(l_peak.temp, crown.loss)) + geom_point()


