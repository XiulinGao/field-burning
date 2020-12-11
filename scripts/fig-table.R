# fig-table.R
# R script making figures and tables for manuscript
source("./analysis.R")
source("./ggplot-theme.R")

RESULTS <- "../results/"

fuel1 = expression(1240~g~m^{-2})
fuel2 = expression(620~g~m^{-2})
units3df$fuel.lab = factor(units3df$fuel, labels=c(fuel1,fuel2))


fig1 <- ggplot(units3df, aes(h_ratio, canopy.inj, color = unit)) + 
  geom_boxplot() +
  facet_grid(.~fuel.lab, labeller = "label_parsed") + 
  labs(x = "", y = "Crown injury index") +
  scale_color_manual(values = schwilkcolors) +
  scale_x_discrete(breaks = c("H", "L"),
                   labels = c("70% fuel above 30cm", "70% fuel on ground")) +
  pubtheme.nogridlines + theme(legend.title = element_blank(),
                                legend.position = "bottom",
                               legend.margin = margin(t = -18))
fig1
ggsave("../results/fig1.jpeg", plot = fig1, width = col2, height = 0.7*col2,
       unit = "cm", dpi = 800)

###################### tables ######################
tab4.aic <- xtable(mortality_aics)
print(tab4.aic, type = "html", file = file.path(RESULTS, "mortality-aics.html"))

tab5mortality.aov <- xtable(mortality_aov, digits = 3)
print(tab5mortality.aov, type = "html", file = file.path(RESULTS, "mortality-aov.html"))
tab5mortality.coef <- xtable(mortality_coef, digits = 3)
print(tab5mortality.coef, type = "html", file = file.path(RESULTS, "mortality-coef.html"))

tab6.aic <- xtable(cainj_aics, digits = 3)
print(tab6.aic, type = "html", file = file.path(RESULTS, "canopy-injury-aics.html"))

tab5injury.aov <- xtable(injmota_aov, digits = 3)
print(tab5injury.aov, type = "html", file = file.path(RESULTS, "injury-mortality-aov.html"))
tab5injury.coef <- xtable(injmota_coef, digits = 3)
print(tab5injury.coef, type = "html", file = file.path(RESULTS, "injury-mortality-coef.html"))

tab7.aov <- xtable(cainj_aov, digits = 3)
print(tab7.aov, type = "html", file = file.path(RESULTS, "canopy-injury-aov.html"))
tab7.coef <- xtable(cainj_coef, digits = 3)
print(tab7.coef, type = "html", file = file.path(RESULTS, "canopy-injury-coef.html"))



