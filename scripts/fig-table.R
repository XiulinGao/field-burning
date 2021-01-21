# fig-table.R
# R script making figures and tables for manuscript
source("./analysis.R")
source("./ggplot-theme.R")

RESULTS <- "../results/"

## fig1
seve.loads <- as.data.frame(loadings(seve_noimpu))
seve.loads
seve.scores <- as.data.frame(scores(seve_noimpu))
var.name <- c("crown scorch", "crown loss")

get_ratio <- function(pcscores, pcloads) {
  # get the ratio for PC1-PC2 biplot
  mult <- min(
    (max(pcscores$PC2) - min(pcscores$PC2)/(max(pcloads$PC2)-min(pcloads$PC2))),
    (max(pcscores$PC1) - min(pcscores$PC1)/(max(pcloads$PC1)-min(pcloads$PC1)))
  )
  return(mult)
}

mult <- get_ratio(seve.scores,seve.loads)

## produce new loading coordinates by the score/loading ratio 
## to propotionally extend loading segments on biplot

seve.loads <- transform(seve.loads,
                           v1 = 0.6 * mult * PC1,
                           v2 = 0.6 * mult * PC2)

## adjust text coordinate for each variable name to avoid overlap of
## text
text.cor <- seve.loads %>% select(v1, v2)

text.cor$v1[2] <- text.cor$v1[2] - 0.01
text.cor$v2[1] <- text.cor$v2[1] - 0.01

fig1 <- ggplot() +
  geom_point(data = seve.scores, aes(x=PC1, y=PC2), alpha = 0.5, shape = 16, 
             size = 1) +
  geom_segment(data = seve.loads, aes(x = 0, y = 0, xend = v1, yend = v2),
               size = 0.3,
               arrow = arrow(length= unit(0.1, "cm")), alpha = 0.75)+
  geom_text(data = text.cor, aes(x=v1, y=v2, label=var.name),
            size = 1.5, vjust=0.5, hjust="inward", color="black") +
  xlab("Principal component 1 (96.0%)") +
  ylab("Principal component 2 (4.0%)") +
  pubtheme.nogridlines + theme(legend.position = "none",
                               axis.title.y = element_text(size = 0.5*textsize),
                               axis.title.x = element_text(size = 0.5*textsize),
                               axis.text.y = element_text(size = 0.5*smsize),
                               axis.text.x = element_text(size = 0.5*smsize),
                               strip.text.x = element_text(family=fontfamily,size = 0.5*textsize),
                               panel.border = element_rect(size = 0.8))

fig1
ggsave("../results/fig1.jpeg", plot = fig1, width = col1, height = 0.7*col1,
       unit = "cm", dpi = 800)

##fig2

fuel1 = expression(1240~g~m^{-2})
fuel2 = expression(620~g~m^{-2})
units3df$fuel.lab = factor(units3df$fuel, labels=c(fuel1,fuel2))

fig2 <- ggplot(units3df, aes(h_ratio, canopy.inj, color = unit)) + 
  geom_boxplot(size = 0.3, outlier.shape = 16, outlier.size = 0.8) +
  facet_grid(.~fuel.lab, labeller = "label_parsed") + 
  labs(x = "", y = "Crown injury index") +
  scale_color_manual(values = schwilkcolors) +
  scale_x_discrete(breaks = c("H", "L"),
                   labels = c("70% fuel above 30cm", "70% fuel on ground")) +
  pubtheme.nogridlines + theme(axis.ticks.x = element_blank(),
                               legend.title = element_blank(),
                               legend.position = "bottom",
                               legend.margin = margin(t = -18),
                               legend.key.size = unit(0.3, "cm"),
                               legend.text = element_text(size = 0.5*smsize),
                               axis.title.y = element_text(size = 0.5*textsize),
                               axis.title.x = element_text(size = 0.5*textsize),
                               axis.text.y = element_text(size = 0.5*smsize),
                               axis.text.x = element_text(size = 0.5*smsize),
                               strip.text.x = element_text(family=fontfamily,size = 0.5*textsize),
                               panel.border = element_rect(size = 0.8))
fig2
ggsave("../results/fig2.jpeg", plot = fig2, width = col1, height = 0.7*col1,
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

################ appendeix tables #################
temp.fuel <- tempsec.sum %>% left_join(treatments, by = "tree.id") %>% 
  select(fuel, h_ratio, location, dur, degsec, peak.temp) %>% 
  group_by(fuel, h_ratio, location) %>% summarise_at(c("dur", "degsec", "peak.temp"),
                                                     list(~mean(., na.rm=TRUE),
                                                          ~sd(., na.rm = TRUE)))
temp.fuel <- temp.fuel %>% filter(fuel %in% c("H", "L"))
tabS2 <- xtable(temp.fuel)
print(tabS2, type = "html", file = file.path(RESULTS, "temp-summary.html"))


tabS3.aov <- xtable(cainj_aov, digits = 3)
print(tabS3.aov, type = "html", file = file.path(RESULTS, "canopy-injury-aov.html"))
tabS3.coef <- xtable(cainj_coef, digits = 3)
print(tabS3.coef, type = "html", file = file.path(RESULTS, "canopy-injury-coef.html"))



