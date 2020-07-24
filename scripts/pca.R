##pca.R
## do pca to select variables for further analysis
source("./all-data.R")

library(pcaMethods)

# pca of flammability measurements w/o imputation
flam_noimpu <- tempsec.sum %>% 
  select(degsec, dur, peak.temp) %>% 
  pca(method = "svd", nPcs = 3, scale = "uv")
  
summary(flam_noimpu)
biplot(flam_noimpu)
flam_load <- loadings(flam_noimpu)
flam_load #so select degsec & peak.temp for further analysis

# pca of trait measurements w/o imputation
#trait_noimpu <- traits %>% select(dbh, height) %>% 
  #pca(method = "svd", nPcs = 2, scale = "none") 
#summary(trait_noimpu)
#trait_load <- loadings(trait_noimpu)
#trait_load #or course height....

#pca of post-fire measurements w/o imputation
#seve_noimpu <- postfire_measures %>% 
  #select(bole.ch, bole.cs30,crown.scorch, crown.loss) %>% 
  #pca(method = "svd", nPcs = 4, scale = "none")
#summary(seve_noimpu)
#biplot(seve_noimpu)
#seve_load <- loadings(seve_noimpu)
#seve_load #bole.cs30?
