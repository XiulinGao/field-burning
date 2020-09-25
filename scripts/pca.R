##pca.R
## do pca to select variables for further analysis
source("./all-data.R")

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
