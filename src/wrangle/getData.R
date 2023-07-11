## Wrangle data from ACAriskfactors project

library(tidyverse)

# load data
summsa2all <- readRDS("C:/r_proj/ACAriskfactors/data/summary_files/summsa2all.Rds")

# Convert to required format
out <- list()
out$point <- summsa2all %>% 
  dplyr::select(ps_area, SA2, model, mu_median) %>% 
  pivot_wider(names_from = model, values_from = mu_median)
out$sd <- summsa2all %>% 
  dplyr::select(ps_area, SA2, model, mu_sd) %>% 
  pivot_wider(names_from = model, values_from = mu_sd)

# Save new file
saveRDS(out, "data/y_mats.rds")

## END SCRIPT ## ---------------------------------------------------------------

cor(out$point[,-c(1,2)])
