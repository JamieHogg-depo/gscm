## Wrangle data from ACAriskfactors project

library(tidyverse)
rm(list = ls())

# load data
summsa2all <- readRDS("C:/r_proj/ACAriskfactors/data/summary_files/summsa2all.Rds")

# create temporary dataset with inverse transform
temp <- summsa2all %>% 
  filter(model %in% c("smoking",
                      "overweight",
                      "diet",
                      "alcohol",
                      "activityleiswkpl")) %>% 
  mutate(point = log(mu_median/(1-mu_median)),
         se = sqrt(mu_sd^2/((mu_median * (1 - mu_median))^2))) 

## Convert to required format - prevalence scale #### --------------------------
out <- list()
out$point <- temp %>% 
  dplyr::select(ps_area, SA2, model, mu_median) %>% 
  pivot_wider(names_from = model, values_from = mu_median)
out$sd <- temp %>% 
  dplyr::select(ps_area, SA2, model, mu_sd) %>% 
  pivot_wider(names_from = model, values_from = mu_sd)

# Save new file
saveRDS(out, "data/y_mats.rds"); rm(out)

## Convert to required format - unconstrained scale #### -----------------------

# create list
out <- list()
out$point <- temp %>% 
  dplyr::select(ps_area, SA2, model, point) %>% 
  pivot_wider(names_from = model, values_from = point)
out$sd <- temp %>% 
  dplyr::select(ps_area, SA2, model, se) %>% 
  pivot_wider(names_from = model, values_from = se)

# Save new file
saveRDS(out, "data/y_mats_unc.rds"); rm(out)

## END SCRIPT ## ---------------------------------------------------------------

cor(out$point[,-c(1,2)])
