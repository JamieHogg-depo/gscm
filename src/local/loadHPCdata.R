####
## Load HPC data
####

# Libraries
library(tidyverse)
rm(list = ls())
gc()
source("src/local/funs.R")

# Set date
cur_date <- c("202310022")
# 202309173 is 24 combos with no scale and no latent fixed
# 202309153 is 24 combos with scale and latent fixed

# list files
files <- list.files(paste0("Z:/gscm/outputs/", cur_date, "/r"), full.names = T)
files_fl <- files[!str_detect(files, "_f.rds|_fitonly.rds|_tr.rds|_ld.rds")]

# read files
out_all <- lapply(files_fl, readRDS)
names(out_all) <- files_fl

# get grid
grid <- bind_rows(lapply(1:length(out_all), FUN = function(x)out_all[[x]]$cur_model_spec), .id = "ix")

# read performance data
perf_ll <- lapply(1:length(out_all), FUN = function(x)out_all[[x]]$perf)
perf <- bind_rows(perf_ll, .id = "ix")

# read convergence data
conv_ll <- lapply(1:length(out_all), FUN = function(x)out_all[[x]]$conv)
conv <- bind_rows(conv_ll, .id = "ix")

## Overall convergence ---- ##
conv_ix <- (conv %>% 
              filter(#max_Rhat < 1.1,
                     n_Rhatgr1.05 == 0,
                     set == "all"))$ix
perf %>% 
  filter(nu_div ==0,
         ix %in% conv_ix,
         nu_bfmi == 0) %>% 
  #mutate(conv_flag = ix %in% conv_ix) %>% 
  view()
## ---- ##

# Convergence plot
out_all[[9]]$summ %>% 
  ggplot(aes(y = rhat, x = variable_gr))+
  geom_boxplot()+
  geom_hline(yintercept = c(1,1.02))+
  ylim(1,1.2)

# compare
conv %>% 
  filter(set == "all") %>% 
  ggplot(aes(y = max_Rhat, x = fo, col = as.factor(specific_latent_rho_fixed)))+
  geom_point()+
  facet_grid(.~as.factor(L))+
  theme_bw()

# ELPD
conv_ix <- (conv %>% 
              filter(set == "all",
                     n_Rhatgr1.05 == 0))$ix
perf %>% 
  filter(nu_div ==0,
         nu_bfmi == 0,
         ix %in% conv_ix,
         nu_tree ==0) %>%
  mutate(lower = elpd_loo - 1.96 * elpd_loo_se,
         upper = elpd_loo + 1.96 * elpd_loo_se) %>% 
  ggplot(aes(y = elpd_loo, ymin = lower, ymax = upper,
             x = ix, col = as.factor(L)))+
  geom_errorbar()+
  geom_point()+
  facet_wrap(shared_latent_rho_fixed~specific_latent_rho_fixed)

ggplot(perf, aes(y = WAIC, x = shared_latent_rho_fixed, col = as.factor(specific_latent_rho_fixed)))+
  geom_point()+
  facet_grid(.~L)

## END SCRIPT ## ---------------------------------------------------------------