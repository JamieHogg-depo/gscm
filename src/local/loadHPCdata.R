####
## Load HPC data
####

# Libraries
library(tidyverse)
library(corrplot)
rm(list = ls())
source("src/local/funs.R")

# Set date
cur_date <- "202309181"
# 202309173 is 24 combos with no scale and no latent fixed
# 202309153 is 24 combos with scale and latent fixed

# list files
files <- list.files(paste0("Z:/gscm/outputs/", cur_date, "/r"), full.names = T)
files_fl <- files[!str_detect(files, "_f.rds|_fitonly.rds")]

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

# Convergence plot
out_all[[15]]$summ %>% 
  ggplot(aes(y = rhat, x = variable_gr))+
  geom_boxplot()+
  geom_hline(yintercept = c(1,1.02))+
  ylim(1,2)

# compare
conv %>% 
  filter(set == "all") %>% 
  ggplot(aes(y = max_Rhat, x = gamma_var_prior, col = as.factor(specific_latent_rho_fixed)))+
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
         nu_tree ==0,
         ix %in% conv_ix) %>% 
  mutate(lower = elpd_loo - 1.96 * elpd_loo_se,
         upper = elpd_loo + 1.96 * elpd_loo_se) %>% 
  ggplot(aes(y = elpd_loo, ymin = lower, ymax = upper,
             x = ix, col = as.factor(specific_latent_rho_fixed)))+
  geom_errorbar()+
  geom_point()+
  facet_grid(.~L)

# Load specific large files
out_full1 <- readRDS("Z:/gscm/outputs/20230904/r/ix1_model_GSCM__L_2__shared1_fitonly.rds")
out_full2 <- readRDS("Z:/gscm/outputs/20230904/r/ix2_model_GSCM__L_2__shared1_fitonly.rds")

bayesplot::mcmc_pairs(out_full2, pars = c("sigma[2]", "psi[2]", "sigma[3]"), transformations = "log")
bayesplot::mcmc_pairs(out_full1, pars = c("sigma[2]", "psi[2]", "sigma[3]"), transformations = "log")

## END SCRIPT ## ---------------------------------------------------------------