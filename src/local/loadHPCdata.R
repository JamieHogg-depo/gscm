####
## Load HPC data
####

# Libraries
library(tidyverse)
library(corrplot)
rm(list = ls())
source("src/local/funs.R")

# Set date
cur_date <- "20230910"

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
perf <- bind_rows(perf_ll)

# read convergence data
conv_ll <- lapply(1:length(out_all), FUN = function(x)out_all[[x]]$conv)
conv <- bind_rows(conv_ll, .id = "ix")

# Convergence plot
out_all[[1]]$summ %>% 
  ggplot(aes(y = rhat, x = variable_gr))+
  geom_boxplot()+
  geom_hline(yintercept = c(1,1.02))+
  ylim(1,1.2)

# Load specific large files
out_full1 <- readRDS("Z:/gscm/outputs/20230904/r/ix1_model_GSCM__L_2__shared1_fitonly.rds")
out_full2 <- readRDS("Z:/gscm/outputs/20230904/r/ix2_model_GSCM__L_2__shared1_fitonly.rds")

bayesplot::mcmc_pairs(out_full2, pars = c("sigma[2]", "psi[2]", "sigma[3]"), transformations = "log")
bayesplot::mcmc_pairs(out_full1, pars = c("sigma[2]", "psi[2]", "sigma[3]"), transformations = "log")

## END SCRIPT ## ---------------------------------------------------------------