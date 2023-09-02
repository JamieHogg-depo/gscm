####
## Load HPC data
####

# Libraries
library(tidyverse)

# Set date
cur_date <- "20230901"

# list files
files <- list.files(paste0("Z:/gscm/outputs/", cur_date, "/r"), full.names = T)
files_fl <- files[!str_detect(files, "_f.rds|_fitonly.rds")]

# read files
all <- lapply(files_fl, readRDS)
names(all) <- files_fl
out_all <- lapply(files_fl, readRDS)

# read performance data
perf_ll <- lapply(1:length(out_all), FUN = function(x)out_all[[x]]$perf)
perf <- bind_rows(perf_ll)

# read convergence data
conv_ll <- lapply(1:length(out_all), FUN = function(x)out_all[[x]]$conv)
conv <- bind_rows(conv_ll)

# Load specific large files
out_l <- readRDS(paste0("Z:/gscm/outputs/", cur_date, "/r/GSCM_L_1__shared_latent_rho_fixed_0__specific_latent_rho_fixed_0_ix1_f.rds"))

## END SCRIPT ## ---------------------------------------------------------------
