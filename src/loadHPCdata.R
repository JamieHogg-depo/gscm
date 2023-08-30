####
## Load HPC data
####

# Libraries
library(tidyverse)

# Set date
cur_date <- "20230830"

# list files
files <- list.files(paste0("Z:/gscm/outputs/", cur_date, "/r"), full.names = T)
files_fl <- files[!str_detect(files, "_f.rds")]

# read out files
all <- lapply(files_fl, readRDS)
names(all) <- files_fl

# read performance data
foo <- function(loc){
  temp <- readRDS(loc)$perf %>% 
    mutate(model = ifelse(str_detect(loc, "BFM"), "BFM", "GSCM")) %>% 
    relocate(model)
  return(temp)
}
out <- bind_rows(lapply(files_fl, foo))

# Load specific large files
out_l <- readRDS(paste0("Z:/gscm/outputs/", cur_date, "/r/GSCM_L_1__shared_latent_rho_fixed_0__specific_latent_rho_fixed_0_ix1_f.rds"))
