####
## Load HPC data
####

# Libraries
library(tidyverse)
source("src/funs.R")

# Set date
cur_date <- "20230901"

# list files
files <- list.files(paste0("Z:/gscm/outputs/", cur_date, "/r"), full.names = T)
files_fl <- files[!str_detect(files, "_f.rds|_fitonly.rds")]

# read files
all <- lapply(files_fl, readRDS)
names(all) <- files_fl
out_all <- lapply(files_fl, readRDS)

# get grid
grid <- bind_rows(lapply(1:length(out_all), FUN = function(x)out_all[[x]]$cur_model_spec))

# read performance data
perf_ll <- lapply(1:length(out_all), FUN = function(x)out_all[[x]]$perf)
perf <- bind_rows(perf_ll)

# read convergence data
conv_ll <- lapply(1:length(out_all), FUN = function(x)out_all[[x]]$conv)
conv <- bind_rows(conv_ll, .id = "ix")

# Convergence plot
out_all[[13]]$summ %>% ggplot(aes(y = rhat, x = variable_gr))+geom_boxplot()

# Load specific large files
out_full <- readRDS("Z:/gscm/outputs/20230901/r/model_GSCM__L_2__shared1_ix10_fitonly.rds")

## END SCRIPT ## ---------------------------------------------------------------
