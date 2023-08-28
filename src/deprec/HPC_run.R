## Summarise HPC run

files <- list.files("Z:/gscm/outputs/20230828/r", full.names = T)
files_fl <- files[!str_detect(files, "_f.rds")]
foo <- function(loc){
  temp <- readRDS(loc)
  return(temp$perf)
}
out <- lapply(files_fl, foo)
bind_rows(out) %>% 
  filter(shared_latent_rho_fixed == 2)