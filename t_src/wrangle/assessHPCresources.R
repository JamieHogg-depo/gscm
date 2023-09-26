
dt <- "202309251"
ff <- list.files(paste0("Z:/gscm/outputs/", dt, "/lyra_out"))
ll <- list()
for(i in 1:16){
ll[[i]] <-suppressMessages(read_csv(paste0("Z:/gscm/outputs/", dt, "/lyra_out/GSCM_ix", i)) %>% 
  setNames(c("this")) %>% 
  filter(str_detect(this, "Mem usage|Wall time")))
}

