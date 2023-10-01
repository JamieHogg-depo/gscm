
dt <- "202310011"
ff <- list.files(paste0("Z:/gscm/outputs/", dt, "/lyra_out"), full.names = T)
ll <- list()
for(i in 1:length(ff)){
ll[[i]] <-suppressMessages(read_csv(ff[i]) %>% 
  setNames(c("this")) %>% 
  filter(str_detect(this, "Mem usage|Wall time"))%>% 
  separate(this, into = c("Metric", "Value"), sep = " : ") %>% 
  pivot_wider(names_from = Metric, values_from = Value) %>% 
  setNames(c("wall_time", "mem_usage")))
}

bind_rows(ll) %>% view()

