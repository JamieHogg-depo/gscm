####
## create plots
####


source("src/local/funs.R")


data.frame(bfm = out_all[[2]]$summ_latent1$raww$se,
           gscm = out_all[[4]]$summ_latent2$raww$se) %>% 
  ggplot(aes(y = bfm, x = gscm))+
  geom_abline()+
  geom_point()

data.frame(bfm = out_all[[2]]$summ_latent1$raww$point,
           gscm = out_all[[4]]$summ_latent2$raww$point) %>% 
  ggplot(aes(y = bfm, x = gscm))+
  geom_abline()+
  geom_point()

data.frame(bfm = out_all[[7]]$summ_latent1$rankk$se,
           gscm = out_all[[5]]$summ_latent1$rankk$se) %>% 
  mutate(r = bfm/gscm) %>% 
  summary(.)

out_all[[5]]$summ_latent1$rankk %>% 
  arrange(point) %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = 1:nrow(.)))+
  geom_errorbar(col = "grey")+
  geom_point()

out_all[[6]]$summ_latent1$rankk %>% 
  arrange(point) %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = 1:nrow(.)))+
  geom_errorbar(col = "grey")+
  geom_point()