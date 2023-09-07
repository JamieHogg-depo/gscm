####
## create plots
####


source("src/local/funs.R")

## Correlation plot ## ---------------------------------------------------------

res <- cor(data)
corrplot(res, type = "upper", 
         tl.col = "black", tl.srt = 45)
jf$jsave()

## END SCRIPT ## ---------------------------------------------------------------

id1x <- 7
id2x <- 8
data.frame(x1 = out_all[[id1x]]$summ_latent1$rankk$point,
           x1_l = out_all[[id1x]]$summ_latent1$rankk$lower,
           x1_u = out_all[[id1x]]$summ_latent1$rankk$upper,
           x2 = out_all[[id2x]]$summ_latent1$rankk$point,
           x2_l = out_all[[id2x]]$summ_latent1$rankk$lower,
           x2_u = out_all[[id2x]]$summ_latent1$rankk$upper) %>% 
  ggplot(aes(y = x1, ymin = x1_l, ymax = x1_u,
             x = x2, xmin = x2_l, xmax = x2_u))+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
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