####
## create plots
####

source("src/local/funs.R")

## Moran's I ## ----------------------------------------------------------------

listw <- mat2listw(W)
mi <- unlist(lapply(1:5, FUN = function(x)moran.mc(unlist(data[,x]), listw = listw, nsim = 999)$p.value))

## PCA - scree plot ## ---------------------------------------------------------

pr <- prcomp(data, scale. = T)
summary(pr)
pr$rotation
plot(pr)

barplot(pr$rotation[,1], main="PC 1 Loadings", las=2)
barplot(pr$rotation[,2], main="PC 2 Loadings", las=2)

## Correlation plot ## ---------------------------------------------------------

# rename columns of data
data2 <- data %>% 
  rename("Current\nSmoking" = smoking,
         'Inadequate\nphysical\nactivity' = activityleiswkpl,
         "Inadequate\ndiet" = diet,
         "Risky\nalcohol\nconsumption" = alcohol,
         "Overweight/\nobese" = overweight)

res <- cor(data2)
ggcorrplot(res, hc.order = TRUE,
           outline.col = "white", lab = TRUE,
           colors = c("#6D9EC1", "white", "#E46726"),
           tl.cex = 8)
jf$jsave(filename = paste0("cor.png"),
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

## Caterpillar plots - rank ## -------------------------------------------------

ix1$summ_latent1$rank %>% 
  cbind(.,ix1$data$census) %>% 
  arrange(point) %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = 1:nrow(.)))+
  geom_errorbar(col = "grey")+
  geom_point()+
  labs(y = "Posterior ranks",
       x = "")+
  theme_bw()+
  theme(text = element_text(size = 8))+
  geom_hline(yintercept = c(5, 95), linetype = "dotted")

## EP plots ## ----------------------------------------------------------------

as.data.frame(ix1$EP) %>% 
  arrange(V1) %>% 
  ggplot(aes(y = V1,
             x = 1:nrow(.)))+
  geom_point()+
  labs(y = "Posterior ranks",
       x = "")+
  theme_bw()+
  theme(text = element_text(size = 8))+
  geom_hline(yintercept = c(0.025, 0.975), linetype = "dotted")


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