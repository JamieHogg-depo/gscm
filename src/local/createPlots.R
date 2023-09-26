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

# loadings
rbind(data.frame(loading = pr$rotation[,1], f = "PC1 - 42%") %>% rownames_to_column("name"),
      data.frame(loading = pr$rotation[,2], f = "PC2 - 30%") %>% rownames_to_column("name")) %>% 
  mutate(dir = ifelse(loading > 0, "pos", "neg"),
         name = JHCW(name)) %>% 
  ggplot(aes(x = loading, y = name, fill = dir))+
  theme_bw()+
  geom_col()+
  scale_fill_manual(values = c("skyblue", "coral"),
                    breaks = c("pos", "neg"))+
  scale_y_discrete(breaks = c("Risky\nalcohol\nconsumption", "Current\nSmoking", "Inadequate\ndiet", 'Inadequate\nphysical\nactivity', "Overweight/\nobese"))+
  labs(y = "", 
       x = "")+
  theme(legend.position = "none",
        text = element_text(size = 8))+
  facet_grid(.~f)
jf$jsave(filename = paste0("pc_loadings.png"),
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

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

## Measurement error ## --------------------------------------------------------

ix_bfm <- 8
ix_gscm <- 9

# equivalence - raw
cbind(
  out_all[[ix_gscm]]$summ_latent1$raww %>% 
    setNames(paste0("gscm_", names(.))),
  out_all[[ix_bfm]]$summ_latent1$raww %>% 
    setNames(paste0("bf_", names(.)))
) %>% 
  ggplot(aes(y = gscm_point, ymin = gscm_lower, ymax = gscm_upper,
             x = bf_point, xmin = bf_lower, xmax = bf_upper))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline(col = "red")+
  labs(y = "Raw factor 1 - scores (1-factor GSCM)",
       x = "Raw factor 1 - scores (1-factor BSFM)")+
  theme(text = element_text(size = 8))
jf$jsave(filename = "equiv_raw_me.png",
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

# equivalence - raw
cbind(
  out_all[[ix_gscm]]$summ_latent1$rankk %>% 
    setNames(paste0("gscm_", names(.))),
  out_all[[ix_bfm]]$summ_latent1$rankk %>% 
    setNames(paste0("bf_", names(.)))
) %>% 
  ggplot(aes(y = gscm_point, ymin = gscm_lower, ymax = gscm_upper,
             x = bf_point, xmin = bf_lower, xmax = bf_upper))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline(col = "red")+
  labs(y = "Raw factor 1 - rank (1-factor GSCM)",
       x = "Raw factor 1 - rank (1-factor BSFM)")+
  theme(text = element_text(size = 8))
jf$jsave(filename = "equiv_rank_me.png",
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

# cleanup
rm(ix_bfm, ix_gscm)

## Multiple shared components ## -----------------------------------------------

grid %>% 
  filter(shared_latent_rho_fixed == 2,
         specific_latent_rho_fixed == 0)
ix_l1 <- 17
ix_l2 <- 18

# equivalence - raw 
cbind(
out_all[[ix_l1]]$summ_latent1$raww %>% 
  setNames(paste0("l1_", names(.))),
out_all[[ix_l2]]$summ_latent1$raww %>% 
  setNames(paste0("l2_", names(.)))
) %>% 
  ggplot(aes(y = l1_point, ymin = l1_lower, ymax = l1_upper,
             x = l2_point, xmin = l2_lower, xmax = l2_upper))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline(col = "red")+
  labs(y = "Raw factor 1 - scores (1-factor GSCM)",
       x = "Raw factor 1 - scores (2-factor GSCM)")+
  theme(text = element_text(size = 8))
jf$jsave(filename = "equiv_raw_multfactors.png",
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

# equivalence - ranked 
cbind(
  out_all[[ix_l1]]$summ_latent1$rankk %>% 
    setNames(paste0("l1_", names(.))),
  out_all[[ix_l2]]$summ_latent1$rankk %>% 
    setNames(paste0("l2_", names(.)))
) %>% 
  ggplot(aes(y = l1_point, ymin = l1_lower, ymax = l1_upper,
             x = l2_point, xmin = l2_lower, xmax = l2_upper))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline(col = "red")+
  labs(y = "Raw factor 1 - ranked (1-factor GSCM)",
       x = "Raw factor 1 - ranked (2-factor GSCM)")+
  theme(text = element_text(size = 8))
jf$jsave(filename = "equiv_rank_multfactors.png",
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

# Uncertainty - raw 
cbind(
  out_all[[ix_l1]]$summ_latent1$raww %>% 
    setNames(paste0("l1_", names(.))),
  out_all[[ix_l2]]$summ_latent1$raww %>% 
    setNames(paste0("l2_", names(.)))
) %>% 
  dplyr::select(l1_se, l2_se) %>% 
  arrange(l2_se) %>% 
  setNames(c("1-factor GSCM",
             "2-factor GSCM")) %>% 
  mutate(x = 1:nrow(.)) %>% 
  pivot_longer(-x) %>% 
  ggplot(aes(y = value, x = x,
             col = name))+
  theme_bw()+
  geom_point()+
  labs(y = "Posterior standard error (raw factor 1)",
       x = "",
       col = "")+
  theme(text = element_text(size = 8))
jf$jsave(filename = "cat_rawse_multfactors.png",
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

# cleanup
rm(ix_l1, ix_l2)

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