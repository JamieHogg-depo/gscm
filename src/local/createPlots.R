####
## create plots
####

## Moran's I ## ----------------------------------------------------------------

listw <- mat2listw(W)
mi <- unlist(lapply(1:5, FUN = function(x)moran.mc(unlist(data[,x]), listw = listw, nsim = 999)$p.value))

## PCA - scree plot ## ---------------------------------------------------------

pr <- prcomp(data, scale. = T)
summary(pr)
pr$rotation
plot(pr)

# loadings
plt <- rbind(data.frame(loading = pr$rotation[,1], f = "PC1 - 42%") %>% rownames_to_column("name"),
      data.frame(loading = pr$rotation[,2], f = "PC2 - 30%") %>% rownames_to_column("name")) %>% 
  mutate(dir = ifelse(loading > 0, "pos", "neg"),
         name = JHCW(name)) %>% 
  ggplot(aes(x = loading, y = name, fill = dir))+
  theme_bw()+
  geom_col()+
  xlim(-0.8,0.8)+
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
         plot = plt,
         square = F,
         square_size = 1200,
         dpi = 300)

plt + theme(text = element_text(size = 12))
jf$jsave(filename = paste0("pc_loadings2.png"),
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

## Pairs plot - rank sum, modeled indices and principal components ## ----------

data.frame(raw_RS = cut_number(raw_RS, n = 100, label = FALSE), 
           PC1 = cut_number(pr$x[,1], n = 100, label = FALSE),
           PC2 = cut_number(pr$x[,2], n = 100, label = FALSE),
           F1 = cur_list$summ_latent1$perc$point,
           F2 = cur_list$summ_latent2$perc$point,
           Combined = cur_list$summ_latent3$perc$point,
           CombinedPW = cur_list$summ_latent4$perc$point,
           ra = census$ra_sa2_3c) %>% 
  GGally::ggpairs(.,
                  columns = 1:7,
                  #ggplot2::aes(color = ra)
                  columnLabels = c("Rank Sum", 
                                   "PC1", 
                                   "PC2",
                                   "Index 1",
                                   "Index 2",
                                   "Index 3 -\nCombined",
                                   "Index 3 -\nCombined PW"),
                  lower = list(continuous = wrap("points", size=0.05)))+
  theme_bw()+
  theme(text = element_text(size = 8))
jf$jsave(filename = paste0("pairs_pc.png"),
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

## Pairs plot - modeled indices ## ---------------------------------------------

data.frame(F1 = cur_list$summ_latent1$perc$point,
           F2 = cur_list$summ_latent2$perc$point,
           Combined = cur_list$summ_latent3$perc$point,
           CombinedPW = cur_list$summ_latent4$perc$point) %>% 
  GGally::ggpairs(.,
                  columnLabels = c("Index 1",
                                   "Index 2",
                                   "Index 3 -\nCombined",
                                   "Index 3 -\nCombined PW"),
                  lower = list(continuous = wrap("points", size=0.05)))+
  theme_bw()+
  theme(text = element_text(size = 8))
jf$jsave(filename = paste0("pairsperc_indexonly.png"),
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

## Pairs - observed data ## ----------------------------------------------------

y_mats$point[,-c(1,2)] %>% 
  rename("Current\nSmoking" = smoking,
         'Inadequate\nphysical\nactivity' = activityleiswkpl,
         "Inadequate\ndiet" = diet,
         "Risky\nalcohol\nconsumption" = alcohol,
         "Overweight/\nobese" = overweight) %>% 
  mutate(`Factor 1` = cur_list$summ_latent1$raww$point,
         `Factor 2` = cur_list$summ_latent2$raww$point,
         Combined = cur_list$summ_latent3$raww$point,
         `Combined - weighted` = cur_list$summ_latent4$perc$point,
         ra = census$ra_sa2_3c) %>% 
  GGally::ggpairs(.,
                  columns = 1:9)+
  theme_bw()+
  theme(text = element_text(size = 5))
jf$jsave(filename = paste0("pairs_observed_factors.png"),
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

## Scatter - Alcohol ## --------------------------------------------------------

y_mats$point[,-c(1,2)] %>% 
  mutate(`Factor 1` = cur_list$summ_latent1$raww$point,
         `Factor 2` = cur_list$summ_latent2$raww$point,
         Combined = cur_list$summ_latent3$raww$point,
         ra = census$ra_sa2_3c) %>% 
  ggplot(aes(y = alcohol, x = `Factor 1`, col = ra))+
  geom_point()+
  theme_bw()+
  theme(text = element_text(size = 8),
        legend.position = "bottom")+
  labs(y = "Risky alcohol consumption (proportion)",
       col = "")
jf$jsave(filename = paste0("pairs_alcohol_f1.png"),
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

y_mats$point[,-c(1,2)] %>% 
  mutate(`Factor 1` = cur_list$summ_latent1$raww$point,
         `Factor 2` = cur_list$summ_latent2$raww$point,
         Combined = cur_list$summ_latent3$raww$point,
         ra = census$ra_sa2_3c) %>% 
  ggplot(aes(y = alcohol, x = `Factor 2`, col = ra))+
  geom_point()+
  theme_bw()+
  theme(text = element_text(size = 8),
        legend.position = "bottom")+
  labs(y = "Risky alcohol consumption (proportion)",
       col = "")
jf$jsave(filename = paste0("pairs_alcohol_f2.png"),
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

## Scatter - smoking ## --------------------------------------------------------

y_mats$point[,-c(1,2)] %>% 
  mutate(`Factor 1` = cur_list$summ_latent1$raww$point,
         `Factor 2` = cur_list$summ_latent2$raww$point,
         Combined = cur_list$summ_latent3$raww$point,
         ra = census$ra_sa2_3c) %>% 
  ggplot(aes(y = smoking, x = `Factor 1`, col = ra))+
  geom_point()+
  theme_bw()+
  theme(text = element_text(size = 8),
        legend.position = "bottom")+
  labs(y = "Current smoking (proportion)",
       col = "")
jf$jsave(filename = paste0("pairs_smoking_f1.png"),
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

y_mats$point[,-c(1,2)] %>% 
  mutate(`Factor 1` = cur_list$summ_latent1$raww$point,
         `Factor 2` = cur_list$summ_latent2$raww$point,
         Combined = cur_list$summ_latent3$raww$point,
         ra = census$ra_sa2_3c) %>% 
  ggplot(aes(y = smoking, x = `Factor 2`, col = ra))+
  geom_point()+
  theme_bw()+
  theme(text = element_text(size = 8),
        legend.position = "bottom")+
  labs(y = "Current smoking (proportion)",
       col = "")
jf$jsave(filename = paste0("pairs_smoking_f2.png"),
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

## Correlation plot - observed data ## -----------------------------------------

# rename columns of data
data2 <- data %>% 
  rename("Current\nSmoking" = smoking,
         'Inadequate\nphysical\nactivity' = activityleiswkpl,
         "Inadequate\ndiet" = diet,
         "Risky\nalcohol\nconsumption" = alcohol,
         "Overweight/\nobese" = overweight)

res <- cor(data2)
ggcorrplot::ggcorrplot(res, hc.order = TRUE,
           outline.col = "white", lab = TRUE,
           colors = c("#6D9EC1", "white", "#E46726"),
           tl.cex = 8)
jf$jsave(filename = paste0("cor_observed.png"),
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)
rm(data2)

## Correlation plot - observed data with latent factors ## ---------------------

# rename columns of data
data2 <- data %>% 
  rename("Current\nSmoking" = smoking,
         'Inadequate\nphysical activity' = activityleiswkpl,
         "Inadequate\ndiet" = diet,
         "Risky alcohol\nconsumption" = alcohol,
         "Overweight/\nobese" = overweight) %>% 
  mutate(`Rank Sum` = raw_RS,
         PC1 = pr$x[,1],
         PC2 = pr$x[,2],
         `Index 1` = cur_list$summ_latent1$raww$point,
         `Index 2` = cur_list$summ_latent2$raww$point,
         `Index 3` = cur_list$summ_latent3$raww$point,
         `Index 4` = cur_list$summ_latent4$raww$point)

res <- cor(data2)
ggcorrplot::ggcorrplot(res, #hc.order = TRUE,
                       outline.col = "white", #lab = TRUE,
                       colors = c("#6D9EC1", "white", "#E46726"),
                       tl.cex = 4)
jf$jsave(filename = paste0("cor_withfactors.png"),
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)
rm(data2)

## Measurement error ## --------------------------------------------------------

grid %>% 
  filter(L == 2,
         shared_latent_rho_fixed == "LCAR",
         specific_latent_rho_fixed == "IID")
ix_bfm <- 1
ix_gscm <- 16

#### equivalence - raw - factor 1
eqv_raw_1 <- cbind(
  out_all[[ix_gscm]]$summ_latent1$raww %>% 
    setNames(paste0("gscm_", names(.))),
  out_all[[ix_bfm]]$summ_latent1$raww %>% 
    setNames(paste0("bf_", names(.)))
) %>% 
  mutate(abs = abs(gscm_point - bf_point),
         ra = cur_list$data$census$ra_sa2,
         irsd = cur_list$data$census$ABS_irsd_decile_nation_complete) %>% 
  ggplot(aes(y = gscm_point, ymin = gscm_lower, ymax = gscm_upper,
             x = bf_point, xmin = bf_lower, xmax = bf_upper))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline(col = "red")+
  labs(y = "Factor 1 - raw scores (with ME)",
       x = "Factor 1 - raw scores (no ME)")+
  theme(text = element_text(size = 8))+
  ylim(-5,5)+xlim(-5,5)

## se - raw - factor 1
se_raw_1 <- cbind(
  out_all[[ix_gscm]]$summ_latent1$raww %>% 
    setNames(paste0("gscm_", names(.))),
  out_all[[ix_bfm]]$summ_latent1$raww %>% 
    setNames(paste0("bf_", names(.)))
) %>% 
  dplyr::select(bf_se, gscm_se) %>% 
  arrange(bf_se) %>% 
  setNames(c("no ME",
             "with ME")) %>% 
  mutate(x = 1:nrow(.)) %>% 
  pivot_longer(-x) %>% 
  ggplot(aes(y = value, x = x,
             col = name))+
  theme_bw()+
  geom_point()+
  labs(y = "Posterior standard error (factor 1)",
       x = "",
       col = "")+
  theme(text = element_text(size = 8),
        legend.position = "bottom")

## Combine
lay <- rbind(c(1,2))
full_inset_plt <- gridExtra::arrangeGrob(grobs = list(eqv_raw_1, se_raw_1), 
                                         layout_matrix  = lay)
full_inset_plt <- ggpubr::as_ggplot(full_inset_plt)+
  cowplot::draw_plot_label(label = c("(a)", "(b)"), size = 8,
                           x = c(0, 0.5), y = c(1, 1)) # Add labels

## save
jf$jsave(filename = "equiv1_raw_me.png",
         plot = full_inset_plt,
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

## Cleanup 
rm(se_raw_1, eqv_raw_1, full_inset_plt, lay)

#### equivalence - rank - factor 1
eqv_rank_1 <- cbind(
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
  labs(y = "Factor 1 - rank (with ME)",
       x = "Factor 1 - rank (no ME)")+
  theme(text = element_text(size = 8))

## se - rank - factor 1
se_rank_1 <- cbind(
  out_all[[ix_gscm]]$summ_latent1$rankk %>% 
    setNames(paste0("gscm_", names(.))),
  out_all[[ix_bfm]]$summ_latent1$rankk %>% 
    setNames(paste0("bf_", names(.)))
) %>% 
  dplyr::select(bf_se, gscm_se) %>% 
  arrange(bf_se) %>% 
  setNames(c("no ME",
             "with ME")) %>% 
  mutate(x = 1:nrow(.)) %>% 
  pivot_longer(-x) %>% 
  ggplot(aes(y = value, x = x,
             col = name))+
  theme_bw()+
  geom_point()+
  labs(y = "Posterior standard error (factor 1 - rank)",
       x = "",
       col = "")+
  theme(text = element_text(size = 8),
        legend.position = "bottom")

## Combine
lay <- rbind(c(1,2))
full_inset_plt <- gridExtra::arrangeGrob(grobs = list(eqv_rank_1, se_rank_1), 
                                         layout_matrix  = lay)
full_inset_plt <- ggpubr::as_ggplot(full_inset_plt)+
  cowplot::draw_plot_label(label = c("(a)", "(b)"), size = 8,
                           x = c(0, 0.5), y = c(1, 1)) # Add labels

## save
jf$jsave(filename = "equiv1_rank_me.png",
         plot = full_inset_plt,
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

## Cleanup 
rm(se_rank_1, eqv_rank_1, full_inset_plt, lay)

#### equivalence - raw - Factor 2
eqv_raw_2 <- cbind(
  out_all[[ix_gscm]]$summ_latent2$raww %>% 
    setNames(paste0("gscm_", names(.))),
  out_all[[ix_bfm]]$summ_latent2$raww %>% 
    setNames(paste0("bf_", names(.)))
) %>% 
  ggplot(aes(y = gscm_point, ymin = gscm_lower, ymax = gscm_upper,
             x = bf_point, xmin = bf_lower, xmax = bf_upper))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline(col = "red")+
  labs(y = "Factor 2 - raw scores (with ME)",
       x = "Factor 2 - raw scores (no ME)")+
  theme(text = element_text(size = 8))+
  ylim(-5,5)+xlim(-5,5)

## se - raw - Factor 2
se_raw_2 <- cbind(
  out_all[[ix_gscm]]$summ_latent2$raww %>% 
    setNames(paste0("gscm_", names(.))),
  out_all[[ix_bfm]]$summ_latent2$raww %>% 
    setNames(paste0("bf_", names(.)))
) %>% 
  dplyr::select(bf_se, gscm_se) %>% 
  arrange(bf_se) %>% 
  setNames(c("no ME",
             "with ME")) %>% 
  mutate(x = 1:nrow(.)) %>% 
  pivot_longer(-x) %>% 
  ggplot(aes(y = value, x = x,
             col = name))+
  theme_bw()+
  geom_point()+
  labs(y = "Posterior standard error (Factor 2)",
       x = "",
       col = "")+
  theme(text = element_text(size = 8),
        legend.position = "bottom")

## Combine
lay <- rbind(c(1,2))
full_inset_plt <- gridExtra::arrangeGrob(grobs = list(eqv_raw_2, se_raw_2), 
                                         layout_matrix  = lay)
full_inset_plt <- ggpubr::as_ggplot(full_inset_plt)+
  cowplot::draw_plot_label(label = c("(a)", "(b)"), size = 8,
                           x = c(0, 0.5), y = c(1, 1)) # Add labels

## save
jf$jsave(filename = "equiv2_raw_me.png",
         plot = full_inset_plt,
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

## Cleanup 
rm(se_raw_2, eqv_raw_2, full_inset_plt, lay)

#### equivalence - rank - Factor 2
eqv_rank_2 <- cbind(
  out_all[[ix_gscm]]$summ_latent2$rankk %>% 
    setNames(paste0("gscm_", names(.))),
  out_all[[ix_bfm]]$summ_latent2$rankk %>% 
    setNames(paste0("bf_", names(.)))
) %>% 
  ggplot(aes(y = gscm_point, ymin = gscm_lower, ymax = gscm_upper,
             x = bf_point, xmin = bf_lower, xmax = bf_upper))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline(col = "red")+
  labs(y = "Factor 2 - rank (with ME)",
       x = "Factor 2 - rank (no ME)")+
  theme(text = element_text(size = 8))

## se - rank - Factor 2
se_rank_2 <- cbind(
  out_all[[ix_gscm]]$summ_latent2$rankk %>% 
    setNames(paste0("gscm_", names(.))),
  out_all[[ix_bfm]]$summ_latent2$rankk %>% 
    setNames(paste0("bf_", names(.)))
) %>% 
  dplyr::select(bf_se, gscm_se) %>% 
  arrange(bf_se) %>% 
  setNames(c("no ME",
             "with ME")) %>% 
  mutate(x = 1:nrow(.)) %>% 
  pivot_longer(-x) %>% 
  ggplot(aes(y = value, x = x,
             col = name))+
  theme_bw()+
  geom_point()+
  labs(y = "Posterior standard error (Factor 2 - rank)",
       x = "",
       col = "")+
  theme(text = element_text(size = 8),
        legend.position = "bottom")

## Combine
lay <- rbind(c(1,2))
full_inset_plt <- gridExtra::arrangeGrob(grobs = list(eqv_rank_2, se_rank_2), 
                                         layout_matrix  = lay)
full_inset_plt <- ggpubr::as_ggplot(full_inset_plt)+
  cowplot::draw_plot_label(label = c("(a)", "(b)"), size = 8,
                           x = c(0, 0.5), y = c(1, 1)) # Add labels

## save
jf$jsave(filename = "equiv2_rank_me.png",
         plot = full_inset_plt,
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

## Cleanup 
rm(se_rank_2, eqv_rank_2, full_inset_plt, ix_bfm, ix_gscm, lay)

## Multiple shared components ## -----------------------------------------------

grid %>% 
  filter(model == "GSCM",
         shared_latent_rho_fixed == "LCAR",
         specific_latent_rho_fixed == "LCAR")
ix_l1 <- 10 #17
ix_l2 <- 11 #18

#### equivalence - raw - factor 1
eqv_raw_1 <- cbind(
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
  labs(y = "Factor 1 - raw scores (1-factor GSCM)",
       x = "Factor 1 - raw scores (2-factor GSCM)")+
  theme(text = element_text(size = 8))

## se - raw - factor 1
se_raw_1 <- cbind(
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
  theme(text = element_text(size = 8),
        legend.position = "bottom")

## Combine
lay <- rbind(c(1,2))
full_inset_plt <- gridExtra::arrangeGrob(grobs = list(eqv_raw_1, se_raw_1), 
                              layout_matrix  = lay)
full_inset_plt <- ggpubr::as_ggplot(full_inset_plt)+
  cowplot::draw_plot_label(label = c("(a)", "(b)"), size = 8,
                           x = c(0, 0.5), y = c(1, 1)) # Add labels

## save
jf$jsave(filename = "equiv_raw_multfactors.png",
         plot = full_inset_plt,
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

## Cleanup 
rm(se_raw_1, eqv_raw_1, full_inset_plt)

#### equivalence - rank - factor 1
eqv_rank_1 <- cbind(
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
  labs(y = "Factor 1 - rank (1-factor GSCM)",
       x = "Factor 1 - rank (2-factor GSCM)")+
  theme(text = element_text(size = 8))

## se - rank - factor 1
se_rank_1 <- cbind(
  out_all[[ix_l1]]$summ_latent1$rankk %>% 
    setNames(paste0("l1_", names(.))),
  out_all[[ix_l2]]$summ_latent1$rankk %>% 
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
  labs(y = "Posterior standard error (rank factor 1)",
       x = "",
       col = "")+
  theme(text = element_text(size = 8),
        legend.position = "bottom")

## Combine
lay <- rbind(c(1,2))
full_inset_plt <- gridExtra::arrangeGrob(grobs = list(eqv_rank_1, se_rank_1), 
                                         layout_matrix  = lay)
full_inset_plt <- ggpubr::as_ggplot(full_inset_plt)+
  cowplot::draw_plot_label(label = c("(a)", "(b)"), size = 8,
                           x = c(0, 0.5), y = c(1, 1)) # Add labels

## save
jf$jsave(filename = "equiv_rank_multfactors.png",
         plot = full_inset_plt,
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

## Cleanup 
rm(se_rank_1, eqv_rank_1, full_inset_plt)

## Multiple shared components - IID feature specific priors ## -----------------

grid %>% 
  filter(model == "GSCM",
         shared_latent_rho_fixed == "LCAR",
         specific_latent_rho_fixed == "IID")
ix_l1 <- 15
ix_l2 <- 16

#### equivalence - raw - factor 1
eqv_raw_1 <- cbind(
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
  labs(y = "Factor 1 - raw scores (1-factor GSCM)",
       x = "Factor 1 - raw scores (2-factor GSCM)")+
  theme(text = element_text(size = 8))

## se - raw - factor 1
se_raw_1 <- cbind(
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
  theme(text = element_text(size = 8),
        legend.position = "bottom")

## Combine
lay <- rbind(c(1,2))
full_inset_plt <- gridExtra::arrangeGrob(grobs = list(eqv_raw_1, se_raw_1), 
                                         layout_matrix  = lay)
full_inset_plt <- ggpubr::as_ggplot(full_inset_plt)+
  cowplot::draw_plot_label(label = c("(a)", "(b)"), size = 8,
                           x = c(0, 0.5), y = c(1, 1)) # Add labels

## save
jf$jsave(filename = "equiv_raw_multfactors_IID.png",
         plot = full_inset_plt,
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

## Cleanup 
rm(se_raw_1, eqv_raw_1, full_inset_plt)

#### equivalence - perc - factor 1
eqv_perc_1 <- cbind(
  out_all[[ix_l1]]$summ_latent1$perc %>% 
    setNames(paste0("l1_", names(.))),
  out_all[[ix_l2]]$summ_latent1$perc %>% 
    setNames(paste0("l2_", names(.)))
) %>% 
  ggplot(aes(y = l1_point, ymin = l1_lower, ymax = l1_upper,
             x = l2_point, xmin = l2_lower, xmax = l2_upper))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline(col = "red")+
  labs(y = "Factor 1 - perc (1-factor GSCM)",
       x = "Factor 1 - perc (2-factor GSCM)")+
  theme(text = element_text(size = 8))

## se - perc - factor 1
se_perc_1 <- cbind(
  out_all[[ix_l1]]$summ_latent1$perc %>% 
    setNames(paste0("l1_", names(.))),
  out_all[[ix_l2]]$summ_latent1$perc %>% 
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
  labs(y = "Posterior standard error (perc factor 1)",
       x = "",
       col = "")+
  theme(text = element_text(size = 8),
        legend.position = "bottom")

## Combine
lay <- rbind(c(1,2))
full_inset_plt <- gridExtra::arrangeGrob(grobs = list(eqv_perc_1, se_perc_1), 
                                         layout_matrix  = lay)
full_inset_plt <- ggpubr::as_ggplot(full_inset_plt)+
  cowplot::draw_plot_label(label = c("(a)", "(b)"), size = 8,
                           x = c(0, 0.5), y = c(1, 1)) # Add labels

## save
jf$jsave(filename = "equiv_perc_multfactors_IID.png",
         plot = full_inset_plt,
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

## Cleanup 
rm(se_perc_1, eqv_perc_1, full_inset_plt)

#### equivalence - rank - factor 1
eqv_rank_1 <- cbind(
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
  labs(y = "Factor 1 - rank (1-factor GSCM)",
       x = "Factor 1 - rank (2-factor GSCM)")+
  theme(text = element_text(size = 8))

## se - rank - factor 1
se_rank_1 <- cbind(
  out_all[[ix_l1]]$summ_latent1$rankk %>% 
    setNames(paste0("l1_", names(.))),
  out_all[[ix_l2]]$summ_latent1$rankk %>% 
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
  labs(y = "Posterior standard error (rank factor 1)",
       x = "",
       col = "")+
  theme(text = element_text(size = 8),
        legend.position = "bottom")

## Combine
lay <- rbind(c(1,2))
full_inset_plt <- gridExtra::arrangeGrob(grobs = list(eqv_rank_1, se_rank_1), 
                                         layout_matrix  = lay)
full_inset_plt <- ggpubr::as_ggplot(full_inset_plt)+
  cowplot::draw_plot_label(label = c("(a)", "(b)"), size = 8,
                           x = c(0, 0.5), y = c(1, 1)) # Add labels

## save
jf$jsave(filename = "equiv_rank_multfactors_IID.png",
         plot = full_inset_plt,
         base_folder = "out",
         square = F,
         square_size = 1200,
         dpi = 300)

## Cleanup 
rm(se_rank_1, eqv_rank_1, full_inset_plt)

## Caterpillar plots - rank ## -------------------------------------------------

chose_ix <- 10

# Factor 1
out_all[[chose_ix]]$summ_latent1$raww %>% 
  mutate(EP = out_all[[chose_ix]]$EP[,1],
         ra = out_all[[chose_ix]]$data$census$ra_sa2_3c) %>% 
  arrange(point) %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = 1:nrow(.), col = EP))+
  geom_errorbar(col = "grey")+
  geom_point()+
  labs(y = "Factor 1 - raw scores",
       x = "")+
  theme_bw()+
  theme(text = element_text(size = 8))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_gradientn(colors = c("#008837", "#a6dba0", "white","white","white", "#c2a5cf", "#7b3294"),
                       limits = c(-0.0000001,1.0000001),
                       #oob = squish,
                       #trans = "logit",
                       breaks = c(0,0.2,0.25,0.5,0.75,0.8,1),
                       labels = as.character(c(0,0.2,"",0.5,"",0.8,1)))+
  facet_grid(.~ra)

# Factor 2
out_all[[chose_ix]]$summ_latent2$raww %>% 
  mutate(EP = out_all[[chose_ix]]$EP[,2],
         ra = out_all[[chose_ix]]$data$census$ra_sa2_3c) %>% 
  arrange(point) %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = 1:nrow(.), col = EP))+
  geom_errorbar(col = "grey")+
  geom_point()+
  labs(y = "Factor 2 - raw scores",
       x = "")+
  theme_bw()+
  theme(text = element_text(size = 8))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  scale_color_gradientn(colors = c("#008837", "#a6dba0", "white","white","white", "#c2a5cf", "#7b3294"),
                        limits = c(-0.0000001,1.0000001),
                        #oob = squish,
                        #trans = "logit",
                        breaks = c(0,0.2,0.25,0.5,0.75,0.8,1),
                        labels = as.character(c(0,0.2,"",0.5,"",0.8,1)))+
  facet_grid(.~ra)

# Compare factors
data.frame(f1 = out_all[[chose_ix]]$summ_latent1$raww$point,
           f2 = out_all[[chose_ix]]$summ_latent2$raww$point,
           EP = 0.5*(out_all[[chose_ix]]$EP[,1] + out_all[[chose_ix]]$EP[,2])) %>% 
  ggplot(aes(y = f1, x = f2, col = EP))+
  geom_point()+
  labs(y = "Factor 1 - raw scores",
       x = "Factor 2 - raw scores")+
  theme_bw()+
  theme(text = element_text(size = 8))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  geom_vline(xintercept = 0, linetype = "dotted")+
  scale_color_gradientn(colors = c("#008837", "#a6dba0", "grey","grey","grey", "#c2a5cf", "#7b3294"),
                        limits = c(-0.0000001,1.0000001),
                        #oob = squish,
                        #trans = "logit",
                        breaks = c(0,0.2,0.25,0.5,0.75,0.8,1),
                        labels = as.character(c(0,0.2,"",0.5,"",0.8,1)))

## S-plot - percentiles ## -----------------------------------------------------

foo <- function(x){
  data.frame(point = cur_list[[paste0("summ_latent", x)]]$perc$point,
             prob = cur_list$probs[[paste0("latent", x, "_perc")]]$perc80)%>% 
    ggplot(aes(y = prob, x = point,
               col = prob))+
    geom_point()+theme_bw()+
    scale_color_viridis_c(begin = 0, end = 1,
                          direction = -1,
                          option = "A")+
    labs(title = paste0("Index ", x),
         y = "Posterior probability above the 80th percentile",
         x = "Posterior median percentile")+
    theme(text = element_text(size = 8),
          legend.position = "none")+
    ylim(0,1)
  jf$jsave(filename = paste0("splot_perc", x, ".png"),
           base_folder = "out",
           square = F,
           square_size = 1200,
           dpi = 300)
}
lapply(1:4, foo)
rm(foo)

## S-plot - ranks ## -----------------------------------------------------------

foo <- function(x){
  data.frame(point = cur_list[[paste0("summ_latent", x)]]$rankk$point,
             prob = cur_list$probs[[paste0("latent", x, "_rank")]]$rank100)%>% 
    ggplot(aes(y = prob, x = point,
               col = prob))+
    geom_point()+theme_bw()+
    scale_color_viridis_c(begin = 0, end = 1,
                          direction = -1,
                          option = "D")+
    labs(title = paste0("Index ", x),
         y = "Posterior probability ranked in top 100",
         x = "Posterior median rank")+
    theme(text = element_text(size = 8),
          legend.position = "none")+
    geom_vline(xintercept = 2221-100, linetype = "dotted")+
    ylim(0,1)
  jf$jsave(filename = paste0("splot_rank", x, ".png"),
           base_folder = "out",
           square = F,
           square_size = 1200,
           dpi = 300)
}
lapply(1:4, foo)
rm(foo)

## Remoteness and IRSD - percentiles ## ----------------------------------------

temp_fun <- function(x){
cbind(global_obj$census, cur_list[[paste0("summ_latent",x)]]$perc) %>% 
  mutate(ABS_irsd_decile_nation_complete = factor(ABS_irsd_decile_nation_complete, 
                                                  levels = 1:10,
                                                  labels = c("Most\ndisadvantaged", 
                                                             as.character(2:9), 
                                                             "Least\ndisadvantaged")),
         ra_sa2 = fct_relevel(ra_sa2, "Major Cities")) %>% 
  ggplot(aes(x = point, fill = ABS_irsd_decile_nation_complete, y = ra_sa2))+
  geom_boxplot()+
    scale_fill_viridis_d(option = "B", direction = 1)+
  theme_bw()+
  labs(x = "Posterior median percentile",
       y = "",
       fill = "")+
  theme(text = element_text(size = 8))+
  labs(title = paste0("Index ", x))
  
  jf$jsave(filename = paste0("ra_irsd_perc", x, ".png"),
           base_folder = "out",
           square = F,
           square_size = 1200,
           dpi = 300)
}

lapply(1:4, temp_fun)
rm(temp_fun)

## Remoteness and IRSD - percentiles ## ----------------------------------------

temp_fun <- function(x){
  cbind(global_obj$census, cur_list[[paste0("summ_latent",x)]]$rankk) %>% 
    mutate(ABS_irsd_decile_nation_complete = factor(ABS_irsd_decile_nation_complete, 
                                                    levels = 1:10,
                                                    labels = c("Most\ndisadvantaged", 
                                                               as.character(2:9), 
                                                               "Least\ndisadvantaged")),
           ra_sa2 = fct_relevel(ra_sa2, "Major Cities")) %>% 
    ggplot(aes(x = point, fill = ABS_irsd_decile_nation_complete, y = ra_sa2))+
    geom_boxplot()+
    scale_fill_viridis_d(option = "B", direction = 1)+
    theme_bw()+
    labs(x = "Posterior median rank",
         y = "",
         fill = "")+
    theme(text = element_text(size = 8))+
    labs(title = paste0("Index ", x))
  
  jf$jsave(filename = paste0("ra_irsd_rank", x, ".png"),
           base_folder = "out",
           square = F,
           square_size = 1200,
           dpi = 300)
}

lapply(1:4, temp_fun)

## END SCRIPT ## ---------------------------------------------------------------