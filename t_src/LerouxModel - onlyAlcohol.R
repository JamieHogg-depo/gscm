# GSCM - dynamic loading

## Load packages ## ------------------------------------------------------------
library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
library(posterior)
library(spdep)
library(tmap)
library(Matrix)
tmap_mode("view")
library(loo)
library(bayesplot)
library(patchwork)
rm(list = ls())
source("t_src/funs.R")

foo <- function(x)cut_number(x, n = 100, labels = FALSE)

# load global data
global_obj <- readRDS("data/global_obj.rds")
#out <- readRDS("data/y_mats_unc.rds")
out <- readRDS("data/y_mats.rds")

# all aussie
W <- global_obj$W
census <- global_obj$census
data <- out$point[,-c(1:2)]
data_sd <- out$sd[,-c(1:2)]

# just state 1
W <- global_obj$W[global_obj$census$ps_state == 1, global_obj$census$ps_state == 1]
census <- global_obj$census %>% filter(ps_state == 1)
data <- out$point[census$ps_area,-c(1:2)]
data_sd <- out$sd[census$ps_area,-c(1:2)]

# stan objects
for_stan <- jf$prep4MLCAR(W)
icar_for_stan <- jf$prep4ICAR(W)

# Load map
# Load map
map_sa2 <- st_read("C:/r_proj/ACAriskfactors/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>%
  mutate(SA2 = as.numeric(SA2_MAIN16)) %>%
  filter(!str_detect(SA2_NAME, "Island")) %>%
  filter(STATE_NAME != "Other Territories") %>%
  right_join(.,global_obj$area_concor, by = "SA2") %>%
  right_join(.,census, by = "ps_area")

# compile model
unlink("t_src/stan/*.rds")
comp <- stan_model(file = "t_src/stan/onlyAlcohol.stan")

# scale data
tt <- jf$scaleData(data, data_sd)
data <- tt$data %>% relocate(smoking, alcohol)
data_sd <- tt$data_sd %>% relocate(smoking, alcohol)

# data list
d <- list(# data
          N = nrow(data),
          Y = data$alcohol,
          Y_sd = 100*data_sd$alcohol)
d <- c(d, for_stan, icar_for_stan)

# fit model
m_s <- Sys.time()
fit <- sampling(object = comp, 
                data = d, 
                chains = 2,
                control = list(adapt_delta = 0.95),
                iter = 4000, warmup = 2000, 
                cores = 2)
(rt <- as.numeric(Sys.time() - m_s, units = "mins"))
# Summarise draws
summ <- as.data.frame(summary(fit)$summary) %>% 
  rownames_to_column("parameter")
100*mean(summ$Rhat > 1.01, na.rm = T)

print(fit, pars = c("alpha", "Lambda", "sigma", "psi", 'rho', "kappa"))
print(fit, pars = "Lambda")
stan_trace(fit, pars = c("alpha", "Lambda", "sigma", "psi", 'rho', "kappa"))

# get latent field
draws <- rstan::extract(fit)
latent <- apply(draws$z, c(2,3), median)

# LOOCV
(loo_out1 <- rstan::loo(fit))
loo_out2 <- rstan::loo(fit, moment_match = T)

## PPC
fe <- 1
yrep <- draws$yrep[,,fe]
mean_y <- function(x) weighted.mean(x, w = 1/unlist(data_sd[,fe]), na.rm = T)
var_y <- function(x) var(x, na.rm = T)
(ppc_stat(unlist(data[,fe]), yrep, stat = "mean_y") + labs(title = "Mean"))/
(ppc_stat(unlist(data[,fe]), yrep, stat = "var_y") + labs(title = "Variance"))

# PPC
fe <- 2
yrep <- draws$yrep[,,fe]
ppc_dens_overlay(unlist(data[,fe]), yrep[1:100,])+xlim(-4,2)
ppc_boxplot(unlist(data[,fe]), yrep[1:8,])
ppc_ecdf_overlay(unlist(data[,fe]), yrep[1:50,])

## Correlation
cor_rep <- t(apply(draws$yrep, 1, cor))

## Plot #### -------------------------------------------------------------------

## Compare latent to raw
data_wl <- cbind(latent = latent, data)
data_pc <- apply(data_wl, 2, FUN = function(x)cut_number(x, n = 100, labels = FALSE))
data_pc2 <- apply(data_wl, 2, FUN = function(x)cut_number(x, n = 10, labels = FALSE))

## Map the factors - values
data_wl %>% 
  as.data.frame() %>% 
  dplyr::select(contains("latent")) %>% 
  mutate(ps_area = census$ps_area) %>% 
  pivot_longer(-ps_area) %>% 
  left_join(.,map_sa2) %>% 
  ggplot(aes(fill = value, geometry = geometry))+
  geom_sf()+
  theme_void()+
  scale_fill_viridis_c()+
  facet_wrap(.~name)

## Uncertainty of risk factors (features)
data_sd %>% 
  mutate(ps_area = census$ps_area) %>% 
  pivot_longer(-ps_area) %>% 
  left_join(.,map_sa2) %>% 
  ggplot(aes(fill = value, geometry = geometry))+
  geom_sf()+
  theme_void()+
  scale_fill_viridis_c()+
  facet_wrap(.~name)

## Map the factors - percentiles
data_pc %>% 
  as.data.frame() %>% 
  #dplyr::select(contains("latent")) %>% 
  mutate(ps_area = census$ps_area) %>% 
  pivot_longer(-ps_area) %>% 
  left_join(.,map_sa2) %>% 
  ggplot(aes(fill = value, geometry = geometry))+
  geom_sf()+
  theme_void()+
  scale_fill_viridis_c()+
  facet_wrap(.~name)+
  labs(fill = "Percentiles")

## DEPREC ## -------------------------------------------------------------------

## Compare raw values to latent values
data.frame(latent = latent, 
           overweight = data$overweight,
           overweight_sd = data_sd$overweight,
           smoking = data$smoking,
           smoking_sd = data_sd$smoking) %>% 
  ggplot(aes(y = latent, 
             x = overweight))+
  geom_point(aes(col = cut_number(overweight_sd, 5)))+
  scale_color_viridis_d(direction = -1)+
  geom_smooth(method = "lm")

data.frame(latent = latent, 
           overweight = data$overweight,
           overweight_sd = data_sd$overweight,
           smoking = data$smoking,
           smoking_sd = data_sd$smoking) %>% 
  ggplot(aes(y = latent, 
             x = smoking))+
  geom_point(aes(col = cut_number(smoking_sd, 5)))+
  scale_color_viridis_d(direction = -1)+
  geom_smooth(method = "lm")

## Map the latent field #### ---------------------------------------------------

# Load map
map_sa2_full <- st_read("C:/r_proj/ACAriskfactors/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>%
  mutate(SA2 = as.numeric(SA2_MAIN16)) %>%
  filter(!str_detect(SA2_NAME, "Island")) %>%
  filter(STATE_NAME != "Other Territories")

# keep non-estimated geometries
map_sa2 <- map_sa2_full %>%
  right_join(.,global_obj$area_concor, by = "SA2") %>%
  right_join(.,census, by = "ps_area") %>% 
  arrange(ps_area) %>% 
  mutate(latent = latent) %>% 
  cbind(.,epsilon)

# Use ggplot
apply_cut_number <- function(x) {
  cut_number(x, 20, labels = FALSE)
}
map_sa2 %>% 
  dplyr::select(latent, X1, X2, X3, X4, X5, ps_area, geometry) %>% 
  mutate_at(all_of(c("latent", "X1", "X2", "X3", "X4", "X5")), apply_cut_number) %>% 
  pivot_longer(-c(geometry, ps_area)) %>% 
  st_as_sf() %>% 
  ggplot(aes(geometry = geometry, fill = value))+
  geom_sf()+
  facet_wrap(.~name)

# Use tmap - plot latent field
map_sa2 %>% 
  filter(!st_is_empty(.)) %>% 
  tm_shape(.)+
  tm_polygons(col = "latent",
              palette = "YlOrRd",
              # use command tmaptools::palette_explorer()
              style = "quantile",
              n = 20)+
  tm_facets(by = "model")

# three plots
map_sa2_full %>%
  right_join(.,global_obj$area_concor, by = "SA2") %>%
  right_join(.,census, by = "ps_area") %>% 
  arrange(ps_area) %>% 
  mutate(latent = cut_number(latent,20,labels = FALSE),
         Z_e1 = cut_number(Z_e1,20,labels = FALSE),
         Z_e2 = cut_number(Z_e2,20,labels = FALSE)) %>% 
  dplyr::select(latent, Z_e1, Z_e2, geometry) %>% 
  pivot_longer(-geometry) %>% 
  ggplot(aes(fill = value, geometry = geometry)) + 
  geom_sf()+
  scale_fill_viridis_c()+
  facet_wrap(.~name)

# Interactive three maps - 20 quantiles
map_sa2 %>% 
  dplyr::select(latent, X1, X2, X3, X4, X5, ps_area, geometry) %>% 
  mutate_at(all_of(c("latent", "X1", "X2", "X3", "X4", "X5")), apply_cut_number) %>% 
  pivot_longer(-c(geometry, ps_area)) %>% 
  st_as_sf() %>% 
  filter(!st_is_empty(.)) %>% 
  tm_shape(.)+
  tm_polygons(col = "value",
              palette = "YlOrRd",
              # use command tmaptools::palette_explorer()
              style = "cont",
              n = 20)+
  tm_facets("name")

# Interactive three maps - rank
map_sa2_full %>%
  right_join(.,global_obj$area_concor, by = "SA2") %>%
  right_join(.,census, by = "ps_area") %>% 
  arrange(ps_area) %>% 
  mutate(latent = order(latent),
         Z_e1 = order(Z_e1),
         Z_e2 = order(Z_e2),
         overweight = order(data$overweight),
         smoking = order(data$smoking)) %>% 
  dplyr::select(latent, Z_e1, Z_e2, overweight, smoking, geometry) %>% 
  #dplyr::select(latent, overweight, smoking, geometry) %>% 
  pivot_longer(-geometry) %>% 
  st_as_sf(.) %>% 
  filter(!st_is_empty(.)) %>% 
  tm_shape(.)+
  tm_polygons(col = "value",
              palette = "YlOrRd",
              # use command tmaptools::palette_explorer()
              style = "cont",
              n = 20)+
  tm_facets("name")


## END SCRIPT ## ---------------------------------------------------------------

freeParams <- function(K, L){
  L*K - ((L*(L-1))/2)
}
