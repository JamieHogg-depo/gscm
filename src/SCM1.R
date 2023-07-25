# Shared Component Model - only two factors

## Load packages ## ------------------------------------------------------------
library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
library(posterior)
library(spdep)
library(tmap)
tmap_mode("view")
rm(list = ls())
source("src/funs.R")

# load global data
global_obj <- readRDS("data/global_obj.rds")
W <- global_obj$W[global_obj$census$ps_area == 1, global_obj$census$ps_area == 1]
for_stan <- jf$prep4MLCAR(W)

# subset census to state 1 - NSW
census <- global_obj$census %>% filter(ps_state == 1)
out <- readRDS("data/y_mats_unc.rds")
data <- out$point[census$ps_area,-c(1:2)]
data_sd <- out$sd[census$ps_area,-c(1:2)]
#data <- out$point[,c(6:7)]

# compile model
unlink("src/stan/*.rds")
comp <- stan_model(file = "src/stan/SCM2.stan")

# data list
d <- list(N = nrow(data),
          K = ncol(data),
          Y = data,
          sd_mat = matrix(0.01, nrow = nrow(data),ncol = ncol(data))#,
          #sd_mat = data_sd,
          )

# fit model
m_s <- Sys.time()
fit <- sampling(object = comp, 
                pars = c("Z_epsilon", "Z_z"),
                include = FALSE,
                data = d, 
                chains = 2,
                iter = 6000, warmup = 3000, 
                cores = 2)
(rt <- as.numeric(Sys.time() - m_s, units = "mins"))

print(fit, pars = c("alpha", "lambda", "sigma_e", "sigma_z"))
print(fit, pars = "lambda")
stan_trace(fit, pars = c("alpha", "lambda", "sigma_e", "sigma_z"))

# get latent field
draws <- rstan::extract(fit)
latent <- apply(draws$z, 2, median)
epsilon <- apply(draws$epsilon, c(2,3), median)

# LOOCV
library(loo)
log_lik <- extract_log_lik(fit, merge_chains=F)
r_eff <- relative_eff(exp(log_lik))
loo <- loo(log_lik, r_eff = r_eff)

# compare means with observed data
mu <- as.data.frame(apply(draws$mu, c(2,3), median)) %>% 
  setNames(paste0("X", 1:5))

# assess observed and fitted means
i <- 5
ggplot(data = NULL,
       aes(y = unlist(mu[,i]), 
           x = unlist(data[,i]),
           col = cut_number(unlist(data_sd[,i]), 10, labels = F))) + 
  geom_point()+
  geom_abline()+
  scale_color_viridis_c()

## Plot #### -------------------------------------------------------------------

## Compare latent to raw
cbind(latent = latent, data) %>% 
  cbind(.,epsilon) %>% 
  plot(.)

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
