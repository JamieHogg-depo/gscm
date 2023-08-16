# Shared Component Model - only two factors

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
rm(list = ls())
source("src/funs.R")

# load global data
global_obj <- readRDS("data/global_obj.rds")
W <- global_obj$W[global_obj$census$ps_state == 6, global_obj$census$ps_state == 6]
#W <- global_obj$W
for_stan <- jf$prep4MLCAR(W)
icar_for_stan <- jf$prep4ICAR(W)

# subset census to state 1 - NSW
census <- global_obj$census %>% filter(ps_state == 6)
#census <- global_obj$census
out <- readRDS("data/y_mats_unc.rds")
#data <- out$point[,-c(1:2)]
data_raw <- out$point[census$ps_area,-c(1:2)]
#data <- scale(data_raw)
data <- data_raw
#data <- jf$scaleMarginal(data_raw)

# compile model
unlink("src/stan/*.rds")
comp <- stan_model(file = "src/stan/GSCM_nome2_finalversion.stan")

# data list
d <- list(N = nrow(data),
          K = ncol(data),
          Y = data,
          L = 2,
          shared_latent_rho_fixed = 2,
          specific_latent_rho_fixed = 2
          )
d <- c(d, for_stan, icar_for_stan)

# fit model
m_s <- Sys.time()
fit <- sampling(object = comp, 
                pars = c("Z_z", "mu", "Z_epsilon", "epsilon"),
                include = FALSE,
                data = d, 
                init = 0, 
                chains = 4,
                iter = 4000, warmup = 2000, 
                cores = 4)
(rt <- as.numeric(Sys.time() - m_s, units = "mins"))
# Summarise draws
summ <- as.data.frame(summary(fit)$summary) %>% 
  rownames_to_column("parameter")
100*mean(summ$Rhat > 1.01, na.rm = T)
Lambda_point <- matrix(summ[str_detect(summ$parameter, "Lambda\\["),]$mean, byrow = T, ncol = d$L)

print(fit, pars = c("alpha", "Lambda_ld", "sigma", "psi", 'rho_z'))
print(fit, pars = "Lambda")
stan_trace(fit, pars = c("alpha", "Lambda_ld", "sigma", "psi", 'rho_z', "sigma_mar", "rho_epsilon"))

# get latent field
draws <- rstan::extract(fit)
latent <- apply(draws$z, c(2,3), median)

# LOOCV
log_lik <- extract_log_lik(fit, merge_chains=F)
r_eff <- relative_eff(exp(log_lik))
loo_out <- loo(log_lik, r_eff = r_eff)
loo_out

## Plot #### -------------------------------------------------------------------

## Compare latent to raw
cbind(latent = latent, data) %>% 
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
