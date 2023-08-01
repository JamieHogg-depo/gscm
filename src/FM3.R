# Factor model - https://discourse.mc-stan.org/t/fitting-a-bayesian-factor-analysis-model-in-stan/17823/2

## Load packages ## ------------------------------------------------------------
library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
library(posterior)
library(spdep)
library(tmap)
tmap_mode("view")
rm(list = ls())

# load data
global_obj <- readRDS("data/global_obj.rds")
census <- global_obj$census %>% filter(ps_state == 1)
out <- readRDS("data/y_mats_unc.rds")
data <- out$point[census$ps_area,-c(1:2)]
#data <- scale(data)

# compile model
unlink("src/stan/*.rds")
comp <- stan_model(file = "src/stan/FM3.stan")

# data list
d <- list(N = nrow(data),
          K = ncol(data),
          Y = data,
          L = 2)

# fit model
m_s <- Sys.time()
fit <- sampling(object = comp, 
                #pars= c("z"),
                #include = FALSE, 
                seed = 42,
                init=0,
                data = d, 
                chains = 2,
                #iter = 6000, warmup = 3000, 
                control = list(adapt_delta = 0.95),
                cores = 2)
(rt <- as.numeric(Sys.time() - m_s, units = "mins"))

#print(fit)
print(fit, pars = c("Lambda", "psi", "alpha"))
stan_trace(fit, pars = c("Lambda", "psi", "alpha"))

# get latent field
draws <- rstan::extract(fit)
latent <- apply(draws$z, 2, median)

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
  mutate(latent = latent)

# plot latent field
map_sa2 %>% 
  filter(!st_is_empty(.)) %>% 
  tm_shape(.)+
  tm_polygons(col = "latent",
              palette = "YlOrRd",
              # use command tmaptools::palette_explorer()
              style = "quantile",
              n = 20)

## END SCRIPT ## ---------------------------------------------------------------

freeParams <- function(K, L){
  L*K - ((L*(L-1))/2)
}
