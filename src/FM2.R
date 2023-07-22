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
comp <- stan_model(file = "src/stan/FM2.stan")

# data list
d <- list(N = nrow(data),
          P = ncol(data),
          Y = data,
          D = 2)

# initial values
init_fun = function() {
  init.values<-list(L_t=rep(0,24)+runif(1,-.1,.1),
                    L_d=rep(.5,D)+runif(1,-.1,.1),
                    psi=rep(.2,P)+runif(1,-.1,.1),
                    sigma_psi=0.15+runif(1,-.1,.1),
                    mu_psi=0.2++runif(1,-.1,.1),
                    sigma_lt=0.5+runif(1,-.1,.1),
                    mu_lt=0.0+runif(1,-.1,.1))
  return(init.values); 
} 

# fit model
m_s <- Sys.time()
fit <- sampling(object = comp, 
                pars= c("L","psi","sigma_psi","mu_psi","sigma_lt","mu_lt"), 
                seed = 42,
                init=0,
                data = d, 
                chains = 4,
                #iter = 6000, warmup = 3000, 
                cores = 4)
(rt <- as.numeric(Sys.time() - m_s, units = "mins"))

#print(fit)
print(fit, pars = "L")
stan_trace(fit, pars = "L")

# get latent field
draws <- rstan::extract(fit)
latent <- apply(draws$fi, 2, median)

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
