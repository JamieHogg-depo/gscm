# Factor model - https://rfarouni.github.io/assets/projects/BayesianFactorAnalysis/BayesianFactorAnalysis.html

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
census <- global_obj$census %>% filter(ps_state == 7)
#census <- global_obj$census
out <- readRDS("data/y_mats_unc.rds")
data <- out$point[census$ps_area,-c(1:2)]
#data <- data[,c(5,1,2,3,4)]
#data <- scale(data)

# compile model
unlink("src/stan/*.rds")
comp <- stan_model(file = "src/stan/FM.stan")

# data list
d <- list(N = nrow(data),
          K = ncol(data),
          Y = data,
          L = 2,
          psi_cut = 0.0)
L <- d$L
K <- d$K

# initial values
init_fun = function() {
  init.values<-list(Lambda_t=rep(0,24)+runif(1,-.1,.1),
                    Lambda_d=rep(.5,L)+runif(1,-.1,.1),
                    psi=rep(.1,K)+runif(1,-.1,.1),
                    alpha = rnorm(K))
  return(init.values)
} 

# fit model
m_s <- Sys.time()
fit <- sampling(object = comp, 
                pars= c("Q", "Lambda_t", "Lambda_d", "Z_z"),
                include = FALSE, 
                #seed = 42,
                init = 0, #init_fun,
                data = d, 
                chains = 4,
                iter = 6000, warmup = 3000, 
                control = list(adapt_delta = 0.95),
                cores = 4)
(rt <- as.numeric(Sys.time() - m_s, units = "mins"))
summ <- as.data.frame(summary(fit)$summary) %>% 
  rownames_to_column("parameter")
100*mean(summ$Rhat > 1.01, na.rm = T)

#print(fit)
print(fit, pars = c("Lambda", "psi"))
matrix(summ[str_detect(summ$parameter, "Lambda\\["),]$mean, byrow = T, ncol = L)
stan_trace(fit, pars = c("Lambda", "alpha", "psi", "sigma_z"))
pairs(fit, pars = c("psi", "sigma_z"))

# get latent field
draws <- rstan::extract(fit)
latent <- apply(draws$z, c(2,3), median)

# compare latent spaces
#plot(latent[,1], latent3[,1]); abline(a = 0, b = 1)
#plot(latent[,2], latent2[,2]); abline(a = 0, b = 1)

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
