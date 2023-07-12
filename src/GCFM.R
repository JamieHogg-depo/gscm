# Generalised Common Factor Model - P0315

## Load packages ## ------------------------------------------------------------
library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
library(posterior)
rm(list = ls())

# load data
out <- readRDS("data/y_mats_unc.rds")
data <- out$point[1:200,-c(1:2)]

# compile model
unlink("src/stan/*.rds")
comp <- stan_model(file = "src/stan/GCFM.stan")

# data list
d <- list(N = nrow(data),
          K = ncol(data),
          Y = data)

# fit model
m_s <- Sys.time()
fit <- sampling(object = comp, 
                data = d, 
                cores = 1)
(rt <- as.numeric(Sys.time() - m_s, units = "mins"))

print(fit)

## END SCRIPT ## ---------------------------------------------------------------

