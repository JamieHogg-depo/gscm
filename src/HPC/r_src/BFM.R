####
## BFM
####

# compile model
unlink(paste0(base_folder, "/r_src/stan/*.rds"))
comp <- stan_model(file = paste0(base_folder, "/r_src/stan/BFM.stan"))

# data list
d <- list(N = nrow(rf_data), 
          K = ncol(rf_data),
          Y = rf_data)
d <- c(d, for_stan, icar_for_stan, cur_model_spec)

# fit model
m_s <- Sys.time()
ll_out$fit <- sampling(object = comp, 
                pars = c("Z_z", "mu"),
                include = FALSE,
                data = d, 
                init = 0,
				#refresh = 0, 				
                chains = 4,
                control = list(adapt_delta = 0.95,
								max_treedepth = 12),
                iter = 4000, warmup = 2000, 
                cores = 4)
(ll_out$rt <- as.numeric(Sys.time() - m_s, units = "mins"))

# Summarise draws
ll_out$summ <- summarise_draws(ll_out$fit)
ll_out$trace_pl <- stan_trace(ll_out$fit, pars = c("alpha", "Lambda", "sigma", "psi", "rho"))
ll_out$Lambda_point <- matrix(ll_out$summ[str_detect(ll_out$summ$variable, "Lambda\\["),]$mean, 
                              byrow = F, ncol = d$L)

## END SCRIPT ## ---------------------------------------------------------------