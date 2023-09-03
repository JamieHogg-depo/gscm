####
## GSCM
####

# compile model
unlink(paste0(base_folder, "/r_src/stan/*.rds"))
comp <- stan_model(file = paste0(base_folder, "/r_src/stan/GSCM.stan"))

# data list
d <- list(N = nrow(rf_data), 
          K = ncol(rf_data),
          Y_v = as.numeric(as.matrix(rf_data)),
          Y_sd_v = as.numeric(as.matrix(rf_data_sd)))
d <- c(d, for_stan, icar_for_stan, cur_model_spec)

# fit model
m_s <- Sys.time()
ll_out$fit <- sampling(object = comp, 
                pars = c("Z_z", "mu", "Z_epsilon"),
                include = FALSE,
                data = d, 
                init = 0,
				#refresh = 0, 				
                chains = chains,
                control = list(adapt_delta = 0.95,
								max_treedepth = 12),
                iter = iter, warmup = warmup, 
				thin = thin,
                cores = chains)
(ll_out$rt <- as.numeric(Sys.time() - m_s, units = "mins"))

# Summarise draws
ll_out$summ <- summarise_draws(ll_out$fit) %>% 
  mutate(variable_gr = str_extract(variable, "^[^\\[]+")) %>% 
  relocate(variable_gr)
  
# trace plots
ll_out$trace$hyperparams <- stan_trace(ll_out$fit, pars = c("alpha", "Lambda_ld", "sigma", "psi", "rho", "kappa"))

## END SCRIPT ## ---------------------------------------------------------------