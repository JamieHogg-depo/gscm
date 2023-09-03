####
## summ
####

# trace plots
worst_rhats <- ll_out$summ %>% filter(!str_detect(variable, "log_lik\\[|yrep\\[|epsilon\\[")) %>% arrange(desc(rhat)) %>% head(10)
ll_out$trace$worst <- stan_trace(ll_out$fit, pars = worst_rhats$variable)

# lambda
ll_out$Lambda_point <- matrix(ll_out$summ[str_detect(ll_out$summ$variable, "Lambda\\["),]$mean, 
                              byrow = F, ncol = d$L)

# Add cur model spec
ll_out$cur_model_spec <- ll_out$cur_model_spec %>% mutate(model = which_m) %>% relocate(model)

# data
ll_out$data$y <- rf_data
ll_out$data$y_sd <- rf_data_sd
ll_out$data$W <- W

# NUTS diagnostics
diag_list <- list()
diag_list$nu_div <- get_num_divergent(ll_out$fit)
diag_list$nu_tree <- get_num_max_treedepth(ll_out$fit)
diag_list$bfmi <- length(which(rstan::get_bfmi(ll_out$fit) < 0.2))

# Convergence
ll_out$conv <- bind_rows(jf$getSubsetConvergence(ll_out$summ, "-log_lik\\[|yrep\\[|epsilon\\[", "all except"),
						 jf$getSubsetConvergence(ll_out$summ, "", "all"),
						 jf$getSubsetConvergence(ll_out$summ, "log_lik\\[|yrep\\[|epsilon\\[", "loglik_yrep_epsilon"),
						 jf$getSubsetConvergence(ll_out$summ, "alpha|Lambda_ld|sigma|psi\\[|rho|kappa", "hyperpriors"),
						 jf$getSubsetConvergence(ll_out$summ, "z\\[", "latents"),
						 jf$getSubsetConvergence(ll_out$summ, "epsilon\\[", "epsilon")) %>%
				bind_cols(.,ll_out$cur_model_spec)
				
# Get posterior draws
draws <- rstan::extract(ll_out$fit)
# get latent field
ll_out$latent_draws <- draws$z

# summarise residuals
summ_epsilon <- apply(draws$epsilon, c(2,3), median)
# Moran test of residuals
ll_out$moran_pvalues <- unlist(lapply(1:5, FUN = function(x)moran.mc(summ_epsilon[,x], listw = listw, nsim = 999)$p.value))

# Get results for hyperpriors
foo <- function(x){
	if(x %in% names(draws)) {
	  jf$getResultsData(draws[[x]]) %>% 
		mutate(variable = paste0(x, "[", 1:nrow(.), "]")) %>% 
		relocate(variable)
	}else{
	  NULL
	}
}
ll_out$summ_hp <- bind_rows(lapply(c("sigma", "Lambda_ld", "psi", "rho", "kappa"), foo))
rm(foo)


# Summarise latent field
summ <- list()
summ$raww <- jf$getResultsData(draws$z[,,1])
summ$norm <- jf$getResultsData(t(apply(draws$z[,,1], 1, FUN = function(x)(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)))))
summ$perc <- jf$getResultsData(t(apply(draws$z[,,1], 1, ggplot2::cut_number, n = 100, labels = FALSE)))
summ$rankk <- jf$getResultsData(t(apply(draws$z[,,1], 1, FUN = function(x)order(order(x)))))
ll_out$summ_latent1 <- summ
if(ll_out$cur_model_spec$L > 1){
	summ <- list()
	summ$raww <- jf$getResultsData(draws$z[,,2])
	summ$norm <- jf$getResultsData(t(apply(draws$z[,,2], 1, FUN = function(x)(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)))))
	summ$perc <- jf$getResultsData(t(apply(draws$z[,,2], 1, ggplot2::cut_number, n = 100, labels = FALSE)))
	summ$rankk <- jf$getResultsData(t(apply(draws$z[,,2], 1, FUN = function(x)order(order(x)))))
	ll_out$summ_latent2 <- summ
}

# LOOCV
log_lik <- extract_log_lik(ll_out$fit, merge_chains=F)
r_eff <- relative_eff(exp(log_lik))
ll_out$loo <- loo(log_lik, r_eff = r_eff)

# Performance and comparison
ll_out$perf <- cbind(ll_out$cur_model_spec, 
					 as.data.frame(diag_list),
					 data.frame(elpd_loo = as.numeric(ll_out$loo$estimates[1,1]), 
					            elpd_loo_se = as.numeric(ll_out$loo$estimates[1,2]),
								perc_lookgr1 = 100*mean(ll_out$loo$diagnostics$pareto_k > 1),
								perc_lookgr07 = 100*mean(ll_out$loo$diagnostics$pareto_k > 0.7),
								rt_mins = ll_out$rt,
								rt_hours = ll_out$rt/60))

# PPC
ppc_list <- list()
for(i in 1:5){
	ppc_list[[i]] <- ppc_dens_overlay(as.numeric(unlist(rf_data[,i])), as.matrix(draws$yrep[,,i])[1:100,])
}
ll_out$ppc <- ppc_list

# Define file name
cc = str_remove_all(paste0(paste0(names(ll_out$cur_model_spec)[1:3], "_", ll_out$cur_model_spec[1,1:3]), collapse = "__"), "_latent_rho_fixed_")

# Save output - stan fit object only
saveRDS(ll_out$fit, paste0(base_folder, "/outputs/", cur_date, "/r/ix", grid_ix, "_", cc, "_fitonly.rds"))

# Save output - full
ll_out$fit <- NULL
saveRDS(ll_out, paste0(base_folder, "/outputs/", cur_date, "/r/ix", grid_ix, "_", cc, "_f.rds"))

# Save output
ll_out$latent_draws <- NULL
#ll_out$trace_pl <- NULL
ll_out$ppc <- NULL
saveRDS(ll_out, paste0(base_folder, "/outputs/", cur_date, "/r/ix", grid_ix, "_", cc, ".rds"))

## END SCRIPT ## ---------------------------------------------------------------