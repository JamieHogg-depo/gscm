####
## summ
####

# convergence stats
conv_list <- list()
conv_list$perc_Rhatgr1.02 <- 100*mean(ll_out$summ$rhat > 1.02, na.rm = T)
conv_list$max_Rhat <- max(ll_out$summ$rhat, na.rm = T)
conv_list$perc_ebbelow400 <- 100*mean(ll_out$summ$ess_bulk < 400, na.rm = T)
conv_list$perc_etbelow400 <- 100*mean(ll_out$summ$ess_tail < 400, na.rm = T)
conv_list$nu_div <- get_num_divergent(ll_out$fit)
conv_list$nu_tree <- get_num_max_treedepth(ll_out$fit)
conv_list$bfmi <- length(which(rstan::get_bfmi(ll_out$fit) < 0.2))

# get latent field
draws <- rstan::extract(ll_out$fit)
ll_out$latent_draws <- draws$z

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
					 as.data.frame(conv_list),
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

# Save output - full
cc = paste0(paste0(names(ll_out$cur_model_spec)[1:3], "_", ll_out$cur_model_spec[1,1:3]), collapse = "__")
saveRDS(ll_out, paste0(base_folder, "/outputs/", cur_date, "/r/", which_m, "_", cc, "_ix", grid_ix, "_f.rds"))
#saveRDS(ll_out, paste0(base_folder, "/outputs/", cur_date, "/r/", which_m, "_ix", grid_ix, "_f.rds"))

# Save output
ll_out$fit <- NULL
ll_out$latent_draws <- NULL
ll_out$trace_pl <- NULL
ll_out$ppc <- NULL
saveRDS(ll_out, paste0(base_folder, "/outputs/", cur_date, "/r/", which_m, "_", cc, "_ix", grid_ix, ".rds"))
#saveRDS(ll_out, paste0(base_folder, "/outputs/", cur_date, "/r/", which_m, "_ix", grid_ix, ".rds"))

## END SCRIPT ## ---------------------------------------------------------------