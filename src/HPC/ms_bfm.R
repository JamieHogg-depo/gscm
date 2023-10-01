####
## Master file
####

# Load packages
	library(tidyverse)
	library(rstan)
	#rstan_options(auto_write = TRUE)
	library(posterior)
	library(spdep)
	library(sf)
	library(Matrix)
	library(loo)
	library(bayesplot)

# Load functions
	source(paste0(base_folder, "/r_src/funs.R"))
	
# Set Stan settings
	chains = 4
	iter = 10000 
	warmup = 4000
	thin = 3

# Load data

	# global_obj
	global_obj <- readRDS(paste0(base_folder, "/r_src/data/global_obj.rds"))
	area_concor <- global_obj$area_concor %>%
		mutate(SA2 = ifelse(SA2 == 114011271, 901031003, SA2))

	# data
	rf_list <- readRDS(paste0(base_folder, "/r_src/data/y_mats_unc.rds"))

	# geography
	map_temp <- st_read(paste0(base_folder, "/r_src/data/shape/SA2_2016_AUSTv4_m.shp")) %>% 
		mutate(SA2 = as.numeric(Sa2_main16)) %>%
		right_join(.,area_concor, by = "SA2") %>%
		arrange(ps_area)
	message(paste0("map_temp has ", nrow(map_temp), " rows."))
	map_out <- jf$connectW(map_temp)
	W <- map_out$W

	# Set input data
	if(all_aus){
		census <- global_obj$census
		# risk factor estimates
		rf_data <- rf_list$point[,-c(1:2)]
		rf_data_sd <- rf_list$sd[,-c(1:2)]
	}else{
		census <- global_obj$census %>% filter(ps_state == 1)
		# weight matrix
		W <- W[global_obj$census$ps_state == 1, global_obj$census$ps_state == 1]
		# risk factor estimates
		rf_data <- rf_list$point[global_obj$census$ps_state == 1,-c(1:2)]
		rf_data_sd <- rf_list$sd[global_obj$census$ps_state == 1,-c(1:2)]
	}

# spatial objects
listw <- mat2listw(W)
for_stan <- jf$prep4MLCAR(W)
icar_for_stan <- jf$prep4ICAR(W)

# Define grid of values
grid <- expand.grid(L = c(1,2),
                    shared_latent_rho_fixed = c(0,1,2),
                    specific_latent_rho_fixed = 0, #c(0,1,2),
					kappa_fixed = 0.9,
                    gamma_var_prior = 1,
					beta_sa_prior = 1,
                    me0_std = 0.01,
					meas_dist = 0, 
                    me = 1,
                    gamma_a = 2,
                    gamma_b = 3,
					latent_var_fixed = 1, #c(0,1),
					scale_data = 1, #c(0,1),
					fo = "alcohol__smoking")
grid2 <- data.frame(L = c(1,2,2),
                   shared_latent_rho_fixed = c(0,0,2),
                   specific_latent_rho_fixed = 0,
                   gamma_var_prior = 1,
                   me0_std = 0.01,
                   me = c(0,0,0),
                   gamma_a = 2,
                   gamma_b = 1,
				   latent_var_fixed = 1, 
				   scale_data = 1,
				   fo = "activityleiswkpl__alcohol")
cur_model_spec <- grid[grid_ix,]

# Recorder columns
feat_order <- as.character(str_split_fixed(cur_model_spec$fo, "__", 2))
rf_data <- rf_data %>% relocate(feat_order)
rf_data_sd <- rf_data_sd %>% relocate(feat_order)

# prepare output list
ll_out <- list()
ll_out$cur_model_spec = cur_model_spec

# compile model
unlink(paste0(base_folder, "/r_src/stan/*.rds"))
comp <- stan_model(file = paste0(base_folder, "/r_src/stan/BFM.stan"))

# Scale data
if(cur_model_spec$scale_data == 1){
	tt <- jf$scaleData(rf_data, rf_data_sd)
	rf_data <- tt$data
	rf_data_sd <- tt$data_sd
	rm(tt)
}

# data list
d <- list(N = nrow(rf_data), 
          K = ncol(rf_data),
          Y = rf_data)
d <- c(d, for_stan, icar_for_stan, cur_model_spec)
d$M <- d$L*(d$K-d$L)+ d$L*(d$L-1)/2

# Initial values
if(d$L == 1){
	init_fun = function() {
	  init.values<-list(Lambda_d = array(runif(1,.77,.78), dim = 1),
						Lambda_ld=c(-0.2,-0.46,-0.29,0.04)+runif(4,-.01,.01))
	  return(init.values)
	}
}else{
	init_fun = function() {
	  init.values<-list(Lambda_d = runif(d$L,-.01,.01),
						Lambda_ld = runif(d$M,-.01,.01))
	  return(init.values)
	}
}

# fit model
m_s <- Sys.time()
ll_out$fit <- sampling(object = comp, 
                #pars = c("Z_z", "mu"),
				pars = "Z_z",
                include = FALSE,
                data = d, 
                init = init_fun,
				init_r = 0.01,
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
ll_out$trace$hyperparams <- stan_trace(ll_out$fit, pars = c("alpha", "Lambda_ld", "sigma", "psi", "rho"))

# Summarise models
source(paste0(base_folder, "/r_src/summ.R"))

## END SCRIPT ## ---------------------------------------------------------------
