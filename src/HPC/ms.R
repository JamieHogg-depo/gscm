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
		# weight matrix
		#W <- global_obj$W
		for_stan <- jf$prep4MLCAR(W)
		icar_for_stan <- jf$prep4ICAR(W)
		# risk factor estimates
		rf_data <- rf_list$point[,-c(1:2)]
		rf_data_sd <- rf_list$sd[,-c(1:2)]
	}else{
		census <- global_obj$census %>% filter(ps_state == 6)
		# weight matrix
		#W <- global_obj$W[global_obj$census$ps_state == 6, global_obj$census$ps_state == 6]
		W <- W[global_obj$census$ps_state == 6, global_obj$census$ps_state == 6]
		for_stan <- jf$prep4MLCAR(W)
		icar_for_stan <- jf$prep4ICAR(W)
		# risk factor estimates
		rf_data <- rf_list$point[global_obj$census$ps_state == 6,-c(1:2)]
		rf_data_sd <- rf_list$sd[global_obj$census$ps_state == 6,-c(1:2)]
	}

# Define grid of values
grid <- expand.grid(L = c(1,2),
                    shared_latent_rho_fixed = 1, #c(0,1,2),
                    specific_latent_rho_fixed = 0, #c(0,1,2),
                    gamma_var_prior = c(0,1),
                    me0_std = 0.01,
                    me = 1, #c(0,1),
                    gamma_a = 2,
                    gamma_b = 1)
grid <- data.frame(L = c(1,2,2),
                   shared_latent_rho_fixed = c(0,0,2),
                   specific_latent_rho_fixed = 0,
                   gamma_var_prior = 1,
                   me0_std = 0.01,
                   me = c(0,0,0),
                   gamma_a = 2,
                   gamma_b = 1)
cur_model_spec <- grid[grid_ix,]

# prepare output list
ll_out <- list()
ll_out$cur_model_spec = cur_model_spec

# Run models
if(which_m == "BFM") source(paste0(base_folder, "/r_src/BFM.R"))
if(which_m == "GSCM") source(paste0(base_folder, "/r_src/GSCM.R"))

# Summarise models
source(paste0(base_folder, "/r_src/summ.R"))

## END SCRIPT ## ---------------------------------------------------------------
