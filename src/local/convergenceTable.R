## Create convergence table for additional file ## -----------------------------

cur_list$conv %>% 
  filter(set != "hyperpriors") %>% 
  dplyr::select(1:10) %>% 
  bind_rows(.,jf$getSubsetConvergence(cur_list$summ, "Lambda_ld|Lambda_d|sigma|rho", "hyperpriors")) %>% 
  dplyr::select(set, n_params, max_Rhat, min_essbulk, min_esstail) %>%
  filter(set %in% c("hyperpriors", "latents", "epsilon")) %>% 
  knitr::kable(., "latex", digits = c(NA, 0, 3, 0, 0))

cur_list$summ %>% 
  mutate(mcse = sd/sqrt(ess_bulk)) %>% 
  group_by(variable_gr) %>% 
  summarise(max_mcse = max(mcse)) %>% 
  filter(str_detect(variable_gr, "Lambda_ld|Lambda_d|sigma|rho|z|epsilon"))

## create plots for sensitivity analysis ## ------------------------------------

## ---- Setup ---- ##

# empty environment 
rm(list = ls())

# load and define functions
source("src/local/funs.R")
createSensitivityPlot <- function(y_id, y_lab,
                                  x_id, x_lab, 
                                  which_factor = 1){

cbind(out_all[[y_id]][[paste0("summ_latent",which_factor)]]$raww %>% setNames(paste0("y_", names(.))),
      out_all[[x_id]][[paste0("summ_latent",which_factor)]]$raww %>% setNames(paste0("x_", names(.)))) %>% 
  ggplot(aes(y = y_point, ymin = y_lower, ymax = y_upper,
             x = x_point, xmin = x_lower, xmax= x_upper))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline(col = "blue")+
  labs(y = y_lab,
       x = x_lab) + #,
       #title = paste0("Posterior distribution of shared factor ", which_factor))+
  theme(text = element_text(size = 8))

}

# Load modelled results
cur_date <- c("202405031")
files <- list.files(paste0("Z:/gscm/outputs/", cur_date, "/r"), full.names = T)
files_fl <- files[!str_detect(files, "_f.rds|_fitonly.rds|_tr.rds|_ld.rds")]
out_all <- lapply(files_fl, readRDS)
names(out_all) <- files_fl
grid <- bind_rows(lapply(1:length(out_all), FUN = function(x)out_all[[x]]$cur_model_spec), .id = "ix")

## ---- Create plots ---- ##

grid %>% 
  dplyr::select(ix, gamma_var_prior, beta_sa_prior, gamma_a, gamma_b)

# Latent 1

# Plot 1
createSensitivityPlot(1,"Tau ~ Gamma(2,3)", 
                      5, "Tau ~ Gamma(5,5)", 1)

jf$jsave(filename = "sensitivity/sensitivity_latent1_1.png",
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

# Plot 2
createSensitivityPlot(1,"Tau ~ Gamma(2,3)", 
                      4, "Tau ~ Gamma(1.5,1.5)", 1)

jf$jsave(filename = "sensitivity/sensitivity_latent1_2.png",
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

# Plot 3
createSensitivityPlot(1,"Tau ~ Gamma(2,3)", 
                      9, "Tau ~ Half-Standard Normal", 1)

jf$jsave(filename = "sensitivity/sensitivity_latent1_3.png",
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

# Plot 4
createSensitivityPlot(1,"Rho ~ Beta(6,2)", 
                      8, "Rho ~ Uniform(0,1)", 1)

jf$jsave(filename = "sensitivity/sensitivity_latent1_4.png",
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

# Latent 2

# Plot 1
createSensitivityPlot(1,"Tau ~ Gamma(2,3)", 
                      5, "Tau ~ Gamma(5,5)", 2)

jf$jsave(filename = "sensitivity/sensitivity_latent2_1.png",
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

# Plot 2
createSensitivityPlot(1,"Tau ~ Gamma(2,3)", 
                      4, "Tau ~ Gamma(1.5,1.5)", 2)

jf$jsave(filename = "sensitivity/sensitivity_latent2_2.png",
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

# Plot 3
createSensitivityPlot(1,"Tau ~ Gamma(2,3)", 
                      9, "Tau ~ Half-Standard Normal", 2)

jf$jsave(filename = "sensitivity/sensitivity_latent2_3.png",
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

# Plot 4
createSensitivityPlot(1,"Rho ~ Beta(6,2)", 
                      8, "Rho ~ Uniform(0,1)", 2)

jf$jsave(filename = "sensitivity/sensitivity_latent2_4.png",
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

## END SCRIPT ## ---------------------------------------------------------------