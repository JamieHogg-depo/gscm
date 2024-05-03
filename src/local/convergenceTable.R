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

# define function
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
       x = x_lab,
       title = paste0("Posterior distribution of shared factor ", which_factor))+
  theme(text = element_text(size = 8))

}

# create plots
createSensitivityPlot(3,"Gamma", 5, "Gamma", 2)

jf$jsave(filename = "sensitivity_Gamma12.png",
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

# create plots
createSensitivityPlot(3,"Gamma", 5, "Gamma", 2)

jf$jsave(filename = "sensitivity_Gamma12.png",
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

## END SCRIPT ## ---------------------------------------------------------------