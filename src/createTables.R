####
## Create model tables 
####

# Define functions
getSumColumn <- function(point, lower, upper, rr){
  point <- sprintf(point, fmt = paste0('%#.',rr,'f'))
  lower <- sprintf(lower, fmt = paste0('%#.',rr,'f'))
  upper <- sprintf(upper, fmt = paste0('%#.',rr,'f'))
  as.character(paste0(point, " (", lower, ", ", upper, ")"))
}

getLooColumn <- function(loo, se, rr){
  loo = sprintf(loo, fmt = paste0('%#.',rr,'f'))
  se = sprintf(se, fmt = paste0('%#.',rr,'f'))
  as.character(paste0(loo, " (", se, ")"))
}

getModelSpecs <- function(cur_model_spec){
  shared = ifelse(cur_model_spec$shared_latent_rho_fixed == 0, "IID",
                  ifelse(cur_model_spec$shared_latent_rho_fixed == 1, "ICAR", "LCAR"))
  specific = ifelse(cur_model_spec$specific_latent_rho_fixed == 0, "IID",
                  ifelse(cur_model_spec$specific_latent_rho_fixed == 1, "ICAR", "LCAR"))
  paste0("L: ", cur_model_spec$L, ", Shared: ", shared, ", Specific: ", specific)
}

getMedQuant <- function(x, rr){
  median = sprintf(median(x, na.rm = T), fmt = paste0('%#.',rr,'f'))
  q25 = sprintf(quantile(x, p = 0.025, na.rm = T), fmt = paste0('%#.',rr,'f'))
  q75 = sprintf(quantile(x, p = 0.975, na.rm = T), fmt = paste0('%#.',rr,'f'))
  paste0(median, " (", q25, ", ", q75, ")")
}

# Hyperpriors
foo <- function(x){
out_all[[x]]$summ_hp %>% 
  filter(!str_detect(variable, "Lambda")) %>% 
  mutate(sum = getSumColumn(point, lower, upper, 2)) %>% 
  dplyr::select(variable, sum) %>% 
  pivot_wider(names_from = variable, values_from = sum) %>% 
  mutate(model_spec = getModelSpecs(out_all[[x]]$cur_model_spec)) %>% 
  relocate(model_spec)
}
hyper_table <- bind_rows
rm(foo)

# Model fit 
foo <- function(x){
out_all[[x]]$perf %>% 
  mutate(loo = getLooColumn(elpd_loo, elpd_loo_se, 1),
         model_spec = getModelSpecs(out_all[[x]]$cur_model_spec)) %>% 
  dplyr::select(model_spec, loo)
}
modelfit_table <- bind_rows(lapply(1:length(out_all), foo))
rm(foo)

## Average SE of raw latent factors
foo <- function(x){
out_all[[x]]$summ_latent1$raww %>% 
  summarise(summ_SE = getMedQuant(se, 2)) %>% 
  mutate(model_spec = getModelSpecs(out_all[[x]]$cur_model_spec)) %>% 
  dplyr::select(model_spec, summ_SE)
}
latentvar_table <- bind_rows(lapply(1:length(out_all), foo))
rm(foo)

## JOIN ALL TABLES ## ----------------------------------------------------------
list(latentvar_table, modelfit_table, hyper_table) %>% 
  reduce(left_join, by = "model_spec")

## END SCRIPT ## ---------------------------------------------------------------
