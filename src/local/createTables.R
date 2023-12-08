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
  shared = toCharacterSpec(cur_model_spec$shared_latent_rho_fixed)
  specific = toCharacterSpec(cur_model_spec$specific_latent_rho_fixed)
  paste0("L: ", cur_model_spec$L, ", Shared: ", shared, ", Specific: ", specific)
}

getMedQuant <- function(x, rr){
  median = sprintf(median(x, na.rm = T), fmt = paste0('%#.',rr,'f'))
  q25 = sprintf(quantile(x, p = 0.025, na.rm = T), fmt = paste0('%#.',rr,'f'))
  q75 = sprintf(quantile(x, p = 0.975, na.rm = T), fmt = paste0('%#.',rr,'f'))
  paste0(median, " (", q25, ", ", q75, ")")
}

# RF full names
lookup <- data.frame(rf = c("activityleis", 
                            "activityleiswkpl",
                            "alcohol",
                            "diet",
                            "obesity",
                            "overweight",
                            "smoking",
                            "waist_circum"),
                     rf_full = c("Inadequate physical activity (leisure)",
                                 "Inadequate physical activity (all)",
                                 "Risky alcohol consumption",
                                 "Inadequate diet",
                                 "Obese",
                                 "Overweight/obese",
                                 "Current smoking",
                                 "Risky waist circumference"))

## Data summary ## -------------------------------------------------------------

rr <- 2
data %>% 
  summarise_all(list(min = ~sprintf(min(.), fmt = paste0('%#.',rr,'f')),
                     q25 = ~sprintf(quantile(., probs = 0.25), fmt = paste0('%#.',rr,'f')),
                     median = ~sprintf(median(.), fmt = paste0('%#.',rr,'f')),
                     q75 = ~sprintf(quantile(., probs = 0.75), fmt = paste0('%#.',rr,'f')),
                     max = ~sprintf(max(.), fmt = paste0('%#.',rr,'f')))) %>% 
  pivot_longer(everything()) %>% 
  separate(name, c("rf", "metric")) %>% 
  pivot_wider(names_from = metric, values_from = value) %>% 
  left_join(.,lookup) %>% 
  dplyr::select(-1) %>% 
  relocate(rf_full)

rr <- 2
data_sd %>% 
  summarise_all(list(min = ~sprintf(min(.), fmt = paste0('%#.',rr,'f')),
                     q25 = ~sprintf(quantile(., probs = 0.25), fmt = paste0('%#.',rr,'f')),
                     median = ~sprintf(median(.), fmt = paste0('%#.',rr,'f')),
                     q75 = ~sprintf(quantile(., probs = 0.75), fmt = paste0('%#.',rr,'f')),
                     max = ~sprintf(max(.), fmt = paste0('%#.',rr,'f')))) %>% 
  pivot_longer(everything()) %>% 
  separate(name, c("rf", "metric")) %>% 
  pivot_wider(names_from = metric, values_from = value) %>% 
  left_join(.,lookup) %>% 
  dplyr::select(-1) %>% 
  relocate(rf_full)

## Model comparison ## ---------------------------------------------------------

# Hyperpriors
foo <- function(x){
out_all[[x]]$summ_hp %>% 
  filter(!str_detect(variable, "Lambda|psi")) %>% 
  mutate(sum = getSumColumn(point, lower, upper, 2)) %>% 
  dplyr::select(variable, sum) %>% 
  pivot_wider(names_from = variable, values_from = sum) %>% 
  mutate(model_spec = getModelSpecs(out_all[[x]]$cur_model_spec)) %>% 
  relocate(model_spec)
}
# only for L2 and GSCM
hyper_table <- bind_rows(lapply(X = which(grid$L == 2 & grid$model == "GSCM"), FUN = foo))
rm(foo)

# Model fit 
foo <- function(x){
out_all[[x]]$perf %>% 
  mutate(loo = getLooColumn(elpd_loo, elpd_loo_se, 1),
         WAIC2 = getLooColumn(WAIC, WAIC_se, 1),
         WAIC = round(WAIC),
         DIC = round(DIC),
         MAB = round(MAB, 3),
         model_spec = getModelSpecs(out_all[[x]]$cur_model_spec)) %>% 
  dplyr::select(model_spec, DIC, WAIC, MAB)
}
modelfit_table <- bind_rows(lapply(X = which(grid$L == 2 & grid$model == "GSCM"), FUN = foo))

## Table for multishared factors
bind_rows(lapply(X = which(grid$model == "GSCM" &
                           grid$shared_latent_rho_fixed == "LCAR" &
                           grid$specific_latent_rho_fixed == "LCAR"), FUN = foo))
rm(foo)

## Average SE of raw latent factors
foo <- function(x){
cbind(out_all[[x]]$summ_latent1$raww %>% 
  summarise(summ_SE1 = getMedQuant(se, 2)) %>% 
  mutate(model_spec = getModelSpecs(out_all[[x]]$cur_model_spec)) %>% 
  dplyr::select(model_spec, summ_SE1),
out_all[[x]]$summ_latent2$raww %>% 
  summarise(summ_SE2 = getMedQuant(se, 2)) %>% 
  dplyr::select(summ_SE2))
}
latentvar_table <- bind_rows(lapply(X = which(grid$L == 2 & grid$model == "GSCM"), FUN = foo))
rm(foo)

## JOIN ALL TABLES ## ----------------------------------------------------------
full_table <- list(latentvar_table, modelfit_table, hyper_table) %>% 
  reduce(left_join, by = "model_spec") %>% 
  cbind(.,grid[which(grid$L == 2 & grid$model == "GSCM"),]) %>% 
  arrange(shared_latent_rho_fixed, specific_latent_rho_fixed)%>% 
  # Set some cells to NA
  mutate(`rho[1]` = ifelse(shared_latent_rho_fixed == "LCAR", `rho[1]`, NA),
         `rho[2]` = ifelse(shared_latent_rho_fixed == "LCAR", `rho[2]`, NA),
         `kappa[1]` = NA,
         `kappa[2]` = ifelse(specific_latent_rho_fixed == "LCAR", `kappa[2]`, NA),
         `kappa[3]` = ifelse(specific_latent_rho_fixed == "LCAR", `kappa[3]`, NA),
         `kappa[4]` = ifelse(specific_latent_rho_fixed == "LCAR", `kappa[4]`, NA),
         `kappa[5]` = ifelse(specific_latent_rho_fixed == "LCAR", `kappa[5]`, NA))

# Table - model selection
full_table %>% 
  dplyr::select(shared_latent_rho_fixed, specific_latent_rho_fixed, DIC, WAIC, MAB, summ_SE1, summ_SE2) %>% 
  rename(Shared = shared_latent_rho_fixed,
         Specific = specific_latent_rho_fixed) %>% 
  arrange(WAIC) %>% 
  knitr::kable(., "latex", booktabs = TRUE)

# Table - sigma
full_table %>% 
  dplyr::select(shared_latent_rho_fixed, specific_latent_rho_fixed, `sigma[1]`:`sigma[5]`) %>% 
  rename(Shared = shared_latent_rho_fixed,
         Specific = specific_latent_rho_fixed) %>% 
  knitr::kable(., "latex", booktabs = TRUE)

# Table - SA parameters
full_table %>% 
  dplyr::select(shared_latent_rho_fixed, specific_latent_rho_fixed, `rho[1]`:`kappa[5]`) %>% 
  dplyr::select(-`kappa[1]`) %>% 
  rename(Shared = shared_latent_rho_fixed,
         Specific = specific_latent_rho_fixed) %>% 
  knitr::kable(., "latex", booktabs = TRUE)

## Factor loadings table - measurement error ## --------------------------------

# ME
tf = 15
point <- sprintf(out_all[[tf]]$summ_loadings$point, fmt = paste0('%#.2f'))
lower <- sprintf(out_all[[tf]]$summ_loadings$lower, fmt = paste0('%#.2f'))
upper <- sprintf(out_all[[tf]]$summ_loadings$upper, fmt = paste0('%#.2f'))
me <- as.data.frame(matrix(paste0(point, " (", lower, ", ", upper, ")"), nrow = 5, ncol = 2)) %>% 
  setNames(c("Factor 1", "Factor 2")) %>% 
  mutate(Feature = JHCW(names(data)),
         model = "Two-factor GSCM") %>% 
  relocate(model, Feature)
rm(tf, point, lower, upper)

# no ME
tf = 24
point <- sprintf(out_all[[tf]]$summ_loadings$point, fmt = paste0('%#.2f'))
lower <- sprintf(out_all[[tf]]$summ_loadings$lower, fmt = paste0('%#.2f'))
upper <- sprintf(out_all[[tf]]$summ_loadings$upper, fmt = paste0('%#.2f'))
nme <- as.data.frame(matrix(paste0(point, " (", lower, ", ", upper, ")"), nrow = 5, ncol = 2)) %>% 
  setNames(c("nmeFactor 1", "nmeFactor 2"))
rm(tf, point, lower, upper)

# Join
cbind(nme, me)%>% 
  dplyr::select(4,1,2,5,6) %>% 
  knitr::kable(., "latex", booktabs = TRUE)
rm(f2, f1)

## Factor loadings table - multifactor ## --------------------------------------

# Two factor
tf = 10
point <- sprintf(out_all[[tf]]$summ_loadings$point, fmt = paste0('%#.2f'))
lower <- sprintf(out_all[[tf]]$summ_loadings$lower, fmt = paste0('%#.2f'))
upper <- sprintf(out_all[[tf]]$summ_loadings$upper, fmt = paste0('%#.2f'))
f2 <- as.data.frame(matrix(paste0(point, " (", lower, ", ", upper, ")"), nrow = 5, ncol = 2)) %>% 
  setNames(c("Factor 1", "Factor 2")) %>% 
  mutate(Feature = JHCW(names(data)),
         model = "Two-factor GSCM") %>% 
  relocate(model, Feature)
rm(tf, point, lower, upper)

# One factor
tf = 9
point <- sprintf(out_all[[tf]]$summ_loadings$point, fmt = paste0('%#.2f'))
lower <- sprintf(out_all[[tf]]$summ_loadings$lower, fmt = paste0('%#.2f'))
upper <- sprintf(out_all[[tf]]$summ_loadings$upper, fmt = paste0('%#.2f'))
f1 <- as.data.frame(matrix(paste0(point, " (", lower, ", ", upper, ")"), nrow = 5, ncol = 1)) %>% 
  setNames(c("Factor 14"))
rm(tf, point, lower, upper)

# Join
cbind(f1, f2)%>% 
  dplyr::select(3,1,4,5) %>% 
  knitr::kable(., "latex", booktabs = TRUE)
rm(f2, f1)

## FINAL MODEL - FACTOR LOADINGS ## --------------------------------------------

tf = 4
point <- sprintf(out_all[[tf]]$summ_loadings$point, fmt = paste0('%#.2f'))
lower <- sprintf(out_all[[tf]]$summ_loadings$lower, fmt = paste0('%#.2f'))
upper <- sprintf(out_all[[tf]]$summ_loadings$upper, fmt = paste0('%#.2f'))
as.data.frame(matrix(paste0(point, " (", lower, ", ", upper, ")"), nrow = 5, ncol = 2)) %>% 
  setNames(c("Factor 1", "Factor 2")) %>% 
  mutate(Feature = JHCW(names(data))) %>% 
  relocate(Feature) %>% 
  knitr::kable(., "latex", booktabs = TRUE)
rm(tf, point, lower, upper)

## TOP 4 AREAS FOR EACH INDEX ## -----------------------------------------------

    # principal components
    pr <- prcomp(data, scale. = T)

    # convert proportions to quantiles
    temp <- bind_cols(lapply(y_mats$point[,-c(1,2)], ggplot2::cut_number, n = 100, labels = FALSE))
    
    # make dataset of top areas
    list(
      temp %>% 
        mutate(perc95 = raw_RS) %>% 
        cbind(.,map_sa2) %>% 
        left_join(.,global_obj$census) %>% 
        slice_max(perc95, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "ra_sa2", 1:5),
      
      temp %>% 
        mutate(perc95 = pr$x[,1]) %>% 
        cbind(.,map_sa2) %>% 
        left_join(.,global_obj$census) %>% 
        slice_max(perc95, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "ra_sa2", 1:5),
      
      temp %>% 
        mutate(perc95 = pr$x[,2]) %>% 
        cbind(.,map_sa2) %>% 
        left_join(.,global_obj$census) %>% 
        slice_max(perc95, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "ra_sa2", 1:5),
      
      cur_list$probs$latent1 %>% 
        cbind(.,cur_list$summ_latent1$perc) %>% 
        cbind(.,map_sa2) %>% 
        left_join(.,global_obj$census) %>% 
        cbind(.,temp) %>% 
        slice_max(perc95, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "point", "ra_sa2", 50:54),
      
      cur_list$probs$latent2 %>% 
        cbind(.,cur_list$summ_latent2$perc) %>%
        cbind(.,map_sa2) %>% 
        left_join(.,global_obj$census) %>% 
        cbind(.,temp) %>%
        slice_max(perc95, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "point", "ra_sa2", 50:54),
      
      cur_list$probs$latent3 %>% 
        cbind(.,cur_list$summ_latent3$perc) %>%
        cbind(.,map_sa2) %>% 
        left_join(.,global_obj$census) %>% 
        cbind(.,temp) %>%
        slice_max(perc95, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "point", "ra_sa2", 50:54),
      
      cur_list$probs$latent4 %>% 
        cbind(.,cur_list$summ_latent4$perc) %>%
        cbind(.,map_sa2) %>% 
        left_join(.,global_obj$census) %>% 
        cbind(.,temp) %>% 
        slice_max(perc95, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "point", "ra_sa2", 50:54)
    ) %>% 
      bind_rows(.id = "Index") %>% 
      group_by(Index) %>% 
      mutate(total_pop_effected = sum(N_persons)) %>% 
      ungroup() %>% 
      mutate(Index = c(rep("Rank Sum", 4),
                       rep("PC1", 4),
                       rep("PC2", 4),
                       rep("Index 1", 4),
                       rep("Index 2", 4),
                       rep("Index 3 - combined", 4),
                       rep("Index 4 - combined PW", 4)),
             N_persons = round(N_persons),
             Ste_name16 = case_when(
               Ste_name16 == "New South Wales" ~ "NSW",
               Ste_name16 == "Queensland" ~ "QLD",
               Ste_name16 == "Australian Capital Territory" ~ "ACT",
               Ste_name16 == "Northern Territory" ~ "NT",
               Ste_name16 == "Western Australia" ~ "WA",
               Ste_name16 == "Victoria" ~ "VIC"
             )) %>% 
      dplyr::select(Index, Sa2_name16, Ste_name16, N_persons, activityleiswkpl, alcohol, diet, overweight, smoking) %>% 
      rename(`Area Name` = Sa2_name16,
             `State` = Ste_name16,
             `Population of area` = N_persons,
             `Inadequate physical activity` = activityleiswkpl,
             `Risky alcohol consumption` = alcohol,
             `Inadequate diet` = diet,
             `Current smoking` = smoking,
             `Overweight/obese` = overweight) %>% 
      knitr::kable(., "latex", booktabs = TRUE, format.args = list(big.mark = ","))
    
    # cleanup
    rm(temp, pr)

## END SCRIPT ## ---------------------------------------------------------------
