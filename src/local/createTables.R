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

reLabSS <- function(.data){
  .data %>% 
    mutate(Shared = case_when(
      Shared == 2 ~ "LCAR",
      Shared == 1 ~ "ICAR",
      Shared == 0 ~ "IID",
    ),
    Specific = case_when(
      Specific == 2 ~ "LCAR",
      Specific == 1 ~ "ICAR",
      Specific == 0 ~ "IID",
    ))
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
                     mean = ~sprintf(mean(.), fmt = paste0('%#.',rr,'f')),
                     median = ~sprintf(median(.), fmt = paste0('%#.',rr,'f')),
                     q75 = ~sprintf(quantile(., probs = 0.75), fmt = paste0('%#.',rr,'f')),
                     max = ~sprintf(max(.), fmt = paste0('%#.',rr,'f')))) %>% 
  pivot_longer(everything()) %>% 
  separate(name, c("rf", "metric")) %>% 
  pivot_wider(names_from = metric, values_from = value) %>% 
  left_join(.,lookup) %>% 
  dplyr::select(-1) %>% 
  relocate(rf_full)%>% 
  knitr::kable(., "latex", booktabs = TRUE)

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
  relocate(rf_full)%>% 
  knitr::kable(., "latex", booktabs = TRUE)

## Model comparison ## ---------------------------------------------------------

# Hyperpriors
foo <- function(x){
out_all[[x]]$summ_hp %>% 
  filter(!str_detect(variable, "Lambda|psi")) %>% 
  mutate(sum = getSumColumn(point, lower, upper, 4)) %>% 
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
  mutate(`rho[1]` = ifelse(shared_latent_rho_fixed == 'LCAR', `rho[1]`, NA),
         `rho[2]` = ifelse(shared_latent_rho_fixed == 'LCAR', `rho[2]`, NA),
         `kappa[1]` = NA,
         `kappa[2]` = ifelse(specific_latent_rho_fixed == 'LCAR', `kappa[2]`, NA),
         `kappa[3]` = ifelse(specific_latent_rho_fixed == 'LCAR', `kappa[3]`, NA),
         `kappa[4]` = ifelse(specific_latent_rho_fixed == 'LCAR', `kappa[4]`, NA),
         `kappa[5]` = ifelse(specific_latent_rho_fixed == 'LCAR', `kappa[5]`, NA))

# Table - model selection
full_table %>% 
  dplyr::select(shared_latent_rho_fixed, specific_latent_rho_fixed, DIC, WAIC, MAB, summ_SE1, summ_SE2) %>% 
  rename(Shared = shared_latent_rho_fixed,
         Specific = specific_latent_rho_fixed) %>% 
  arrange(WAIC) %>% 
  reLabSS() %>% 
  knitr::kable(., "latex", booktabs = TRUE)

# Table - sigma
full_table %>% 
  dplyr::select(shared_latent_rho_fixed, specific_latent_rho_fixed, `sigma[1]`:`sigma[5]`) %>% 
  rename(Shared = shared_latent_rho_fixed,
         Specific = specific_latent_rho_fixed) %>% 
  reLabSS() %>% 
  knitr::kable(., "latex", booktabs = TRUE)

# Table - SA parameters
full_table %>% 
  dplyr::select(shared_latent_rho_fixed, specific_latent_rho_fixed, `rho[1]`:`kappa[5]`) %>% 
  dplyr::select(-`kappa[1]`) %>% 
  rename(Shared = shared_latent_rho_fixed,
         Specific = specific_latent_rho_fixed) %>% 
  #reLabSS() %>% 
  knitr::kable(., "latex", booktabs = TRUE)

## Factor loadings table - measurement error ## --------------------------------

# ME
tf = 16
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
tf = 1
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
tf = 11
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
tf = 10
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

tf = 5
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

    # subset census
    census <- filter(global_obj$census, N_persons > 99)

    # principal components
    pr <- prcomp(data, scale. = T)

    # convert proportions to quantiles
    rf_point_perc <- bind_cols(lapply(y_mats$point[,-c(1,2)], ggplot2::cut_number, n = 100, labels = FALSE))
    rf_se_perc <- bind_cols(lapply(y_mats$sd[,-c(1,2)], ggplot2::cut_number, n = 100, labels = FALSE))
    names(rf_se_perc) <- paste0(names(rf_se_perc), "_se")
    
    # coefficients of variation
    rf_cv <- 100*(y_mats$sd[,-c(1,2)]/y_mats$point[,-c(1,2)])
    names(rf_cv) <- paste0(names(rf_cv), "_cv")
    
    # lower limit of interval
    rf_lci <- bind_cols(lapply(y_mats$point[,-c(1,2)] - 1.96*y_mats$sd[,-c(1,2)], 
                               ggplot2::cut_number, n = 100, labels = FALSE))
    names(rf_lci) <- paste0(names(rf_lci), "_lci")
    
    # make dataset of top areas
    full_selected <- list(
      rf_point_perc %>% 
        cbind(.,rf_se_perc) %>%
        cbind(.,rf_cv) %>%
        mutate(perc95 = raw_RS) %>% 
        cbind(.,map_sa2) %>% 
        right_join(.,census) %>% 
        slice_max(perc95, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "ra_sa2",
                      activityleiswkpl, alcohol, diet, overweight, smoking),
      
      rf_point_perc %>% 
        cbind(.,rf_se_perc) %>%
        cbind(.,rf_cv) %>%
        mutate(perc95 = raw_MMN) %>% 
        cbind(.,map_sa2) %>% 
        right_join(.,census) %>% 
        slice_max(perc95, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "ra_sa2",
                      activityleiswkpl, alcohol, diet, overweight, smoking),
      
      rf_point_perc %>% 
        cbind(.,rf_se_perc) %>%
        cbind(.,rf_cv) %>%
        mutate(perc95 = pr$x[,1]) %>% 
        cbind(.,map_sa2) %>% 
        right_join(.,census) %>% 
        slice_max(perc95, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "ra_sa2", 
                      activityleiswkpl, alcohol, diet, overweight, smoking),
      
      rf_point_perc %>% 
        cbind(.,rf_se_perc) %>%
        cbind(.,rf_cv) %>%
        mutate(perc95 = pr$x[,2]) %>% 
        cbind(.,map_sa2) %>% 
        right_join(.,census) %>% 
        slice_max(perc95, n = 4, with_ties = FALSE) %>%
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "ra_sa2", 
                      activityleiswkpl, alcohol, diet, overweight, smoking),
      
      cur_list$probs$perc$i1 %>% 
        cbind(.,cur_list$summ_latent1$perc) %>% 
        cbind(.,map_sa2) %>% 
        cbind(.,rf_point_perc) %>% 
        cbind(.,rf_se_perc) %>%
        cbind(.,rf_cv) %>%
        right_join(.,census) %>% 
        slice_max(perc95, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "point", "ra_sa2", 
                      activityleiswkpl, alcohol, diet, overweight, smoking),
      
      cur_list$probs$rank$i1_r %>% 
        cbind(.,cur_list$summ_latent1$rankk) %>% 
        cbind(.,map_sa2) %>% 
        cbind(.,rf_point_perc) %>% 
        right_join(.,census) %>% 
        slice_max(rank10, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "rank10", "Ste_name16", "N_persons", "point", "ra_sa2", 
                      activityleiswkpl, alcohol, diet, overweight, smoking),
      
      cur_list$probs$perc$i2 %>% 
        cbind(.,cur_list$summ_latent2$perc) %>%
        cbind(.,map_sa2) %>% 
        cbind(.,rf_point_perc) %>%
        cbind(.,rf_se_perc) %>%
        cbind(.,rf_cv) %>%
        right_join(.,census) %>% 
        slice_max(perc95, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "point", "ra_sa2", 
                      activityleiswkpl, alcohol, diet, overweight, smoking),
      
      cur_list$probs$rank$i2_r %>% 
        cbind(.,cur_list$summ_latent2$rankk) %>% 
        cbind(.,map_sa2) %>% 
        cbind(.,rf_point_perc) %>% 
        right_join(.,census) %>% 
        slice_max(rank10, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "rank10", "Ste_name16", "N_persons", "point", "ra_sa2", 
                      activityleiswkpl, alcohol, diet, overweight, smoking),
      
      cur_list$probs$perc$i3 %>% 
        cbind(.,cur_list$summ_latent3$perc) %>%
        cbind(.,map_sa2) %>%
        cbind(.,rf_point_perc) %>%
        cbind(.,rf_se_perc) %>%
        #cbind(.,rf_cv) %>% 
        right_join(.,census) %>%
        slice_max(perc95, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "point", "ra_sa2", 
                      activityleiswkpl, alcohol, diet, overweight, smoking),
      
      cur_list$probs$rank$i3_r %>% 
        cbind(.,cur_list$summ_latent3$rankk) %>% 
        cbind(.,map_sa2) %>% 
        cbind(.,rf_point_perc) %>% 
        right_join(.,census) %>% 
        slice_max(rank10, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "rank10", "Ste_name16", "N_persons", "point", "ra_sa2", 
                      activityleiswkpl, alcohol, diet, overweight, smoking),
      
      cur_list$probs$perc$i4 %>% 
        cbind(.,cur_list$summ_latent4$perc) %>%
        cbind(.,map_sa2) %>% 
        cbind(.,rf_point_perc) %>%
        cbind(.,rf_se_perc) %>%
        cbind(.,rf_cv) %>%
        right_join(.,census) %>%
        slice_max(perc95, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "perc95", "Ste_name16", "N_persons", "point", "ra_sa2", 
                      activityleiswkpl, alcohol, diet, overweight, smoking),
      
      cur_list$probs$rank$i4_r %>% 
        cbind(.,cur_list$summ_latent4$rankk) %>% 
        cbind(.,map_sa2) %>% 
        cbind(.,rf_point_perc) %>% 
        right_join(.,census) %>% 
        slice_max(rank10, n = 4, with_ties = FALSE) %>% 
        dplyr::select("Sa2_name16", "rank10", "Ste_name16", "N_persons", "point", "ra_sa2", 
                      activityleiswkpl, alcohol, diet, overweight, smoking)
      
    ) %>% 
      bind_rows(.id = "Index") %>% 
      group_by(Index) %>% 
      mutate(total_pop_effected = sum(N_persons)) %>% 
      ungroup() %>% 
      mutate(Index = c(rep("Rank Sum", 4),
                       rep("Min-Max Normalisation", 4),
                       rep("PC1", 4),
                       rep("PC2", 4),
                       rep("Index 1", 4),
                       rep("Index 1 (top 20)", 4),
                       rep("Index 2", 4),
                       rep("Index 2 (top 20)", 4),
                       rep("Index 3 - combined", 4),
                       rep("Index 3 - combined (top 20)", 4),
                       rep("Index 4 - combined PW", 4),
                       rep("Index 3 - combined PW (top 20)", 4)),
             N_persons = round(N_persons),
             Ste_name16 = case_when(
               Ste_name16 == "New South Wales" ~ "NSW",
               Ste_name16 == "Queensland" ~ "QLD",
               Ste_name16 == "Australian Capital Territory" ~ "ACT",
               Ste_name16 == "Northern Territory" ~ "NT",
               Ste_name16 == "Western Australia" ~ "WA",
               Ste_name16 == "Victoria" ~ "VIC"
             ))
    round(unique(full_selected$total_pop_effected), -2)
    
    full_selected %>% 
      dplyr::select(Index, Sa2_name16, Ste_name16, N_persons, 
                    activityleiswkpl, alcohol, diet, overweight, smoking) %>% #,
                    #activityleiswkpl_se, alcohol_se, diet_se, overweight_se, smoking_se) %>% 
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
    rm(pr)
    
## TABLE 6 ## ------------------------------------------------------------------

# Percentiles    
  rf_se_perc <- bind_cols(lapply(y_mats$sd[,-c(1,2)], 
                                 ggplot2::cut_number, 
                                 n = 100, labels = FALSE)) %>% 
    setNames(paste0(names(.), "_se")) %>% 
    cbind(.,map_sa2)
  
  # Collapsed percentiles: point (error) --- both percentiles
  intern2 <- left_join(full_selected[c(2,7:11)], rf_se_perc, by = c("Sa2_name16"))
  intern3 <- as.data.frame(lapply(intern2[2:6],as.character))
  for(i in 1:nrow(emp)){
    for(j in 1:5){ # iterate over columns
      # proportions
      #emp[i,j] <- paste0(round(y_mats$point[i,2+j],2), " (", round(y_mats$sd[i,2+j],2), ")")
      # percentages
      intern3[i,j] <- paste0(intern2[i,j+1], " (", intern2[i,j+6], ")")
    }
  }
  perc <- intern3 %>% 
    setNames(paste0(names(.), "_perc"))
  rm(intern2, intern3)
  
# Percentages - actual
  interm1 <- as.data.frame(lapply(y_mats$point[,-c(1:2)], as.character))
  for(i in 1:nrow(y_mats$point)){
    for(j in 1:5){
      # proportions
      #emp[i,j] <- paste0(round(y_mats$point[i,2+j],2), " (", round(y_mats$sd[i,2+j],2), ")")
      # percentages
      interm1[i,j] <- paste0(round(100*y_mats$point[i,2+j]), " (", round(100*y_mats$sd[i,2+j],1), ")")
    }
  }
  percentages <- interm1 %>% 
    setNames(paste0(names(emp), "_percentages")) %>% 
    cbind(.,map_sa2) %>% 
    left_join(full_selected[c(1,2,5)], ., by = c("Sa2_name16")) %>% 
    dplyr::select(Index,
                  Sa2_name16,
                  Ste_name16, 
                  N_persons, 
                  activityleiswkpl_percentages,
                  alcohol_percentages, 
                  diet_percentages, 
                  overweight_percentages, 
                  smoking_percentages)

## JOIN tables
  cbind(percentages, perc)

## TOP 4 AREAS FOR EACH INDEX - standard errors ## -----------------------------

# convert proportions to quantiles
  rf_se_perc <- bind_cols(lapply(y_mats$sd[,-c(1,2)], 
                                 ggplot2::cut_number, 
                                 n = 100, labels = FALSE)) %>% 
        setNames(paste0(names(rf_se_perc), "_se")) %>% 
        cbind(.,map_sa2)
    
# Collapsed percentiles: point (error) --- both percentiles
  comb2 <- left_join(full_selected[c(2,7:11)], rf_se_perc, by = c("Sa2_name16"))
  emp <- as.data.frame(lapply(comb2[2:6],as.character))
  for(i in 1:nrow(emp)){
    for(j in 1:5){ # iterate over columns
      # proportions
      #emp[i,j] <- paste0(round(y_mats$point[i,2+j],2), " (", round(y_mats$sd[i,2+j],2), ")")
      # percentages
      emp[i,j] <- paste0(comb2[i,j+1], " (", comb2[i,j+6], ")")
    }
  }
  emp %>% 
    setNames(paste0(names(emp), "_se")) %>% 
    cbind(.,full_selected[c(1:2)]) %>% 
    filter(Index == "Rank Sum") %>% 
    knitr::kable(., "latex", booktabs = TRUE, format.args = list(big.mark = ","))
    
# Prevalence and standard error
  emp <- as.data.frame(lapply(y_mats$point[,-c(1:2)], as.character))
  for(i in 1:nrow(y_mats$point)){
    for(j in 1:5){
      # proportions
      #emp[i,j] <- paste0(round(y_mats$point[i,2+j],2), " (", round(y_mats$sd[i,2+j],2), ")")
      # percentages
      emp[i,j] <- paste0(round(100*y_mats$point[i,2+j]), " (", round(100*y_mats$sd[i,2+j],1), ")")
    }
  }
  rf_se_perc <- emp %>% 
    setNames(paste0(names(emp), "_se")) %>% 
    cbind(.,map_sa2)

# create temporary data
temp3 <- lapply(c("Rank Sum", "Min-Max Normalisation", 
                  "PC1", "PC2", 
                  "Index 1", "Index 2", 
                  "Index 3 - combined", 
                  "Index 4 - combined PW"), 
                FUN = function(x){rf_se_perc %>% 
                    filter(Sa2_name16 %in% 
                             filter(full_selected, Index == x)$Sa2_name16) %>% 
                    dplyr::select(1:5, Sa2_name16) %>% 
                    mutate(Index = x)}) %>% 
  bind_rows(.)

# join to data
left_join(full_selected[-c(7:11)], temp3, by = c("Index", "Sa2_name16")) %>% 
  dplyr::select(1, 2, 10:14) %>% 
  #filter(Index == "Index 4 - combined PW") %>% 
  filter(Index == "Rank Sum") %>% 
  knitr::kable(., "latex", booktabs = TRUE, format.args = list(big.mark = ","))

# cleanup
rm(temp3, comb2)

## END SCRIPT ## ---------------------------------------------------------------
