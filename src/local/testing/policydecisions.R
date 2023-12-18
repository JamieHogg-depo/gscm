# policy decisions

# convert proportions to quantiles
temp <- bind_cols(lapply(y_mats$point[,-c(1,2)], ggplot2::cut_number, n = 100, labels = FALSE))

# make dataset of top areas
list(
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
  dplyr::select(Index, Sa2_name16, Ste_name16, N_persons, 8:12) %>% 
  rename(`Area Name` = Sa2_name16,
         `State` = Ste_name16,
         `Population of area` = N_persons,
         `Inadequate physical activity` = activityleiswkpl,
         `Risky alcohol consumption` = alcohol,
         `Inadequate diet` = diet,
         `Current smoking` = smoking,
         `Overweight/obese` = overweight) %>% 
  knitr::kable(., "latex", booktabs = TRUE)

# cleanup
rm(temp)
