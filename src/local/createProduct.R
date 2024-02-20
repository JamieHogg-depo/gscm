# create HDCIA Product

# Write data to excel in different sheets
wb <- createWorkbook("AIBIC_Product.xlsx")

for(ix in 1:4){

  temp <- cbind(
    SA2_2016 = cur_list$data$census$SA2,
    STATE_2016 = cur_list$data$map$Ste_name16,
    # - NATIONAL - #
    cur_list[[paste0("summ_latent", ix)]]$raww %>% 
      dplyr::select(point, lower, upper) %>% 
      setNames(paste0("score_", names(.))),
    # -- #
    cur_list[[paste0("summ_latent", ix)]]$perc %>% 
      dplyr::select(point, lower, upper) %>% 
      setNames(paste0("national_percentile_", names(.))),
    # -- #
    national_percentile_PPAbove80 = cur_list$probs$perc[[paste0("i", ix)]]$perc80,
    national_percentile_PPAbove95 = cur_list$probs$perc[[paste0("i", ix)]]$perc95,
    national_percentile_PPAbove99 = cur_list$probs$perc[[paste0("i", ix)]]$perc99,
    # -- #
    cur_list[[paste0("summ_latent", ix)]]$rankk %>% 
      dplyr::select(point, lower, upper) %>% 
      setNames(paste0("national_rank_", names(.))),
    # -- #
    national_rank_PPtop10 = cur_list$probs$rank[[paste0("i", ix, "_r")]]$rank10,
    national_rank_PPtop20 = cur_list$probs$rank[[paste0("i", ix, "_r")]]$rank20,
    national_rank_PPtop100 = cur_list$probs$rank[[paste0("i", ix, "_r")]]$rank100,
    # - STATE - #
    cur_list[[paste0("summ_latent", ix)]]$perc_s %>% 
      dplyr::select(point, lower, upper) %>% 
      setNames(paste0("state_percentile_", names(.))),
    # -- #
    state_percentile_PPAbove80 = cur_list$probs$perc[[paste0("i", ix, 's')]]$perc80,
    state_percentile_PPAbove95 = cur_list$probs$perc[[paste0("i", ix, 's')]]$perc95,
    state_percentile_PPAbove99 = cur_list$probs$perc[[paste0("i", ix, 's')]]$perc99,
    # -- #
    cur_list[[paste0("summ_latent", ix)]]$rankk_s %>% 
      dplyr::select(point, lower, upper) %>% 
      setNames(paste0("state_rank_", names(.))),
    # -- #
    state_rank_PPtop10 = cur_list$probs$rank[[paste0("i", ix, "s_r")]]$rank10,
    state_rank_PPtop20 = cur_list$probs$rank[[paste0("i", ix, "s_r")]]$rank20,
    state_rank_PPtop100 = cur_list$probs$rank[[paste0("i", ix, "s_r")]]$rank100
  )
  
  addWorksheet(wb, paste0("Index ", ix))
  writeData(wb, ix, temp)

}
saveWorkbook(wb, "AIBIC_Product.xlsx", overwrite = T)

## END SCRIPT ## ---------------------------------------------------------------
