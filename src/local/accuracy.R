


getAccuracy <- function(factor_id, filter_var, cutoff = 90){

  foo <- function(x){cut_number(x, n = 100, labels = FALSE) > cutoff}
  filter_var_sym <- sym(filter_var)
    
  if(factor_id == 1){
    accu <- cur_list$data$y %>% 
      mutate_all(foo) %>% 
      mutate(lg = foo(cur_list$summ_latent1$raww$point)) %>% 
      filter(!!filter_var_sym) %>%
      group_by(lg, !!filter_var_sym) %>% 
      tally(.) %>% ungroup() %>% 
      mutate(p = n/sum(n)) %>% 
      as.matrix()
  }else if(factor_id == 2){
    accu <- cur_list$data$y %>% 
      mutate_all(foo) %>% 
      mutate(lg = foo(cur_list$summ_latent2$raww$point)) %>% 
      filter(!!filter_var_sym) %>%
      group_by(lg, !!filter_var_sym) %>% 
      tally(.) %>% ungroup() %>% 
      mutate(p = n/sum(n)) %>% 
      as.matrix()
  }else{
    accu <- cur_list$data$y %>% 
      mutate_all(foo) %>% 
      mutate(lg = foo(cur_list$summ_latent3$raww$point)) %>% 
      filter(!!filter_var_sym) %>%
      group_by(lg, !!filter_var_sym) %>% 
      tally(.) %>% ungroup() %>% 
      mutate(p = n/sum(n)) %>% 
      as.matrix()
  }
  
  if(nrow(accu) == 2){
    return(unname(accu[2,4]))
  }else{
    return(NA)
  }
 

}

df <- expand.grid(rf = names(cur_list$data$y),
                  factor = 1:3, 
                  cutoff = c(80, 85, 90, 95)) %>% 
  mutate(accuracy = NA)
for(i in 1:nrow(df)){
  df$accuracy[i] <- getAccuracy(df$factor[i], as.character(df$rf[i]), df$cutoff[i])
}
df %>% 
  pivot_wider(names_from = factor, values_from = accuracy)

