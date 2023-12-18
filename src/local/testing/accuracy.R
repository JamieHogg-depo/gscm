# define function
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
  }else if(factor_id == 3){
    accu <- cur_list$data$y %>% 
      mutate_all(foo) %>% 
      mutate(lg = foo(cur_list$summ_latent3$raww$point)) %>% 
      filter(!!filter_var_sym) %>%
      group_by(lg, !!filter_var_sym) %>% 
      tally(.) %>% ungroup() %>% 
      mutate(p = n/sum(n)) %>% 
      as.matrix()
  }else{
    accu <- cur_list$data$y %>% 
      mutate_all(foo) %>% 
      mutate(lg = foo(cur_list$summ_latent4$raww$point)) %>% 
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
                  factor = 1:4, 
                  cutoff = c(seq(50, 95, by = 5), 99)) %>% 
  mutate(accuracy = NA)
for(i in 1:nrow(df)){
  df$accuracy[i] <- getAccuracy(factor_id = df$factor[i], 
                                filter_var = as.character(df$rf[i]), 
                                cutoff = df$cutoff[i])
  
}
df %>% 
  pivot_wider(names_from = factor, values_from = accuracy)

# Plot 
df %>% 
  ggplot(aes(y = accuracy, x = rf, fill = as.factor(factor)))+
  theme_bw()+
  geom_col(position = position_dodge())+
  coord_flip()+
  facet_wrap(.~cutoff)

