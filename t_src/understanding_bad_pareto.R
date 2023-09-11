
# smoking, alcohol
pk <- matrix(out_all[[2]]$loo$diagnostics$pareto_k, nrow = 2221)

as.data.frame(pk) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(y = value, col = name))+
  geom_boxplot()+
  geom_hline(yintercept = 0.7)+
  ylim(-0.5,1.5)

# which areas are high
temp <- which(pk > 1, arr.ind = TRUE)
high_areas <- temp[temp[,2] == 4,1] # 442
# get census for high areas
global_obj$census %>% 
  mutate(yes = ps_area %in% high_areas) %>% 
  ggplot(aes(y = ABS_irsd_decile_nation_complete, fill = yes))+
  geom_bar(position = "fill")+
  facet_wrap(.~ABS_irsd_decile_nation_complete)

# disparite areas
out_all[[2]]$data$y %>% 
  cbind(.,global_obj$census) %>% 
  ggplot(aes(y = alcohol, x = smoking, col = ABS_irsd_decile_nation_complete))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(.~ra_sa2_3c)

# smoking, diet
pk <- matrix(out_all[[6]]$loo$diagnostics$pareto_k, nrow = 2221)

as.data.frame(pk) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(y = value, col = name))+
  geom_boxplot()+
  geom_hline(yintercept = 0.7)+
  ylim(-0.5,1.5)

bad_ks <- which(pk > 1, arr.ind = TRUE)
table(bad_ks[,2])
prop.table(table(bad_ks[,2]))

# disparite areas
out_all[[2]]$data$y[,c(1,2)] %>% 
  setNames(c("smoking_point", "alcohol_point")) %>% 
  cbind(.,global_obj$census) %>% 
  cbind(.,out_all[[2]]$data$y_sd) %>% 
  mutate(yes = ps_area %in% high_areas) %>% 
  ggplot(aes(y = smoking_point, x = alcohol_point, col = alcohol))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(.~yes)

# table
temp2 <- out_all[[2]]$data$y[,c(1,2)] %>% 
  setNames(c("smoking_point", "alcohol_point")) %>% 
  cbind(.,global_obj$census) %>% 
  cbind(.,out_all[[2]]$data$y_sd) %>% 
  mutate(yes = ps_area %in% high_areas)

with(temp2, table(yes, ra_sa2))



draws <- rstan::extract(ix2)
loo1 <- loo(draws$log_lik)
loo2 <- loo(draws$log_lik[,-c(2222:4442)])

