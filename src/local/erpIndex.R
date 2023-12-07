


# Full dataset for comparison
comp_data <- data.frame(perc_erp =cur_list$summ_latent4$perc$point,
                        perc_nonerp = cur_list$summ_latent3$perc$point,
                        erp = cur_list$data$census$N_persons,
                        pop = cut_number(cur_list$data$census$N_persons, 
                                         n = 100, label = F)) %>% 
  mutate(diff = abs(perc_erp - perc_nonerp))


# compare
comp_data %>% 
  ggplot(aes(y = perc_erp, 
             x = perc_nonerp, 
             col = pop))+
  theme_bw()+
  geom_point()+
  geom_abline()+
  scale_color_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "D")+
  labs(y = "ERP weighted (median percentile)",
       x = "No weighting (median percentile)",
       col = "ERP\n(percentiles)")

# selecting the top bands
comp_data %>% 
  mutate(high_erp = perc_erp > 90,
         high = perc_nonerp > 90) %>% 
  ggplot(aes(y = pop, x = high_erp))+
  geom_boxplot()

# selecting the low bands
comp_data %>% 
  mutate(low_erp = perc_erp < 10,
         low = perc_nonerp < 10) %>% 
  ggplot(aes(y = pop, x = low_erp))+
  geom_boxplot()

# combined data
mapping_data <- comp_data %>% 
  cbind(.,map_sa2) %>% 
  dplyr::select(perc_erp, perc_nonerp, pop, geometry) %>% 
  pivot_longer(-geometry) %>% 
  st_as_sf() %>%
  st_transform(4326)

# make map
i <- 3
mapping_data %>% 
  ggplot()+
  geom_sf(aes(fill = value))+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "D")+
  theme(legend.position = "bottom",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))+
  #xlim(lims$xmin[i], lims$xmax[i]) +
  #ylim(lims$ymin[i], lims$ymax[i])+
  facet_grid(.~name)

# how uncertainty of index depends on ERP
cur_list$summ_latent4$perc %>% 
  cbind(.,comp_data) %>% 
  ggplot(aes(y = se, x = pop))+
  geom_point()

# Caterpillar plots
cur_list$summ_latent4$perc %>% 
  arrange(point) %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = 1:nrow(.)))+theme_bw()+
  geom_errorbar(col = "grey")+
  geom_point()+
  theme(text = element_text(size = 8))+
  labs(x = "")
