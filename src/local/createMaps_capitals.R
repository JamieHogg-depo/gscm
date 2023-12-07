## Explore Sydney ## 

# create data
mapping_data <- data.frame(f1 = cur_list$summ_latent1$perc$point,
                           f2 = cur_list$summ_latent2$perc$point,
                           fc = cur_list$summ_latent3$perc$point,
                           fcerp = cur_list$summ_latent4$perc$point,
                           alcohol = cut_number(cur_list$data$y$alcohol, 100, labels = F),
                           smoking = cut_number(cur_list$data$y$smoking, 100, labels = F),
                           overweight = cut_number(cur_list$data$y$overweight, 100, labels = F)) %>% 
  cbind(.,map_sa2) %>% 
  pivot_longer(cols = f1:overweight, names_to = "type", values_to = "y") %>% 
  st_as_sf() %>%
  st_transform(4326)

## Loop over major cities ## ---------------------------------------------------
for(jkl in 1:8){

mapping_data %>% 
  mutate(type = fct_relevel(as.factor(type), c("alcohol", "smoking", "overweight", "f1", "f2", "fc", "fcerp"))) %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = y), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "F")+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  labs(fill = "Percentiles",
       title = lims$city[jkl])+
  guides(fill = guide_colourbar(barwidth = 13, 
                                title.position = "top",
                                title.hjust = 0.5))+
  theme(text = element_text(size = 8),
        legend.position = "bottom",
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))+
  xlim(lims$xmin[jkl], lims$xmax[jkl]) +
  ylim(lims$ymin[jkl], lims$ymax[jkl]) +
  facet_wrap(.~type, labeller = labeller(type = c(alcohol = "Risky\nAlcohol\nConsumption",
                                                  smoking = "Current\nSmoking",
                                                  overweight = "Overweight/\nobese",
                                                  f1 = "Index:\nFactor 1",
                                                  f2 = "Index:\nFactor 2",
                                                  fc = "Index:\nCombined",
                                                  fcerp = "Index:\nCombined\nERP weighted")))
jf$jsave(filename = paste0("map_perc_capital_", lims$city[jkl], ".png"), 
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)

message(paste0("Finished plot for ", lims$city[jkl]))

}

## END SCRIPT ## ---------------------------------------------------------------