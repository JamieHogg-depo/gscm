
IDSelections <- function(sa2s, point_est = TRUE){
  
  temp <- map_sa2 %>% 
    left_join(.,global_obj$census) %>% 
    filter(Sa2_name16 %in% sa2s) %>% 
    dplyr::select("Sa2_name16", "ps_area")
  red_points <- temp$ps_area

  # data for pairs
  if(point_est){
    ggp <- y_mats$point[-c(1,2)] %>% 
      rename("Current\nSmoking" = smoking,
             'Inadequate\nphysical\nactivity' = activityleiswkpl,
             "Inadequate\ndiet" = diet,
             "Risky\nalcohol\nconsumption" = alcohol,
             "Overweight/\nobese" = overweight) %>% 
      ggpairs(., lower = list(continuous = wrap("points", size=0.05)))
  }else{
    ggp <- as.data.frame(100*(y_mats$sd[-c(1,2)]/y_mats$point[-c(1,2)])) %>% 
      rename("Current\nSmoking" = smoking,
             'Inadequate\nphysical\nactivity' = activityleiswkpl,
             "Inadequate\ndiet" = diet,
             "Risky\nalcohol\nconsumption" = alcohol,
             "Overweight/\nobese" = overweight) %>% 
      ggpairs(., lower = list(continuous = wrap("points", size=0.05)))
  }

  
  # point panels
  point_panels <- c(6,11,12,16:18,21:24)
  
  # set colors
  ggp$data$colors <- "normal"
  ggp$data$colors[red_points] <- "special"
  
  for(i in point_panels) {
    ggp$plots[[i]]$mapping <- 
      `class<-`(c(ggp$plots[[i]]$mapping, aes(color = colors)), "uneval")
  }
  
  # return the plot
  ggp + scale_color_manual(values = c("grey", "black"))+theme_bw()+
    labs(caption = paste0(sa2s, collapse = ", "))

}


IDSelections(cur_list$probs$perc$i1$perc95)


y_mats$point[-2] %>% 
  mutate(type = "point",
         red_points = (1:nrow(.) %in% red_points)) %>% 
  rbind(.,as.data.frame(y_mats$sd[-2]) %>% 
          mutate(type = "se",
                 red_points = (1:nrow(.) %in% red_points))) %>% 
  pivot_longer(-c(ps_area, red_points, type)) %>% 
  mutate(name = JHCW(name)) %>% 
  ggplot(aes(x = name, y = value, group = ps_area)) + 
  theme_bw()+
  geom_line() + 
  gghighlight(red_points, use_direct_label = FALSE)+
  labs(x = "",
       y = "Proportion")+
  facet_grid(.~type)



ggplot()+
  geom_line(data = data %>% 
              mutate(ps_area = 1:nrow(.),
                     red_points = (1:nrow(.) %in% red_points)) %>% 
              pivot_longer(-c(ps_area, red_points)) %>% 
              filter(!red_points), aes(x = name, y = value, group = ps_area, col = red_points),
            col = "grey")+
  geom_line(data = data %>% 
              mutate(ps_area = 1:nrow(.),
                     red_points = (1:nrow(.) %in% red_points)) %>% 
              pivot_longer(-c(ps_area, red_points)) %>% 
              filter(red_points), aes(x = name, y = value, group = ps_area, col = red_points),
            col = "black")
