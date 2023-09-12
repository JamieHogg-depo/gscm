

plot(cbind(out_all[[4]]$data$y_sd, out_all[[4]]$summ_latent1$raww$se))
GGally::ggpairs(cbind(out_all[[4]]$data$y, out_all[[4]]$summ_latent1$rankk$point))+
  theme_bw()

GGally::ggpairs(cbind(data, out_all[[4]]$summ_latent1$raww$point, out_all[[4]]$summ_latent2$raww$point))+
  theme_bw()

## Rank #### -------------------------------------------------------------------

# SETUP
mapping_data <- out_all[[4]]$summ_latent1$raww %>% 
  cbind(.,map_sa2) %>% 
  st_as_sf() %>%
  st_transform(4326)

# base map
base <- mapping_data %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = point), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "D")+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Posterior median")+
    guides(fill = guide_colourbar(barwidth = 13, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    jf$addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay,
                              top = textGrob("Latent 1",gp=gpar(fontsize=8)))

# save object
jf$jsave(filename = paste0("raw_", rf ,".png"), 
         base_folder = paste0(base_folder, "/maps_lowres"),
         plot = full_inset_plt, square = F,
         square_size = 1200,
         dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished raw")

## Rank #### -------------------------------------------------------------------

# SETUP
mapping_data <- out_all[[4]]$summ_latent1$rankk %>% 
  cbind(.,map_sa2) %>% 
  st_as_sf() %>%
  st_transform(4326)

# base map
base <- mapping_data %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = point), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "D")+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Posterior rank")+
    guides(fill = guide_colourbar(barwidth = 13, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    jf$addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay,
                              top = textGrob("Latent 1",gp=gpar(fontsize=8)))

# save object
jf$jsave(filename = paste0("rank_", rf ,".png"), 
         base_folder = paste0(base_folder, "/maps_lowres"),
         plot = full_inset_plt, square = F,
         square_size = 1200,
         dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished rank")

## Percentiles #### ------------------------------------------------------------

# SETUP
mapping_data <- out_all[[4]]$summ_latent1$perc %>% 
  cbind(.,map_sa2) %>% 
  st_as_sf() %>%
  st_transform(4326)

# base map
base <- mapping_data %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = point), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "D")+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Percentiles")+
    guides(fill = guide_colourbar(barwidth = 13, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    jf$addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay,
                              top = textGrob("Latent 1",gp=gpar(fontsize=8)))

# save object
jf$jsave(filename = paste0("perc_", rf ,".png"), 
         base_folder = paste0(base_folder, "/maps_lowres"),
         plot = full_inset_plt, square = F,
         square_size = 1200,
         dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished perc")

## EP #### ---------------------------------------------------------------------

# SETUP
mapping_data <- as.data.frame(out_all[[4]]$EP) %>% 
  cbind(.,map_sa2) %>% 
  mutate(V1 = ifelse(V1 == 0, 0.001, V1),
         V1 = ifelse(V1 == 1, 0.999, V1),
         V2 = ifelse(V2 == 0, 0.001, V2),
         V2 = ifelse(V2 == 1, 0.999, V2)) %>% 
  st_as_sf() %>%
  st_transform(4326)

# base map
base <- mapping_data %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = V2), col = NA)+
  scale_fill_gradientn(colors = c("#008837", "#a6dba0", "white","white","white", "#c2a5cf", "#7b3294"),
                       limits = c(-0.0000001,1.0000001),
                       #oob = squish,
                       #trans = "logit",
                       breaks = c(0,0.2,0.25,0.5,0.75,0.8,1),
                       labels = as.character(c(0,0.2,"",0.5,"",0.8,1)))+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Exceedance probability")+
    guides(fill = guide_colourbar(barwidth = 13, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    jf$addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay,
                              top = textGrob("Latent 1",gp=gpar(fontsize=8)))

# save object
jf$jsave(filename = paste0("ep_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished ep")

## -----------------------------------------------------------------------------