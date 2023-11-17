
for(jkl in 1:3){
  
message(paste0("Round: ", jkl))
top_label <- ifelse(jkl == 3, "Combined", paste0("Factor ", jkl))

## Raw #### --------------------------------------------------------------------
  
# Caterpillar
  cur_list[[paste0("summ_latent", jkl)]]$raw %>% 
    arrange(point) %>% 
    ggplot(aes(y = point, ymin = lower, ymax = upper,
               x = 1:nrow(.)))+theme_bw()+
    geom_errorbar(col = "grey")+
    geom_point()+
    theme(text = element_text(size = 8))+
    labs(x = "",
         y = paste0("Factor ", jkl, " (raw)"))
  jf$jsave(filename = paste0("cat_raw", jkl, ".png"), 
           base_folder = "out",
           square = T,
           square_size = 1200,
           dpi = 300)
  message("---- Finished cater raw")

# SETUP
mapping_data <- cur_list[[paste0("summ_latent", jkl)]]$raw %>% 
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
    labs(fill = "Raw (posterior median)")+
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
                              top = textGrob(top_label,gp=gpar(fontsize=8)))

# save object
jf$jsave(filename = paste0("map_raw", jkl, ".png"), 
         base_folder = "out",
         plot = full_inset_plt, square = F,
         square_size = 1200,
         dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished raw")

## Raw - cisize #### -----------------------------------------------------------

# SETUP
mapping_data <- cur_list[[paste0("summ_latent", jkl)]]$raww %>% 
  cbind(.,map_sa2) %>% 
  st_as_sf() %>%
  st_transform(4326) %>% 
  mutate(cisize = upper - lower)

# base map
base <- mapping_data %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = cisize), col = NA)+
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
    labs(fill = "With of 95% HPDI")+
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
                              layout_matrix  = lay)

# save object
jf$jsave(filename = paste0("map_raw_cisize", jkl, ".png"), 
         base_folder = "out",
         plot = full_inset_plt, square = F,
         square_size = 1200,
         dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished raw_cisize")

## Rank #### -------------------------------------------------------------------

# Caterpillar
cur_list[[paste0("summ_latent", jkl)]]$rankk %>% 
  arrange(point) %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = 1:nrow(.)))+theme_bw()+
  geom_errorbar(col = "grey")+
  geom_point()+
  theme(text = element_text(size = 8))+
  labs(x = "",
       y = paste0("Factor ", jkl, " (rank)"))
jf$jsave(filename = paste0("cat_rank", jkl, ".png"), 
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)
message("---- Finished cater rank")

# SETUP
mapping_data <- cur_list[[paste0("summ_latent", jkl)]]$rankk %>% 
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
                       option = "E")+
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
    labs(fill = "Rank (posterior median)")+
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
                              top = textGrob(top_label,gp=gpar(fontsize=8)))

# save object
jf$jsave(filename = paste0("map_rank", jkl, ".png"), 
         base_folder = "out",
         plot = full_inset_plt, square = F,
         square_size = 1200,
         dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished rank")

## Rank - cisize #### ----------------------------------------------------------

# SETUP
mapping_data <- cur_list[[paste0("summ_latent", jkl)]]$rankk %>% 
  cbind(.,map_sa2) %>% 
  st_as_sf() %>%
  st_transform(4326) %>% 
  mutate(cisize = upper - lower)

# base map
base <- mapping_data %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = cisize), col = NA)+
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
    labs(fill = "With of 95% HPDI")+
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
                              layout_matrix  = lay)

# save object
jf$jsave(filename = paste0("map_rank_cisize", jkl, ".png"), 
         base_folder = "out",
         plot = full_inset_plt, square = F,
         square_size = 1200,
         dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished rank_cisize")

## Percentiles #### ------------------------------------------------------------

# caterpillar
cur_list[[paste0("summ_latent", jkl)]]$perc %>% 
  arrange(point) %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = 1:nrow(.)))+theme_bw()+
  geom_errorbar(col = "grey")+
  geom_point()+
  theme(text = element_text(size = 8))+
  labs(x = "",
       y = paste0("Factor ", jkl, " (percentilese)"))
jf$jsave(filename = paste0("cat_perc", jkl, ".png"), 
         base_folder = "out",
         square = T,
         square_size = 1200,
         dpi = 300)
message("---- Finished cater perc")

# SETUP
mapping_data <- cur_list[[paste0("summ_latent", jkl)]]$perc %>% 
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
                       option = "F")+
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
    labs(fill = "Percentiles (posterior median)")+
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
                              top = textGrob(top_label,gp=gpar(fontsize=8)))

# save object
jf$jsave(filename = paste0("map_perc", jkl, ".png"), 
         base_folder = "out",
         plot = full_inset_plt, square = F,
         square_size = 1200,
         dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished perc")

## Percentiles - cisize #### ---------------------------------------------------

# SETUP
mapping_data <- cur_list[[paste0("summ_latent", jkl)]]$perc %>% 
  cbind(.,map_sa2) %>% 
  st_as_sf() %>%
  st_transform(4326) %>% 
  mutate(cisize = upper - lower)

# base map
base <- mapping_data %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = cisize), col = NA)+
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
    labs(fill = "With of 95% HPDI")+
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
                              layout_matrix  = lay)

# save object
jf$jsave(filename = paste0("map_perc_cisize", jkl, ".png"), 
         base_folder = "out",
         plot = full_inset_plt, square = F,
         square_size = 1200,
         dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished perc_cisize")

## EP #### ---------------------------------------------------------------------

# SETUP
mapping_data <- map_sa2 %>% 
  mutate(value = ifelse(cur_list$EP[,jkl] == 0, 0.001, cur_list$EP[,jkl]),
         value = ifelse(value == 1, 0.999, value)) %>% 
  st_as_sf() %>%
  st_transform(4326)

# base map
base <- mapping_data %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = value), col = NA)+
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
                              top = textGrob(top_label,gp=gpar(fontsize=8)))

# save object
jf$jsave(filename = paste0("map_ep", jkl, ".png"), 
      base_folder = "out",
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished ep")

## FINISH FOR LOOP ## ----------------------------------------------------------

}

## -----------------------------------------------------------------------------