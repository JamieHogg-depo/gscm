y_mats$point

map_sa2 %>% filter(Sa2_name16 == "Laurieton - Bonny Hills")

sel <- 108041162

y_mats$point %>% 
  filter(SA2 == sel)


y_mats$sd %>% 
  filter(SA2 == sel)

cur_list$summ_latent3$perc[which(y_mats$point$SA2 == sel),]

cur_list$probs$perc$i3[which(y_mats$point$SA2 == sel),]

cur_list$summ_latent4$perc[which(y_mats$point$SA2 == sel),]

cur_list$probs$perc$i4[which(y_mats$point$SA2 == sel),]
