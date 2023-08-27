## working 

# load global data
global_obj <- readRDS("data/global_obj.rds")
area_concor <- global_obj$area_concor %>%
  mutate(SA2 = ifelse(SA2 == 114011271, 901031003, SA2))

map_temp <- st_read("C:/Users/n9401849/OneDrive - Queensland University of Technology/DataLab/Requested file load/request 4/request 4/SA2_Austr_shape/SA2_2016_AUSTv4.shp") %>% 
  mutate(SA2 = as.numeric(Sa2_main16)) %>%
  #filter(!str_detect(SA2_NAME, "Island")) %>%
  #filter(STATE_NAME != "Other Territories") %>%
  right_join(.,area_concor, by = "SA2") %>%
  arrange(ps_area)

nb <- poly2nb(map_temp)
W <- nb2mat(nb, style = "B", zero.policy = T)

tm_shape(map_temp)+
  tm_polygons(popup.vars = c("Sa2_name16"))
# "Kind Island" -> "Otway" and "North West"
# "Flinders and Cape Barren Islands" -> "Wilsons Promontory" and "Scottsdale - Bridport"

# Load map
map_sa2 <- st_read("C:/r_proj/ACAriskfactors/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>%
  mutate(SA2 = as.numeric(SA2_MAIN16)) %>%
  #filter(!str_detect(SA2_NAME, "Island")) %>%
  #filter(STATE_NAME != "Other Territories") %>%
  left_join(.,area_concor, by = "SA2") %>%
  mutate(Sa2_5dig16 = as.numeric(s9_to_s5(SA2_MAIN16))) %>%
  arrange(ps_area) %>%
  #filter(!st_is_empty(.)) %>% 
  filter(!is.na(ps_area))

# get weight Matrix
map_out <- jf$getConnectedNB(map_sa2, sa2_name = "Sa2_5dig16")
W <- map_out$W
