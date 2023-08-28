## working 

# load global data
global_obj <- readRDS("data/global_obj.rds")
area_concor <- global_obj$area_concor %>%
  mutate(SA2 = ifelse(SA2 == 114011271, 901031003, SA2))

SA2_2016_AUSTv4 <- st_read("C:/Users/n9401849/OneDrive - Queensland University of Technology/DataLab/Requested file load/request 4/request 4/SA2_Austr_shape/SA2_2016_AUSTv4.shp")
SA2_2016_AUSTv4_m <- rmapshaper::ms_simplify(SA2_2016_AUSTv4, keep = 0.03)
st_write(SA2_2016_AUSTv4_m, "SA2_2016_AUSTv4_m.shp")

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

## Simplified version ## -------------------------------------------------------

map_temp <- st_read("data/shape/SA2_2016_AUSTv4_m.shp") %>% 
  mutate(SA2 = as.numeric(Sa2_main16)) %>%
  #filter(!str_detect(SA2_NAME, "Island")) %>%
  #filter(STATE_NAME != "Other Territories") %>%
  right_join(.,area_concor, by = "SA2") %>%
  arrange(ps_area)

# get weight Matrix
map_out <- jf$getConnectedNB(map_temp, sa2_name = "Sa2_5dig16")
W <- map_out$W


## THIS WORKS ## ---------------------------------------------------------------
map_temp <- st_read("C:/Users/n9401849/OneDrive - Queensland University of Technology/DataLab/Requested file load/request 4/request 4/SA2_Austr_shape/SA2_2016_AUSTv4.shp") %>% 
  mutate(SA2 = as.numeric(Sa2_main16)) %>%
  #filter(!str_detect(SA2_NAME, "Island")) %>%
  #filter(STATE_NAME != "Other Territories") %>%
  right_join(.,area_concor, by = "SA2") %>%
  arrange(ps_area)

# get weight Matrix
map_out <- jf$getConnectedNB(map_temp, sa2_name = "Sa2_5dig16")
W <- map_out$W

## Editing by name ## ----------------------------------------------------------

map_temp %>% 
  mutate(this = ifelse(ps_area %in% unlist(cc), 1, 0)) %>% 
  tm_shape(.)+
  tm_polygons(col = "this", popup.vars = c("ps_area", "Sa2_name16"))


gg <- graph.adjacency(W)
clu <- components(gg)
cc <- igraph::groups(clu)

out <- connectW(map_temp)

## Tasmania 
# "King Island" -> "Otway" and "North West"
# "Flinders and Cape Barren Islands" -> "Wilsons Promontory" and "Scottsdale - Bridport"

##
# "Bribie Island" -> "Beachmere - Sandstone Point"
# "Palm Island" -> "Ingham Region"
# "Magnetic Island" -> "Belgian Gardens - Pallarenda"
# "Redland Islands" -> "Jacobs Well - Alberton"
# "Phillip Island" -> "Wonthaggi - Inverloch"
# "French Island" -> "Wonthaggi - Inverloch"
# "Kangaroo Island" -> "Yankalilla"
# "Tiwi Islands" -> "Koolpinyah"
# "Anindilyakwa" -> "Gulf"
# "Torres Strait Islands" -> "Torres"

connectW <- function(sf_data){
  
  # get original W
  W <- nb2mat(poly2nb(sf_data), style = "B", zero.policy = TRUE)
  
  if(!any(names(sf_data) %in% "Sa2_name16")){
    message("Must have column called Sa2_name16")
    break
  }

# set changes 
new_assigns <- data.frame(area1 = c("King Island", "King Island",
                                    "Flinders and Cape Barren Islands", "Flinders and Cape Barren Islands",
                                    "Bribie Island",
                                    "Palm Island",
                                    "Magnetic Island",
                                    "Redland Islands",
                                    "Phillip Island",
                                    "French Island",
                                    "Kangaroo Island",
                                    "Tiwi Islands",
                                    "Anindilyakwa",
                                    "Torres Strait Islands"),
                          area2 = c("Otway", "North West",
                                    "Wilsons Promontory", "Scottsdale - Bridport",
                                    "Beachmere - Sandstone Point",
                                    "Ingham Region",
                                    "Belgian Gardens - Pallarenda",
                                    "Jacobs Well - Alberton",
                                    "Wonthaggi - Inverloch",
                                    "Wonthaggi - Inverloch",
                                    "Yankalilla",
                                    "Koolpinyah",
                                    "Gulf",
                                    "Torres"))

W_working <- W
for(i in 1:nrow(new_assigns)){
  W_working[sf_data$Sa2_name16 == new_assigns$area1[i],sf_data$Sa2_name16 == new_assigns$area2[i]] <- 1
  W_working[sf_data$Sa2_name16 == new_assigns$area2[i],sf_data$Sa2_name16 == new_assigns$area1[i]] <- 1
}

# check connectedness
gg <- graph.adjacency(W_working)
clu <- components(gg)
cc <- igraph::groups(clu)
message("There are ", length(cc), " unique groups of neighbours!")

# return the nb object
return(list(W = W_working,
            group_membership = cc))

}
