## -----------------------------------------------------------------------------
## SETUP ## --------------------------------------------------------------------
## -----------------------------------------------------------------------------

# Packages
library(tidyverse)
library(scales)
library(sf)
library(MASS)
library(patchwork)
library(readr)
library(readxl)
library(grid)
library(gridExtra)
library(Matrix)
library(spdep)
rm(list = ls())

# load global data
global_obj <- readRDS("data/global_obj.rds")

# load map
map_sa2 <- st_read("C:/r_proj/ACAriskfactors/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>%
  mutate(SA2 = as.numeric(SA2_MAIN16)) %>%
  filter(!str_detect(SA2_NAME, "Island")) %>%
  filter(STATE_NAME != "Other Territories") %>%
  right_join(.,global_obj$area_concor, by = "SA2") %>%
  right_join(.,global_obj$census, by = "ps_area")

# Load data
out <- readRDS("data/y_mats.rds")
data <- out$point[,-c(1:2)]
data_sd <- out$sd[,-c(1:2)]

# Australia outline
aus_border <- suppressMessages(map_sa2 %>% 
                                 summarise(geometry = st_union(geometry)) %>% 
                                 st_as_sf() %>%
                                 st_transform(4326))

# State outline
state_border <- suppressMessages(map_sa2 %>% 
                                   mutate(state = str_sub(SA2_MAIN16, 1, 1)) %>% 
                                   group_by(state, STATE_NAME) %>% 
                                   summarise(geometry = st_union(geometry), .groups = "drop") %>% 
                                   filter(!st_is_empty(.)) %>% 
                                   #mutate(st_init = c("NSW", "VIC", "QLD", "SA", "WA", NA, "NT", NA)) %>% 
                                   st_as_sf() %>%
                                   st_transform(4326))

# City Insets 
lims <- data.frame(
  xmin = c(152.6, 150.35, 144.5, 115.45, 138.1, 146.8, 148.6, 130.3),
  xmax = c(153.6, 151.35, 145.5, 116.45, 139.1, 147.8, 149.6, 131.3),
  ymin = -c(28, 34.4, 38.4, 32.5, 35.4, 43.4, 35.8, 13),
  ymax = -c(27, 33.4, 37.4, 31.5, 34.4, 42.4, 34.8, 12),
  city = c("Brisbane", "Sydney", "Melbourne", "Perth", "Adelaide", "Hobart", "Canberra", "Darwin"),
  position = c("r", "r", "b", "l", "b", "b", "r", "l"),
  inset_labs = c("B - Brisbane (Qld)", "S - Sydney (NSW)",
                 "M - Melbourne (Vic)", "P - Perth (WA)",
                 "A - Adelaide (SA)", "H - Hobart (Tas)",
                 "C - Canberra (ACT)", "D - Darwin (NT)")
) %>% 
  mutate(initials = str_sub(city, 1, 1))

# lookup function
JHCW <- function(this){
  case_when(
    {{ this }} == "smoking" ~  "Current\nSmoking",
    {{ this }} == "activityleiswkpl" ~ 'Inadequate\nphysical\nactivity',
    {{ this }} == "diet" ~ "Inadequate\ndiet",
    {{ this }} == "alcohol" ~ "Risky\nalcohol\nconsumption",
    {{ this }} == "overweight" ~ "Overweight/\nobese",
  )
}
