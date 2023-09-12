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
