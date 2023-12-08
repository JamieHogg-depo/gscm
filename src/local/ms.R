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
library(corrplot)
rm(list = ls())

# load functions
source("src/local/funs.R")

# Load unscaled data
y_mats <- readRDS("C:/r_proj/gscm/data/y_mats.rds")

# load global data
global_obj <- readRDS("data/global_obj.rds")

# load map
# map_sa2 <- st_read("C:/r_proj/ACAriskfactors/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>%
#   mutate(SA2 = as.numeric(SA2_MAIN16)) %>%
#   filter(!str_detect(SA2_NAME, "Island")) %>%
#   filter(STATE_NAME != "Other Territories") %>%
#   right_join(.,global_obj$area_concor, by = "SA2") %>%
#   right_join(.,global_obj$census, by = "ps_area")

# Load modelled results
cur_date <- c("202310022")
files <- list.files(paste0("Z:/gscm/outputs/", cur_date, "/r"), full.names = T)
files_fl <- files[!str_detect(files, "_f.rds|_fitonly.rds|_tr.rds|_ld.rds")]
out_all <- lapply(files_fl, readRDS)
names(out_all) <- files_fl

# get grid
grid <- bind_rows(lapply(1:length(out_all), FUN = function(x)out_all[[x]]$cur_model_spec), .id = "ix") %>% 
  mutate(shared_latent_rho_fixed = factor(toCharacterSpec(shared_latent_rho_fixed), c("IID", "ICAR", 'LCAR')),
         specific_latent_rho_fixed = factor(toCharacterSpec(specific_latent_rho_fixed), c("IID", "ICAR", 'LCAR')))

# Load data
data <- out_all[[1]]$data$y
data_sd <- out_all[[1]]$data$y_sd
map_sa2 <- out_all[[1]]$data$map
W <- out_all[[1]]$data$W
census <- out_all[[1]]$data$census

# Australia outline
aus_border <- suppressMessages(map_sa2 %>% 
                                 summarise(geometry = st_union(geometry)) %>% 
                                 st_as_sf() %>%
                                 st_transform(4326))

# State outline
state_border <- suppressMessages(map_sa2 %>% 
                                   group_by(Ste_name16) %>% 
                                   summarise(geometry = st_union(geometry), .groups = "drop") %>% 
                                   filter(!st_is_empty(.)) %>% 
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

## Final model ## --------------------------------------------------------------

# selected
sele <- 5
cur_list <- out_all[[sele]]

# latent draws
latent_draws <- readRDS(paste0(str_remove(files_fl[[sele]], ".rds"), "_ld.rds"))

## Combined Index ## -----------------------------------------------------------

# weighted combo
w <- apply(cur_list$summ_loadings$point^2, 2, sum)/sum(apply(cur_list$summ_loadings$point^2, 2, sum))
z_comb <- w[1] * latent_draws[,,1] + w[2] * latent_draws[,,2]

# summarise
cur_list$summ_latent3$raww <- jf$getResultsData(z_comb)
cur_list$summ_latent3$perc <- jf$getResultsData(t(apply(z_comb, 1, ggplot2::cut_number, n = 100, labels = FALSE)))
cur_list$summ_latent3$rankk <- jf$getResultsData(t(apply(z_comb, 1, FUN = function(x)order(order(x)))))
cur_list$EP[,3] <- apply(z_comb > 0, 2, mean)

# cleanup
rm(sele)

## ERP weighted Combined index ## ----------------------------------------------

# get the posterior draws
z_erp_comb <- t(t(z_comb)*cur_list$data$census$N_persons)

# summarise
cur_list$summ_latent4$raww <- jf$getResultsData(z_erp_comb)
cur_list$summ_latent4$perc <- jf$getResultsData(t(apply(z_erp_comb, 1, ggplot2::cut_number, n = 100, labels = FALSE)))
cur_list$summ_latent4$rankk <- jf$getResultsData(t(apply(z_erp_comb, 1, FUN = function(x)order(order(x)))))
cur_list$EP <- cbind(cur_list$EP, apply(z_erp_comb > 0, 2, mean))

## Probabilities ## ------------------------------------------------------------

cur_list$probs$latent1 <- jf$getProbs(latent_draws[,,1])

cur_list$probs$latent2 <- jf$getProbs(latent_draws[,,2])

cur_list$probs$latent3 <- jf$getProbs(z_comb)

cur_list$probs$latent4 <- jf$getProbs(z_erp_comb)

## Rank Sum Method ## ----------------------------------------------------------

raw_RS <- order(order(rowSums(apply(data, 2, FUN = function(x)order(order(x))))))

## END SCRIPT ## ---------------------------------------------------------------
