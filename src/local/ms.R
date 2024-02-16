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
library(readr)
library(openxlsx)
library(grid)
library(gridExtra)
library(Matrix)
library(spdep)
library(corrplot)
library(GGally)
rm(list = ls())
load_presaved <- TRUE

## Load presaved environment ##
if(load_presaved){
#save(list = ls(), file = "data/presaved.Rdata")
load(file = "data/presaved.Rdata")
}else{
##

# load functions
source("src/local/funs.R")

# Load unscaled data
y_mats <- readRDS("C:/r_proj/gscm/data/y_mats.rds")

# load global data
global_obj <- readRDS("data/global_obj.rds")

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
aus_border <- suppressMessages(st_read("C:/r_proj/ACAriskfactors/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>%
  filter(!str_detect(SA2_NAME, "Island")) %>%
  summarise(geometry = st_union(geometry)) %>% 
  st_as_sf() %>%
  st_transform(4326))

# State outline
state_border <- suppressMessages(st_read("C:/r_proj/ACAriskfactors/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>%
  filter(!str_detect(SA2_NAME, "Island")) %>%
  group_by(STATE_CODE) %>% 
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

# create other draws objects
perc_draws <- list()
rank_draws <- list()

## Index 1 ## ------------------------------------------------------------------

message("Index 1")

# draws
perc_draws$i1 <- t(apply(latent_draws[,,1], 1, ggplot2::cut_number, n = 100, labels = FALSE))
perc_draws$i1s <- t(pbapply::pbapply(latent_draws[,,1], 1, 
                                     FUN = function(row) StateCalcs$perc(row, cur_list$data$census$ps_state)))

rank_draws$i1 <- t(apply(latent_draws[,,1], 1, FUN = function(x)order(order(x))))
rank_draws$i1_r <- t(apply(latent_draws[,,1], 1, FUN = function(x)2222-order(order(x))))
rank_draws$i1s <- t(pbapply::pbapply(latent_draws[,,1], 1, 
                                     FUN = function(row) StateCalcs$rank(row, cur_list$data$census$ps_state)))
rank_draws$i1s_r <- t(pbapply::pbapply(latent_draws[,,1], 1, 
                                       FUN = function(row) StateCalcs$rank(row, 
                                                                           cur_list$data$census$ps_state, 
                                                                           reverse = TRUE)))

# summarise
cur_list$summ_latent1$perc_s <- jf$getResultsData(perc_draws$i1s) %>% 
  mutate(ps_state = cur_list$data$census$ps_state) %>% relocate(ps_state)

cur_list$summ_latent1$rankk_s <- jf$getResultsData(rank_draws$i1s) %>% 
  mutate(ps_state = cur_list$data$census$ps_state) %>% relocate(ps_state)

## Index 2 ## ------------------------------------------------------------------

message("Index 2")

# draws
perc_draws$i2 <- t(apply(latent_draws[,,2], 1, ggplot2::cut_number, n = 100, labels = FALSE))
perc_draws$i2s <- t(pbapply::pbapply(latent_draws[,,2], 1, 
                                     FUN = function(row) StateCalcs$perc(row, cur_list$data$census$ps_state)))

rank_draws$i2 <- t(apply(latent_draws[,,2], 1, FUN = function(x)order(order(x))))
rank_draws$i2_r <- t(apply(latent_draws[,,2], 1, FUN = function(x)2222-order(order(x))))
rank_draws$i2s <- t(pbapply::pbapply(latent_draws[,,2], 1, 
                                     FUN = function(row) StateCalcs$rank(row, cur_list$data$census$ps_state)))
rank_draws$i2s_r <- t(pbapply::pbapply(latent_draws[,,2], 1, 
                                       FUN = function(row) StateCalcs$rank(row, 
                                                                           cur_list$data$census$ps_state, 
                                                                           reverse = TRUE)))

# summarise
cur_list$summ_latent2$perc_s <- jf$getResultsData(perc_draws$i2s) %>% 
  mutate(ps_state = cur_list$data$census$ps_state) %>% relocate(ps_state)

cur_list$summ_latent2$rankk_s <- jf$getResultsData(rank_draws$i2s) %>% 
  mutate(ps_state = cur_list$data$census$ps_state) %>% relocate(ps_state)

## Index 3 - Combined ## -------------------------------------------------------

message("Index 3")

# weighted combo
w <- apply(cur_list$summ_loadings$point^2, 2, sum)/sum(apply(cur_list$summ_loadings$point^2, 2, sum))
z_comb <- w[1] * latent_draws[,,1] + w[2] * latent_draws[,,2]

# draws
perc_draws$i3 <- t(apply(z_comb, 1, ggplot2::cut_number, n = 100, labels = FALSE))
perc_draws$i3s <- t(pbapply::pbapply(z_comb, 1, 
                                     FUN = function(row) StateCalcs$perc(row, cur_list$data$census$ps_state)))

rank_draws$i3 <- t(apply(z_comb, 1, FUN = function(x)order(order(x))))
rank_draws$i3_r <- t(apply(z_comb, 1, FUN = function(x)2222-order(order(x))))
rank_draws$i3s <- t(pbapply::pbapply(z_comb, 1, 
                                     FUN = function(row) StateCalcs$rank(row, cur_list$data$census$ps_state)))
rank_draws$i3s_r <- t(pbapply::pbapply(z_comb, 1, 
                                       FUN = function(row) StateCalcs$rank(row, 
                                                                           cur_list$data$census$ps_state, 
                                                                           reverse = TRUE)))

# summarise
cur_list$summ_latent3$raww <- jf$getResultsData(z_comb)

cur_list$summ_latent3$perc <- jf$getResultsData(perc_draws$i3)
cur_list$summ_latent3$perc_s <- jf$getResultsData(perc_draws$i3s) %>% 
  mutate(ps_state = cur_list$data$census$ps_state) %>% relocate(ps_state)

cur_list$summ_latent3$rankk <- jf$getResultsData(rank_draws$i3)
cur_list$summ_latent3$rankk_s <- jf$getResultsData(rank_draws$i3s) %>% 
  mutate(ps_state = cur_list$data$census$ps_state) %>% relocate(ps_state)

cur_list$EP[,3] <- apply(z_comb > 0, 2, mean)

# cleanup
rm(sele)

## Index 4 - PW Combined ## ----------------------------------------------------

message("Index 4")

# get the posterior draws
z_erp_comb <- t(t(z_comb)*cur_list$data$census$N_persons)

# draws
perc_draws$i4 <- t(apply(z_erp_comb, 1, ggplot2::cut_number, n = 100, labels = FALSE))
perc_draws$i4s <- t(pbapply::pbapply(z_erp_comb, 1, 
                                     FUN = function(row) StateCalcs$perc(row, cur_list$data$census$ps_state)))

rank_draws$i4 <- t(apply(z_erp_comb, 1, FUN = function(x)order(order(x))))
rank_draws$i4_r <- t(apply(z_erp_comb, 1, FUN = function(x)2222 - order(order(x))))
rank_draws$i4s <- t(pbapply::pbapply(z_erp_comb, 1, 
                                     FUN = function(row) StateCalcs$rank(row, cur_list$data$census$ps_state)))
rank_draws$i4s_r <- t(pbapply::pbapply(z_erp_comb, 1, 
                                       FUN = function(row) StateCalcs$rank(row, 
                                                                           cur_list$data$census$ps_state, 
                                                                           reverse = TRUE)))

# summarise
cur_list$summ_latent4$raww <- jf$getResultsData(z_erp_comb)

cur_list$summ_latent4$perc <- jf$getResultsData(perc_draws$i4)
cur_list$summ_latent4$perc_s <- jf$getResultsData(perc_draws$i4s) %>% 
  mutate(ps_state = cur_list$data$census$ps_state) %>% relocate(ps_state)

cur_list$summ_latent4$rankk <- jf$getResultsData(rank_draws$i4)
cur_list$summ_latent4$rankk_s <- jf$getResultsData(rank_draws$i4s) %>% 
  mutate(ps_state = cur_list$data$census$ps_state) %>% relocate(ps_state)

cur_list$EP <- cbind(cur_list$EP, apply(z_erp_comb > 0, 2, mean))

## Probabilities above percentile or rank ## -----------------------------------

cur_list$probs$perc <- lapply(perc_draws, jf$getProbs, perc = TRUE)
cur_list$probs$rank <- lapply(rank_draws, jf$getProbs, perc = FALSE)

## Rank Sum Method ## ----------------------------------------------------------

raw_RS <- order(order(rowSums(apply(data, 2, FUN = function(x)order(order(x))))))

## Min-Max normalises ## -------------------------------------------------------

raw_MMN <- order(order(rowSums(mutate(data, across(everything(), ~(. - min(.)) / (max(.) - min(.))))))) 

## K-means to get 10 groups ## -------------------------------------------------

# library(cluster)
# km <- kmeans(data, centers = 10)
# cl <- as.factor(km$cluster)
# ce <- as.data.frame(km$centers)
# 
# temp <- apply(ce, 2, FUN = function(x)order(order(x))) %>% 
#   as.data.frame() %>% 
#   mutate(s = rowSums(.))

## END SCRIPT ## ---------------------------------------------------------------

}
