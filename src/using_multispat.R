# Using multispat - https://cran.r-project.org/web/packages/Guerry/vignettes/MultiSpat.html#spatial-partition

# libraries
library(tidyverse)
library(sp)           # management of spatial data
library(ade4)         # multivariate analysis
library(adegraphics)  # graphical representation
library(spdep)        # spatial dependency
library(adespatial)   # multivariate spatial analysis
library(tmap)
tmap_mode("view")

# load data
df <- readRDS("data/y_mats.rds")
global_obj <- readRDS("data/global_obj.rds")

# Load map
map_sa2_full <- st_read("C:/r_proj/ACAriskfactors/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>%
  mutate(SA2 = as.numeric(SA2_MAIN16)) %>%
  filter(!str_detect(SA2_NAME, "Island")) %>%
  filter(STATE_NAME != "Other Territories")

# keep non-estimated geometries
map_sa2 <- map_sa2_full %>%
  right_join(.,global_obj$area_concor, by = "SA2") %>%
  right_join(.,global_obj$census, by = "ps_area") %>% 
  arrange(ps_area)

# pca
pca <- prcomp(df$point[,-c(1,2)], scale. = T)
summary(pca)
map_sa2$pc1 <- pca$x[,1]

# plot latent field
map_sa2 %>% 
  filter(!st_is_empty(.)) %>% 
  tm_shape(.)+
  tm_polygons(col = "pc1",
              palette = "-YlOrRd",
              # use command tmaptools::palette_explorer()
              style = "cont")

# using other package
pca <- dudi.pca(df$point[,-c(1,2)], scannf = F, nf = 3)
biplot(pca)
pca$eig/sum(pca$eig) * 100
s.corcircle(pca$co)