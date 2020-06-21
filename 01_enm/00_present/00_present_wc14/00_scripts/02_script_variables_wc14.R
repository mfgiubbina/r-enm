#' ---
#' title: variables - download, adjust extent and resolution, and correlation
#' author: mauricio vancine
#' date: 2019-06-19
#' ---

# prepare r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(geobr)
library(GGally)
library(raster)
library(sf)
library(tidyverse)
library(tmap)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/00_present/00_present_wc14/"
setwd(path)
dir.create("02_variables"); setwd("02_variables")
path <- getwd()
path

# limits ------------------------------------------------------------------
# limits
li <- rnaturalearth::ne_countries(scale = 110, continent = "South America", returnclass = "sf")
li

li_ex <- li %>% 
  sf::st_bbox() %>% 
  sf::st_as_sfc()
li_ex

# plot
tm_shape(li_ex) +
  tm_polygons(border.col = "red") +
  tm_shape(li) +
  tm_polygons(col = "gray70")

# directory
dir.create("00_limit"); setwd("00_limit")
getwd()

# export
sf::write_sf(li, "limit.shp")
sf::write_sf(li_ex, "limit_ext.shp")

# download variables ------------------------------------------------------
# directory
setwd(path); dir.create("01_raw"); setwd("01_raw")
getwd()

# donwload
var <- raster::getData(name = "worldclim", var = "bio", res = 10)
var

# names
names(var)
names(var) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(var)
var

# plot
tm_shape(var$bio01/10) +
  tm_raster(palette = "-Spectral") +
  tm_layout(legend.position = c("left", "bottom"))

# extent and scale -----------------------------------------------------
# directory
setwd(path); dir.create("02_processed"); setwd("02_processed")

# adjust extent and resolution
var_li <- raster::crop(x = var, y = li) %>% 
  raster::mask(li) %>% 
  raster::aggregate(., fact = .5/res(.)[1])
var_li

# plot
tm_shape(var_li$bio01) +
  tm_raster(palette = "-Spectral") +
  tm_shape(li) +
  tm_borders(col = "black") +
  tm_layout(legend.position = c("right", "bottom"))

# export
raster::writeRaster(x = var_li, 
                    filename = paste0("var_wc14_li_55km_", names(var_li)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    progress = "text",
                    overwrite = TRUE)

# correlation -------------------------------------------------------------
# directory
setwd(path); dir.create("03_correlation"); setwd("03_correlation")
getwd()

# extract values
var_da <- var_li %>% 
  raster::values() %>% 
  tibble::as_tibble() %>% 
  tidyr::drop_na()
var_da

# verify
tibble::glimpse(var_da)

# correlation
cor_table <- corrr::correlate(var_da, method = "spearman") 
cor_table

# prepare table
cor_table_summary <- cor_table %>% 
  corrr::shave() %>%
  corrr::fashion()
cor_table_summary

# export
readr::write_csv(cor_table_summary, "var_table_correlacao.csv")

# select variables
# correlated variables
fi_07 <- cor_table %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .7, names = TRUE, verbose = TRUE)
fi_07

# select
var_da_cor07 <- var_da %>% 
  dplyr::select(-fi_07)
var_da_cor07
colnames(var_da_cor07)

# verify
var_da_cor07 %>% 
  corrr::correlate(method = "spearman") %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .7, names = TRUE, verbose = TRUE)

# graphic
var_ggpairs <- var_da_cor07 %>% 
  dplyr::select(sort(tidyselect::peek_vars())) %>% 
  dplyr::sample_n(1e3) %>% 
  ggpairs(lower = list(continuous = wrap(ggally_smooth_loess, pch = 21, 
                                         color = "gray30", fill = "gray50", size = 1)),
          diag = list(continuous = wrap(ggally_barDiag, color = "black", 
                                        color = "gray30", fill = "gray50", bins = 10)),
          upper = list(continuous = wrap(ggally_cor, color = "black", size = 5, 
                                         method = "spearman")),
          axisLabels = "none") +
  theme_bw()
var_ggpairs
ggsave(filename = "var_plot_correlation.png", plot = var_ggpairs, wi = 20, he = 15, un = "cm", dpi = 300)

# create variables --------------------------------------------------------
# directory
setwd(path); dir.create("04_processed_correlation"); setwd("04_processed_correlation")
getwd()

# export
raster::writeRaster(x = raster::subset(var_li, colnames(var_da_cor07)), 
                    filename = paste0("var_wc14_55km_", colnames(var_da_cor07)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff",
                    progress = "text",
                    overwrite = TRUE)

# end ---------------------------------------------------------------------