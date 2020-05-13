#' ---
#' title: variables - download, adjust extention and resolution, and correlation
#' author: mauricio vancine
#' date: 2019-05-13
#' ---

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(GGally)
library(raster)
library(sf)
library(corrr)
library(rnaturalearth)
library(tidyverse)
library(tmap)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)
raster::beginCluster(n = parallel::detectCores() - 1)

# sources
source("F:/mauricio/r-enm/01_enm/00_present/00_present_ecoclimate/00_scripts/src/function_variable_download_ecoclimate.R")

# directory
path <- "F:/mauricio/r-enm/01_enm/00_present/00_present_ecoclimate"
setwd(path)
dir.create("01_variables")

path <- paste0(path, "/01_variables")
setwd(path)
getwd()

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
  tm_polygons() +
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

# download
var_download_ecoclimate(baseline = "modern",
                        scenario = c("present"), 
                        variable = "bioclimate", 
                        aogcm = c("CCSM"), 
                        path = "F:/mauricio/r-enm/01_enm/00_present/00_present_ecoclimate/01_variables/01_raw", 
                        erase_zip_files = TRUE, 
                        erase_txt_files = TRUE, 
                        operational_system = "windows")

# import
var <- dir(patter = ".tif", recursive = TRUE) %>% 
  raster::stack()
var

# names
names(var)
names(var) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(var)
var

# plot
tm_shape(var$bio01) +
  tm_raster(palette = "-Spectral") +
  tm_layout(legend.position = c("left", "bottom"))

# extention and scale -----------------------------------------------------
# directory
setwd(path); dir.create("02_processed"); setwd("02_processed")

# adust extention
var_li <- var %>% 
  raster::crop(li) %>% 
  raster::mask(li)
var_li

# plot
tm_shape(var_li$bio01) +
  tm_raster(palette = "-Spectral") +
  tm_shape(li) +
  tm_borders(col = "black") +
  tm_layout(legend.position = c("right", "bottom"))

# export
raster::writeRaster(x = var_li, 
                    filename = paste0("var_ecoclimate_li_55km_", names(var_li)), 
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

# preparate table
cor_table_summary <- cor_table %>% 
  corrr::shave() %>%
  corrr::fashion()
cor_table_summary

# export
readr::write_csv(cor_table_summary, "correlacao.csv")

# select variables
# correlated variables
fin_cor <- cor_table %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .6, names = TRUE, verbose = TRUE)
fin_cor

# select
var_da_fin_cor <- var_da %>% 
  dplyr::select(-fin_cor)
var_da_fin_cor
colnames(var_da_fin_cor)

# verify
var_da_fin_cor %>% 
  corrr::correlate(method = "spearman") %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .7, names = TRUE, verbose = TRUE)

# graphic
var_ggpairs <- var_da_fin_cor %>% 
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
ggsave(filename = "correlation_plot.png", plot = var_ggpairs, wi = 20, he = 15, un = "cm", dpi = 300)

# create variables --------------------------------------------------------
# dictory
setwd(path); dir.create("04_processed_correlation"); setwd("04_processed_correlation")
getwd()

# export
raster::writeRaster(x = raster::subset(var_li, colnames(var_da_fin_cor)), 
                    filename = paste0("var_ecoclimate_55km_", colnames(var_da_fin_cor)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff",
                    progress = "text",
                    overwrite = TRUE)

# end ---------------------------------------------------------------------