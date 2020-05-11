#' ---
#' title: variables - download, adjust extention and resolution, and correlation to future
#' author: mauricio vancine
#' date: 2020-05-09
#' ---

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(geobr)
library(GGally)
library(terra)
library(rvest)
library(sf)
library(tidyverse)
library(tmap)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)
raster::beginCluster(n = 4)
raster::rasterOptions()

# directory
path <- "/home/mude/data/github/r-enm/01_enm/01_future_wc21/01_variables"
setwd(path)
dir()

# limits ------------------------------------------------------------------
# limits
li <- rnaturalearth::ne_countries(scale = 110, country = "Brazil", returnclass = "sf")
li

li_ex <- rnaturalearth::ne_countries(scale = 110, country = "Brazil", returnclass = "sf") %>% 
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

# download present bioclimates
# wordclim
da_wc <- tibble::tibble(
  url = paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/",
               c("wc2.1_10m_elev.zip", "wc2.1_10m_bio.zip")),
  destfile = c("wc2.1_10m_elev.zip", "wc2.1_10m_bio.zip")
) %>% as.list
da_wc

# download
purrr::map2(da_wc$url, da_wc$destfile, download.file)

# unzip
purrr::map(dir(pattern = ".zip"), unzip)

# import
var_p <- dir(pattern = ".tif$") %>% 
  raster::stack()
var_p

## future
# download future bioclimates - https://worldclim.org/data/cmip6/cmip6_clim10m.html
url <- "https://worldclim.org/data/cmip6/cmip6_clim10m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc") %>% 
  stringr::str_subset("BCC-CSM2-MR|CNRM-CM6-1|GFDL-ESM4|IPSL-CM6A-LR|MIROC-ES2L|MRI-ESM2-0", negate = TRUE) %>% 
  stringr::str_subset("ssp126", negate = TRUE)
url

destfiles <- "https://worldclim.org/data/cmip6/cmip6_clim10m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc") %>% 
  stringr::str_subset("BCC-CSM2-MR|CNRM-CM6-1|GFDL-ESM4|IPSL-CM6A-LR|MIROC-ES2L|MRI-ESM2-0", negate = TRUE) %>% 
  stringr::str_subset("ssp126", negate = TRUE) %>% 
  stringr::str_split(pattern = "[/]", simplify = TRUE) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(9) %>% 
  dplyr::pull()
destfiles

# download
purrr::map2(url, destfiles, download.file)

# unzip
purrr::map(dir(pattern = ".zip"), unzip)

# adust extention and resolution ------------------------------------------
# directory
setwd(path); dir.create("02_processed"); setwd("02_processed")

# adust extention and resolution - present
var_p_li <- raster::crop(x = var_p, y = li) %>% 
  raster::aggregate(., fact = .5/res(.)[1])
var_p_li

plot(var_p_li[[1]])

# adust extention to mask and resolution - present
var_f_li <- raster::crop(x = var_f, y = li) %>% 
  raster::aggregate(., fact = .5/res(.)[1])
var_f_li

plot(var_f_li[[1]])

# export present
raster::writeRaster(x = var_p_li, 
                    filename = paste0("wc14_li_55km_present_", names(var_p_li)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff",
                    progress = "text",
                    overwrite = TRUE)

# export
raster::writeRaster(x = var_f_li, 
                    filename = paste0("wc14_li_55km_future_", names(var_f_li)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    progress = "text",
                    overwrite = TRUE)

# correlation -------------------------------------------------------------
# directory
setwd(path); dir.create("03_correlation"); setwd("03_correlation")

# extract values
var_p_da <- var_p_li %>% 
  raster::values() %>% 
  tibble::as_tibble() %>% 
  tidyr::drop_na()
var_p_da

# verify
dplyr::glimpse(var_p_da)

# correlation
cor_table <- corrr::correlate(var_p_da, method = "spearman") 
cor_table

# preparate table
cor_table_summary <- cor_table %>% 
  corrr::shave() %>%
  corrr::fashion()
cor_table_summary

# export
readr::write_csv(cor_table_summary, "correlation.csv")

# select variables
# correlated variables
fi_07 <- cor_table %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .7, names = TRUE, verbose = TRUE)
fi_07

# select
var_da_cor07 <- var_p_da %>% 
  dplyr::select(-fi_07)
var_da_cor07

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
ggsave(filename = "correlation_plot.png", plot = var_ggpairs, wi = 20, he = 15, un = "cm", dpi = 300)


# export variables --------------------------------------------------------
# directory
setwd(path); dir.create("04_processed_correlation"); setwd("04_processed_correlation")
getwd()

# selection
var_p_li_sel <- raster::subset(var_p_li, colnames(var_da_cor07))
var_p_li_sel

# export present
raster::writeRaster(x = var_p_li_sel, 
                    filename = paste0("wc14_55km_present_", names(var_p_li_sel)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff",
                    progress = "text",
                    overwrite = TRUE)

# selection
var_f_li_sel <- raster::subset(var_f_li, stringr::str_subset(names(var_f_li), paste0(colnames(var_da_cor07), collapse = "|")))
var_f_li_sel

# export
raster::writeRaster(x = var_f_li_sel, 
                    filename = paste0("wc14_55km_future_", names(var_f_li_sel)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    progress = "text",
                    overwrite = TRUE)

# end ---------------------------------------------------------------------