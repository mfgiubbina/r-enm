#' ---
#' title: variables - download, adjust extent and resolution, and correlation to future
#' author: mauricio vancine
#' date: 2020-06-20
#' ---

# prepare r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(geobr)
library(GGally)
library(raster)
library(rvest)
library(sf)
library(tidyverse)
library(tmap)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/01_future/01_future_wc21"
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
# download
download.file(url = "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_bio.zip", 
            destfile = "wc2.1_10m_bio.zip", mode = "wb")

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
  stringr::str_subset("CNRM-ESM2-1|CanESM5|MIROC6") %>% 
  stringr::str_subset("ssp126", negate = TRUE)
url

destfiles <- "https://worldclim.org/data/cmip6/cmip6_clim10m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc") %>% 
  stringr::str_subset("CNRM-ESM2-1|CanESM5|MIROC6") %>%
  stringr::str_subset("ssp126", negate = TRUE) %>% 
  stringr::str_split(pattern = "[/]", simplify = TRUE) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(9) %>% 
  dplyr::pull()
destfiles

# download
purrr::map2(url, destfiles, download.file, mode = "wb")

# unzip
purrr::map(dir(pattern = ".zip"), unzip)

# import variables --------------------------------------------------------
# directory
setwd(path); dir.create("01_raw"); setwd("01_raw")
getwd()

# import present
var_p <- dir(pattern = ".tif$") %>% 
  raster::stack()
var_p

# names
names(var_p)
names(var_p) <- c("bio01", paste0("bio", 10:19), paste0("bio0", 2:9))
names(var_p)
var_p

# import future
var_f <- dir(pattern = "bioc", recursive = TRUE) %>% 
  stringr::str_subset(".tif$") %>% 
  raster::stack()
var_f

# names
names(var_f)
names(var_f) <- names(var_f) %>% 
  stringr::str_to_lower() %>%
  stringr::str_replace("wc2.1_10m_bioc_", "") %>% 
  stringr::str_replace("cnrm.esm2.1", "cnrmesm21") %>% 
  stringr::str_replace("_2021.|_2041.|_2061.|_2081.", "_") %>% 
  stringr::str_replace("[.]", "_bio0") %>% 
  stringr::str_replace("_bio01", "_bio1") %>% 
  stringr::str_replace("bio1$", "bio01")
names(var_f)
var_f

# adjust extent and resolution ------------------------------------------
# directory
setwd(path); dir.create("02_processed"); setwd("02_processed")

# adjust extent to mask and resolution - present
var_p_li <- raster::crop(x = var_p, y = li) %>% 
  raster::mask(li) %>% 
  raster::aggregate(., fact = .5/res(.)[1])
var_p_li

plot(var_p_li[[1]])

# adjust extent to mask and resolution - future
var_f_li <- raster::crop(x = var_f, y = li) %>% 
  raster::mask(li) %>% 
  raster::aggregate(., fact = .5/res(.)[1])
var_f_li

plot(var_f_li[[1]])

# export present
raster::writeRaster(x = var_p_li, 
                    filename = paste0("var_wc21_li_55km_present_", names(var_p_li)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff",
                    progress = "text",
                    overwrite = TRUE)

# export future
raster::writeRaster(x = var_f_li, 
                    filename = paste0("var_wc21_li_55km_future_", names(var_f_li)), 
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

# prepare table
cor_table_summary <- cor_table %>% 
  corrr::shave() %>%
  corrr::fashion()
cor_table_summary

# export
readr::write_csv(cor_table_summary, "var_table_correlation.csv")

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
ggsave(filename = "var_correlation_plot.png", plot = var_ggpairs, wi = 20, he = 15, un = "cm", dpi = 300)

# export variables --------------------------------------------------------
# directory
setwd(path); dir.create("04_processed_correlation"); setwd("04_processed_correlation")
getwd()

# selection
var_p_li_sel <- raster::subset(var_p_li, colnames(var_da_cor07))
var_p_li_sel

# export present
raster::writeRaster(x = var_p_li_sel, 
                    filename = paste0("var_wc21_55km_present_", names(var_p_li_sel)), 
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
                    filename = paste0("var_wc21_55km_future_", names(var_f_li_sel)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    progress = "text",
                    overwrite = TRUE)

# end ---------------------------------------------------------------------