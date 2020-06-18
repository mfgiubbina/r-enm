#' ---
#' title: variables - download, adjust extension and resolution, and correlation to future
#' author: mauricio vancine
#' date: 2020-06-17
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
# raster::beginCluster(n = parallel::detectCores() - 1)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/01_future/01_future_wc14"
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
var_p <- raster::getData(name = "worldclim", var = "bio", res = 10)
var_p

# names
names(var_p)
names(var_p) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(var_p)
var_p

# download future bioclimates - https://worldclim.org/data/v1.4/cmip5_10m.html
gcm <- c("AC", "CC", "MR")
rcp <- c(45, 85)
year <- c(50, 70)

da_down_fut <- data.frame(gcm = rep(gcm, each = length(rcp) * length(year)),
                          rcp = rep(rcp, times = length(gcm) * length(year)),
                          year = rep(year, each = length(rcp), times = length(gcm)))
da_down_fut

var_f <- raster::stack()
var_f

for(i in 1:nrow(da_down_fut)){
  
  # information
  print(paste0("Download ", i, " from ", nrow(da_down_fut)))
  
  # download
  var_t <- raster::getData(name = "CMIP5", 
                           var = "bio", 
                           res = 10, 
                           rcp = da_down_fut[i, ]$rcp, 
                           model = da_down_fut[i, ]$gcm, 
                           year = da_down_fut[i, ]$year)
  
  # names
  names(var_t) <- paste0(tolower(da_down_fut[i, ]$gcm), 
                         "_rcp", da_down_fut[i, ]$rcp, 
                         "_20", da_down_fut[i, ]$year,
                         c(paste0("_bio0", 1:9), paste0("_bio", 10:19)))
  
  # combine
  var_f <- raster::stack(var_f, var_t)
  
}

var_f

# adjust extension and resolution ------------------------------------------
# directory
setwd(path); dir.create("02_processed"); setwd("02_processed")

# adjust extension and resolution - present
var_p_li <- raster::crop(x = var_p, y = li) %>% 
  raster::mask(li) %>% 
  raster::aggregate(., fact = .5/res(.)[1])
var_p_li
plot(var_p_li[[1]])

# adjust extension to mask and resolution - present
var_f_li <- raster::crop(x = var_f, y = li) %>% 
  raster::mask(li) %>% 
  raster::aggregate(., fact = .5/res(.)[1])
var_f_li
plot(var_f_li[[1]])

# export present
raster::writeRaster(x = var_p_li, 
                    filename = paste0("var_wc14_li_55km_present_", names(var_p_li)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff",
                    progress = "text",
                    overwrite = TRUE)

# export
raster::writeRaster(x = var_f_li, 
                    filename = paste0("var_wc14_li_55km_future_", names(var_f_li)), 
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
readr::write_csv(cor_table_summary, "var_correlation.csv")

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
  ggpairs(lower = list(continuous = wrap(ggally_smooth_loess,
                                        pch = 21, color = "gray30", fill = "gray50", size = 1)),
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
                    filename = paste0("var_wc14_55km_present_", names(var_p_li_sel)), 
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
                    filename = paste0("var_wc14_55km_future_", names(var_f_li_sel)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    progress = "text",
                    overwrite = TRUE)

# end ---------------------------------------------------------------------