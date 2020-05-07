#' ---
#' title: variables - download, adjust extention, adjust resolution, and correlation
#' author: mauricio vancine
#' date: 2019-01-15
#' ---

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(psych)
library(raster)
library(rgdal)
library(rnaturalearth)
library(tidyverse)
library(tmap)
library(wesanderson)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)
raster::beginCluster(n = 4)
raster::rasterOptions()

# directory
path <- "/home/mude/data/gitlab/r-sdm/00_pragmatico/01_var"
setwd(path)
dir()

# adjust extention --------------------------------------------------------
# limit
li <- rnaturalearth::ne_countries(scale = 110, country = "Brazil", returnclass = "sf")
li

# plot
tm_shape(li) +
  tm_polygons()

# import bioclimates
var <- raster::getData(name = "worldclim", var = "bio", res = 10)
var

# names
names(var)
names(var) <- c("bio01", paste0("bio", 10:19), paste0("bio0", 2:9))
names(var)
var

# plot
plot(var$bio01)

# adust extention
# crop = adjust extention
var_li_crop <- raster::crop(x = var, y = li)
var_li_crop

plot(var_li_crop$bio01)

# mask = adjust to mask
var_li_mask <- raster::mask(x = var_li_crop, mask = li)
var_li_mask

plot(var_li_mask$bio01)

# aggregate
var_li_mask_55km <- raster::aggregate(var_li_mask, fact = 0.5/res(var_li_mask)[1])
var_li_mask_55km

# directory
dir.create("00_raw")
setwd("00_raw")

# export
raster::writeRaster(x = var_li_mask_55km, 
                    filename = paste0("wc14_55km_", names(var_li_mask_55km)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    overwrite = TRUE)

# delete folder
unlink("wc10", recursive = TRUE)

# correlation -------------------------------------------------------------
# import
var <- dir(pattern = ".tif$") %>% 
  raster::stack() %>% 
  raster::brick()
var

# names
names(var)
names(var) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(var)
var

# directory
dir.create("01_correlation") 
setwd("01_correlation")
getwd()

# extract values
var_da <- var %>% 
  raster::values() %>% 
  tibble::as_tibble() %>% 
  tidyr::drop_na()
var_da

# verify
head(var_da)
dim(var_da)

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
fi_07 <- cor_table %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .7, names = TRUE, verbose = TRUE)
fi_07

# select
var_da_cor07 <- var_da %>% 
  dplyr::select(-fi_07)
var_da_cor07

# verify
var_da_cor07 %>% 
  corrr::correlate(method = "spearman") %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .7, names = TRUE, verbose = TRUE)

# graphic
tiff("correlacao_plot.tiff", wi = 30, he = 25, un = "cm", res = 300, comp = "lzw")
pairs.panels(x = var_da_cor07 %>% dplyr::sample_n(1e3), 
             method = "spearman",
             pch = 20, 
             ellipses = FALSE, 
             density = FALSE, 
             stars = TRUE, 
             hist.col = "gray",
             digits = 2,
             rug = FALSE,
             breaks = 10,
             ci = TRUE)
dev.off()

# create variables --------------------------------------------------------
setwd("..")
raster::writeRaster(x = raster::subset(var, colnames(var_da_cor07)), 
                    filename = colnames(var_da_cor07), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    overwrite = TRUE)

# end ---------------------------------------------------------------------