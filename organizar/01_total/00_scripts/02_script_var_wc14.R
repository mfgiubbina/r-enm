# -------------------------------------------------------------------------
# var - download, adjust extention, adjust resolution, and correlation - wc 1.4
# mauricio vancine - mauricio.vancine@gmail.com
# 19-10-2019
# -------------------------------------------------------------------------

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

# informations
# https://ropensci.org/
# https://github.com/ropensci/rnaturalearth
# https://www.naturalearthdata.com/
# https://github.com/r-spatial/sf
# https://www.worldclim.org/

# directory
path <- "/home/mude/data/gitlab/r-sdm/01_data/01_var"
setwd(path)
dir()

# bioclimates -------------------------------------------------------------
# https://www.worldclim.org/bioclim
# https://www.worldclim.org/methods1
# BIO01 = Temperatura media anual
# BIO02 = Variacao da media diurna (media por mes (temp max - temp min))
# BIO03 = Isotermalidade (BIO02/BIO07) (* 100)
# BIO04 = Sazonalidade da temperatura (desvio padrao deviation * 100)
# BIO05 = Temperatura maxima do mes mais quente
# BIO06 = Temperatura minima do mes mais frio
# BIO07 = Variacao da temperatura anual (BIO5-BIO6)
# BIO08 = Temperatura media do trimestre mais chuvoso
# BIO09 = Temperatura media do trimestre mais seco
# BIO10 = Temperatura media do trimestre mais quente
# BIO11 = Temperatura media do trimestre mais frio
# BIO12 = Precipitacao anual
# BIO13 = Precipitacao do mes mais chuvoso
# BIO14 = Precipitacao do mes mais seco
# BIO15 = Sazonalidade da precipitacao (coeficiente de variacao)
# BIO16 = Precipitacao do trimestre mais chuvoso
# BIO17 = Precipitacao do trimestre mais seco
# BIO18 = Precipitacao do trimestre mais quente
# BIO19 = Precipitacao do trimestre mais frio

# adjust extention --------------------------------------------------------
# limit
li <- sf::st_read("/home/mude/data/gitlab/r-sdm/01_data/01_var/00_limit/ma.shp")
li

# plot
tm_shape(li) +
  tm_polygons()

# import bioclimates
# list files
setwd("/home/mude/data/onedrive/trabalho/ram/3_modelagem_estatistica/131_marcones/02_data/02_var/02_wc14/00_raw/00_present")
tif <- dir(pattern = ".bil$")
tif

# import
var <- raster::stack(tif)
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

# directory
setwd("/home/mude/data/gitlab/r-sdm/01_data/01_var")
dir.create("02_mask")
setwd("02 _mask")
dir.create("01_present"); setwd("01_present")

# export
raster::writeRaster(x = var_li_mask, 
                    filename = paste0("wc14_5km_", names(var_li_mask)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    overwrite = TRUE)

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
setwd(".."); setwd("..")
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

# adjust variables future -------------------------------------------------
# import bioclimates
# list files
setwd("/home/mude/data/onedrive/trabalho/ram/3_modelagem_estatistica/131_marcones/02_data/02_var/02_wc14/00_raw/01_future")
tif <- grep(paste0("03.tif|07.tif|08.tif|12.tif|16.tif|19.tif"), dir(pattern = ".tif$"), value = TRUE)
tif

# import
var <- raster::stack(tif)
var

# names
names(var)
names(var) <- c("ac45bi5012.tif", "ac45bi5016.tif", "ac45bi5019.tif", "ac45bi5003.tif", "ac45bi5007.tif", 
                "ac45bi5008.tif", "ac45bi7012.tif", "ac45bi7016.tif", "ac45bi7019.tif", "ac45bi7003.tif", 
                "ac45bi7007.tif", "ac45bi7008.tif", "ac85bi5012.tif", "ac85bi5016.tif", "ac85bi5019.tif",
                "ac85bi5003.tif", "ac85bi5007.tif", "ac85bi5008.tif", "ac85bi7012.tif", "ac85bi7016.tif",
                "ac85bi7019.tif", "ac85bi7003.tif", "ac85bi7007.tif", "ac85bi7008.tif", "cc45bi5012.tif",
                "cc45bi5016.tif", "cc45bi5019.tif", "cc45bi5003.tif", "cc45bi5007.tif", "cc45bi5008.tif",
                "cc45bi7012.tif", "cc45bi7016.tif", "cc45bi7019.tif", "cc45bi7003.tif", "cc45bi7007.tif", 
                "cc45bi7008.tif", "cc85bi5012.tif", "cc85bi5016.tif", "cc85bi5019.tif", "cc85bi5003.tif", 
                "cc85bi5007.tif", "cc85bi5008.tif", "cc85bi7012.tif", "cc85bi7016.tif", "cc85bi7019.tif",
                "cc85bi7003.tif", "cc85bi7007.tif", "cc85bi7008.tif", "hd45bi5012.tif", "hd45bi5016.tif",
                "hd45bi5019.tif", "hd45bi5003.tif", "hd45bi5007.tif", "hd45bi5008.tif", "hd45bi7012.tif",
                "hd45bi7016.tif", "hd45bi7019.tif", "hd45bi7003.tif", "hd45bi7007.tif", "hd45bi7008.tif", 
                "hd85bi5012.tif", "hd85bi5016.tif", "hd85bi5019.tif", "hd85bi5003.tif", "hd85bi5007.tif", 
                "hd85bi5008.tif", "hd85bi7012.tif", "hd85bi7016.tif", "hd85bi7019.tif", "hd85bi7003.tif", 
                "hd85bi7007.tif", "hd85bi7008.tif", "ip45bi5012.tif", "ip45bi5016.tif", "ip45bi5019.tif",
                "ip45bi5003.tif", "ip45bi5007.tif", "ip45bi5008.tif", "ip45bi7012.tif", "ip45bi7016.tif",
                "ip45bi7019.tif", "ip45bi7003.tif", "ip45bi7007.tif", "ip45bi7008.tif", "ip85bi5012.tif",
                "ip85bi5016.tif", "ip85bi5019.tif", "ip85bi5003.tif", "ip85bi5007.tif", "ip85bi5008.tif", 
                "ip85bi7012.tif", "ip85bi7016.tif", "ip85bi7019.tif", "ip85bi7003.tif", "ip85bi7007.tif", 
                "ip85bi7008.tif", "mg45bi5012.tif", "mg45bi5016.tif", "mg45bi5019.tif", "mg45bi5003.tif", 
                "mg45bi5007.tif", "mg45bi5008.tif", "mg45bi7012.tif", "mg45bi7016.tif", "mg45bi7019.tif",
                "mg45bi7003.tif", "mg45bi7007.tif", "mg45bi7008.tif", "mg85bi5012.tif", "mg85bi5016.tif",
                "mg85bi5019.tif", "mg85bi5003.tif", "mg85bi5007.tif", "mg85bi5008.tif", "mg85bi7012.tif",
                "mg85bi7016.tif", "mg85bi7019.tif", "mg85bi7003.tif", "mg85bi7007.tif", "mg85bi7008.tif", 
                "mr45bi5012.tif", "mr45bi5016.tif", "mr45bi5019.tif", "mr45bi5003.tif", "mr45bi5007.tif", 
                "mr45bi5008.tif", "mr45bi7012.tif", "mr45bi7016.tif", "mr45bi7019.tif", "mr45bi7003.tif", 
                "mr45bi7007.tif", "mr45bi7008.tif", "mr85bi5012.tif", "mr85bi5016.tif", "mr85bi5019.tif",
                "mr85bi5003.tif", "mr85bi5007.tif", "mr85bi5008.tif", "mr85bi7012.tif", "mr85bi7016.tif",
                "mr85bi7019.tif", "mr85bi7003.tif", "mr85bi7007.tif", "mr85bi7008.tif" )
names(var)
var

# plot
plot(var$ac45bi5012.tif)

# adust extention
# crop = adjust extention
var_li_crop <- raster::crop(x = var, y = li)
var_li_crop

plot(var_li_crop$ac45bi5015)

# mask = adjust to mask
var_li_mask <- raster::mask(x = var_li_crop, mask = li)
var_li_mask

plot(var_li_mask$ac45bi5010)

# directory
setwd("/home/mude/data/gitlab/r-sdm/01_data/01_var/01_mask")
dir.create("02_future"); setwd("02_future")

# export
raster::writeRaster(x = var_li_mask, 
                    filename = paste0("wc14_5km_", names(var_li_mask)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    overwrite = TRUE)

# end ---------------------------------------------------------------------