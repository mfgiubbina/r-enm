# script description #
# script: variables - download worldclim v2
# author: mauricio vancine
# date:   06-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)

# raster options
raster::rasterOptions(maxmemory = 1e+10)
raster::rasterOptions(chunksize = 1e+10)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/02_var"
setwd(path)
dir()

# worldclim v20 -----------------------------------------------------------
# directory
dir.create("bioclim_v20")
setwd("bioclim_v20")

# download
download.file(url = "http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0_10m_bio.zip",
              destfile = "wc2.0_10m_bio.zip")

# unzip
unzip("wc2.0_10m_bio.zip")

# delete file
unlink("wc2.0_10m_bio.zip")

# worldclim ---------------------------------------------------------------
# http://www.worldclim.org/

# bioclimates -------------------------------------------------------------
# BIO01 = Temperatura media anual
# BIO02 = Variacao da media diurna (media por mes (temp max - temp min))
# BIO03 = Isotermalidade (BIO02/BIO07) (* 100)
# BIO04 = Sazonalidade da temperatura (desvio padrao deviation *100)
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

# end ---------------------------------------------------------------------