# script description #
# script: variables - download ecoclimate
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
path <- "/media/mude/data/spatial_data/raster"
setwd(path)
dir()

# sources
source("/media/mude/data/gitlab/r-enm/scripts/02_var/02_script_function_var_download_ecoclimate.R")

# ecoclimate -----------------------------------------------------------
# download
var_download_ecoclimate(baseline = "modern",
                        scenario = c("past", "present", "future"), 
                        variable = "bioclimate", 
                        aogcm = c("CCSM", "CNRM", "MIROC", "COSMOS", "FGOALS", "GISS", "IPSL","MRI", "MPI"), 
                        path = "/media/mude/data/spatial_data/raster", 
                        erase_zip_files = TRUE, 
                        erase_txt_files = TRUE, 
                        operational_system = "linux")

  # information -------------------------------------------------------------
# www.ecoclimate.org

#' ecoClimate is a project to pre-process and make available the climatic 
#' layers from the PMIP3 – CMIP5 projects for macroecologists and biogeographers

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