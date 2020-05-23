### script create raster from txt ###

# mauricio vancine
# 14-03-2019


# preparate r -------------------------------------------------------------

# memory 
rm(list = ls())

# diretório temporario de processamento do R 
tempdir <- function() "D:\\temps"
unlockBinding("tempdir", baseenv()) 
assignInNamespace("tempdir", tempdir, ns="base", envir=baseenv())
assign("tempdir", tempdir, baseenv())
lockBinding("tempdir", baseenv())
tempdir()


# packages 
library(raster)
library(rgdal)
library(tidyverse)

# directory
setwd("D:/BKP Kew/modelos_plantas_temporal/03_enm")
dir()

# import data -------------------------------------------------------------
enm <- dir(pattern = "enm")
enm

# create directory
dir.create("enm_raster")

for(i in enm){
  
  # import data
  enm.i <- data.table::fread(i)
  
  # gridded
  sp::gridded(enm.i) <- ~x + y

  # stack
  enm.i.ra <- raster::stack(enm.i) 
  enm.i.ra
  
  # directory
  setwd("enm_raster")
  
  # export
  raster::writeRaster(enm.i.ra, filename = paste0(names(enm.i.ra) %>% stringr::str_replace("[.]", "_") %>% stringr::str_to_lower(), ".tif"),
                      bylayer = TRUE, options = c("COMPRESS=DEFLATE"), format = "GTiff")
  
  # back directory
  setwd("..")
  
}