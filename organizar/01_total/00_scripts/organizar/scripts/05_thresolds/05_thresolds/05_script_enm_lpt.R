### script lpt ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 27/05/2017

###------------------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, maps, maptools)

###------------------------------------------------------------------------------###

# import data

## occurrences
# directory
setwd("D:/pequenos_mamiferos/01_data")

po <- read.table("pontos_final.txt", h = T)

plot(po[, 2], po[, 3], col = po$sp, pch = 20, xlab = "long", ylab = "lat")

# species
sp <- levels(po$sp)
sp

## enms
# directory
setwd("D:/pequenos_mamiferos/02_output/ensemble_wei")

# enms
# list files
tif <- list.files()
tif

enm <- stack(tif)
enm

## lpt
# diretory
setwd("..")
dir.create("03_lpt")
setwd("03_lpt")
getwd()

# lpts
for(i in sp){
  lpt <- extract(enm[[grep(paste0(i, "_pres"), names(enm))]], po[po$sp == i, 2:3]) 
  lpt <- min(lpt[lpt > 0])
  enm.lpt <- enm[[grep(i, names(enm))]] >= lpt
  
  for(j in 1:length(names(enm.lpt))){
      writeRaster(enm.lpt[[j]], paste0("lpt", sub("ensemble_wei_aver", "", names(enm.lpt[[j]])), ".tif"), format = "GTiff")}}



