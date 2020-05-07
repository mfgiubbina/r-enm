### script rename files ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 05/06/2017

###----------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, stringr, data.table)

# verify packages
search()

# diretorio
setwd("D:/pequenos_mamiferos/02_output")

tif <- dir(patt = ".tif$")
tif

for(i in tif){
  
  ra <- raster(i)
  
  ch <- str_extract(names(ra), "[aA-zZ]+")
  
  nu <- as.numeric(str_extract(names(ra), "[0-9]+"))
  
  setwd("D:/pequenos_mamiferos/03_output")
  
  writeRaster(ra, paste0(ch, ifelse(nu <= 9, paste0("0", nu), nu), ".tif"), format = "GTiff")
  
  setwd("D:/pequenos_mamiferos/02_output")}
