### script enm - dismo ###

## variables selection - vif ##

# mauricio vancine
# 10-03-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(tidyverse)
library(viridis)
library(usdm)

# directory
setwd("/media/mauricio/data/gitlab/r-enm/data/var/01_variables")
dir()

# import variables e get data ---------------------------------------------
# list variables
ti <- dir(pattern = ".tif")
ti

# import rasters
var <- raster::stack(ti)
var

plot(var$bio01)

# extract values
var.da <- var %>% 
  raster::values() %>% 
  na.omit
var.da

# verify
head(var.da)
dim(var.da)

# vif ---------------------------------------------------------------------

# directory
setwd("..")
dir.create("03_vif") 
setwd("03_vif") 

# vif 10
vi.10 <- usdm::vifstep(var.da, th = 10, maxobservations = nrow(var.da))
vi.10
vi.10@results

# vif 02
vi.02 <- usdm::vifstep(var.da, th = 2, maxobservations = nrow(var.da))
vi.02
vi.02@results

# export
readr::write_csv(vi.10@results, "vif_10.csv")
readr::write_csv(vi.02@results, "vif_02.csv")

# end ---------------------------------------------------------------------