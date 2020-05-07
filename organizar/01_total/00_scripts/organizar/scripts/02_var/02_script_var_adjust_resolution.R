# script description #
# script: variables - adjust resolution
# author: mauricio vancine
# date:   06-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(landscapetools)
library(raster)
library(rgdal)

# raster options
raster::rasterOptions(maxmemory = 1e+10)
raster::rasterOptions(chunksize = 1e+10)
raster::rasterOptions()

# directory
setwd("/media/mude/data/gitlab/r-enm/data/02_var/bioclim_v20")
dir()

# import climate variables ------------------------------------------------
# list files
tif <- dir(pattern = "tif$")
tif

# import
var <- raster::stack(tif)
var

# names
names(var)
names(var) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(var)
var

# plot
landscapetools::show_landscape(var$bio01)

# adjust resolution -------------------------------------------------------
# resolution
raster::res(var)

# aggregation factor 
res_actual <- res(var)[1]
res_actual

res_adjust <- 0.5
res_adjust

agg_fac <- res_adjust/res_actual
agg_fac

# aggregation
var_05 <- raster::aggregate(var, fact = agg_fac)
var_05

# new resolution
raster::res(var_05)[1]

# plot
landscapetools::show_landscape(var$bio01)
landscapetools::show_landscape(var_05$bio01)

# export ------------------------------------------------------------------
raster::writeRaster(x = var_05, 
                    filename = paste0("wc20_masknebrazil_res05g_", names(var)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    overwrite = TRUE)

# exclude -----------------------------------------------------------------
# list
li <- dir(pattern = "10m")
li

# exclude
unlink(li)

# end ---------------------------------------------------------------------