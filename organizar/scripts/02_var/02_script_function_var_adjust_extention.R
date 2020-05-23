# script description #
# function: variables - adjust extention
# author:   mauricio vancine
# date:     06-05-2018

# adjust extention function -----------------------------------------------

#' var_adjust_extention
#' 
#' adjust extention of rasters
#' 
#' @usage var_adjust_extention(raster, vector, names, path)
#' 
#' @param raster - raster.
#' Options are: a raster stack, brick ou raster.
#' @param vector - vector. 
#' Options are: a vector.
#' @param names - output names. 
#' Options are: a character with output names.
#' @param path - inform path.
#' Options is: "your_folder_path/"
#' 

# function ----------------------------------------------------------------

var_adjust_extention <- function(raster, vector, names, path){
  
  # packages
  if(!require(beepr)) install.packages("beepr")
  if(!require(raster)) install.packages("raster")
  if(!require(rgdal)) install.packages("rgdal")
  if(!require(sf)) install.packages("sf")
  if(!require(tibble)) install.packages("tidyverse")
  
  # raster options
  raster::rasterOptions(maxmemory = 1e+50)
  raster::rasterOptions(chunksize = 1e+50)
  
  # adjust extention
  for(i in raster %>% names){
    
    # information
    paste0(i, "_", names) %>% print
    
    # select raster
    raster_i <- raster[[i]]
    
    # crop
    raster_crop <- raster::crop(raster_i, vector)
    
    # mask
    raster_mask <- raster::mask(raster_crop, vector)
    
    # directory
    setwd(path)
    
    # export
    raster::writeRaster(raster_mask, 
                        filename = paste0(i, "_", names), 
                        options = c("COMPRESS=DEFLATE"), 
                        format = "GTiff", 
                        overwrite = TRUE)
  }

  # notification
  beepr::beep(5)
  
}

# end ---------------------------------------------------------------------