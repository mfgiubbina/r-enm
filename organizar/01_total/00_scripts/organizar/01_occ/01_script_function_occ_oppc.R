# script description #
# aim:    function occurences - one point per cell
# author: mauricio vancine
# date:   05-05-2018

# one point per cell ------------------------------------------------------

#' occ_oppc
#' 
#' one point per cell
#' 
#' @usage occ_oppc(occ, var, output_raw, output_oppc)
#' 
#' @param occ - vector. 
#' Options are: a vector.
#' @param var - raster.
#' Options are: a raster stack, brick ou raster.
#' @param output_raw - occ ouput raw.
#' Options are: raw oppc output.
#' @param output_oppc - occ ouput oppc.
#' Options are: oppc output.
#' 

# function ----------------------------------------------------------------

occ_oppc <- function(occ, raster, output_raw, output_oppc){
  
  # packages
  if(!require(beepr)) install.packages("beepr")
  if(!require(raster)) install.packages("raster")
  if(!require(rgdal)) install.packages("rgdal")
  if(!require(tibble)) install.packages("tidyverse")
  
  # raster options
  raster::rasterOptions(maxmemory = 1e+10)
  raster::rasterOptions(chunksize = 1e+10)
  
  # raster id
  raster_id <- var
  raster_id[!is.na(raster_id)] <- raster::cellFromXY(raster_id, raster::rasterToPoints(raster_id)[, 1:2])
  
  # extract
  occ_id_cell <- dplyr::mutate(occ, oppc = raster::extract(raster_id, dplyr::select(., longitude, latitude)))

  # oppc 
  occ_oppc <- occ_id_cell %>% 
    dplyr::distinct(species, oppc, .keep_all = TRUE) %>% 
    dplyr::filter(!is.na(oppc)) %>% 
    dplyr::add_count(species) %>% 
    dplyr::arrange(species)
  occ_oppc

  # condition raw
  if(output_raw == TRUE) {
    
    # export
    readr::write_csv(occ_id_cell, paste(occ, "_raw.csv"))
  
    }
  
  # condition oppc
  if(output_oppc == TRUE){
  
    # export
    readr::write_csv(occ_oppc, paste(occ, "_oppc.csv"))  
    
  }
  
  # notification
  beepr::beep(3)
  
}

# end ---------------------------------------------------------------------