# script description #
# function: variables - pca projection
# authors:  matheus lima-ribeiro, mauricio vancine
# date:     29-04-2018

# var pca future function -------------------------------------------------

#' pca projection function
#' 
#' @usage var_pca_proj(raster_pres, raster_proj, cum_sum, graphics, graphic_names,
#'                     graphic_var_pres_names, graphic_var_pres_names, prefix_pca_raster_pres, 
#'                     prefix_pca_raster_proj, path_output_pca_pres, path_output_pca_proj)
#' 
#' @param raster_pres - raster present.
#' Options are: raster stack or brick of present.
#' @param raster_proj - raster projection.
#' Options are: raster stack or brick to project.
#' @param cum_sum - values to cumulative sum.
#' Options is: value to cumulative sum - 0 to 1 - at least 0.95.
#' @param graphics - pca graphics.
#' Options is: TRUE or FALSE
#' @param graphic_names - variables names.
#' Options is: variables names.
#' @param graphic_var_pres_names - raster present.
#' Options are: raster stack or brick of present.
#' @param prefix_pca_pres - variables names.
#' Options is: variables names.
#' @param prefix_pca_proj - variables names.
#' Options is: variables names.
#' @param path_output_pca_pres - folder to output pca present.
#' Options is: output directory to present pca.
#' @param path_output_pca_proj - folder to output.
#' Options is: output directory to project pca.
#' 

# function ----------------------------------------------------------------

var_pca_proj <- function(raster_pres, raster_proj, cum_sum, graphics, graphic_names,
                         graphic_var_pres_names, prefix_pca_raster_pres, 
                         prefix_pca_raster_proj, path_output_pca_pres, path_output_pca_proj){
  
  # packages
  if(!require(beepr)) install.packages("beepr")
  if(!require(factoextra)) install.packages("factoextra")
  if(!require(FactoMineR)) install.packages("FactoMineR")
  if(!require(raster)) install.packages("raster")
  if(!require(rgdal)) install.packages("rgdal")
  if(!require(tidyverse)) install.packages("tidyverse")
  
  # raster options
  raster::rasterOptions(maxmemory = 1e+10)
  raster::rasterOptions(chunksize = 1e+10)
  
  # pca present ----------------------------------------------------------
  # information
  print("Adjusting PCA to present")
  
  # data var present
  da_pres <- raster::rasterToPoints(raster_pres) %>% 
    tibble::as_tibble() %>% 
    na.omit()
  
  da_pres_var <- da_pres %>% 
    dplyr::select(-1, -2)
  
  da_pres_coord <- da_pres %>% 
    dplyr::select(1, 2)
  
  # names
  colnames(da_pres_var) <- graphic_var_pres_names
  
  # pca present
  pca_pres <- prcomp(da_pres_var, scale = TRUE)
  
  # statistics var present
  mean_pres <- colMeans(da_pres_var)
  sd_pres <- apply(da_pres_var, 2, sd)
  coef_pres <- pca_pres$rotation
  
  # engivalues
  cum_var <- summary(pca_pres)$importance[3, ]
  cum_var_95 <- sum(cum_var <= cum_sum) + 1
  
  # scores
  eix_pres_95 <- pca_pres$x[, seq(cum_var_95)] %>% 
    as.data.frame %>% 
    tibble::as_tibble()
  
  # raster of scores
  eix_pres_95_coords <- dplyr::bind_cols(da_pres_coord, eix_pres_95)
  gridded(eix_pres_95_coords) <- ~x+y
  pca_pres_raster <- raster::stack(eix_pres_95_coords)
  
  ## export
  # information
  print("Exporting PCA rasters to present")
  
  # directory
  setwd(path_output_pca_pres)
  
  # raster
  raster::writeRaster(x = pca_pres_raster, 
                      filename = paste0(prefix_pca_raster_pres, "_pc0", seq(cum_var_95)), 
                      bylayer = TRUE,
                      options = c("COMPRESS=DEFLATE"), 
                      format = "GTiff", 
                      overwrite = TRUE)
  
  # graphics
  if(graphics == TRUE){
    
    # information
    print("Exporting PCA graphics to present")
    
    # eigenvalues plot
    factoextra::fviz_eig(pca_pres, addlabels = TRUE, ggtheme = theme_classic())
    ggsave(paste0(graphic_names, "_pca_screeplot.tiff"), he = 15, wi = 20, un = "cm", dpi = 300, comp = "lzw")
    
    # biplot
    factoextra::fviz_pca(pca_pres, geom = "point", col.ind = "black", alpha.ind = .5, repel = TRUE) + theme_bw()
    ggsave(paste0(graphic_names, "_pca_biplot.tiff"), he = 15, wi = 20, un = "cm", dpi = 300, comp = "lzw")
    
  }
  
  
  # pca projection ----------------------------------------------------------
  
  # information
  print("Adjusting PCA to projection")
  
  # data var future
  da_proj <- raster::rasterToPoints(raster_proj) %>% 
    tibble::as_tibble() %>% 
    na.omit()
  
  da_proj_var <- da_proj %>% 
    dplyr::select(-1, -2)
  
  da_proj_coord <- da_proj %>% 
    dplyr::select(1, 2)
  
  # standardization to presence
  scale <- sweep(da_proj_var, 2, mean_pres) %>% as.matrix
  scale <- scale %*% diag(1 / sd_pres)
  
  # pca projection
  eix_proj <- NULL
  
  for(i in 1:ncol(da_proj_var)){
    
    coef_proj <- coef_pres[, i] %>% as.numeric
    pc_proj <- scale %*% coef_proj
    eix_proj <- cbind(eix_proj, pc_proj)
    
  }
  
  # pca 
  eix_proj_95 <- eix_proj[, seq(cum_var_95)]  %>% 
    as.data.frame %>% 
    tibble::as_tibble()
    
  colnames(eix_proj_95) <- colnames(eix_pres_95)
  
  # raster
  eix_proj_95_coords <- dplyr::bind_cols(da_proj_coord, eix_proj_95)
  gridded(eix_proj_95_coords) <- ~x+y
  pca_proj_raster <- raster::stack(eix_proj_95_coords)

  # export
  # information
  print("Exporting PCA rasters to projection")
  
  # directory
  setwd(path_output_pca_proj)
  
  # rasters
  raster::writeRaster(x = pca_proj_raster, 
                      filename = paste0(prefix_pca_raster_proj, "_pc0", seq(cum_var_95)), 
                      bylayer = TRUE,
                      options = c("COMPRESS=DEFLATE"), 
                      format = "GTiff", 
                      overwrite = TRUE)
  
  # notification
  beepr::beep(3)
  
}

# end ---------------------------------------------------------------------