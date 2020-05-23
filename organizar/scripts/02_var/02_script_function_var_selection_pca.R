# script description #
# function: variables - pca
# authors:  mauricio vancine
# date:     05-05-2018

# var pca function -------------------------------------------------

#' pca function
#' 
#'@usage var_pca(raster, cum_sum, export_eigenvalues, export_loadings, 
#'               export_scores, var_names, export_graphics, 
#'               prefix_pca_raster, path_output_pca)
#'
#' @param raster - raster.
#' Options are: raster stack or brick.
#' @param cum_sum - values to cumulative sum.
#' Options is: value to cumulative sum - 0 to 1 - at least 0.95.
#' @param export_eigenvalues - export graphics.
#' Options is: TRUE or FALSE.
#' @param export_loadings - export graphics.
#' Options is: TRUE or FALSE.
#' @param export_scores - export graphics.
#' Options is: TRUE or FALSE.
#' @param export_graphics - export graphics.
#' Options is: TRUE or FALSE.
#' @param var_names - variables names.
#' Options is: variables names.
#' @param prefix_pca_raster - export graphics.
#' Options is: TRUE or FALSE.
#' @param path_output_pca - folder to output pca.
#' Options is: output directory to present pca.
#' 

# function ----------------------------------------------------------------
var_pca <- function(raster, cum_sum, export_eigenvalues, export_loadings, export_scores, 
                    var_names, export_graphics, prefix_pca_raster, path_output_pca){
  
  # packages ----------------------------------------------------------------
  if(!require(beepr)) install.packages("beepr")
  if(!require(factoextra)) install.packages("factoextra")
  if(!require(FactoMineR)) install.packages("FactoMineR")
  if(!require(raster)) install.packages("raster")
  if(!require(rgdal)) install.packages("rgdal")
  if(!require(tidyverse)) install.packages("tidyverse")
  
  # raster options
  raster::rasterOptions(maxmemory = 1e+10)
  raster::rasterOptions(chunksize = 1e+10)
  
  # pca ---------------------------------------------------------------------
  # information
  print("Adjusting PCA")
  
  # raster data 
  raster_da <- raster::rasterToPoints(raster) %>% 
    tibble::as_tibble() %>% 
    na.omit()
  
  raster_da_var <- raster_da %>% 
    dplyr::select(-1, -2)
  
  raster_da_coords <- raster_da %>% 
    dplyr::select(1, 2)
  
  # names
  colnames(raster_da_var) <- var_names
  
  # pca
  pca <- prcomp(raster_da_var, scale = TRUE)
  
  # cummulative variation
  cum_var <- summary(pca)$importance[3, ]
  cum_var_95 <- sum(cum_var <= cum_sum) + 1
  
  # scores
  eix_pca_95 <- pca$x[, seq(cum_var_95)] %>% 
    as.data.frame %>% 
    tibble::as_tibble()
  
  # raster of scores
  eix_pca_95_coord <- dplyr::bind_cols(raster_da_coords, eix_pca_95)
  gridded(eix_pca_95_coord) <- ~x+y
  pca_raster <- raster::stack(eix_pca_95_coord)
  
  # export ------------------------------------------------------------------
  
  # information
  print("Exporting PCA rasters")
  
  # rasters
  setwd(path_output_pca)
  raster::writeRaster(x = pca_raster, 
                      filename = paste0(prefix_pca_raster, "_pc0", seq(cum_var_95)), 
                      bylayer = TRUE,
                      options = c("COMPRESS=DEFLATE"), 
                      format = "GTiff", 
                      overwrite = TRUE)
  
  # eigenvalues
  # information
  print("Exporting PCA eigenvalues")
  
  if(export_eigenvalues == TRUE){
    summary(pca)$importance %>% 
      t %>% 
      tibble::as_tibble() %>% 
      readr::write_csv("pca_cumulativa_variation.csv")
  }
  
  # loadings
  # information
  print("Exporting PCA loadings")
  
  if(export_loadings == TRUE){
    pca$rotation %>% 
      cbind(graphic_names, .) %>% 
      tibble::as_tibble() %>% 
      readr::write_csv("pca_loadings.csv")
  }
  
  # scores
  # information
  print("Exporting PCA scores")
  
  if(export_scores == TRUE){
    pca$x %>% 
      tibble::as_tibble() %>% 
      readr::write_csv("pca_scores.csv")
  }
  
  
  # graphics
  if(export_graphics == TRUE){
    
    # information
    print("Exporting PCA graphics")
    
    # eigenvalues plot
    factoextra::fviz_eig(pca, addlabels = TRUE, ggtheme = theme_classic())
    ggsave("pca_screeplot.tiff", he = 15, wi = 20, un = "cm", dpi = 300, comp = "lzw")
    
    # biplot
    factoextra::fviz_pca(pca, geom = "point", col.ind = "black", alpha.ind = .5, repel = TRUE) + theme_bw()
    ggsave("pca_biplot.tiff", he = 15, wi = 20, un = "cm", dpi = 300, comp = "lzw")
    
  }
  
  # notification
  beepr::beep(3)
  
}

# end ---------------------------------------------------------------------