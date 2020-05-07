# script description #
# function: variables - correlation
# author:   mauricio vancine
# date:     28-04-2018


# var correlation function ------------------------------------------------

#' var correlation function
#' 
#' correlation
#' 
#' @usage var_correlation(raster, cutoff, method, names, path)
#' 
#' @param raster - raster.
#' Options are: a raster stack or brick.
#' @param cutoff - value to considerate correlation
#' Options are: 0 to 1.
#' @param method - type of correlation
#' Options are: pearson or spearman.
#' @param names - names of variables
#' Options are: a character vector of variables names.
#' @param path - inform output path.
#' Options is: "your_folder_path/"
#' @param folder - folder to output.
#' Options is: "correlation"
#' 

# function ----------------------------------------------------------------

var_correlation <- function(raster, cutoff, method, names, path, folder){
  
  # packages
  if(!require(beepr)) install.packages("beepr")
  if(!require(caret)) install.packages("caret")
  if(!require(corrr)) install.packages("corrr")
  if(!require(GGally)) install.packages("GGally")
  if(!require(raster)) install.packages("raster")
  if(!require(rgdal)) install.packages("rgdal")
  if(!require(tidyverse)) install.packages("tidyverse")
  if(!require(wesanderson)) install.packages("wesanderson")
  
  # directory
  setwd(path)
  dir.create(folder, showWarnings = FALSE)
  setwd(folder)
  
  # get values
  raster_da <- raster %>% 
    raster::values() %>% 
    tibble::as_tibble() %>% 
    na.omit
  
  # rename variables
  colnames(raster_da) <- names
  
  # correlation
  cor <- corrr::correlate(raster_da, method = method, quiet = TRUE)
  
  # lower
  cor_lower <- corrr::shave(cor)
  
  # visualizate
  cor_vis <- corrr::fashion(cor_lower)
  
  # export
  readr::write_csv(cor_vis, paste0("correlation_table_", method, "_", sub("[.]", "", cutoff), ".csv"))
  
  # plot
  corrr::rplot(cor_lower, 
               shape = 20, 
               print_cor = TRUE, 
               colors = wesanderson::wes_palette(name = "Zissou1", 10, type = "continuous")) %>% 
    ggsave(filename = paste0("correlation_plot_", method, ".tiff"), 
           wi = 20, he = 15, units = "cm", dpi = 300, comp = "lzw")
  
  corrr::network_plot(cor, 
                      min_cor = cutoff,
                      colors = wesanderson::wes_palette(name = "Zissou1", 10, type = "continuous")) %>% 
    ggsave(filename = paste0("correlation_fluxes_", method, "_", sub("[.]", "", cutoff), "_cutoff.tiff"),
           wi = 20, he = 15, units = "cm", dpi = 300, comp = "lzw")
  
  # select variables --------------------------------------------------------
  # correlated variables
  var_cor <- corrr::as_matrix(cor) %>% 
    caret::findCorrelation(cutoff = cutoff)
  
  # information
  paste0(paste0("Correlation above ", cutoff, " to "), paste0(var_cor %>% sort, collapse = ", ")) %>% print()
  
  # select
  raster_da_cutoff <- raster_da %>% 
    dplyr::select(-var_cor)
  
  # correlation cutoff
  corrr::correlate(raster_da_cutoff, method = method, quiet = TRUE) %>% 
    corrr::shave() %>% 
    corrr::fashion() %>% 
    readr::write_csv(paste0("correlation_table_", method, "_", sub("[.]", "", cutoff), "_cutoff.csv"))
  
  # graphic
  cor_plot_cutoff <- GGally::ggpairs(
    raster_da_cutoff %>% dplyr::sample_n(1e3), 
    lower = list(continuous = wrap(ggally_points, pch = 21, color = "black", fill = "gray", size = 2, alpha = .7)),
    diag = list(continuous = wrap(ggally_barDiag, fill = "gray", color = "black", bins = 10)),
    upper = list(continuous = wrap(ggally_cor, color = "black", size = 5, method = "spearman"))) +
    theme_bw() +
    theme(text = element_text(colour = "black"),
          axis.text = element_text(size = 8, colour = "black"), 
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13), 
          panel.grid.major = element_line(colour = "white"))
  
    ggsave(cor_plot_cutoff, 
           filename = paste0("correlation_plot_", method, "_", sub("[.]", "", cutoff), "_cutoff.tiff"),
           wi = 20, he = 15, units = "cm", dpi = 300, comp = "lzw")
    
  # notification
  beepr::beep(3)
  
}

# end ---------------------------------------------------------------------