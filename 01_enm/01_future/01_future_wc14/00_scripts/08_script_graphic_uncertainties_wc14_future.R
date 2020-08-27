#' ---
#' title: uncertainties graphics
#' author: mauricio vancine
#' date: 2020-06-19
#' ---

# prepare r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(tidyverse)
library(vegan)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/01_future/01_future_wc14"
setwd(path)
dir()

# graphics ----------------------------------------------------------------
# directory
dir.create("08_graphic_uncertainties")

# directory
setwd(path); setwd("05_ensembles")
sp <- dir()
sp

# plot
for(i in sp){
  
  # information
  print(i)
  
  # suitability -------------------------------------------------------------
  # directory
  setwd(path); setwd("05_ensembles"); setwd(i)
  
  # import
  ens <- dir(pattern = i) %>%
    stringr::str_subset(".tif$") %>% 
    raster::stack() %>% 
    raster::values() %>% 
    dplyr::as_tibble()
  
  # uncertainties -----------------------------------------------------------
  # directory
  setwd(path); setwd("07_uncertainties"); setwd(i)
  
  # import
  unc <- dir(pattern = i) %>%
    stringr::str_subset(".tif$") %>% 
    raster::stack() %>% 
    raster::values() %>% 
    dplyr::as_tibble()
  
  # graphics ----------------------------------------------------------------
  # directory
  setwd(path); setwd("08_graphic_uncertainties"); dir.create(i); setwd(i)
  
  # scenarios
  sce <- names(ens) %>% 
    stringr::str_split_fixed("_", 4) %>% 
    tibble::as_tibble() %>% 
    dplyr::select(4) %>% 
    dplyr::pull()
  
  for(j in sce){
    
    # information
    print(paste0(i, "for scenario ", j))
    
    # filter
    ens_j <- ens %>% 
      dplyr::select(contains(j))
    
    unc_j <- unc %>% 
      dplyr::select(contains(j))
    
    # combine
    da_sui_unc <- tibble::tibble(sui = ens_j %>% dplyr::pull(), 
                                 unc_gcm = unc_j %>% dplyr::select(contains("unc_gcms_")) %>% dplyr::pull(), 
                                 unc_met_gcm = unc_j %>% dplyr::select(contains("unc_methodsgcms_")) %>% dplyr::pull(), 
                                 unc_met = unc_j %>% dplyr::select(contains("unc_methods_")) %>% dplyr::pull(), 
                                 unc_res = unc_j %>% dplyr::select(contains("_residuals_")) %>% dplyr::pull())
    
    # uncertainties and suitability
    ggplot(data = da_sui_unc) +
      aes(x = sui, y = unc_met) +
      stat_density2d(aes(alpha = ..level.., fill = ..level..), size = 2, bins = 10, geom = "polygon") + 
      scale_fill_gradient(low = "yellow", high = "red") +
      scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
      geom_point(color = "black", size = .5, alpha = .5, pch = 20) +
      geom_density2d(bins = 10) +
      labs(x = "Suitability", y = "Methods uncertainties (%)") +
      xlim(0, 1) +
      ylim(0, 100) +
      theme_bw() +
      theme(axis.title = element_text(size = 15),
            axis.text = element_text(size = 12))
    ggsave(paste0("plot_uncertainties_",  i, "_", j, "_methods.png"), wi = 25, he = 20, un = "cm", dpi = 300)
    
    ggplot(data = da_sui_unc) +
      aes(x = sui, y = unc_gcm) +
      stat_density2d(aes(alpha = ..level.., fill = ..level..), size = 2, bins = 10, geom = "polygon") + 
      scale_fill_gradient(low = "yellow", high = "red") +
      scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
      geom_point(color = "black", size = .5, alpha = .5, pch = 20) +
      geom_density2d(bins = 10) +
      labs(x = "Suitability", y = "GCMs uncertainties (%)") +
      xlim(0, 1) +
      ylim(0, 100) +
      theme_bw() +
      theme(axis.title = element_text(size = 15),
            axis.text = element_text(size = 12))
    ggsave(paste0("plot_uncertainties_",  i, "_", j, "_gcms.png"), wi = 25, he = 20, un = "cm", dpi = 300)
    
    ggplot(data = da_sui_unc) +
      aes(x = sui, y = unc_met_gcm) +
      stat_density2d(aes(alpha = ..level.., fill = ..level..), size = 2, bins = 10, geom = "polygon") + 
      scale_fill_gradient(low = "yellow", high = "red") +
      scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
      geom_point(color = "black", size = .5, alpha = .5, pch = 20) +
      geom_density2d(bins = 10) +
      labs(x = "Suitability", y = "Methods * GCMs uncertainties (%)") +
      xlim(0, 1) +
      ylim(0, 100) +
      theme_bw() +
      theme(axis.title = element_text(size = 15),
            axis.text = element_text(size = 12))
    ggsave(paste0("plot_uncertainties_",  i, "_", j, "_methods_gcms.png"), wi = 25, he = 20, un = "cm", dpi = 300)
    
    ggplot(data = da_sui_unc) +
      aes(x = sui, y = unc_res) +
      stat_density2d(aes(alpha = ..level.., fill = ..level..), size = 2, bins = 10, geom = "polygon") + 
      scale_fill_gradient(low = "yellow", high = "red") +
      scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
      geom_point(color = "black", size = .5, alpha = .5, pch = 20) +
      geom_density2d(bins = 10) +
      labs(x = "Suitability", y = "Residual uncertainties (%)") +
      xlim(0, 1) +
      ylim(0, 100) +
      theme_bw() +
      theme(axis.title = element_text(size = 15),
            axis.text = element_text(size = 12))
    ggsave(paste0("plot_uncertainties_",  i, "_", j, "_residuals.png"), wi = 25, he = 20, un = "cm", dpi = 300)    
    
  }
  
  print("All right, finish!")
  
}

# end ---------------------------------------------------------------------