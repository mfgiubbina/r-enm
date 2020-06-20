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

# directory
path <- "/home/mude/data/github/r-enm/01_enm/00_present/00_present_wc14"
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
    stringr::str_subset(paste0("ens_", i, ".tif")) %>% 
    raster::raster() %>% 
    raster::values()
  
  # uncertainties -----------------------------------------------------------
  # directory
  setwd(path); setwd("07_uncertainties"); setwd(i)
  
  # import
  unc <- dir(pattern = i) %>% 
    stringr::str_subset(".tif$") %>% 
    stringr::str_subset("unc") %>% 
    raster::stack()
  
  # directory
  setwd(path); setwd("08_graphic_uncertainties"); dir.create(i); setwd(i)
  
  # graphics ----------------------------------------------------------------
  # combine
  da_sui_unc <- tibble::tibble(sui = ens, unc_met = unc[[1]][], unc_res = unc[[2]][]) %>% 
    tidyr::drop_na()
  
  # uncertainties and suitability
  ggplot(data = da_sui_unc) +
    aes(x = sui, y = unc_met) +
    stat_density2d(aes(alpha = ..level.., fill = ..level..), size = 2, bins = 10, geom = "polygon") + 
    scale_fill_gradient(low = "yellow", high = "red") +
    scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
    geom_point(color = "black", size = .5, alpha = .5, pch = 20) +
    geom_density2d(bins = 10) +
    labs(x = "Suitability", y = "Method uncertainties (%)") +
    theme_bw() +
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 12))
  ggsave(paste0("plot_method_uncertainties_and_suitability_", i, ".png"), wi = 25, he = 20, un = "cm", dpi = 300)
  
  ggplot(data = da_sui_unc) +
    aes(x = sui, y = unc_res) +
    stat_density2d(aes(alpha = ..level.., fill = ..level..), size = 2, bins = 10, geom = "polygon") + 
    scale_fill_gradient(low = "yellow", high = "red") +
    scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
    geom_point(color = "black", size = .5, alpha = .5, pch = 20) +
    geom_density2d(bins = 10) +
    labs(x = "Suitability", y = "Residual uncertainties (%)") +
    theme_bw() +
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 12))
  ggsave(paste0("plot_residual_uncertainties_and_suitability_", i, ".png"), wi = 25, he = 20, un = "cm", dpi = 300)
  
}

# end ---------------------------------------------------------------------