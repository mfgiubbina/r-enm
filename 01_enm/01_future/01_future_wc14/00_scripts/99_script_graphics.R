#' ---
#' title: graphics
#' authors: mauricio vancine
#' date: 2020-04-29
#' ---

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(tidyverse)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/00_present"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ <- readr::read_csv("02_occurrences/03_clean/00_occ_clean_taxa_date_bias_limit_spatial_2020-04-29.csv")
occ

# graphics ----------------------------------------------------------------
# directory
dir.create("07_graphics")

# plot
for(i in occ$species %>% unique){
  
  # information
  sp <- i %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")
  print(sp)
  
  # import data
  # directory
  setwd(path); setwd("05_ensembles")
  
  # import ensembles
  sui <- dir(pattern = paste0(i, ".tif$")) %>%
    grep("ensemble", ., value = TRUE) %>% 
    raster::raster() %>% 
    raster::values() %>% 
    na.omit
  
  # import uncertainties
  unc <- dir(pattern = paste0(i, ".tif$")) %>% 
    grep("uncertainties_algorithm", ., value = TRUE) %>% 
    raster::raster() %>% 
    raster::values() %>% 
    na.omit
  
  # combine
  da_sui_unc <- tibble::tibble(sui = sui, unc = unc)

  # import uncertainties thr
  # directory
  setwd(path); setwd("06_ensembles_uncertainties_thrs")
  
  # unc_thr <- dir(pattern = i) %>% 
  #   grep("uncertainties_algorithm", ., value = TRUE) %>% 
  #   raster::stack() %>% 
  #   raster::values() %>% 
  #   na.omit %>% 
  #   tibble::as_tibble() %>% 
  #   tidyr::pivot_longer(cols = colnames(.), names_to = "thr", values_to = "unc") %>% 
  #   dplyr::mutate(thr = stringr::str_sub(thr, start = -3L, end = -1L) %>% stringr::str_to_upper())
  
  # directory
  setwd(path); setwd("07_graphics")
  
  # uncertainties and suitability
  ggplot(data = da_sui_unc) +
    aes(x = sui, y = unc * 100) +
    stat_density2d(aes(alpha = ..level.., fill = ..level..), size = 2, bins = 10, geom = "polygon") + 
    scale_fill_gradient(low = "yellow", high = "red") +
    scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
    geom_point(color = "black", size = .5, alpha = .5, pch = 20) +
    geom_density2d(bins = 10) +
    labs(x = "Suitability", y = "Algorithms uncertainties (%)") +
    theme_bw() +
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 12))
  ggsave(paste0("00_uncertainties_and_suitability_", i, ".png"), wi = 25, he = 20, un = "cm", dpi = 300)
  
  # uncertainties and thresholds
  # ggplot(data = unc_thr) +
  #   aes(x = thr, y = unc * 100) +
  #   geom_jitter(size = .3, width = .2) +
  #   geom_boxplot(color = "gray40", fill = NA) +
  #   labs(x = "Thresholds", y = "Algorithms uncertainties (%)") +
  #   theme_bw() +
  #   theme(axis.title = element_text(size = 15),
  #         axis.text = element_text(size = 12))
  # ggsave(paste0("00_uncertainties_and_thresholds_", i, ".png"), wi = 25, he = 20, un = "cm", dpi = 300)
  
  }

# end ---------------------------------------------------------------------
  