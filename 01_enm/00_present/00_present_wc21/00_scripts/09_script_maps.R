#' ---
#' title: maps
#' authors: mauricio vancine
#' date: 2020-06-19
#' ---

# prepare r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(ggspatial)
library(raster)
library(rnaturalearth)
library(tidyverse)
library(RColorBrewer)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/00_present/00_present_wc21"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ <- readr::read_csv("01_occurrences/03_clean/occ_clean_taxa_date_bias_limit_spatial.csv")
occ

# limits
li <- rnaturalearth::ne_countries(scale = "small", continent = "South America", returnclass = "sf")
li
ggplot(li) + geom_sf() + theme_bw()

# maps --------------------------------------------------------------------
# directory
dir.create("09_maps")

# plot
for(i in occ$species %>% unique){
  
  # information
  sp <- i %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")
  print(sp)
  
  # occurrences -------------------------------------------------------------
  # directory
  setwd(path); setwd("09_maps"); dir.create(i); setwd(i)
  
  # map
  map_occ <- ggplot() +
    geom_sf(data = li, fill = "gray90") +
    geom_point(data = occ %>% dplyr::filter(species == i), 
               aes(longitude, latitude, 
                   color = species %>% str_to_title() %>% sub("_", " ", .)), 
               size = 2, alpha = .7) +
    scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
    coord_sf(xlim = sf::st_bbox(li)[c(1, 3)], ylim = sf::st_bbox(li)[c(2, 4)]) +
    labs(x = "Longitude", y = "Latitude", color = "Occurrences") +
    annotation_scale(location = "br", width_hint = .3) +
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(.3, "cm"), pad_y = unit(.3, "cm"),
                           style = north_arrow_fancy_orienteering) +
    theme_bw() +
    theme(legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 10, face = "italic"),
          legend.background = element_rect(fill = "white",
                                           size = 0.3, 
                                           linetype = "solid", 
                                           colour = "black"),
          axis.title = element_text(size = 12, face = "plain"),
          legend.position = c(.75, .185))
  map_occ
  
  # export
  ggsave(paste0("01_map_occ_", i, ".png"), map_occ, wi = 20, he = 25, un = "cm", dpi = 300)
  
  # continuum ensemble -------------------------------------------------------
  # directory
  setwd(path); setwd("05_ensembles"); setwd(i)
  
  # import
  ens <- dir(pattern = i) %>%
    stringr::str_subset(paste0("ens_", i, ".tif")) %>% 
    raster::raster()
  
  # directory
  setwd(path); setwd("09_maps"); setwd(i)
  
  # map
  # data
  da <- raster::rasterToPoints(ens) %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(sui = names(ens))
  
  map_sui <- ggplot() +
    geom_raster(data = da, aes(x, y, fill = sui)) +
    geom_sf(data = li, fill = NA, color = "gray30") +
    scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
    scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", n = 100, type = "continuous"),
                         limits = c(0, 1)) +
    coord_sf(xlim = sf::st_bbox(li)[c(1, 3)], ylim = sf::st_bbox(li)[c(2, 4)]) +
    labs(x = "Longitude", y = "Latitude", fill = "Suitability") +
    annotation_scale(location = "br", width_hint = .3) +
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(.3, "cm"), pad_y = unit(.3, "cm"),
                           style = north_arrow_fancy_orienteering) +
    theme_bw() +
    theme(legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 10),
          legend.background = element_rect(fill = "white",
                                           size = 0.3, 
                                           linetype = "solid", 
                                           colour = "black"),
          axis.title = element_text(size = 12, face = "plain"),
          legend.position = c(.75, .17))
  map_sui
  
  # export
  ggsave(paste0("02_map_", names(ens), ".png"), map_sui, wi = 20, he = 25, un = "cm", dpi = 300)
  
  
  # binary ensemble ---------------------------------------------------------
  # directory
  setwd(path); setwd("06_ensemble_thresholds"); setwd(i)
  
  # import
  ens <- dir(pattern = i) %>% 
    stringr::str_subset(paste0("ens_", i, "_")) %>% 
    stringr::str_subset(".tif$") %>% 
    raster::stack()
  
  # directory
  setwd(path); setwd("09_maps"); setwd(i)
  
  for(j in ens %>% raster::nlayers() %>% seq){
    
    # map
    # data
    da <- raster::rasterToPoints(ens[[j]]) %>% 
      tibble::as_tibble() %>%
      dplyr::rename(sui = names(ens[[j]])) %>% 
      dplyr::mutate(sui = ifelse(sui == 0, "Potential absence (0)", "Potential presence (1)"))
    
    thr <- dplyr::last(stringr::str_split(names(ens[[j]]), "_", simplify = TRUE))
    
    map_thr <- ggplot() +
      geom_raster(data = da, aes(x, y, fill = sui)) +
      geom_sf(data = li, fill = NA) +
      scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
      scale_fill_manual(values = c("#3B9AB2", "#F21A00")) +
      coord_sf(xlim = sf::st_bbox(li)[c(1, 3)], ylim = sf::st_bbox(li)[c(2, 4)]) +
      labs(x = "Longitude", y = "Latitude", fill = paste0("Suitability (", thr, ")")) +
      annotation_scale(location = "br", width_hint = .3) +
      annotation_north_arrow(location = "tr", which_north = "true", 
                             pad_x = unit(.3, "cm"), pad_y = unit(.3, "cm"),
                             style = north_arrow_fancy_orienteering) +
      theme_bw() +
      theme(legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 10),
            legend.background = element_rect(fill = "white",
                                             size = 0.3, 
                                             linetype = "solid", 
                                             colour = "black"),
            axis.title = element_text(size = 12, face = "plain"),
            legend.position = c(.75, .15))
    map_thr
    
    # export
    ggsave(paste0("03_map_", names(ens)[[j]], ".png"), map_thr, wi = 20, he = 25, un = "cm", dpi = 300)
    
  }
  
  
  # uncertainties -----------------------------------------------------------
  # directory
  setwd(path); setwd("07_uncertainties"); setwd(i)
  
  # import
  unc <- dir(pattern = i) %>% 
    stringr::str_subset(".tif$") %>% 
    stringr::str_subset("unc_") %>% 
    raster::stack()
  
  # directory
  setwd(path); setwd("09_maps"); setwd(i)
  
  for(j in unc %>% raster::nlayers() %>% seq){
    
    # map
    # data
    da <- raster::rasterToPoints(unc[[j]]) %>% 
      tibble::as_tibble() %>%
      dplyr::rename(unc = names(unc[[j]]))
    
    map_unc <- ggplot() +
      geom_raster(data = da, aes(x, y, fill = unc)) +
      geom_sf(data = li, fill = NA, color = "gray30") +
      scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
      scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(name = "Spectral", n = 9)), limits = c(0, 100)) +
      coord_sf(xlim = sf::st_bbox(li)[c(1, 3)], ylim = sf::st_bbox(li)[c(2, 4)]) +
      labs(x = "Longitude", y = "Latitude", 
           fill = paste0("Uncertainty \n",
                         paste0(names(unc[[j]]) %>% 
                                  stringr::str_replace(i, "") %>% 
                                  stringr::str_replace_all("[0-9]", "") %>%
                                  stringr::str_replace_all("unc", "") %>%
                                  stringr::str_replace_all("_", " ") %>%
                                  stringi::stri_trim()),
                         " (%)")) +
      annotation_scale(location = "br", width_hint = .3) +
      annotation_north_arrow(location = "tr", which_north = "true", 
                             pad_x = unit(.3, "cm"), pad_y = unit(.3, "cm"),
                             style = north_arrow_fancy_orienteering) +
      theme_bw() +
      theme(legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 10),
            legend.background = element_rect(fill = "white",
                                             size = 0.3, 
                                             linetype = "solid", 
                                             colour = "black"),
            axis.title = element_text(size = 12, face = "plain"),
            legend.position = c(.75, .17))
    map_unc
    
    # export
    ggsave(paste0("04_map_unc_", names(unc)[[j]], ".png"), map_unc, wi = 20, he = 25, un = "cm", dpi = 300)
    
  }
  
}

# end ---------------------------------------------------------------------