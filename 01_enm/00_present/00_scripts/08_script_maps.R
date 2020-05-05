#' ---
#' title: maps
#' authors: mauricio vancine
#' date: 2020-04-29
#' ---

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(ggspatial)
library(raster)
library(rgdal)
library(rnaturalearth)
library(tidyverse)

# directory
path <- "/home/mude/data/github/00_github_organizar/r-sdm/00_pragmatico/00_present"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ <- readr::read_csv("02_occurrences/03_clean/00_occ_clean_taxa_date_bias_limit_spatial_2020-04-29.csv")
occ

# limits
li <- rnaturalearth::ne_countries(scale = "medium", country = "Brazil", returnclass = "sf")
li
ggplot(li) + geom_sf() + theme_bw()

sa <- rnaturalearth::ne_countries(scale = "medium", continent = c("South America"), returnclass = "sf")
sa
ggplot(sa) + geom_sf() + theme_bw()

# maps --------------------------------------------------------------------
# directory
dir.create("08_maps")

# plot
for(i in occ$species %>% unique){
  
  # information
  sp <- i %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")
  print(sp)
  
  # occurrences -------------------------------------------------------------
  # directory
  setwd(path); setwd("08_maps")
  
  # map
  map_occ <- ggplot() +
    geom_sf(data = sa, fill = "gray90") +
    geom_sf(data = li, fill = "gray75") +
    geom_point(data = occ %>% dplyr::filter(species == i), 
               aes(longitude, latitude, color = species %>% str_to_title() %>% sub("_", " ", .)), 
               size = 2, alpha = .7) +
    scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
    coord_sf(xlim = sf::st_bbox(li)[c(1, 3)], ylim = sf::st_bbox(li)[c(2, 4)]) +
    labs(x = "Longitude", y = "Latitude", color = "Occurrences") +
    annotation_scale(location = "br", width_hint = .3) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0, "cm"), pad_y = unit(.8, "cm"),
                           style = north_arrow_fancy_orienteering) +
    theme_bw() +
    theme(legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 10, face = "italic"),
          legend.background = element_rect(fill = "white",
                                           size = 0.3, 
                                           linetype = "solid", 
                                           colour = "black"),
          axis.title = element_text(size = 12, face = "plain"),
          legend.position = c(.8, .185))
  map_occ
  
  # export
  ggsave(paste0("00_map_occ_", i, ".png"), map_occ, wi = 20, he = 25, un = "cm", dpi = 300)
  
  # continuum ensemble -------------------------------------------------------
  # directory
  setwd(path); setwd("05_ensembles")
  
  # import
  ens <- dir(pattern = i) %>%
    stringr::str_subset("ensemble") %>% 
    stringr::str_subset(".tif$") %>% 
    raster::raster()
  
  # directory
  setwd(path); setwd("08_maps")
  
  # map
  # data
  da <- raster::rasterToPoints(ens) %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(sui = names(ens))
  
  map_sui <- ggplot() +
    geom_sf(data = sa, fill = "gray90") +
    geom_raster(data = da, aes(x, y, fill = sui)) +
    geom_sf(data = li, fill = NA, color = "gray30") +
    scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
    scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", n = 100, type = "continuous"),
                         limits = c(0, 1)) +
    coord_sf(xlim = sf::st_bbox(li)[c(1, 3)], ylim = sf::st_bbox(li)[c(2, 4)]) +
    labs(x = "Longitude", y = "Latitude", fill = "Suitability", color = "Occurrences") +
    annotation_scale(location = "br", width_hint = .3) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0, "cm"), pad_y = unit(.8, "cm"),
                           style = north_arrow_fancy_orienteering) +
    theme_bw() +
    theme(legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 10),
          legend.background = element_rect(fill = "white",
                                           size = 0.3, 
                                           linetype = "solid", 
                                           colour = "black"),
          axis.title = element_text(size = 12, face = "plain"),
          legend.position = c(.83, .16))
  map_sui
  
  # export
  ggsave(paste0("01_map_", names(ens), ".png"), map_sui, wi = 20, he = 25, un = "cm", dpi = 300)
  
  
  # binary ensemble ---------------------------------------------------------
  # directory
  setwd(path); setwd("06_ensembles_uncertainties_thrs")
  
  # import
  ens <- dir(pattern = i) %>% 
    stringr::str_subset("ensemble") %>% 
    stringr::str_subset(".tif$") %>% 
    raster::stack()
  
  # directory
  setwd(path); setwd("08_maps")
  
  for(j in ens %>% raster::nlayers() %>% seq){
    
    # map
    # data
    da <- raster::rasterToPoints(ens[[j]]) %>% 
      tibble::as_tibble() %>%
      dplyr::rename(sui = names(ens[[j]])) %>% 
      dplyr::mutate(sui = ifelse(sui == 0, "Ausência potencial (0)", "Presença potencial (1)"))
    
    map_thr <- ggplot() +
      geom_sf(data = sa) +
      geom_raster(data = da, aes(x, y, fill = sui)) +
      geom_sf(data = li, fill = NA, color = "gray30") +
      # geom_point(data = occ %>% dplyr::filter(species == i), 
      #            aes(longitude, latitude, color = species %>% str_to_title() %>% sub("_", " ", .)), 
      #            size = 2, alpha = .7) +
      scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
      scale_fill_manual(values = c("#3B9AB2", "#F21A00")) +
      coord_sf(xlim = sf::st_bbox(li)[c(1, 3)], ylim = sf::st_bbox(li)[c(2, 4)]) +
      labs(x = "Longitude", y = "Latitude", fill = "Adequabilidade", color = "Ocorrências") +
      annotation_scale(location = "br", width_hint = .3) +
      annotation_north_arrow(location = "br", which_north = "true", 
                             pad_x = unit(0, "cm"), pad_y = unit(.8, "cm"),
                             style = north_arrow_fancy_orienteering) +
      theme_bw() +
      theme(legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 10),
            legend.background = element_rect(fill = "white",
                                             size = 0.3, 
                                             linetype = "solid", 
                                             colour = "black"),
            axis.title = element_text(size = 12, face = "plain"),
            legend.position = c(.8, .18))
    map_thr
    
    # export
    ggsave(paste0("02_map_", names(ens)[[j]], ".png"), map_thr, wi = 20, he = 25, un = "cm", dpi = 300)
    
  }
  
  
  # uncertainties -----------------------------------------------------------
  # directory
  setwd(path); setwd("05_ensembles")
  
  # import
  unc <- dir(pattern = i) %>% 
    stringr::str_subset(".tif$") %>% 
    stringr::str_subset("uncertainties") %>% 
    raster::stack()
  
  # directory
  setwd(path); setwd("08_maps")
  
  for(j in unc %>% raster::nlayers() %>% seq){
    
    # map
    # data
    da <- raster::rasterToPoints(unc[[j]]) %>% 
      tibble::as_tibble() %>%
      dplyr::rename(unc = names(unc[[j]]))
    
    map_unc <- ggplot() +
      geom_sf(data = sa) +
      geom_raster(data = da, aes(x, y, fill = unc * 100)) +
      geom_sf(data = li, fill = NA, color = "gray30") +
      scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
      scale_fill_gradientn(colours = RColorBrewer::brewer.pal(name = "YlOrBr", n = 9)) +
      coord_sf(xlim = sf::st_bbox(li)[c(1, 3)], ylim = sf::st_bbox(li)[c(2, 4)]) +
      labs(x = "Longitude", y = "Latitude", fill = "Uncertainties (%)", color = "Occurrences") +
      annotation_scale(location = "br", width_hint = .3) +
      annotation_north_arrow(location = "br", which_north = "true", 
                             pad_x = unit(0, "cm"), pad_y = unit(.8, "cm"),
                             style = north_arrow_fancy_orienteering) +
      theme_bw() +
      theme(legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 10),
            legend.background = element_rect(fill = "white",
                                             size = 0.3, 
                                             linetype = "solid", 
                                             colour = "black"),
            axis.title = element_text(size = 12, face = "plain"),
            legend.position = c(.8, .15))
    map_unc
    
    # export
    ggsave(paste0("03_map_", names(unc)[[j]], ".png"), map_unc, wi = 20, he = 25, un = "cm", dpi = 300)
    
  }
  
}

# end ---------------------------------------------------------------------