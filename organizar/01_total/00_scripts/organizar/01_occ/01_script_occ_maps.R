### script maps - proeco ###

# mauricio vancine
# 12-04-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(ggsn)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)
library(wesanderson)

# directory
path <- "/media/mude/data/00_trabalho/00_empresas/proeco/01_data/00_occ/01_data"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ <- readr::read_csv("occ_cba_2019_04_d12_rev_herpeto_aves.csv")
occ

# li
li <- sf::st_read("/media/mude/data/00_trabalho/00_empresas/proeco/01_data/01_var/00_limite/limite_gcswgs84.shp") %>% 
  dplyr::select(-label)
li

# color
col <- tibble::tibble(campanha = paste0("C", 1:3),
                    col = c("blue", "yellow", "red"))
col

# maps --------------------------------------------------------------------
# directory
setwd(path)
setwd("..")
dir.create("02_maps")
setwd("02_maps")

dir.create("aves")
dir.create("reptilia")
dir.create("amphibia")
dir.create("mammalia")

# maps
for(i in occ$especies_modificada %>% unique %>% sort){
  
  # information
  print(i)
  
  # occ
  occ_sp <- occ %>% 
    dplyr::filter(especies_modificada == i) %>% 
    dplyr::select(longitude_dec, latitude_dec, classe, campanha) %>% 
    dplyr::add_count(campanha) %>% 
    dplyr::mutate(campanha = paste0("C", campanha),
                  campanha_n = paste0(campanha, " (", n, ")")) %>%
    dplyr::filter(longitude_dec < -22)
  occ_sp
  
  # color
  col_map <- col %>% 
    dplyr::filter(campanha %in% c(occ_sp$campanha %>% unique %>% as.character)) %>% 
    dplyr::pull()
  col_map
  
  # directory
  setwd(occ_sp %>% 
          dplyr::select(classe) %>% 
          dplyr::distinct(classe) %>% 
          dplyr::pull())
  
  # map
  map.sp <- ggplot() + 
    geom_sf(data = li, color = "black", fill = "darkcyan", alpha = .4) +
    geom_point(data = occ_sp, aes(x = longitude_dec, y = latitude_dec, 
                                  color = campanha_n), size = 3, alpha = .5) +
    scale_color_manual(values = col_map) +
    coord_sf() +
    theme_bw() +
    labs(x = "Longitude", y = "Latitude", fill = "Adequabilidade", color = "Campanha",
         title = paste0(i, "  (n = ", nrow(occ_sp), ")")) +
    ggsn::north(data = li, symbol = 12, scale = .09) +
    ggsn::scalebar(data = li, dist = 5, dist_unit = "km", transform = TRUE, model = "WGS84",
                   height = .02, st.dist = .03, st.size = 4, border.size = .5) +
    theme(title = element_text(face = "italic", size = 15),
          axis.title = element_text(face = "plain", size = 12),
          legend.title = element_text(face = "bold", size = 10))
  map.sp
  
  # export
  ggsave(filename = paste0(i, ".tiff"), plot = map.sp, wi = 20, he = 15, 
         un = "cm", dpi = 300, comp = "lzw")
  
  # directory
  setwd("..")
}

# end ---------------------------------------------------------------------