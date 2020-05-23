download_mapbiomas <- function(biome, year, dir){
  
  # packages
  if(!require(tidyverse)) install.packages("tidyverse")
  
  # directory
  setwd(dir)
  
  # biome
  biome <- stringr::str_to_upper(biome)
  
  # list to download
  list_download <- tibble::tibble(
    url = paste0(rep("https://storage.cloud.google.com/mapbiomas-public/COLECAO/4_1/DOWNLOADS/COLECOES/ANUAL/", length(biome) * length(year)),
                  rep(biome, each = length(year)), 
                  "/", 
                  rep(biome, each = length(year)),
                  "-",
                  rep(year, times = length(biome)),
                  ".tif"),
    destfile = paste0(rep("COLECAO_4_1_DOWNLOADS_COLECOES_ANUAL_", length(biome) * length(year)),
                  rep(biome, each = length(year)), 
                  "_", 
                  rep(biome, each = length(year)),
                  "-",
                  rep(year, times = length(biome)),
                  ".tif")
    ) %>% as.list
  
  # download
  purrr::pmap(list_download, download.file)
  
}

download_mapbiomas(biome = "MATAATLANTICA",
                   year = 2018,
                   dir = "/home/mude/Downloads")
