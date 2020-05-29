#' ---
#' title: download soilgrids
#' author: mauricio vancine
#' date: 2020-05-29
#' ---

# prepare r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(xml2)
library(rvest)
library(tidyverse)

# directory
path <- "/home/mude/Downloads/"
setwd(path)
dir.create("soilgrids")
path <- paste0(path, "soilgrids/")
setwd(path)

# download ----------------------------------------------------------------
# resolution
re <- c("10km", "5km", "1km", "250m")
re

# url
url <- "https://files.isric.org/soilgrids/"
url

# information
download.file(url = paste0("https://files.isric.org/soilgrids/", "README.md"), destfile = "README.md", mode = "wb")

# -------------------------------------------------------------------------
# *former*: the previous released soilgrids version from 2017-03-10

# directory
dir.create("former"); setwd("former")

# url
url_former <- "https://files.isric.org/soilgrids/former/2017-03-10/"
url_former

# metadata
download.file(url = paste0(url_former, "README.md"), destfile = "README.md", mode = "wb")

# dir
dir <- xml2::read_html(url_former) %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  tibble::as_tibble() %>% 
  dplyr::slice(3:5) %>% 
  dplyr::pull() %>% 
  stringr::str_replace_all("/", "")
dir

purrr::map(dir, dir.create)

# docs --------------------------------------------------------------------
# directory
setwd("docs")

# files
files <- paste0(url_former, "docs") %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset(".pdf|.PDF")
files

# download
purrr::map2(paste0(url_former, "/docs/", files), files, download.file, mode = "wb")

# data --------------------------------------------------------------------
# directory
setwd(paste0(path, "former")); setwd("data")

# files
files <- paste0(url_former, "data") %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset(".tif")
files

# download
purrr::map2(paste0(url_former, "/data/", files), files, download.file, mode = "wb")


# legends
# directory
dir.create("legends"); setwd("legends")

# files
files <- paste0(url_former, "/data/", "/legends") %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset(".qml|.sld|.png")
files

# download
purrr::map2(paste0(url_former, "/data/", "/legends/", files), files, download.file, mode = "wb")

# aggregated --------------------------------------------------------------
# directory
setwd(paste0(path, "former")); setwd("aggregated")

# dir
dir_ag <- paste0(url_former, "aggregated") %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  tibble::as_tibble() %>% 
  dplyr::slice(2:4) %>% 
  dplyr::pull() %>% 
  stringr::str_replace_all("/", "")
dir_ag

purrr::map(dir_ag, dir.create)

# 10 km
setwd("10km")

files <- paste0(url_former, "/aggregated/", "/10km") %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset(".tif")
files

purrr::map2(paste0(url_former, "/aggregated/", "/10km/", files), files, download.file, mode = "wb")

# 5 km
setwd(paste0(path, "former")); setwd("aggregated"); setwd("5km")

files <- paste0(url_former, "/aggregated/", "/5km") %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset(".tif")
files

purrr::map2(paste0(url_former, "/aggregated/", "/5km/", files), files, download.file, mode = "wb")

# 1 km
setwd(paste0(path, "former")); setwd("aggregated"); setwd("1km")

files <- paste0(url_former, "/aggregated/", "/1km") %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset(".tif")
files

purrr::map2(paste0(url_former, "/aggregated/", "/1km/", files), files, download.file, mode = "wb")

# -------------------------------------------------------------------------
# *recent*: full map collection of the most recent soilgrids release

# directory
setwd(path); dir.create("latest"); setwd("latest")

# url
url_latest <- "https://files.isric.org/soilgrids/latest/"
url_latest

# metadata
download.file(url = paste0(url_latest, "README.md"), destfile = "README.md", mode = "wb")

# dir
dir <- xml2::read_html(url_latest) %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  tibble::as_tibble() %>% 
  dplyr::slice(3:5) %>% 
  dplyr::pull() %>% 
  stringr::str_replace_all("/", "")
dir

purrr::map(dir, dir.create)

# docs --------------------------------------------------------------------
# directory
setwd("docs")

# files
files <- paste0(url_latest, "docs") %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset(".pdf|.PDF")
files

# download
purrr::map2(paste0(url_latest, "/docs/", files), files, download.file, mode = "wb")

# legends -----------------------------------------------------------------
#  directory
setwd(paste0(path, "latest")); setwd("legends")

# files
files <- paste0(url_latest, "/legends") %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset(".qml|.sld")
files

# download
purrr::map2(paste0(url_latest, "/legends/", files), files, download.file, mode = "wb")

# data --------------------------------------------------------------------
# directory
setwd(paste0(path, "latest")); setwd("data")

# metadata
download.file(url = paste0(url_latest, "/data/", "README.md"), destfile = "README.md", mode = "wb")

# dir
dir_data_latest <- paste0(url_latest, "data") %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  tibble::as_tibble() %>% 
  dplyr::slice(3:15) %>% 
  dplyr::pull()
dir_data_latest

purrr::map(dir_data_latest, dir.create)

# download landmask
setwd("landmask")

download.file(url = paste0(url_latest, "data/", dir_data_latest[5], "landmask_SG_052020_COG512.tif"), 
              destfile = "landmask_SG_052020_COG512.tif", mode = "wb")

# download wrb
setwd(paste0(path, "latest")); setwd("data"); setwd("wrb")

# dir
dir_wrb <- paste0(url_latest, "data/wrb") %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("/")
dir_wrb

# files
for(i in dir_wrb[-1]){
  
  # dir
  dir.create(i); setwd(i)
  
  # files
  files_wrb <- paste0(url_latest, "data/wrb/", i) %>% 
    xml2::read_html() %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_subset("geotiff")
  
  # download
  purrr::map2(paste0(url_latest, "data/wrb/", i, files_wrb), files_wrb, download.file, mode = "wb")
  
  # dir
  setwd("..")
  
}

# download
setwd(paste0(path, "latest")); setwd("data")

for(i in dir_data_latest[-c(5, 13)]){
  
  # directory
  setwd(paste0(path, "latest")); setwd("data"); setwd(i)
  
  # dir
  dir_i <- paste0(url_latest, "data/", i) %>% 
    xml2::read_html() %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_subset("/")
  dir_i
  
  # folder
  for(j in dir_i[-1]){
    
    # dir
    setwd(paste0(path, "latest")); setwd("data"); setwd(i); dir.create(j); setwd(j)
    
    # files
    folder_j <- paste0(url_latest, "data/", i, j) %>% 
      xml2::read_html() %>% 
      rvest::html_nodes("a") %>% 
      rvest::html_attr("href") %>% 
      stringr::str_subset("/")
    
    # files
    for(k in folder_j[-1]){
      
      file_k <- paste0(url_latest, "data/", i, j, k) %>% 
        xml2::read_html() %>% 
        rvest::html_nodes("a") %>% 
        rvest::html_attr("href") %>% 
        stringr::str_subset(".tif")
      file_k
      
      if(length(file_k) == 0){
        
      } else{
        
        # download
        purrr::map2(paste0(url_latest, "data/", i, j, k, file_k), file_k, download.file, mode = "wb")  
        
      }
      
    }
    
  }
  
}
  
# end ---------------------------------------------------------------------