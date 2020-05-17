#' ---
#' title: download variables - worldclim 2.1
#' author: mauricio vancine
#' date: 2020-05-09
#' ---

# preparate r -------------------------------------------------------------
# packages
library(rvest)
library(tidyverse)

# directory
path <- "/media/mude/afe69132-ffdb-4892-b809-a0f7d2b8f423/var/bioclimv21"
setwd(path)
dir()

# -------------------------------------------------------------------------
# present
# -------------------------------------------------------------------------

# directory
dir.create("01_present"); setwd("01_present")

# 10 m --------------------------------------------------------------------
# directory
dir.create("10m"); setwd("10m")

# download bioclimates - https://worldclim.org/data/worldclim21.html
url <- "https://worldclim.org/data/worldclim21.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("10m") %>% # choose resolution
  stringr::str_subset("_srad|_wind|_vapr|_bio|_elev") # choose vars
url

destfiles <- "https://worldclim.org/data/worldclim21.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("10m") %>% # choose resolution
  stringr::str_subset("_srad|_wind|_vapr|_bio|_elev") %>% # choose vars
  stringr::str_split(pattern = "[/]", simplify = TRUE) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(8) %>% 
  dplyr::pull()
destfiles

# download
purrr::map2(url, destfiles, download.file, mode = "wb")

# unzip
purrr::map(dir(pattern = ".zip"), unzip)

# 5 m --------------------------------------------------------------------
# directory
setwd(".."); dir.create("5m"); setwd("5m")

# download bioclimates - https://worldclim.org/data/worldclim21.html
url <- "https://worldclim.org/data/worldclim21.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("_5m") %>% # choose resolution
  stringr::str_subset("_srad|_wind|_vapr|_bio|_elev") # choose vars
url

destfiles <- "https://worldclim.org/data/worldclim21.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("_5m") %>% # choose resolution
  stringr::str_subset("_srad|_wind|_vapr|_bio|_elev") %>% # choose vars
  stringr::str_split(pattern = "[/]", simplify = TRUE) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(8) %>% 
  dplyr::pull()
destfiles

# download
purrr::map2(url, destfiles, download.file, mode = "wb")

# unzip
purrr::map(dir(pattern = ".zip"), unzip)

# 2.5 m --------------------------------------------------------------------
# directory
setwd(".."); dir.create("2_5m"); setwd("2_5m")

# download bioclimates - https://worldclim.org/data/worldclim21.html
url <- "https://worldclim.org/data/worldclim21.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("2.5m") %>% # choose resolution
  stringr::str_subset("_srad|_wind|_vapr|_bio|_elev") # choose vars
url

destfiles <- "https://worldclim.org/data/worldclim21.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("2.5m") %>% # choose resolution
  stringr::str_subset("_srad|_wind|_vapr|_bio|_elev") %>% # choose vars
  stringr::str_split(pattern = "[/]", simplify = TRUE) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(8) %>% 
  dplyr::pull()
destfiles

# download
purrr::map2(url, destfiles, download.file, mode = "wb")

# unzip
purrr::map(dir(pattern = ".zip"), unzip)

# 30 s --------------------------------------------------------------------
# directory
setwd(".."); dir.create("30s"); setwd("30s")

# download bioclimates - https://worldclim.org/data/worldclim21.html
url <- "https://worldclim.org/data/worldclim21.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("30s") %>% # choose resolution
  stringr::str_subset("_srad|_wind|_vapr|_bio|_elev") # choose vars
url

destfiles <- "https://worldclim.org/data/worldclim21.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("30s") %>% # choose resolution
  stringr::str_subset("_srad|_wind|_vapr|_bio|_elev") %>% # choose vars
  stringr::str_split(pattern = "[/]", simplify = TRUE) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(8) %>% 
  dplyr::pull()
destfiles

# download
purrr::map2(url, destfiles, download.file, mode = "wb")

# unzip
purrr::map(dir(pattern = ".zip"), unzip)

# end ---------------------------------------------------------------------