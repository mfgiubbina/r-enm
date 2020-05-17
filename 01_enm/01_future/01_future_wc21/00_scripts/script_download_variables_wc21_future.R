#' ---
#' title: download variables - worldclim 2.1
#' author: mauricio vancine
#' date: 2020-05-17
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
# future
# -------------------------------------------------------------------------

# 10 m --------------------------------------------------------------------
# directory
setwd(path); dir.create("02_future"); setwd("02_future"); dir.create("10m"); setwd("10m")

# download future bioclimates - https://worldclim.org/data/cmip6/cmip6_clim10m.html
url_10m <- "https://worldclim.org/data/cmip6/cmip6_clim10m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc_") %>% # choose vars
  stringr::str_subset("") %>% # choose gcms
  stringr::str_subset("") %>% # choose pathways 
  stringr::str_subset("") # choose periods
url_10m

destfiles_10m <- "https://worldclim.org/data/cmip6/cmip6_clim10m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc_") %>% # choose vars
  stringr::str_subset("") %>% # choose gcms
  stringr::str_subset("") %>% # choose pathways 
  stringr::str_subset("") %>% # choose periods
  stringr::str_split(pattern = "[/]", simplify = TRUE) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(9) %>% 
  dplyr::pull()
destfiles_10m

# download
purrr::map2(url_10m, destfiles_10m, purrr::possibly(download.file, otherwise = NA), mode = "wb")

# unzip
purrr::map(dir(pattern = ".zip"), unzip)

# 5 m --------------------------------------------------------------------
# directory
setwd(".."); dir.create("5m"); setwd("5m")

# download future bioclimates - https://worldclim.org/data/cmip6/cmip6_clim5m.html
url_5m <- "https://worldclim.org/data/cmip6/cmip6_clim5m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc_") %>% # choose vars
  stringr::str_subset("") %>% # choose gcms
  stringr::str_subset("") %>% # choose pathways 
  stringr::str_subset("") # choose periods
url_5m

destfiles_5m <- "https://worldclim.org/data/cmip6/cmip6_clim5m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc_") %>% # choose vars
  stringr::str_subset("") %>% # choose gcms
  stringr::str_subset("") %>% # choose pathways 
  stringr::str_subset("") %>% # choose periods
  stringr::str_split(pattern = "[/]", simplify = TRUE) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(9) %>% 
  dplyr::pull()
destfiles_5m

# download
purrr::map2(url_5m, destfiles_5m, purrr::possibly(download.file, otherwise = NA), mode = "wb")

# unzip
purrr::map(dir(pattern = ".zip"), unzip)

# 2.5 m --------------------------------------------------------------------
# directory
setwd(".."); dir.create("2_5m"); setwd("2_5m")

# download future bioclimates - https://worldclim.org/data/cmip6/cmip6_clim2.5m.html
url_2_5m <- "https://worldclim.org/data/cmip6/cmip6_clim2.5m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc_") %>% # choose vars
  stringr::str_subset("") %>% # choose gcms
  stringr::str_subset("") %>% # choose pathways 
  stringr::str_subset("") # choose periods
url_2_5m

destfiles_2_5m <- "https://worldclim.org/data/cmip6/cmip6_clim2.5m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc_") %>% # choose vars
  stringr::str_subset("") %>% # choose gcms
  stringr::str_subset("") %>% # choose pathways 
  stringr::str_subset("") %>% # choose periods
  stringr::str_split(pattern = "[/]", simplify = TRUE) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(9) %>% 
  dplyr::pull()
destfiles_2_5m

# download
purrr::map2(url_2_5m, destfiles_2_5m, purrr::possibly(download.file, otherwise = NA), mode = "wb")

# unzip
purrr::map(dir(pattern = ".zip"), unzip)

# end ---------------------------------------------------------------------