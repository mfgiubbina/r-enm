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
path <- "/home/mude/data/github/r-enm/01_enm/01_future/01_variables"
setwd(path)
dir()

# 10 m --------------------------------------------------------------------
# download future bioclimates - https://worldclim.org/data/cmip6/cmip6_clim10m.html
url <- "https://worldclim.org/data/cmip6/cmip6_clim10m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc") %>% # choose vars
  stringr::str_subset("BCC-CSM2-MR|CNRM-CM6-1") %>% # choose gcms
  stringr::str_subset("ssp585") %>% # choose pathways 
  stringr::str_subset("2061-2080|2081-2100") # choose periods
url

destfiles <- "https://worldclim.org/data/cmip6/cmip6_clim10m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc") %>% # choose vars
  stringr::str_subset("BCC-CSM2-MR|CNRM-CM6-1") %>% # choose gcms
  stringr::str_subset("ssp585") %>% # choose pathways 
  stringr::str_subset("2061-2080|2081-2100") %>% # choose periods
  stringr::str_split(pattern = "[/]", simplify = TRUE) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(9) %>% 
  dplyr::pull()
destfiles

# download
purrr::map2(url, destfiles, download.file)

# unzip
purrr::map(dir(pattern = ".zip"), unzip)

# 5 m --------------------------------------------------------------------
# download future bioclimates - https://worldclim.org/data/cmip6/cmip6_clim5m.html
url <- "https://worldclim.org/data/cmip6/cmip6_clim5m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc") %>% # choose vars
  stringr::str_subset("BCC-CSM2-MR|CNRM-CM6-1") %>% # choose gcms
  stringr::str_subset("ssp585") %>% # choose pathways 
  stringr::str_subset("2061-2080|2081-2100") # choose periods
url

destfiles <- "https://worldclim.org/data/cmip6/cmip6_clim5m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc") %>% # choose vars
  stringr::str_subset("BCC-CSM2-MR|CNRM-CM6-1") %>% # choose gcms
  stringr::str_subset("ssp585") %>% # choose pathways 
  stringr::str_subset("2061-2080|2081-2100") %>% # choose periods
  stringr::str_split(pattern = "[/]", simplify = TRUE) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(9) %>% 
  dplyr::pull()
destfiles

# download
purrr::map2(url, destfiles, download.file)

# unzip
purrr::map(dir(pattern = ".zip"), unzip)

# 2.5 m --------------------------------------------------------------------
# download future bioclimates - https://worldclim.org/data/cmip6/cmip6_clim2.5m.html
url <- "https://worldclim.org/data/cmip6/cmip6_clim2.5m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc") %>% # choose vars
  stringr::str_subset("BCC-CSM2-MR|CNRM-CM6-1") %>% # choose gcms
  stringr::str_subset("ssp585") %>% # choose pathways 
  stringr::str_subset("2061-2080|2081-2100") # choose periods
url

destfiles <- "https://worldclim.org/data/cmip6/cmip6_clim2.5m.html" %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset("bioc") %>% # choose vars
  stringr::str_subset("BCC-CSM2-MR|CNRM-CM6-1") %>% # choose gcms
  stringr::str_subset("ssp585") %>% # choose pathways 
  stringr::str_subset("2061-2080|2081-2100") %>% # choose periods
  stringr::str_split(pattern = "[/]", simplify = TRUE) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(9) %>% 
  dplyr::pull()
destfiles

# download
purrr::map2(url, destfiles, download.file)

# unzip
purrr::map(dir(pattern = ".zip"), unzip)

# end ---------------------------------------------------------------------