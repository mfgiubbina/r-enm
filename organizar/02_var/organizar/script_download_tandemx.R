# -------------------------------------------------------------------------
# download TanDEM-X
# mauricio vancine
# 13-12-2019
# -------------------------------------------------------------------------

# memory
rm(list = ls())

# packages 
library(rvest)

# directory
setwd("/home/mude/Downloads")

# TanDEM-X 50m Forest/Non-Forest Map --------------------------------------
# url
url <- "https://download.geoservice.dlr.de/FNF50/files/"
url

# pages 
url_list <- url %>% 
  xml2::read_html() %>% 
  rvest::html_nodes("a") %>%  
  rvest::html_attr("href") %>%
  stringr::str_subset(pattern = "[=.]", negate = TRUE) %>% 
  paste0(url, .)
url_list

# pages 2
url2_list <- NULL

for(i in url_list){
  
  # info
  print(i)
  
  # pages
  url2_list_temp <- i %>% 
    xml2::read_html() %>% 
    rvest::html_nodes("a") %>%  
    rvest::html_attr("href") %>%
    stringr::str_subset(pattern = "[=.]", negate = TRUE) %>% 
    paste0(i, .)
  url2_list_temp
  
  # bind
  url2_list <- c(url2_list, url2_list_temp)
  
}

url2_list

# pages 3
url3_list <- NULL

for(j in url2_list){
  
  # info
  print(j)
  
  # pages
  url3_list_temp <- j %>% 
    xml2::read_html() %>% 
    rvest::html_nodes("a") %>%  
    rvest::html_attr("href") %>%
    stringr::str_subset(pattern = ".zip") %>% 
    paste0(j, .)
  url3_list_temp
  
  # bind
  url3_list <- c(url3_list, url3_list_temp)
  
}

url3_list

# names
names_url3_list <- stringr::str_split(url3_list, "[/]", simplify = TRUE)[, 8]
names_url3_list

# download
purrr::pmap(list(url3_list, names_url3_list), download.file)

# unzip
purrr::pmap(list(zipfile = dir(pattern = ".zip"), exdir = dir(pattern = ".zip") %>% stringr::str_remove(".zip")), unzip)

# end ---------------------------------------------------------------------