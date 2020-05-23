### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 16/06/2017

###-----------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())

# packages
library(xml2)
library(rvest)
library(tidyverse)

# directory
path <- "/home/mude/Downloads/"
setwd(path)
dir.create("soilgrids"); setwd("soilgrids")

# download ----------------------------------------------------------------
# resolution
re <- c("10km", "5km", "1km", "250m")
re

# links
li <- tibble::tibble(re = c("10km", "5km", "1km", "250m"),
                     link = c("https://files.isric.org/soilgrids/data/aggregated/10km/",
                              "https://files.isric.org/soilgrids/data/aggregated/5km/",
                              "https://files.isric.org/soilgrids/data/aggregated/1km/",
                              "https://files.isric.org/soilgrids/data/recent/"))
li

# download
for(i in seq(re)){
  
  # ftp
  ftp <- li[i, "link"] %>% as.character()
  
  # files
  files <- xml2::read_html(ftp) %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_subset(".tif$")
  
  # directory
  setwd(paste0(path, "soilgrids")); dir.create(re[i]); setwd(re[i])
  
  # download
  for(j in files){
  
    # information
    paste0(re[i], " - ", j)
    
    # download
    download.file(url = paste0(ftp, j), destfile = j)
    
  }

}

###-----------------------------------------------------------------------------###

