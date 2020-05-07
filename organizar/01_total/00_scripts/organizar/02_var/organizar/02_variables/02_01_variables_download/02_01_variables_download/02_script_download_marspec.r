### script function download marspec ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 2018-17-11

###------------------------------------------------------------------------------###

# function
marspec_download <- function(scenario, resolution, variable, path){
  
  # packages
  if(!require(rvest)) install.packages("rvest")
  if(!require(devtools)) install.packages("devtools")
  if(!require(archive)) devtools::install_github("jimhester/archive")
  if(!require(tidyverse)) install.packages("tidyverse")
  
  # directory
  setwd(path)
  dir.create("marspec", showWarnings = FALSE)
  setwd("marspec")
  
  # url
  url.mo <- "http://marspec.weebly.com/modern-data.html"
  url.pa <- "http://marspec.weebly.com/paleo-data.html"
  
  # links modern
  li.mo <- xml2::read_html(url.mo) %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_subset("drive")
  
  # links paleo
  li.pa <- xml2::read_html(url.pa) %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_subset(".7z")
  
  # data
  da <- tibble::tibble(
    ce = c(rep("modern", length(li.mo)), rep("paleo", length(li.pa))),
    # ya = c(rep("0kya", 90), c("21kya", "6kya", "21kya", "21kya", "6kya", "21kya", "6kya", 
    #                           "6kya", "21kya", "21kya", "21kya", "6kya", "21kya", "6kya", 
    #                           "6kya", "6kya", "21kya", "21kya")),
    re = c(rep(c("30s", "2_5m", "5m", "10m", "kmz"), 18), rep("5m", 18)),
    na = c(paste0(rep(c("bathymetry", paste0("biogeo0", 1:9), paste0("biogeo", 10:17)), each = 5),
                  c("_30s.tif", "_025m.tif", "_05m.tif", "_10m.tif", ".kmz")),
           paste0(c("21kya_bathymetry_geophysical_layers", "6kya_ccsm3", "21kya_ccsm3", 
                    "21kya_cnrm_cm33", "6kya_csiro_mk3l_1_1", "21kya_ecbilt_clio", 
                    "6kya_ecbilt_clio_vecode", "6kya_fgoals_1_0g", "21kya_fgoals_1_0g", 
                    "21kya_foam", "21kya_hadcm3m2", "6kya_miroc3_2", "21kya_miroc3_2_2", 
                    "6kya_mri_cgcm2_3_4fa", "6kya_mri_cgcm2_3_4nfa", "6kya_mh_ensemble", 
                    "21kya_lgm_ensemble", "21kya_lgm_ensemble"), ".7z")),
    li = c(li.mo, li.pa))
  
  # filter
  da.do <- da %>% 
    dplyr::filter(stringr::str_detect(ce, paste0(scenario, collapse = "|")),
                  #stringr::str_detect(ya, paste0(year, collapse = "|")),
                  stringr::str_detect(re, paste0(resolution, collapse = "|")),
                  stringr::str_detect(na, paste0(variable, collapse = "|")))
  
  # bug
  if(resolution == "5m") da.do <- da.do[grep("2_5m", da.do$re, invert = TRUE), ]
  
  # download
  for(i in unique(da.do$ce)){
    
    # path1
    dir.create(i, showWarnings = FALSE)
    setwd(i)
    
    
    for(j in unique(da.do$re)){
      
      # path2
      dir.create(j, showWarnings = FALSE)
      setwd(j)
      
      # link
      down <- da.do %>% 
        dplyr::filter(i %in% ce, j %in% re) %>% 
        dplyr::select(na, li)
      
      # information
      print(paste0("Download Marspec to '", i, "' scenario, with resolution ", j, "."))
      
      # download
      for(k in nrow(down)){
        
        download.file(url = down$li[k], destfile = down$na[k], method = "auto", mode = "wb")
        
        # preparate paleo data
        if(scenario == "paleo"){
        
          # unzip
          archive::archive_extract(down$na[k])
          
          # erase zip
          unlink(down$na[k])
          
          # dir
          setwd(dir())
          
          # import
          ra <- raster::stack(dir(patt = "dblbnd.adf", recursive = TRUE))
          names(ra) <- dir(patt = "5m")
          
          # export
          for(l in seq(names(ra))){
            
            raster::writeRaster(ra[[l]], paste0(names(ra[[l]]), ".tif"), options = "COMPRESS=DEFLATE")
            
          }
          
          # erase grids
          unlink(grep(".tif$", dir(), invert = TRUE, value = TRUE), recursive = TRUE)
          
        }
        
        # back dir 3
        setwd("..")
        
      }
      
      # back dir2
      setwd("..")
      
    }
    
    # back dir1
    setwd("..")
    
  }
  
}

###------------------------------------------------------------------------------###

# usage
marspec_download(scenario = "paleo", 
                 resolution = "", 
                 variable = "",
                 path = "/home/mauricio/Desktop")

###------------------------------------------------------------------------------###
