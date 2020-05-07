### function getdata ecoclimate data ###

# mauricio humberto vancine
# 10-22-2018

# memory
rm(list = ls())

# packages
library(rvest)
library(data.table)
library(raster)
library(tibble)
library(readr)
library(dplyr)
library(stringr)

###---------------------------------------------------------------------------------###

#' ecoclimate_getdata
#' 
#' download ecoclimate layers. more info at www.ecoclimate.org
#' 
#' @usage ecoclimate_getdata(baseline, scenario, variable, aogcm, path, operational_system)
#' 
#' @param baseline - select a baseline for the climatic layers.
#' Options are: "pre_industrial", "historical", "modern"
#' @param scenario - select a temporal scenario. 
#' Options are: "past", "present", "future"
#' @param variable - select the type of variables.
#' Options are: "bioclimate", "pr", "tas", "tasmax", "tasmin"
#' @param aogcm - select the aogcm.
#' Options are: "CCSM", "CNRM", "MIROC", "COSMOS", "FGOALS", "GISS", "IPSL","MRI", "MPI"
#' @param path - inform path.
#' Options is: "your_folder_path/"
#' @param operational_system - inform the operational system.
#' Options are: "windows", "linux", "macos"
#' 

###---------------------------------------------------------------------------------###

## function
ecoclimate_getdata <- function(baseline, scenario, variable, aogcm, path, operational_system){
  
  # packages
  if(!require(rvest)) install.packages("rvest")
  if(!require(data.table)) install.packages("data.table")
  if(!require(raster)) install.packages("raster")
  if(!require(tibble)) install.packages("tibble")
  if(!require(readr)) install.packages("readr")
  if(!require(dplyr)) install.packages("dplyr")
  if(!require(stringr)) install.packages("stringr")
  
  # links dropbox - ecoclimate
  li.dr <- xml2::read_html("http://ecoclimate.org/downloads/") %>%
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_subset("dropbox")
  
  li.dr.txt <- li.dr[seq(1, length(li.dr), 2)]
  
  # data to download
  da <- tibble::tibble(ba = c(rep(c("pre_industrial", "historical", "modern"), each = 3), rep(c("pre_industrial", "historical", "modern"), each = 12)),
                       sc = c(rep(c("present", "past", "future"), times = 3), rep(rep(c("present", "past", "future"), each = 4), times = 3)), 
                       va = c(rep(c("bioclimate"), times = 9), rep(c("pr", "tas", "tasmax", "tasmin"), times = 9)),
                       ba_di = c(rep(c("00_pre_industrial", "01_historical", "02_modern"), each = 3), rep(c("00_pre_industrial", "01_historical", "02_modern"), each = 12)),
                       sc_di = c(rep(c("00_present", "01_past", "02_future"), times = 3), rep(rep(c("00_present", "01_past", "02_future"), each = 4), times = 3)), 
                       va_di = c(rep(c("00_bioclimate"), times = 9), rep(c("01_pr", "02_tas", "03_tasmax", "04_tasmin"), times = 9)),
                       li = li.dr.txt)
  
  # filter
  da.do <- da %>% 
    dplyr::filter(ba %in% baseline, sc %in% scenario, va %in% variable)
  
  # for to download
  for(i in seq(nrow(da.do))){
    
    # path
    setwd(path)
    
    # directory
    dir.create("ecoclimate")
    setwd("ecoclimate")
    
    dir.create(da.do[i, ]$ba_di)
    setwd(da.do[i, ]$ba_di)
    
    dir.create(da.do[i, ]$sc_di)
    setwd(da.do[i, ]$sc_di)
    
    dir.create(da.do[i, ]$va_di)
    setwd(da.do[i, ]$va_di)
    
    # file name
    na <- xml2::read_html(da.do[i, ]$li) %>%
      rvest::html_nodes("title") %>% 
      rvest::html_text() %>% 
      stringr::str_remove("Dropbox - ") %>% 
      stringr::str_remove(" - Simplify your life") %>% 
      stringr::str_c(".zip")
    
    # download
    print(paste0("Download data to '", da.do[i, ]$ba, "' baseline, '", 
                 da.do[i, ]$sc, "' scenario, and '", da.do[i, ]$va, "' variables."))
    
    if(operational_system == "linux"){
      download.file(url = stringr::str_replace(da.do[i, ]$li, "dl=0", "dl=1"), destfile = na, 
                    method = "wget", extra = "-q --show-progress --progress=bar:force 2>&1")   
    } else{
      download.file(url = stringr::str_replace(da.do[i, ]$li, "dl=0", "dl=1"), destfile = na, mode = "wb")
    }
    
    # unzip
    print(c("Unzip files to '", aogcm, "', AOGCM(s)."))
    unzip(zipfile = dir(patt = ".zip"), 
          files = grep(paste0(aogcm, collapse = "|"), unzip(dir(patt = ".zip"), list = TRUE)$Name, value = TRUE))
    
    # for to preparate files
    for(j in dir(patt = ".txt")){
      
      # information
      print(paste0("Creating raster files to ", j))
      
      # files
      fi <- data.table::fread(j)
      
      if(da.do[i, ]$va == "bioclimate"){
        
        # preparate data and change colnames
        fi <- fi[, -1]
        colnames(fi) <- c("x", "y", paste0("bio0", 1:9), paste0("bio", 10:19))
        
        # create directory
        di <- stringr::str_split(stringr::str_split(j, "# ", simplify = TRUE)[, 2], "_", simplify = TRUE)[1]
        dir.create(di)
        setwd(di)
        
      } else{
        
        # change colnames
        colnames(fi) <- c("x", "y", paste0("0", 1:9, "_", colnames(fi)[3:11]), paste0(10:12, "_", colnames(fi)[12:14]))
        
        # create directory
        di <- stringr::str_split(stringr::str_split(j, "# ", simplify = TRUE)[, 2], "_", simplify = TRUE)[2]
        dir.create(di)
        setwd(di)
      }
      
      # transforme in raster
      sp::gridded(fi) <- ~x+y
      ra <- raster::stack(fi)
      crs(ra) <- "+proj=longlat +datum=WGS84"
      
      # names
      na.ra <- sub(".txt", "", stringr::str_split(j, "# ", simplify = TRUE)[, 2])
      
      # export raster
      for(k in 1:length(names(ra))){
        
        if(da.do[i, ]$va == "bioclimate"){
          print(paste0("Exporting raster tif to ", paste0(na.ra, "_", names(ra[[k]]), ".tif")))
          raster::writeRaster(ra[[k]], paste0(na.ra, "_", names(ra[[k]]), ".tif"))
          
        } else{
          print(paste0("Exporting raster tif to ", paste0(na.ra, sub("X", "_", names(ra[[k]]), ".tif"))))
          raster::writeRaster(ra[[k]], paste0(na.ra, sub("X", "_", names(ra[[k]])), ".tif"))
          
        } # end if else
        
      } # end for k
      
      # change directory
      setwd("..")
      
    } # end for j
    
  } # end for i
  
} # end function

###---------------------------------------------------------------------------------###

# usage
# ecoclimate_getdata(baseline = c("pre_industrial", "historical", "modern"),
#                    scenario = c("past", "present", "future"), 
#                    variable = c("bioclimate", "pr", "tas", "tasmax", "tasmin"), 
#                    aogcm = c("CCSM", "CNRM", "MIROC", "COSMOS", "FGOALS", "GISS", "IPSL","MRI", "MPI"), 
#                    path = "/home/mauricio/Desktop",
#                    operational_system = "linux")

###---------------------------------------------------------------------------------###
