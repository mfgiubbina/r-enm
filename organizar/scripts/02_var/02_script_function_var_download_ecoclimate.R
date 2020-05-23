# script description #
# function: variables - download ecoclimate
# author:   mauricio vancine
# date:     06-05-2018


# var_download_ecoclimate -------------------------------------------------

#' var_download_ecoclimate
#' 
#' download ecoclimate layers. more info at www.ecoclimate.org
#' 
#' @usage var_download_ecoclimate(baseline, scenario, variable, aogcm, path, erase_zip_files,
#'                                erase_txt_files, operational_system)
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
#' @param erase_zip_files - erase zip files.
#' Options are: "TRUE", "FALSE"
#' @param erase_txt_files - erase txt files.
#' Options are: "TRUE", "FALSE"
#' @param operational_system - inform the operational system.
#' Options are: "windows", "linux", "macos"
#' 

# function ----------------------------------------------------------------

var_download_ecoclimate <- function(baseline, scenario, variable, aogcm, 
                                    path, erase_zip_files, erase_txt_files, 
                                    operational_system){
  
  # packages
  if(!require(beepr)) install.packages("beepr")
  if(!require(rvest)) install.packages("rvest")
  if(!require(raster)) install.packages("raster")
  if(!require(rgdal)) install.packages("rgdal")
  if(!require(tibble)) install.packages("tidyverse")

  # raster options
  raster::rasterOptions(maxmemory = 1e+10)
  raster::rasterOptions(chunksize = 1e+10)
  
  # links dropbox - ecoclimate
  li_dr <- xml2::read_html("http://ecoclimate.org/downloads/") %>%
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_subset("dropbox")
  
  # link txt
  li_dr_txt <- li_dr[seq(1, length(li_dr), 2)]
  
  # data to download
  da <- tibble::tibble(ba = c(rep(c("pre_industrial", "historical", "modern"), each = 3), rep(c("pre_industrial", "historical", "modern"), each = 12)),
                       sc = c(rep(c("present", "past", "future"), times = 3), rep(rep(c("present", "past", "future"), each = 4), times = 3)), 
                       va = c(rep(c("bioclimate"), times = 9), rep(c("pr", "tas", "tasmax", "tasmin"), times = 9)),
                       ba_di = c(rep(c("00_pre_industrial", "01_historical", "02_modern"), each = 3), rep(c("00_pre_industrial", "01_historical", "02_modern"), each = 12)),
                       sc_di = c(rep(c("00_present", "01_past", "02_future"), times = 3), rep(rep(c("00_present", "01_past", "02_future"), each = 4), times = 3)), 
                       va_di = c(rep(c("00_bioclimate"), times = 9), rep(c("01_pr", "02_tas", "03_tasmax", "04_tasmin"), times = 9)),
                       li = li_dr_txt)
  
  # filter
  da_do <- da %>% 
    dplyr::filter(ba %in% baseline, sc %in% scenario, va %in% variable)
  
  # for to download
  for(i in da_do %>% nrow %>% seq){
    
    # path
    setwd(path)
    
    # directory
    dir.create("ecoclimate", showWarnings = FALSE)
    setwd("ecoclimate")
    
    dir.create(da_do[i, ]$ba_di, showWarnings = FALSE)
    setwd(da_do[i, ]$ba_di)
    
    dir.create(da_do[i, ]$sc_di, showWarnings = FALSE)
    setwd(da_do[i, ]$sc_di)
    
    dir.create(da_do[i, ]$va_di, showWarnings = FALSE)
    setwd(da_do[i, ]$va_di)
    
    # file name
    na <- xml2::read_html(da_do[i, ]$li) %>%
      rvest::html_nodes("title") %>% 
      rvest::html_text() %>% 
      stringr::str_remove("Dropbox - ") %>% 
      stringr::str_remove(" - Simplify your life") %>% 
      stringr::str_c(".zip")
    
    # download
    print(paste0("Download data to '", da_do[i, ]$ba, "' baseline, '", 
                 da_do[i, ]$sc, "' scenario, and '", da_do[i, ]$va, "' variables."))
    
    if(operational_system == "linux"){
      
      # linux
      download.file(url = stringr::str_replace(da_do[i, ]$li, "dl=0", "dl=1"), destfile = na, 
                    method = "wget", extra = "-q --show-progress --progress=bar:force 2>&1")   
    
      } else{
        
        # other operational systems
        download.file(url = stringr::str_replace(da_do[i, ]$li, "dl=0", "dl=1"), 
                      destfile = na, mode = "wb")
    }
    
    # unzip
    print(paste0("Unzip files to '", aogcm, "', AOGCM(s)."))
    unzip(zipfile = dir(patt = ".zip"), 
          files = grep(paste0(aogcm, collapse = "|"), 
                       unzip(dir(patt = ".zip"), list = TRUE)$Name, 
                       value = TRUE))
    
    # for to preparate files
    for(j in dir(patt = ".txt")){
      
      # information
      print(paste0("Creating raster files to ", j))
      
      # import file
      fi <- data.table::fread(j)
      
      # preparate data and directory
      if(da_do[i, ]$va == "bioclimate"){
        
        # preparate data and change colnames
        fi <- fi[, -1]
        colnames(fi) <- c("x", "y", paste0("bio0", 1:9), paste0("bio", 10:19))
        
        # create directory
        di <- stringr::str_split(stringr::str_split(j, "# ", simplify = TRUE)[, 2], "_", simplify = TRUE)[1]
        dir.create(di, showWarnings = FALSE)
        setwd(di)
        
      } else{
        
        # change colnames
        colnames(fi) <- c("x", "y", paste0("pr0", 1:9, colnames(fi)[3:11]), 
                          paste0("pr", 10:12, colnames(fi)[12:14]))  %>% stringr::str_remove_all("\"")
        
        # create directory
        di <- stringr::str_split(stringr::str_split(j, "# ", simplify = TRUE)[, 2], "_", simplify = TRUE)[2]
        dir.create(di, showWarnings = FALSE)
        setwd(di)
        
      }
      
      # transforme in raster
      sp::gridded(fi) <- ~x+y
      ra <- raster::stack(fi)
      crs(ra) <- "+proj=longlat +datum=WGS84"
      
      # names
      na_ra <- stringr::str_split(j, "# ", simplify = TRUE)[, 2] %>% 
        stringr::str_replace(".txt", "") %>% 
        stringr::str_replace("[(]", "_") %>% 
        stringr::str_replace("[)]", "") %>% 
        stringr::str_replace("[-]", "_") %>%
        stringr::str_to_lower()
      
      # export raster
      if(da_do[i, ]$va == "bioclimate"){
          
          # export rasters
          print(paste0("Exporting raster tif to ", na_ra))
          raster::writeRaster(x = ra, 
                              filename = paste0(na_ra, "_", names(ra), ".tif"), 
                              bylayer = TRUE, 
                              options = c("COMPRESS=DEFLATE"), 
                              format = "GTiff", 
                              overwrite = TRUE)
          
        } else{
          
          # export rasters
          print(paste0("Exporting raster tif to ", na_ra))
          raster::writeRaster(x = ra, 
                              filename = paste0(sub("pr_", "", na_ra), "_", names(ra), ".tif"), 
                              bylayer = TRUE, 
                              options = c("COMPRESS=DEFLATE"), 
                              format = "GTiff", 
                              overwrite = TRUE)
          
        } # end if else
        
      # back directory
      setwd("..")
      
    } # end for j
    
    if(erase_zip_files == TRUE){
      
      # erase zip
      dir(pattern = ".zip") %>% unlink
    
      } # end if
    
    if(erase_txt_files == TRUE){
      
      # erase txt
      dir(pattern = ".txt") %>% unlink
    
    } # end if
      
  } # end for i
  
  # notification
  beepr::beep(sample(1:11))
  
} # end function

# end ---------------------------------------------------------------------