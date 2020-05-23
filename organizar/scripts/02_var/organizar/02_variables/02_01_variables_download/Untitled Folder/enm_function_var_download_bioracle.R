### enm package - function download bioracle ###

# mauricio humberto vancine - mauricio.vancine@gmail.com
# 15-12-2018

###------------------------------------------------------------------------------###

# function
marspec_download <- function(scenario, resolution, path){
  
  # information
  print("Installing and loading packages.....")
  
  # packages
  if(!require(rvest)) install.packages("rvest")
  if(!require(tidyverse)) install.packages("tidyverse")
  
  # directory
  setwd(path)
  dir.create("marspec", showWarnings = FALSE)
  setwd("marspec")
  
  # url
  url.mo <- "http://www.esapubs.org/archive/ecol/E094/086"
  url.pa <- "http://www.esapubs.org/archive/ecol/E095/149/"
  
  # information
  print("Reading pages.....")
  
  # links modern
  li.mo <- xml2::read_html(url.mo) %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_subset(".7z")
  
  # links paleo
  li.pa <- xml2::read_html(url.pa) %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_subset(".7z")
  
  # data
  da <- tibble::tibble(
    ce = c(rep("modern", length(li.mo)), rep("paleo", length(li.pa))),
    re = c(rep("30s", 5), "2_5m", "5m", "10m", rep("5m", 18)),
    na = c("bathymetry_30s.7z", "biogeo01_07_30s.7z", "biogeo08_17_30s.7z", "Monthly_Variables_30s.7z", 
           "Sea_Ice_30s.7z", "MARSPEC_2o5m.7z", "MARSPEC_5m.7z", "MARSPEC_10m.7z", li.pa),
    li = c(paste0(url.mo, "/", li.mo), paste0(url.mo, "/", li.pa)))
  
  # filter
  da.do <- da %>% 
    dplyr::filter(stringr::str_detect(ce, paste0(scenario, collapse = "|")),
                  stringr::str_detect(re, paste0(resolution, collapse = "|")))
  
  # bug
  if(resolution == "5m") da.do <- da.do[grep("2_5m", da.do$re, invert = TRUE), ]
  
  # download
  for(i in seq(nrow(da.do))){
    
    # path1
    dir.create(da.do$ce[i], showWarnings = FALSE)
    setwd(da.do$ce[i])
    
    # metadata
    xml2::read_html(paste0(url.mo, "/", "metadata.php")) %>% 
      rvest::html_text()
    
    # path2
    dir.create(da.do$re[i], showWarnings = FALSE)
    setwd(da.do$re[i])
    
    # information
    print(paste0("Getdata from Marspec '", da.do$na[i], "' to '", da.do$ce[i], "' scenario, with resolution ", da.do$re[i], "."))
    
    # downalod
    download.file(url = paste0(da.do$li[i]), destfile = da.do$na[i], method = "auto", mode = "wb")
    
    
    
    ## converting data
    
    # information
    print("Converting paleo data....")
    
    # unzip
    archive::archive_extract(da.do$na[i])
    
    # erase zip
    unlink(da.do$na[i])
    
    # dir
    setwd(dir())
    
    # import
    ra <- raster::stack(dir(patt = "dblbnd.adf", recursive = TRUE))
    names(ra) <- dir(patt = "5m$")
    
    # export
    for(j in seq(names(ra))){
      
      # information
      print(paste0("....exporting '", names(ra)[j], ".tif'"))
      
      # export
      raster::writeRaster(ra[[j]], paste0(names(ra[[j]]), ".tif"), options = "TFW=TRUE,UCOMPRESS=DEFLATE")
      
    }
    
    # erase grids
    unlink(grep(".tif$", dir(), invert = TRUE, value = TRUE), recursive = TRUE)
    
    # back dir 3
    setwd("..")
    
  }
  
  # back dir2
  setwd("..")
  
  # back dir1
  setwd("..")
  
}

}

###------------------------------------------------------------------------------###

# usage
marspec_download(scenario = "", 
                 resolution = "", 
                 variable = "",
                 path = "/media/mauricio/data/004_manuscritos/02_in_prep/asiel")

scenario = "";
resolution = "";
variable = "";
path = "/media/mauricio/data/004_manuscritos/02_in_prep/asiel"

###------------------------------------------------------------------------------###