### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 08/02/2017

###-----------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(downloader, rvest)

###-----------------------------------------------------------------------------###
###                                envirem
###-----------------------------------------------------------------------------###

# directory
setwd("D:/environmental_data")
dir.create("envirem_r")
setwd("envirem_r")
getwd()

# list of url
url <- "https://deepblue.lib.umich.edu/data/concern/generic_works/gt54kn05f"
url

pg <- read_html(url)
pg

list <- grep("downloads", as.list(html_attr(html_nodes(pg, "a"), "href")), value = T)
list

url2 <- "https://deepblue.lib.umich.edu/data/concern/file_sets"
url2


# download
for(i in 591:length(list)){
  pg2 <- read_html(paste0(url2, sub("/data/downloads", "", list[[i]])))
  
  no <- pg2 %>%
    html_nodes("h1") %>%
    html_text()
  
  na <- sub("Open Access", "", no[2])
  
  dir.create(paste0(ifelse(i < 9, paste0("00", i), ifelse(i < 99, paste0("0", i), i)), 
                    "_", sub(".zip", "", na)))
  setwd(paste0(ifelse(i < 9, paste0("00", i), ifelse(i < 99, paste0("0", i), i)), 
               "_", sub(".zip", "", na)))
  
  download(paste0("https://deepblue.lib.umich.edu", list[[i]]), na, mode = "wb")
  unzip(na)
  unlink(na)
  
  setwd("..")
  
  }

# end ---------------------------------------------------------------------