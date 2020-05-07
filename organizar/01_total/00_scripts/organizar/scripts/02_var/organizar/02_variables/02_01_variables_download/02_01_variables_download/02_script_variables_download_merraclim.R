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
###                                 merraclim
###-----------------------------------------------------------------------------###

# directory
setwd("D:/environmental_data")
dir.create("merraclim")
setwd("merraclim")
getwd()

# list of url
url <- "http://datadryad.org/resource/doi:10.5061/dryad.s2v81"
url

pg <- read_html(url)
pg

list <- grep(".zip", html_attr(html_nodes(pg, "a"), "href"), value = T)
list

# download
for(i in list){

  dir.create(sub(".zip", "", strsplit(strsplit(i, "[?]")[[1]][1], "[/]")[[1]][6]))
  setwd(sub(".zip", "", strsplit(strsplit(i, "[?]")[[1]][1], "[/]")[[1]][6]))

  download(paste0("http://datadryad.org/", i), strsplit(strsplit(i, "[?]")[[1]][1], "[/]")[[1]][6], mode = "wb")
  unzip(strsplit(strsplit(i, "[?]")[[1]][1], "[/]")[[1]][6])
  unlink(strsplit(strsplit(i, "[?]")[[1]][1], "[/]")[[1]][6])

  setwd("..")}

###------------------------------------------------------------------------------###

