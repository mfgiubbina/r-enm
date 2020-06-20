#' ---
#' title: install enm packages
#' author: mauricio vancine
#' date: 2020-06-19
#' ---

# occurrences -------------------------------------------------------------
# manipulation and visualization
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(progress)) install.packages("progress")

# occurrences download
if(!require(spocc)) install.packages("spocc")

# occurrences clear
if(!require(CoordinateCleaner)) install.packages("CoordinateCleaner")

# taxonomy and trait
if(!require(taxize)) install.packages("taxize")

# variables ------------------------------------------------------
# download
if(!require(rnaturalearth)) install.packages("rnaturalearth")
if(!require(rvest)) install.packages("rvest")

# manipulation and visualization
if(!require(ggsn)) install.packages("ggsn")
if(!require(raster)) install.packages("raster")
if(!require(rgdal)) install.packages("rgdal")
if(!require(rgeos)) install.packages("rgeos")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(sf)) install.packages("sf")
if(!require(wesanderson)) devtools::install_github("karthik/wesanderson")

# selection - correlation
if(!require(corrr)) install.packages("corrr")
if(!require(caret)) install.packages("caret")

# algorithms --------------------------------------------------------------
# support analysis
if(!require(ecospat)) install.packages("ecospat")

# bioclim, domain, mahalanobis, and brt
if(!require(dismo)) install.packages("dismo")

# svm
if(!require(kernlab)) install.packages("e1071")

# random forest
if(!require(randomForest)) install.packages("randomForest")

# maxent
#' download java
#' windows: https://www.java.com/
#' linux: sudo apt install -y default-jre default-jdk

if(!require(rJava)) install.packages("rJava") 
# end ---------------------------------------------------------------------