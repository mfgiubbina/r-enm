#' ---
#' title: install enm packages
#' author: mauricio vancine
#' date: 2020-05-15
#' ---

# occurrences -------------------------------------------------------------
# manipulation and visualization
if(!require(tidyverse)) install.packages("tidyverse")

# occurrences download
if(!require(spocc)) install.packages("spocc")

# occurrences clear
if(!require(CoordinateCleaner)) install.packages("CoordinateCleaner")

# taxonomy and trait
if(!require(taxize)) install.packages("taxize")

# variables ------------------------------------------------------
# manipulation and visualization
if(!require(ggsn)) install.packages("ggsn")
if(!require(raster)) install.packages("raster")
if(!require(rgdal)) install.packages("rgdal")
if(!require(rgeos)) install.packages("rgeos")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(sf)) install.packages("sf")

# download
if(!require(rnaturalearth)) install.packages("rnaturalearth")

# selection - correlation
if(!require(corrr)) install.packages("corrr")
if(!require(caret)) install.packages("caret")

# algorithms --------------------------------------------------------------
# suppor analysis
if(!require(ecospat)) install.packages("ecospat")

# bioclim, domain, mahalanobis, and brt
if(!require(dismo)) install.packages("dismo")

# svm
if(!require(kernlab)) install.packages("e1071")

# random forest
if(!require(randomForest)) install.packages("randomForest")

# maxent
if(!require(rJava)) install.packages("rJava") # download java

# end ---------------------------------------------------------------------