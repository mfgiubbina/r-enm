#' ---
#' title: install enm packages
#' author: mauricio vancine
#' date: 2020-05-10
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
if(!require(data.table)) install.packages("data.table")
if(!require(ggsn)) install.packages("ggsn")
if(!require(raster)) install.packages("raster")
if(!require(rgdal)) install.packages("rgdal")
if(!require(rgeos)) install.packages("rgeos")
if(!require(sf)) install.packages("sf")
if(!require(viridis)) install.packages("viridis")

# download
if(!require(rnaturalearth)) install.packages("rnaturalearth")

# selection - correlation
if(!require(corrr)) install.packages("corrr")
if(!require(caret)) install.packages("caret")

# selection - pca
if(!require(factoextra)) install.packages("factoextra")
if(!require(FactoMineR)) install.packages("FactoMineR")
if(!require(RStoolbox)) install.packages("RStoolbox")

# selection - factorial
if(!require(psych)) install.packages("psych")

# algorithms --------------------------------------------------------------
# suppor analysis
if(!require(ecospat)) install.packages("ecospat")
if(!require(ENMTools)) devtools::install_github("danlwarren/ENMTools")
if(!require(ENMeval)) install.packages("ENMeval")

# bioclim, domain, mahalanobis, and brt
if(!require(dismo)) install.packages("dismo")

# svm
if(!require(kernlab)) install.packages("kernlab")

# random forest
if(!require(randomForest)) install.packages("randomForest")

# maxent
if(!require(rJava)) install.packages("rJava") # download java

# end ---------------------------------------------------------------------