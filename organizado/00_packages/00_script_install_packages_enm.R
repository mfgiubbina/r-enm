# script description #
# aim:    install packages to enms
# author: mauricio vancine
# date:   05-05-2018

# occurrences -------------------------------------------------------------
# manipulation and visualization
if(!require(tidyverse)) install.packages("tidyverse")

# occurrences download
if(!require(spocc)) install.packages("spocc")
if(!require(BIEN)) install.packages("BIEN")
if(!require(marinespeed)) install.packages("marinespeed")
if(!require(robis)) install.packages("robis")

# occurrences clear
if(!require(CoordinateCleaner)) install.packages("CoordinateCleaner")

# taxonomy and trait
if(!require(taxize)) install.packages("taxize")
if(!require(worrms)) install.packages("worrms")
if(!require(rfishbase)) install.packages("rfishbase")

# variables ------------------------------------------------------
# manipulation and visualization
if(!require(data.table)) install.packages("data.table")
if(!require(ggsn)) install.packages("ggsn")
if(!require(landscapetools)) install.packages("landscapetools")
if(!require(raster)) install.packages("raster")
if(!require(rgdal)) install.packages("rgdal")
if(!require(rgeos)) install.packages("rgeos")
if(!require(sf)) install.packages("sf")
if(!require(viridis)) install.packages("viridis")

# download
if(!require(rnaturalearth)) install.packages("rnaturalearth")
if(!require(rnaturalearthdata)) install.packages("rnaturalearthdata")
if(!require(rnaturalearthhires)) devtools::install_github("ropensci/rnaturalearthhires")
if(!require(geobr)) devtools::install_github("ipeaGIT/geobr")
if(!require(sdmpredictors)) install.packages("sdmpredictors")

# selection - correlation
if(!require(corrr)) install.packages("corrr")
if(!require(caret)) install.packages("caret")

# selection - vif
if(!require(usdm)) install.packages("usdm")

# selection - pca
if(!require(factoextra)) install.packages("factoextra")
if(!require(FactoMineR)) install.packages("FactoMineR")
if(!require(RStoolbox)) install.packages("RStoolbox")

# selection - factorial
if(!require(psych)) install.packages("psych")

# algorithms --------------------------------------------------------------
# bioclim, domain, mahalanobis, and brt
if(!require(dismo)) install.packages("dismo")
if(!require(omnibus)) devtools::install_github("adamlilith/omnibus")
if(!require(statisfactory)) devtools::install_github("adamlilith/statisfactory")
if(!require(enmSdm)) devtools::install_github("adamlilith/enmSdm")

# gam
if(!require(gam)) install.packages("gam")

# mars
if(!require(earth)) install.packages("earth")

# cta
if(!require(rpart)) install.packages("rpart")

# svm
if(!require(kernlab)) install.packages("kernlab")
if(!require(e1071)) install.packages("e1071")

# random forest
if(!require(randomForest)) install.packages("randomForest")

# maxent
if(!require(rJava)) install.packages("rJava") # download java

# ann
if(!require(nnet)) install.packages("nnet")

# sdm
if(!require(sdm)) install.packages("sdm")

# ssdm
if(!require(SSDM)) install.packages("SSDM")

# biomod2
if(!require(biomod2)) install.packages("biomod2")

# hsdm
# cd ~/Downloads
# wget ftp://ftp.gnu.org/gnu/gsl/gsl-latest.tar.gz
# tar -xvzf gsl-latest.tar.gz
# cd gsl-2.5
# ./configure
# make
# sudo make install

if(!require(hSDM)) install.packages("hSDM") 


# notification sound ------------------------------------------------------
if(!require(beepr)) install.packages("beepr")

# end ---------------------------------------------------------------------