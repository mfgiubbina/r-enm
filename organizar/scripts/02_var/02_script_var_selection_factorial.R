### script enm - dismo ###

## variables selection - factorial ##

# mauricio vancine
# 10-03-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(psych)
library(raster)
library(tidyverse)
library(viridis)

# directory
setwd("/media/mauricio/data/gitlab/r-enm/data/var/01_variables")
dir()

# import variables e get data ---------------------------------------------
# list variables
ti <- dir(pattern = ".tif")
ti

# import rasters
var <- raster::stack(ti)
var

plot(var$bio01)

# extract values
var.da <- var %>% 
  raster::values() %>% 
  na.omit
var.da

# verify
head(var.da)
dim(var.da)


# factorial analysis ------------------------------------------------------

# back one directory
setwd("..")

# create directory
dir.create("04_factorial") 
setwd("04_factorial") 

# preliminaries analysis
# kmo e bartlett
KMO(cor(var.da)) # > 0.5
cortest.bartlett(cor(var.da), n = nrow(var.da)) # p < 0.05

# screeplot - number os axis
psych::fa.parallel(var.da, fa = "fa")

# exportar screeplot
png("screeplot_fatorial.tif", wi = 20, he = 15, un = "cm", res = 300)
psych::fa.parallel(var.da, fa = "fa") 
dev.off()

# fatorial
fa.6 <- psych::fa(var.da, nfactors = 6, rotate = "varimax")
fa.6

# explanation
fa.6$Vaccounted

# export
fa.6$Vaccounted %>% 
  abs %>% 
  round(2) %>% 
  write.csv("fa_6_explanation.csv")

# loadings
fa.6$loadings

# export
fa.6$loadings %>% 
  abs %>% 
  round(2) %>% 
  write.csv("fa_6_loadings.csv")

# end ---------------------------------------------------------------------