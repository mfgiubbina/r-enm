### script enm ###

# Maur?cio Humberto Vancine - mauricio.vancine@gmail.com
# 05/06/2017

# adapted of Bruno Vilela - http://rpubs.com/Bruno_Vilela/279257

###---------------------------------------------------------------------------###
# memory
rm(list = ls())

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, dismo, vegan, viridis, colorRamps)

# verify packages
search()

###---------------------------------------------------------------------------###

## data
# occurrences
occurence <- paste(system.file(package = "dismo"), "/ex/bradypus.csv", sep = "")
occ <- read.table(occurence, h = T, sep = ",")[, -1]
occ

plot(occ$lon, occ$lat, pch = 20)

# variables
envnames <- list.files(path = paste(system.file(package = "dismo"), "/ex", sep = ""), 
           pattern = "grd", full.names = T)

env <- stack(envnames[1:8])
env

names(env) <- paste0("bio", c("01", "05", "06", "07", "08", "12", "16", "17"))

plot(env)

# verify
plot(env[[1]], col = viridis(100))
points(occ$lon, occ$lat, pch = 20)


###---------------------------------------------------------------------------###

## function to calculate enviromental euclidean distance - Bruno Vilela (bvilela@wustl.edu)


## parameters
 ## 1. occ = two column matrix or a data.frame with the first column being the 
 # longitude and the second the latitude

 ## 2. env = raster with the enviromental variables, croped to the projection area
 
 ## 3. method = "mean" distance to all points is calculated, if 
 #              "min" minimun distance to any point is calculated

 ## 4. decostand.method = method applied to standardize the enviromental data
 # see the vegan funtion decostand, argument method for others transformations
 
 ## 5. suitability = TRUE return the suitability
 #                   FALSE return the distance calculated


## function
euclidean <- function (occ, env, method = "mean", 
           decostand.method = "standardize",
           suitability = FALSE) {

 if (class(env) != "raster" & class(env) != "RasterStack"
   & class(env) != "RasterBrick") {
  stop("env has to be a raster")
 }
 if (class(occ) != "matrix" & class(occ) != "data.frame") {
  stop("occ has to be a matrix or data.frame")
 }
 if (ncol(occ) != 2) {
  stop("occ has to be a matrix or data.frame of 2 columns (x and y)")
 }
  
 values <- values(env)
 values <- apply(values, 2, decostand, method = decostand.method,
         na.rm = T)
 values(env) <- values
 values_occ <- raster::extract(env, occ)
 pos <- is.na(values[, 1])
 values2 <- values[!pos, ]
 n <- nrow(values2)
 eu <- numeric(n)
 values_occ <- na.omit(values_occ)
 
 for (i in 1:n) {
  for (j in 1:ncol(values_occ)) {
   temp <- eu[i] + ((values2[i, j] - values_occ[, j]) ^ 2)
   if (method == "mean") {
    eu[i] <- mean(temp, na.rm = T)
   }
   if (method == "min") {
    eu[i] <- min(temp, na.rm = T) 
   }
  }
  eu[i] <- sqrt(eu[i])
 }
 env <- raster(env, 1)
 if (!suitability) {
  values(env)[!pos] <- eu
  return(env)
 } 
 if (suitability) {
  values(env)[!pos] <- decostand(-eu, method = "range", na.rm = T)
  return(env)
 }
}

###---------------------------------------------------------------------------###

# use
enm.euc <- euclidean(occ, env, method = "mean", suitability = T,
           decostand.method = "standardize")

plot(enm.euc, col = matlab.like2(100))
points(occ$lon, occ$lat, pch = 20)

###---------------------------------------------------------------------------###
 