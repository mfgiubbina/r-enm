library(dismo)
library(maps)
library(raster)
library(rJava)
library(tidyverse)

guara <- gbif("Chrysocyon", "brachyurus" , download=T, geo=T)
guara

plot(guara$lon,guara$lat,col="red",pch=19,xlab="Longitude",ylab="Latitude")
map(add=T)

guara<-guara[which(guara$lon > -90 & guara$lon < -25),] 
guara<-guara[which(guara$lat > -45 & guara$lat < 0),]

plot(guara$lon,guara$lat,xlim=c(-90,-25),ylim=c(-45,0),col="red",pch=19,xlab="Longitude",ylab="Latitude")
map(add=T)

my0 = getData("GADM", country="BRA", level=0)
plot(my0)

climate=getData("worldclim", var="bio",res=2.5)
climate

plot(climate$bio1, main="Annual Mean Temperature") 

guarapoints=guara[,c("lon","lat")]

# na
guarapoints_na <- na.omit(guarapoints)

# duplicated
dups <- duplicated(guarapoints)
dups

guarapoints_dups <- guarapoints_na[!dups, ]
guarapoints_dups

guarapoints_dups %>% 
  dplyr::mutate(sp = "guara") %>% 
  dplyr::select(sp, lon, lat) %>% 
  readr::write_csv("occ.csv")

group_p <- kfold(guarapoints_dups,5)
group_p

ext = extent(-90,-25,-45,0)
southamerworldclim=crop(climate,ext)
plot(southamerworldclim)

raster::writeRaster(x = southamerworldclim, 
                    filename = names(southamerworldclim),
                    format = "ascii",
                    bylayer = TRUE,
                    progress = "text")


backg = randomPoints(southamerworldclim, n=nrow(guarapoints_dups), ext=ext)
colnames(backg) <- c("lon","lat")

group_b=kfold(backg, 5)
group_b

futbio<-getData("CMIP5", var="bio", res=2.5, rcp=45, model="MI", year=70)
names(futbio)<-names(climate)

sdm <- raster::stack()
sdm

sdm_f <- raster::stack()
sdm_f

for(i in 1:5){
  
  print(i)
  
  pres_train <- guarapoints_dups[group_p!=i,]
  pres_test <- guarapoints_dups[group_p==i,]

  xm <- maxent(southamerworldclim, pres_train)
  
  backg_train <- backg[group_b!=i,]
  backg_test <- backg[group_b==i,]
  
  e = evaluate(pres_test, backg_test, xm, southamerworldclim)
  
  p <- predict(southamerworldclim, xm, ext=ext, progress="text")
  
  sdm <- stack(sdm, p)
  
  plot(p, main="Maxent, raw values – Present")
  
  pfut <- predict(futbio, xm, ext=ext, progress="text")
  
  sdm_f <- stack(sdm_f, pfut)
  
  plot(p, main="Maxent, raw values – Present")
  
}

plot(sdm)
plot(sdm_f)

# media
sdm_mean <- mean(sdm)
sdm_mean
plot(sdm_mean)

sdm_sd <- calc(sdm, fun = sd)
sdm_sd
plot(sdm_sd)

sdm_medio_sd <- raster::stack(sdm_mean, sdm_sd)
sdm_medio_sd

plot(sdm_medio_sd)

raster::plot(sdm_mean, sdm_sd, maxpixels = 1e6, xlab = "Adequabilidade média", ylab = "Desvio padrão")

sdm_f_mean <- mean(sdm_f)
sdm_f_mean
plot(sdm_f_mean)

sdm_f_sd <- calc(sdm_f, fun = sd)
sdm_f_sd
plot(sdm_f_sd)

raster::plot(sdm_f_mean, sdm_f_sd, maxpixels = raster::ncell(sdm_f), xlab = "Adequabilidade média", ylab = "Desvio padrão")
