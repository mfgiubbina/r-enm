### script hierarchical anova ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# Thadeu Sobral de Souza - thadeusobral@gmail.com 

###----------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, data.table, vegan)

# verify packages
search()

###----------------------------------------------------------------------------###

# import data
# directory
setwd("E:/mega/_disciplina_enm_unesp_2017/scripts_r/enm/02_ouput")

# enms
# list files
tif <- list.files(patt = ".tif$")
tif

enm <- raster(tif[[1]])
enm
plot(enm)

###----------------------------------------------------------------------------###

## weighted average ensemble 
# lists
# species
sp <- sub("zEval_CCSM_svm_", "", sub(".txt", "", grep("svm", dir(patt = ".txt"), value = T)))
sp

# raster
ens <- enm[[1]]
ens[] <- NA
names(ens) <- "ens"
ens

# anova
dir.create("anova")

for(i in sp){
  tif.sp <- grep(i, tif, value = T)
  
  tif.00k <- grep("00k", tif.sp, value = T)
  enm.00k <- stack(tif.00k)
  da.00k <- data.table(decostand(enm.00k[], method = "stand"))
  
  tif.06k <- grep("06k", tif.sp, value = T)
  enm.06k <- stack(tif.06k)
  da.06k <- data.table(decostand(enm.06k[], method = "stand"))
  
  tif.21k <- grep("21k", tif.sp, value = T)
  enm.21k <- stack(tif.21k)
  da.21k <- data.table(decostand(enm.21k[], method = "stand"))
  
  da <- cbind(da.00k, da.06k, da.21k)
 
  # fatores para anova

  temp <- c(rep("00k", ncol(da.00k)), rep("06k", ncol(da.06k)), rep("21k", ncol(da.21k)))

  algo <- rep(c("bioclim", "gower", "mahalanobis", "maxent", "svm"), 9)
    
  # anova hierarquica

  MSout <- NULL

  for(i in 1: nrow(suit)){
    suit.parcial <- as.numeric(suit[i,])
    modelo.lm <- lm(suit.parcial ~ tempo + enm%in%tempo + aogcm%in%tempo)
    modelo.anova <- anova(modelo.lm)
    MSout <- rbind(MSout, modelo.anova$"Mean Sq")}


  nrow(MSout)
  colnames(MSout) <- c("tempo", "enm_tempo", "aogcm_tempo", "residuo")

  # calculando proporcao de variancia explicada por cada componente
  MSout.sum <- apply(MSout, 1, sum)

  MSout.prop <- MSout/MSout.sum
  MSout.prop <- cbind(suit0k[,1:2], MSout.prop)

  summary(MSout.prop)

  # mapear incerteza

  gridded(MSout.prop) <- ~x+y
  incerteza <- stack(MSout.prop)

  plot(incerteza)

###----------------------------------------------------------------------------###

