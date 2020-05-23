### script ensemble average ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 17/06/2017

###---------------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, data.table)

# verify packages
search()

###----------------------------------------------------------------------------###

# import data
# directory
setwd("E:/mega/_disciplina_enm_unesp_2017/scripts_r/enm/02_ouput")

# enms
# list files
tif <- dir(patt = ".tif$")
tif

enm <- raster(tif[[1]])
enm
plot(enm)

###----------------------------------------------------------------------------###

## average ensemble 

# lists
# species
sp <- sub("zEval_CCSM_svm_", "", sub(".txt", "", grep("svm", dir(patt = ".txt"), value = T)))
sp

# gcms
gc <- c("CCSM")
gc

# periods
pe <- c("00k", "06k", "21k")
pe

# algorithms
al <- c("bioclim", "gower", "mahalanobis", "maxent", "svm")
al

# replicates
re <- 1:5
re

# variables
da <- data.table()
da

ens <- enm[[1]]
ens[] <- NA
names(ens) <- "ens"
ens

# directory
dir.create("ensemble_aver")

# ensembles
for(i in sp){		
  tif.sp <- grep(i, tif, value = T)
  
  for(j in gc){		
    tif.gc <- grep(j, tif.sp, value = T)
    
    for(k in pe){		
      tif.pe <- grep(k, tif.gc, value = T)
      
      for(l in al){		
        tif.al <- grep(l, tif.pe, value = T)
        
        enm.al <- stack(tif.al)
        da <- data.table(enm.al[])
        
        ens[] <- apply(da, 1, mean)
        
        setwd("ensemble_aver")
        writeRaster(ens, paste0("ensemble_aver_", i, "_", j, "_", k, "_", l, ".tif"), 
                    format = "GTiff")
        setwd("..")
        
        da <- data.table()
        ens[] <- NA}}}}

###-----------------------------------------------------------------------------------------###

### hierarchical anova

# directory
setwd("ensemble_aver")

# list files
tif.ave <- dir(patt = ".tif$")
tif.ave

enm.ave <- stack(tif.ave)

# anova
# valores 
da <- data.table(decostand(na.omit(enm.ave[]), method = "stand"))
da
  
# fatores para anova
temp <- rep(c("00k", "06k", "21k"), each = 5)
temp  

algo <- rep(c("bioclim", "gower", "mahalanobis", "maxent", "svm"), 3)
algo
  
# anova hierarquica
sq <- NULL
  
for(j in 1:nrow(da)){
  da.parcial <- as.numeric(da[j, ])
  modelo.lm <- lm(da.parcial ~ algo + temp)
  modelo.anova <- anova(modelo.lm)
  sq <- rbind(sq, modelo.anova$"Mean Sq")}
  
colnames(sq) <- c("algoritmo", "temporal", "residuo")
  
# calculando proporcao de variancia explicada por cada componente
sq.sum <- apply(sq, 1, sum)
  
sq.prop <- sq / sq.sum


# adicionando coordenadas
co <- cbind(xyFromCell(enm, 1:ncell(enm)), enm[])
co.na <- na.omit(co)
  
sq.prop.c <- data.frame(co.na[, 1:2], sq.prop)

summary(sq.prop.c)
head(sq.prop.c)
  
# mapear incerteza
gridded(sq.prop.c) <- ~x+y
incerteza <- stack(sq.prop.c)

plot(incerteza)
  
###----------------------------------------------------------------------------###
  
