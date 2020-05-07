### Sales et al 2019
# Climate change effects on frugivores of Euterpe edulis
# In this script, we will model only the frugivores potential distribution and project them into different time-steps

#devtools::install_github('babaknaimi/sdm') # to install the latest version of the sdm package from github
#install.packages("installr")
#library(installr)
#updateR()

#install.packages("sdm", dep=T)
library(sdm)
#installAll()

library(raster)
library(usdm)
library(rgdal)
library(rgeos)

#-----
#### Bioclim data for the present time

pres <- stack("D:/lilian/Climaticos/pres.tif")
proj4string(pres) <- projection(raster())

plot(pres[[1]])

name <- NULL
for (i in 1:19){ 
  tmp <- paste0("bio", i)
  name <- c(name, tmp)
}

names(pres) <- name

# Gridded soil data
setwd("D:/lilian/Soil")
soil <- list.files(pattern=".tif")

soil <- stack(soil)

soil <- resample(soil, pres) # Standardize raster resol and extent

plot(soil)
#-------- Palm species occurrences

sp1 <- read.csv("file:///D:/lilian/Ocorr.csv")

sp_name <- as.character(unique(sp1$sp))

evals <- matrix(nrow = length(sp_name), ncol = 6)
colnames(evals) <- c("sp_name","records","AUC","COR","Deviance","TSS")

vars <- NULL

i=1

###------ Palm tree Euterpe edulis

sp <- sp1[sp1$sp==sp_name[i],c('lon','lat')]

sp$species <- 1

coordinates(sp) <- ~ lon + lat
plot(sp)

pres <- stack(pres, soil)


#---- remove collinear variables
spx <- extract(pres, sp)

spx <- data.frame(spx)

v1 <- vifcor(spx, th=0.5) # identify collinear variables with corr>0.6 that should be excluded

bio <- exclude(pres,v1) # exclude the correlated variables that were identified in

plot(bio)

########

# sdmModel data
d <- sdmData(species~., train=sp, predictors= bio, bg=list(n=10000,method='gRandom',remove=TRUE))

# Fitting SDMs
m <- sdm(species ~ . , d, methods=c("svm", "rf" , "mlp"), #mars was not converging for most models, so was removed
         replication=c("sub"), test.p=25, n=100)

# Ensembling
en <- ensemble(m, bio, 
               setting=list(method='weighted',stat='TSS'), overwrite=TRUE)

writeRaster(en,  paste0('D:/lilian/Mapas/', gsub(" ", "_", sp_name[i]),"_pres.tif"), overwrite=T)

# Evaluation
e <- getEvaluation(m)

# Save which variables were used and evaluation results for all species
evals[i, "sp_name"] <- gsub(" ", "_", sp_name[i])
evals[i, "records"] <- nrow(as.data.frame(d))-10000
evals[i, "AUC"] <- round(mean(e$AUC), 2)
evals[i, "COR"] <- round(mean(e$COR), 2)
evals[i, "Deviance"] <- round(mean(e$Deviance),2)
evals[i, "TSS"] <- round(mean(e$TSS),2)

vars <- rbind(names(bio), vars)

# Find binarization threshold
df <- data.frame(as.data.frame(d),coordinates(d)) # presence points and predictors associated
pr <- extract(en, df[,c('lon','lat')]) # estimates of habitat suitability from presence points
ev <- evaluates(df$species, pr) # evaluate prediction (observed vs expected) 
th <- ev@threshold_based$threshold[[2]] # threshold that maximizes sensitiv + specificity

# Binary prediction
pa <- en              
pa[] <- ifelse(pa[] >= th, 1, 0) # convert from continuous to binary

# present PA
writeRaster(pa,  
            paste0('D:/lilian/Mapas/',gsub(" ", "_", sp_name[i]),"_pres_PA.tif"),
            overwrite = T)


#---- Bioclimatic predictors based on future climate projections
setwd("D:/lilian/Climaticos")
#dir()
lista <- list.files(pattern = "mean*")
#length(lista)

# Several years and forecasts
#j=3

for (j in 1: length(lista)) { 

fut <- stack(lista[j])
fut <- stack(fut, soil)
names(fut) <- names(pres)
proj4string(fut) <- projection(pres)

enf <- ensemble(m, fut, setting=list(method='weighted',stat='TSS'))

# Continuous projection
writeRaster(enf,  
            paste0('D:/lilian/Mapas/',gsub(" ", "_", sp_name[i]), gsub("mean", "", lista[j])),
            overwrite=TRUE)

# Binary projection
paf <- enf
paf[] <- ifelse(paf[] >= th, 1, 0)

# future PA
writeRaster(paf,  
            paste0('D:/lilian/Mapas/',gsub(" ", "_", sp_name[i]),"_PA", gsub("mean", "", lista[j])),
            overwrite = T) 
    }

}

write.csv(evals, "D:/lilian/Mapas/evals_palm.csv")

rownames(vars) <- sp_name
write.csv(vars, 'D:/lilian/Mapas/vars_palm.csv')
