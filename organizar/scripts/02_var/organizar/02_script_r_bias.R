####################################################################################################
# script to model peccari distribution in Brazil using maxent, maxent with bias correction & maxlike
#
# written Thadeu Sobral e Maurício Vancine 
#
#(adapted by Julia Oshima using code provided by MC Fitzpatrick)
####################################################################################################

### disciplina - modelagem de nicho ecológico: teoria e prática ###
### ppg ecologia - unicamp 2016 ###

###-----------------------------------------------------------------------------------###
### script gerar modelos de nicho ecologico ### 
###-----------------------------------------------------------------------------------###


# memory
rm(list = ls())
gc() 
memory.limit(size = 1.75e13) 

# packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(raster, rgdal, maxlike, dismo, foreach, gam, doParallel, 
               randomForest, kernlab, rJava, vegan, sp, sm, rpanel, rgl, 
               doSNOW, XLConnect)

options(java.parameters = "-Xmx4g") #importante configurar isso para não ficar dando falha de memória

# verify
search() 


###-----------------------------------------------------------------------------------###

# dados de entrada

# diretorio dos pontos de ocorrencia
setwd("F:/testmax")

# importando os pontos de ocorrencia 
wlp.points <- read.table("BR_wlp_rarefy_FINAL_05_2017.txt", "\t", "\t", header = T)
head(wlp.points, 10)


# importando as variaveis .asc
env.stack <- stack("brdecliv.asc", "brdistgwi.asc", "brdistpav.asc", "brfradist.asc", "brhomogen.asc", "brpop.asc", "brhanstreecover.asc", "brbio2.asc", "brbio3.asc", "brbio10.asc", "brbio16.asc", "brbio17.asc", "brbio18.asc")
names(env.stack) <- c("brdecliv", "brdistwater", "brdistpav", "brfradist", "brhomogen", "brpop", "brtreecover", "bio2", "bio3", "bio10", "bio16", "bio17", "bio18")

head(env.stack)
str(env.stack)
plot(env.stack)

##########Standardize data
# Se quiser padronizar os valores das variaveis - scale climate layers per Royle et al. suggestion - standardization -

env.stackTrans <- env.stack

for(i in 1:dim(env.stack)[3]){
 env.stackTrans[[i]] <- (env.stack[[i]]-cellStats(env.stack[[i]], mean))/cellStats(env.stack[[i]], sd)
}

head(env.stack)
str(env.stack)
str(env.stackTrans)
#plot(env.stackTrans)
mask <- env.stackTrans[[1]]>-1000 #ver porque utilizar este limite de corte

plot(mask)

#colocando as coordenadas
id.na <- na.omit(cbind(1:ncell(env.stackTrans), values(env.stackTrans)[, 1]))
coords <- xyFromCell(env.stackTrans, id.na[, 1])
colnames(coords) <- c("long", "lat")
head(coords, 10)

###-----------------------------------------------------------------------------------###


# ENMs



# aogcms
AOGCM <- "TC"
AOGCM

# enms
for(i in 1:length(levels(wlp.points[, 1]))){ # for para as especies
 
 
 eval.MaxentBIAS <- NULL
 
 eval.names <- NULL
 
 # selecionando presença e ausencia da especie
 id.specie <- levels(wlp.points[, 1])[i]
 wlp.specie <- wlp.points[which(wlp.points[, 1] = = id.specie), 2:3]
 id.background <- sample(nrow(coords), 10000)#vamos usar 10000 pontos de background
 bc.specie <- coords[id.background, ]
 
 
 for(r in 1:10){	# numero de replicas
 ##preparando os modelos!!
 #data treino e teste!!!	
 wlp.sample.train <- sample(nrow(wlp.specie), round(0.75 * nrow(wlp.specie)))
 bc.sample.train <- sample(nrow(bc.specie), round(0.75 * nrow(bc.specie)))
 test <- na.omit(prepareData(x = env.stackTrans, p = wlp.specie[-wlp.sample.train, ], b = bc.specie[-bc.sample.train, ]))
 train <- na.omit(prepareData(x = env.stackTrans, p = wlp.specie[wlp.sample.train, ], b = bc.specie[bc.sample.train, ]))
 
 
 ########Extraído de Fitzpatrick et al 2013 MaxEnt versus MaxLike empirical comparisons#######################################################
 # Criando uma superficie com Kernel para analisar o viés dos pontos e criar uma superficie de densidade pra corrigir o sampling do background
 
 bias <-cellFromXY(mask, wlp.points[ , -1]) # Pra cada Get cell number(s) of a Raster* object from row and/or column numbers. 
 #Cell numbers start at 1 in the upper left corner, and increase from left to right, and then from top to bottom. The last cell number equals the number of cells of the Raster* object.
 
 cells <- unique(sort(bias))#This function returns the unique values in a RasterLayer, or the unique combinations of values in a multi-layer raster object.
 
 kernelXY <- xyFromCell(mask, cells)
 str(kernelXY)
 
 samps <- as.numeric(table(bias))
 
 KDEsur <- sm.density(kernelXY, weights = samps, display = "none", ngrid = 4704, 
    ylim = c(-36, 7), xlim = c(-76, -32), nbins = 0) ##ngrid = ncols da mask
 
 KDErast = SpatialPoints(expand.grid(x = KDEsur$eval.points[, 1], y = KDEsur$eval.points[, 2]))
 KDErast = SpatialPixelsDataFrame(KDErast, data.frame(kde = array(KDEsur$estimate, 
         length(KDEsur$estimate))))
 KDErast <- raster(KDErast)
 KDErast <- resample(KDErast, mask)
 KDErast <- KDErast*mask
 
 plot(KDErast)
 KDEpts <- rasterToPoints(KDErast)
 
 
 ##Maxent com Bias Correction
 MaxentBIAS <- maxent(train[, -1], train[, 1], a = KDEpts[sample(seq(1:nrow(KDEpts)), size = 10000, replace = T, prob = KDEpts[, "layer"]), 1:2], 
    args = c("responsecurves = T", "jackknife = T"))	#write response curves and do Jacknife
 
 
 #Plot variable contribution
 plot(MaxentBIAS)
 
 #plot response curves
 response(MaxentBIAS)
 
 writeRaster(predict(env.stackTrans, MaxentBIAS), paste(AOGCM, "_MaxentBIAS_0k_", id.specie, r, ".asc", sep = ""), format = "ascii")
 
 eMaxentBIAS <- evaluate(p = test[test[, 1] = = 1, -1], a = test[test[, 1] = = 0, -1], model = MaxentBIAS)
 idMaxentBIAS <- which(eMaxentBIAS@t = = as.numeric(threshold(eMaxentBIAS, "spec_sens")))
 eval.MaxentBIAS.sp <- c(eMaxentBIAS@t[idMaxentBIAS], eMaxentBIAS@auc, (eMaxentBIAS@TPR[idMaxentBIAS]+eMaxentBIAS@TNR[idMaxentBIAS]-1))
 eval.MaxentBIAS <- rbind(eval.MaxentBIAS, eval.MaxentBIAS.sp)
 