### script enm dismo - multiple algorithms ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 2018-08-24


###---------------------------------------------------------------------------###

# memory
rm(list = ls())

# # diretório temporario de processamento do R 
tempdir <- function() "D:\\temps"
unlockBinding("tempdir", baseenv())
assignInNamespace("tempdir", tempdir, ns="base", envir=baseenv())
assign("tempdir", tempdir, baseenv())
lockBinding("tempdir", baseenv())
tempdir()


# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, dismo, gam, kernlab, randomForest, gbm, rJava, vegan, 
               spocc, usdm, tidyverse, data.table, viridis, RStoolbox, RCurl, dplyr)

###---------------------------------------------------------------------------###

## data 

# directory
path <- "/media/mauriciovancine/data/04_manuscritos/00_escritos/in_prep/pablo/00_script"
path <- "C:\\Dados\\GitHub\\ModelosRetracaoExpansao"
path <- "F:/pablo/v02"
setwd(path)

###---------------------------------------------------------------------------###

# funcoes 
source('ecoClimate.R') 
source('CutTrheshold.R') 

###---------------------------------------------------------------------------###

# parametros 

# resolucao
resolution = '30m'; resolution2 = '30m'
period = 'pre'; period2 = 'lgm' # no path

# Tempo 1 para interpola??o = Presente
tempo1 = 0                      

# Tempo 2 para interpola??o = Ultimo Glacial M?ximo
tempo2 = 21 

# Per?odo desejado
tempo.ini = 1
tempo.fim = 21

# selecao de variaveis para modelagem
# selbiovar = c("bio02","bio04","bio10","bio13","bio14","bio18")

# numero de repeticoes
nrep = 10

# relacao teste treiro
# 0.75
testeXtreino = 0.75

# resultados dos modelos
dir.create("modelagem")
path.rs <- paste0(path, "/modelagem")

# resultados thresholds
path.result.trheshold <- paste0(path, "/modelagem")
save.raster = FALSE
save.images = FALSE

###-----------------------------------------------------------------------------------###

# AOGCM
prefix <- prefix.lbl <- {}
prefix[1] = 'CCSM';   prefix.lbl[1] = 'CCSM'
prefix[2] = 'CNRM';   prefix.lbl[2] = 'CNRM'
prefix[3] = 'FGOALS'; prefix.lbl[3] = 'FGOALS3'
prefix[4] = 'GISS';   prefix.lbl[4] = 'GISS'
prefix[5] = 'IPSL';   prefix.lbl[5] = 'IPSL'
prefix[6] = 'MIROC';  prefix.lbl[6] = 'MIROC'
prefix[7] = 'MPI';    prefix.lbl[7] = 'MPI'
prefix[8] = 'MRI';    prefix.lbl[8] = 'MRI'
prefix.df = data.frame(aogcm = as.data.frame(prefix), stringsAsFactors = T)

###-----------------------------------------------------------------------------------###

### avaliar area modelos
neotropico=F
paisesNeotropico=F
provinciasNeotropico=F
biomasBR=F
baciasBR=F

# Renomerar Raster Bioclim 
bi.names.order=c(paste0("bio0", 1:9), paste0("bio", 10:19))
bi.names.native=c("bio01", paste0("bio", 10:19), paste0("bio0", 2:9))

# Marca de corte 
mascara <- readOGR(paste0(path,"/shape/Lowenberg_Neto_Neotropics_diss.shp"))
plot(mascara)

###-----------------------------------------------------------------------------------###

#---  Variaveis ---#
aogcm.tempox.bi <- {}

###-----------------------------------------------------------------------------------###

#--- Carrega concentracao de O2 ---#
LR04 = fread(paste0(path,"/data/LR04_Stack.txt"))
o.tempo1 = as.numeric(LR04[LR04$Time==tempo1,c('d18O')])
o.tempo2 = as.numeric(LR04[LR04$Time==tempo2,c('d18O')])
o.erro1 = as.numeric(LR04[LR04$Time==tempo1,c('Error')])
o.erro2 = as.numeric(LR04[LR04$Time==tempo2,c('Error')])

###-----------------------------------------------------------------------------------###

# occurrences
po.all <- fread(paste0(path,"/data/datasetmodel.txt"))
po.all

colnames(po.all) <- c('sp', 'long', 'lat', 'year', 
                      'recordedByStandardized',	'recordNumberStandardized',	'source') 
po.all <- data.frame(po.all[order(po.all$sp)], stringsAsFactors = F)
po.all

coordinates(po.all) <- c("long", "lat")
crs(po.all) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
po.all <- po.all[!is.na(over(po.all, geometry(mascara))),]
# po.all1 <- po.all[mascara,]

plot(po.all, pch = 20, col = adjustcolor('gray50',.1))
plot(mascara, add = T)

po = data.frame(sp = c(po.all$sp),
                long = c(po.all$long),
                lat = c(po.all$lat),
                stringsAsFactors = T)
po

###-----------------------------------------------------------------------------------###

## variables
###.... baixar ecoclimate

# load
# ecoClimateData <- paste("30m")
# setwd(ecoClimateData)
# 
# st <- stack()
# aogcm <- prefix
# 
# for(i in aogcm){
#   
#   setwd(paste0(ecoClimateData, "/", i))
#   
#   for(j in c("0k", "21k")){
#     
#     setwd(paste0(ecoClimateData, "/", i, "/", j))
#     
#     st.j <- stack(dir())
#     names(st.j) <- paste0(names(st.j), "_", i, "_", j)
#     
#     st <- stack(st, st.j)
#     
#     setwd("..")
#     
#     }
# }
# 
# # confer
# names(st)
# 
# # adjust extention
# st.ne <- crop(st, mascara) # adjust extention
# st.ne <- mask(st.ne, mascara) # adjust limit
# 
# plot(st.ne$bio01_CCSM_0k)
# lines(mascara)
# 
# ###---------------------------------------------------------------------------###
# 
# ## variable selection
# st.ne.0k <- st.ne[[grep("0k", names(st.ne))]]
# 
# # vif
# li <- list()
# 
# for(i in 1:length(aogcm)){
# 
#     st.ne.0k.j <- st.ne.0k[[grep(aogcm[i], names(st.ne.0k))]]
#     st.ne.0k.j.co <- vifcor(st.ne.0k.j[], th = .6)
#     li[[i]] <- st.ne.0k.j.co@results$Variables
# }
# 
# # bio03, bio08, bio18, bio19 - variaveis nao correlacionadas (r < 0.6) em nenhum dos aogcms
# 
# # selecting
# en <- st.ne[[grep("bio03|bio08|bio18|bio19", names(st.ne))]]
# en
# 
# import
setwd(paste0(path, "/var"))
en <- stack(dir(patt = ".tif"))

plot(en, col = viridis(100))

# # export
# dir.create("var"); setwd("var")
# 
# for(i in 1:length(names(en))){
#   writeRaster(en[[i]], paste0(names(en[[i]]), ".tif"))
# }

###---------------------------------------------------------------------------###

## occurrences selection

# one point per cell
ra <- data.table(rasterToPoints(en[[1]])[, 1:2])
ra <- data.table(id = 1:nrow(ra), ra)
gridded(ra) <- ~ x + y
ra.r <- raster(ra) 
crs(ra.r) <- crs(en)  
plot(ra.r, col = viridis(100))
points(po$lon, po$lat, pch = 20)

po.fi <- data.frame()

for(i in unique(po$sp)){
  po.i <- po[po$sp == i, ]
  po.i$oppc <- raster::extract(ra.r, po.i[, c(2:3)])
  po.i <- na.omit(distinct(po.i, oppc, .keep_all = TRUE))
  po.fi <- rbind(po.fi, po.i)
}

po.fi

po <- po.fi

table(po.fi[po.fi$sp == unique(po$sp)[20], ]$oppc)

plot(en[[1]], col = viridis(100))
points(po$lon, po$lat, pch = 20)

###---------------------------------------------------------------------------###

## background coordinates
bc <- rasterToPoints(en[[1]])[, 1:2]
colnames(bc[, -3]) <- c("long", "lat")

plot(en[[1]], col = viridis(100))
points(bc[sample(1:nrow(bc), 1000), ], pch = 20, cex = .5, col = "blue")
points(po[, 2:3], pch = 20, cex = .5, col = "red")

###---------------------------------------------------------------------------###

# verify maxent
file.exists(paste0(system.file(package = "dismo"), "/java/maxent.jar"))

###---------------------------------------------------------------------------###

### enms ###

setwd(path.rs)

# dir
dir.create("enm"); setwd("enm"); getwd()

# enms
for(i in 1:length(unique(po[, 1]))){ # for to each specie
  
  
  # graphics
  dir.create("graphics")
  
  # variables for evaluate
  eval.Bioclim <- NULL
  eval.Gower <- NULL
  eval.RandomForest <- NULL
  eval.Maxent <- NULL
  eval.SVM <- NULL
  eval.names <- NULL
  maxent.results <- matrix()
  
  da.bioclim <- data.frame(xyFromCell(en, cell = 1:ncell(en)))
  da.gower <- data.frame(xyFromCell(en, cell = 1:ncell(en)))
  da.randomforest <- data.frame(xyFromCell(en, cell = 1:ncell(en)))
  da.maxent <- data.frame(xyFromCell(en, cell = 1:ncell(en)))
  da.svm <- data.frame(xyFromCell(en, cell = 1:ncell(en)))
  
  # selecting presence and absence
  id.specie <- as.character(unique(po[, 1]))[i]
  pr.specie <- po[which(po[, 1] == id.specie), 2:3]
  id.background <- sample(nrow(bc), nrow(pr.specie))
  bc.specie <- bc[id.background, ]
  
  # export points
  fwrite(data.table(pr.specie), paste0("_", id.specie, "_presence_points.csv"))
  fwrite(data.table(bc.specie), paste0("_", id.specie, "_background_points.csv"))
  
  for(a in aogcm){
    
    en.a <- en[[grep(a, names(en))]]
    en.a.0 <- en.a[[grep("0k", names(en.a))]]
    en.a.21 <- en.a[[grep("21k", names(en.a))]]
  
  for(r in 1:nrep){	# number of replicas
    
    ## preparing the models
    # train and test data	
    pr.sample.train <- sample(nrow(pr.specie), round(.75 * nrow(pr.specie)))
    bc.sample.train <- sample(nrow(bc.specie), round(.75 * nrow(bc.specie)))
    train <- na.omit(prepareData(x = en.a.0, p = pr.specie[pr.sample.train, ], b = bc.specie[bc.sample.train, ]))
    test <- na.omit(prepareData(x = en.a.0, p = pr.specie[-pr.sample.train, ], b = bc.specie[-bc.sample.train, ]))
    
    ### algorithms ###
    
    ## 1. bioclim
    Bioclim <- bioclim(train[which(train[, 1] == 1), -1])
    da.bioclim <- cbind(da.bioclim, predict(en.a.0, Bioclim)[])
    
    colu <- paste0(id.specie, "_bioclim_", a, "_r", ifelse(r < 10, paste0("0", r), r), "_p000")
    colnames(da.bioclim) <- c(colnames(da.bioclim[, -ncol(da.bioclim)]), colu)
    
    eBioclim <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Bioclim)
    idBioclim <- which(eBioclim@t == as.numeric(threshold(eBioclim, "spec_sens")))
    eval.Bioclim.sp <- c(eBioclim@t[idBioclim], eBioclim@auc, (eBioclim@TPR[idBioclim] + eBioclim@TNR[idBioclim] - 1))
    eval.Bioclim <- rbind(eval.Bioclim, eval.Bioclim.sp)
    
     # setwd("graphics")
     # tiff(paste0("bioclim_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); response(Bioclim); dev.off()
     # tiff(paste0("bioclim_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eBioclim, "ROC"); dev.off()
     # setwd("..")
    
    # if raster - writeRaster(predict(en.a.0, Bioclim), paste0("bioclim_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
    
    for(tempox in tempo.ini:tempo.fim){
      
      periodo = ifelse(tempox < 100, ifelse(tempox<10, paste0("00", tempox), paste0("0", tempox)), tempox) 
      
      tempox.lbl = paste0(tempox, 'kyr')
      o.tempox = as.numeric(LR04[LR04$Time==tempox,c('d18O')])
      o.errox = as.numeric(LR04[LR04$Time==tempox,c('Error')])
      
      print(paste0(tempox.lbl,': ',o.tempox) ) 
      print('calculando...') 
      
      # calculo
      var.tempox = en.a.0 - (((en.a.0 - en.a.21) * (o.tempo1 - o.tempox)) / (o.tempo1 - o.tempo2))
      print(paste0('projetando: ', tempox.lbl))
      
      da.bioclim <- cbind(da.bioclim, colu = values(predict(var.tempox, Bioclim)))
      
      colu <- paste0(id.specie, "_bioclim_", a, "_r", ifelse(r < 10, paste0("0", r), r), 
                     "_p", periodo)
      colnames(da.bioclim) <- c(colnames(da.bioclim[, -ncol(da.bioclim)]), colu)
    
    }
  
    print(paste0("Yeh! The model witch AOGMC ", a," of ",id.specie, ", algorithm 'Bioclim', replica ", 
                 ifelse(r < 10, paste0("0", r), r), " it's done!"))
    
    ## 2. gower
    
    Gower <- domain(train[which(train[, 1] == 1), -1])	
    da.gower <- cbind(da.gower, predict(en.a.0, Gower)[])
    
    colu <- paste0(id.specie, "_gower_", a, "_r", ifelse(r < 10, paste0("0", r), r), "_p000")
    colnames(da.gower) <- c(colnames(da.gower[, -ncol(da.gower)]), colu)
    
    #writeRaster(predict(en, Gower), paste0("gower_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
    
    eGower <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Gower)
    idGower <- which(eGower@t == as.numeric(threshold(eGower, "spec_sens")))
    eval.Gower.sp <- c(eGower@t[idGower], eGower@auc, (eGower@TPR[idGower] + eGower@TNR[idGower] - 1))
    eval.Gower <- rbind(eval.Gower, eval.Gower.sp)
    
    # setwd("graphics")
    # tiff(paste0("gower_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); response(Gower); dev.off()
    # tiff(paste0("gower_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eGower, "ROC"); dev.off()
    # setwd("..")
    # 
    
    for(tempox in tempo.ini:tempo.fim){
      
      periodo = ifelse(tempox < 100, ifelse(tempox<10, paste0("00", tempox), paste0("0", tempox)), tempox) 
      
      tempox.lbl = paste0(tempox, 'kyr')
      o.tempox = as.numeric(LR04[LR04$Time==tempox,c('d18O')])
      o.errox = as.numeric(LR04[LR04$Time==tempox,c('Error')])
      
      print(paste0(tempox.lbl,': ',o.tempox) ) 
      print('calculando...') 
      
      # calculo
      var.tempox = en.a.0 - (((en.a.0 - en.a.21) * (o.tempo1 - o.tempox)) / (o.tempo1 - o.tempo2))
      print(paste0('projetando: ', tempox.lbl))
      
      da.gower <- cbind(da.gower, colu = values(predict(var.tempox, Gower)))
      
      colu <- paste0(id.specie, "_gower_", a, "_r", ifelse(r < 10, paste0("0", r), r), 
                     "_p", periodo)
      colnames(da.gower) <- c(colnames(da.gower[, -ncol(da.gower)]), colu)
      
    }

    print(paste0("Yeh! The model witch AOGMC ", a," of ", id.specie, ", algorithm 'Gower', replica ", 
                 ifelse(r < 10, paste0("0", r), r), " it's done!"))
    
    ## 3. random forest
    
    RandomForest <- randomForest(pb ~ ., data = train) ## ????
    da.randomforest <- cbind(da.randomforest, predict(en.a.0, RandomForest)[])
    
    colu <- paste0(id.specie, "_randomforest_", a, "_r", ifelse(r < 10, paste0("0", r), r), "_p000")
    colnames(da.randomforest) <- c(colnames(da.randomforest[, -ncol(da.randomforest)]), colu)
    
    #writeRaster(predict(en, RandomForest), paste0("randomforest_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 

    eRandomForest <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = RandomForest)
    idRandomForest <- which(eRandomForest@t == as.numeric(threshold(eRandomForest, "spec_sens")))
    eval.RandomForest.sp <- c(eRandomForest@t[idRandomForest], eRandomForest@auc, (eRandomForest@TPR[idRandomForest] + eRandomForest@TNR[idRandomForest] - 1))
    eval.RandomForest <- rbind(eval.RandomForest, eval.RandomForest.sp)

    # setwd("graphics")
    # tiff(paste0("random_forest_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eRandomForest, "ROC"); dev.off()
    # setwd("..")
    
    for(tempox in tempo.ini:tempo.fim){
      
      periodo = ifelse(tempox < 100, ifelse(tempox<10, paste0("00", tempox), paste0("0", tempox)), tempox) 
      
      tempox.lbl = paste0(tempox, 'kyr')
      o.tempox = as.numeric(LR04[LR04$Time==tempox,c('d18O')])
      o.errox = as.numeric(LR04[LR04$Time==tempox,c('Error')])
      
      print(paste0(tempox.lbl,': ',o.tempox) ) 
      print('calculando...') 
      
      # calculo
      var.tempox = en.a.0 - (((en.a.0 - en.a.21) * (o.tempo1 - o.tempox)) / (o.tempo1 - o.tempo2))
      print(paste0('projetando: ', tempox.lbl))
      
      da.randomforest <- cbind(da.randomforest, colu = values(predict(var.tempox, RandomForest)))
      
      colu <- paste0(id.specie, "_randomforest_", a, "_r", ifelse(r < 10, paste0("0", r), r), 
                     "_p", periodo)
      colnames(da.randomforest) <- c(colnames(da.randomforest[, -ncol(da.randomforest)]), colu)
      
    }
    
    print(paste0("Yeh! The model witch AOGMC ", a," of ", id.specie, ", algorithm 'Random Forest', replica ", 
                 ifelse(r < 10, paste0("0", r), r), " it's done!"))
    
    
    ## 4. maxent	
    
    Maxent <- maxent(train[, -1], train[, 1])	
    da.maxent <- cbind(da.maxent, predict(en.a.0, Maxent)[])
    
    colu <- paste0(id.specie, "_maxent_", a, "_r", ifelse(r < 10, paste0("0", r), r), "_p000")
    colnames(da.maxent) <- c(colnames(da.maxent[, -ncol(da.maxent)]), colu)

    #writeRaster(predict(en, Maxent), paste0("maxent_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 

    eMaxent <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maxent)
    idMaxent <- which(eMaxent@t == as.numeric(threshold(eMaxent, "spec_sens")))
    eval.Maxent.sp <- c(eMaxent@t[idMaxent], eMaxent@auc, (eMaxent@TPR[idMaxent] + eMaxent@TNR[idMaxent] - 1))
    eval.Maxent <- rbind(eval.Maxent, eval.Maxent.sp)
    
    # setwd("graphics")
    # tiff(paste0("maxent_response_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); response(Maxent); dev.off()
    # tiff(paste0("maxent_contribution_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(Maxent); dev.off()
    # tiff(paste0("maxent_auc_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eMaxent, "ROC"); dev.off()
    # maxent.results <- data.table(maxent.results, as.matrix(Maxent@results[1:50]))
    # setwd("..")
    
    for(tempox in tempo.ini:tempo.fim){
      
      periodo = ifelse(tempox < 100, ifelse(tempox<10, paste0("00", tempox), paste0("0", tempox)), tempox) 
      
      tempox.lbl = paste0(tempox, 'kyr')
      o.tempox = as.numeric(LR04[LR04$Time==tempox,c('d18O')])
      o.errox = as.numeric(LR04[LR04$Time==tempox,c('Error')])
      
      print(paste0(tempox.lbl,': ',o.tempox) ) 
      print('calculando...') 
      
      # calculo
      var.tempox = en.a.0 - (((en.a.0 - en.a.21) * (o.tempo1 - o.tempox)) / (o.tempo1 - o.tempo2))
      print(paste0('projetando: ', tempox.lbl))
      
      da.maxent <- cbind(da.maxent, colu = values(predict(var.tempox, Maxent)))
      
      colu <- paste0(id.specie, "_maxent_", a, "_r", ifelse(r < 10, paste0("0", r), r), 
                     "_p", periodo)
      colnames(da.maxent) <- c(colnames(da.maxent[, -ncol(da.maxent)]), colu)
      
    }
    
    print(paste0("Yeh! The model witch AOGMC ", a," of ", id.specie, ", algorithm 'Maxent', replica ", 
                 ifelse(r < 10, paste0("0", r), r), " it's done!"))
    
    
    ## 5. svm	
    SVM <- ksvm(pb ~ ., data = train)
    da.svm <- cbind(da.svm, predict(en.a.0, SVM)[])
    
    colu <- paste0(id.specie, "_svm_", a, "_r", ifelse(r < 10, paste0("0", r), r), "_p000")
    colnames(da.svm) <- c(colnames(da.svm[, -ncol(da.svm)]), colu)
    
    # writeRaster(predict(en, SVM), paste0("svm_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
    
    eSVM <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = SVM)
    idSVM <- which(eSVM@t == as.numeric(threshold(eSVM, "spec_sens")))
    eval.SVM.sp <- c(eSVM@t[idSVM], eSVM@auc, (eSVM@TPR[idSVM] + eSVM@TNR[idSVM] - 1))
    eval.SVM <- rbind(eval.SVM, eval.SVM.sp)
    
    # setwd("graphics")
    # tiff(paste0("svm_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eSVM, "ROC"); dev.off()
    # setwd("..")
    
    for(tempox in tempo.ini:tempo.fim){
      
      periodo = ifelse(tempox < 100, ifelse(tempox<10, paste0("00", tempox), paste0("0", tempox)), tempox) 
      
      tempox.lbl = paste0(tempox, 'kyr')
      o.tempox = as.numeric(LR04[LR04$Time==tempox,c('d18O')])
      o.errox = as.numeric(LR04[LR04$Time==tempox,c('Error')])
      
      print(paste0(tempox.lbl,': ',o.tempox) ) 
      print('calculando...') 
      
      # calculo
      var.tempox = en.a.0 - (((en.a.0 - en.a.21) * (o.tempo1 - o.tempox)) / (o.tempo1 - o.tempo2))
      print(paste0('projetando: ', tempox.lbl))
      
      da.svm <- cbind(da.svm, colu = values(predict(var.tempox, SVM)))
      
      colu <- paste0(id.specie, "_svm_", a, "_r", ifelse(r < 10, paste0("0", r), r), 
                     "_p", periodo)
      colnames(da.svm) <- c(colnames(da.svm[, -ncol(da.svm)]), colu)
      
    }
    
    print(paste0("Yeh! The model witch AOGMC ", a," of ", id.specie, ", algorithm 'SVM', replica ", 
                 ifelse(r < 10, paste0("0", r), r), " it's done!"))
    
    
    eval.names <- c(eval.names, paste0(id.specie, ifelse(r < 10, paste0("0", r), r)))	
    
  } # ends for "r"
  
  # maxent results
  # setwd("graphics")
  # na <- attributes(Maxent@results)[[2]][[1]]
  # maxent.results <- data.table(na, maxent.results[, -1])
  # colnames(maxent.results) <- c("names", paste0("rep", 1:r))
  # fwrite(maxent.results, paste0("_maxent_results", id.specie, ".csv"))
  # setwd("..")
    
    # evaluations
    dimnames(eval.Bioclim) <- list(eval.names, c("thrs", "AUC", "TSS"))
    dimnames(eval.Gower) <- list(eval.names, c("thrs", "AUC", "TSS"))  
    dimnames(eval.RandomForest) <- list(eval.names, c("thrs", "AUC", "TSS"))
    dimnames(eval.Maxent) <- list(eval.names, c("thrs", "AUC", "TSS"))
    dimnames(eval.SVM) <- list(eval.names, c("thrs", "AUC", "TSS"))
    
    write.table(eval.Bioclim, paste0("zEval_", "bioclim_", a, "_", id.specie, ".txt"))
    write.table(eval.Gower, paste0("zEval_", "gower_", a, "_", id.specie, ".txt"))
    write.table(eval.RandomForest, paste0("zEval_", "randomforest_", a, "_", id.specie, ".txt"))
    write.table(eval.Maxent, paste0("zEval_", "maxent_", a, "_", id.specie, ".txt"))
    write.table(eval.SVM, paste0("zEval_", "svm_", a, "_", id.specie, ".txt"))
    
  } # ends for  "a"
  
  write.table(da.bioclim, paste0("enm_", "bioclim_", id.specie, ".txt"), row.names = FALSE)
  write.table(da.gower, paste0("enm_", "gower_", id.specie, ".txt"), row.names = FALSE)
  write.table(da.randomforest, paste0("enm_", "randomforest_", id.specie, ".txt"), row.names = FALSE)
  write.table(da.maxent, paste0("enm_", "maxent_", id.specie, ".txt"), row.names = FALSE)
  write.table(da.svm, paste0("enm_", "svm_", id.specie, ".txt"), row.names = FALSE)
  
  
} # end i

###----------------------------------------------------------------------------###

da <- fread("enm_gower_Adiantum calcareum.txt")
dim(da)

da.1 <- da[, c(1, 2, 3)]
colnames(da.1)
gridded(da.1) <- ~x + y
da.1.r <- raster(da.1)

plot(da.1.r)
