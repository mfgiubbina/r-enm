### script enm projections past ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 05/06/2017

###---------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, dismo, gam, randomForest, kernlab, rJava, vegan)
search()

# temp
setwd("E:/github_mauriciovancine/R-ENM/data")
dir.create("temp")
tempdir <- function() "E:/github_mauriciovancine/R-ENM/data/temp"
unlockBinding("tempdir", baseenv())
assignInNamespace("tempdir", tempdir, ns = "base", envir = baseenv())
assign("tempdir", tempdir, baseenv())
lockBinding("tempdir", baseenv())
tempdir()

###---------------------------------------------------------------------------###

## data
#  variables
en <- stack(
  getData(name = "worldclim", var = "bio", res = 10, download = T),
  getData(name = "CMIP5", var = "bio", res = 10, download = T, 
          rcp = 45, model = "AC", year = 50),
  getData(name = "CMIP5", var = "bio", res = 10, download = T,
          rcp = 45, model = "AC", year = 70),
  getData(name = "CMIP5", var = "bio", res = 10, download = T,
          rcp = 85, model = "AC", year = 50),
  getData(name = "CMIP5", var = "bio", res = 10, download = T,
          rcp = 85, model = "AC", year = 70))

en

plot(en[[1]], col = viridis(100))

# resampling
en.re <- aggregate(en, fact = 6, fun = "mean", expand = T)
en.re
plot(en.re[[1]])

# limite
br <- getData("GADM", country = "BRA", level = 0)
br

# adjust to mask
en.br <- crop(mask(en.re, br), br)
en.br
plot(en.br[[1]])

# correlation
en.co <- vifcor(en.br[[1:19]], th = .6) # bio05, bio14, bio18, bio19
en.co

en <- en.br[[as.character(en.co@results$Variables)]]
en  

# selection
en.p <- en.br[[1:19]][[as.character(en.co@results$Variables)]]

en.45.50 <- en.br[[grep("45bi50", names(en.br))]]
names(en.45.50) <-  names(en.br[[1:19]])
en.45.50 <- en.45.50[[as.character(en.co@results$Variables)]]

en.45.70 <- en.br[[grep("45bi70", names(en.br))]]
names(en.45.70) <-  names(en.br[[1:19]])
en.45.70 <- en.45.70[[as.character(en.co@results$Variables)]]

en.85.50 <- en.br[[grep("85bi50", names(en.br))]]
names(en.85.50) <-  names(en.br[[1:19]])
en.85.50 <- en.85.50[[as.character(en.co@results$Variables)]]

en.85.70 <- en.br[[grep("85bi70", names(en.br))]]
names(en.85.70) <-  names(en.br[[1:19]])
en.85.70 <- en.85.70[[as.character(en.co@results$Variables)]]

# background coordinates
bc <- rasterToPoints(en.p)[, 1:2]
colnames(bc[, -3]) <- c("long", "lat")

plot(en.p[[1]], col = viridis(100))
points(bc, pch = 20, cex = .5, col = "blue")

# occurrences
ba <- distinct(occ2df(occ(query = "Brachycephalus ephippium", 
                          from = c("gbif", "idigbio", "inat"),
                          has_coords = T))[, 1:3])
ba

ba <- data.table(sp = sub(" ", "_", unique(tolower(ba$name))), 
                 lon = as.numeric(ba$longitude), 
                 lat = as.numeric(ba$latitude), 
                 pres = 1)
ba

plot(ba$lon, ba$lat, pch = 20)

# one point per cell
po <- mask(rasterize(ba[, 2:3], en.p[[1]], ba$pres), br)
po

po <- data.table(sp = unique(ba$sp), 
                 lon = rasterToPoints(po)[, 1], 
                 lat = rasterToPoints(po)[, 2])
po

plot(en.p[[1]], col = viridis(100))
points(bc, pch = 20, cex = .5, col = "blue")
points(po$lon, po$lat, pch = 20, cex = .5, col = "red")

###---------------------------------------------------------------------------###

# verify maxent

# copy maxent.jar in "C:\Users\john01\Documents\R\win-library\3.4\dismo\java"

jar <- paste0(system.file(package = "dismo"), "/java/maxent.jar")
file.exists(jar)

###---------------------------------------------------------------------------###

### ENMs ###

# diretory
setwd("..")
getwd()
dir.create("ouput")
setwd("ouput")
getwd()

# aogcms
AOGCM <- "CCSM"
AOGCM

# enms
for(i in 1:length(levels(po[, 1]))){ # for to each specie
  
  # variables for evaluate
  eval.Bioclim <- NULL
  eval.Gower <- NULL
  eval.Maha <- NULL
  eval.Maxent <- NULL
  eval.SVM <- NULL
  eval.names <- NULL
  
  # selecting presence and absence
  id.specie <- levels(po[, 1])[i]
  pr.specie <- po[which(po[, 1] == id.specie), 2:3]
  id.background <- sample(nrow(cs), nrow(pr.specie))
  bc.specie <- cs[id.background, ]
  
  # export background points
  fwrite(data.table(back = "background", bc.specie), "_background points.csv")
  
  
  for(r in 1:5){	# number of replicas
    ## preparing the models
    # train and test data	
    pr.sample.train <- sample(nrow(pr.specie), round(0.7 * nrow(pr.specie)))
    bc.sample.train <- sample(nrow(bc.specie), round(0.7 * nrow(bc.specie)))
    test <- na.omit(prepareData(x = en.00k, p = pr.specie[-pr.sample.train, ], b = bc.specie[-bc.sample.train, ]))
    train <- na.omit(prepareData(x = en.00k, p = pr.specie[pr.sample.train, ], b = bc.specie[bc.sample.train, ]))
    
    
    
    ### algorithms
    
    ## 1. bioclim
    # 1.1 calibration
    Bioclim <- bioclim(train[which(train[, 1] == 1), -1])	
    
    # 1.2 projection
    writeRaster(predict(en.00k, Bioclim), paste0(AOGCM, "_bioclim_00k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")	
    writeRaster(predict(en.06k, Bioclim), paste0(AOGCM, "_bioclim_06k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
    writeRaster(predict(en.21k, Bioclim), paste0(AOGCM, "_bioclim_21k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
    
    # 1.3 evaluation
    eBioclim <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Bioclim)
    idBioclim <- which(eBioclim@t == as.numeric(threshold(eBioclim, "spec_sens")))
    eval.Bioclim.sp <- c(eBioclim@t[idBioclim], eBioclim@auc, (eBioclim@TPR[idBioclim] + eBioclim@TNR[idBioclim] - 1))
    eval.Bioclim <- rbind(eval.Bioclim, eval.Bioclim.sp)
    
    # verify 
    print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Bioclim', replica ", 
                 ifelse(r < 10, paste0("0", r), r), " it's done!"))
    
    
    ## 2. gowerit
    # 2.1 calibration
    Gower <- domain(train[which(train[, 1] == 1), -1])	
    
    # 2.2 projection
    writeRaster(predict(en.00k, Gower), paste0(AOGCM, "_gower_00k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
    writeRaster(predict(en.06k, Gower), paste0(AOGCM, "_gower_06k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
    writeRaster(predict(en.21k, Gower), paste0(AOGCM, "_gower_21k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
    
    # 2.3 evaluation
    eGower <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Gower)
    idGower <- which(eGower@t == as.numeric(threshold(eGower, "spec_sens")))
    eval.Gower.sp <- c(eGower@t[idGower], eGower@auc, (eGower@TPR[idGower] + eGower@TNR[idGower] - 1))
    eval.Gower <- rbind(eval.Gower, eval.Gower.sp)
    
    # verify 
    print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Gower', replica ", 
                 ifelse(r < 10, paste0("0", r), r), " it's done!"))
    
    
    ## 3. mahalanobis	
    # 3.1 calibration
    Maha <- mahal(train[which(train[, 1] == 1), -1])	
    
    # 3.2 projection
    writeRaster(predict(en.00k, Maha), paste0(AOGCM, "_mahalanobis_00k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
    writeRaster(predict(en.06k, Maha), paste0(AOGCM, "_mahalanobis_06k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
    writeRaster(predict(en.21k, Maha), paste0(AOGCM, "_mahalanobis_21k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
    
    # 3.3 evaluation
    eMaha <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maha)
    idMaha <- which(eMaha@t == as.numeric(threshold(eMaha, "spec_sens")))
    eval.Maha.sp <- c(eMaha@t[idMaha], eMaha@auc, (eMaha@TPR[idMaha] + eMaha@TNR[idMaha] - 1))
    eval.Maha <- rbind(eval.Maha, eval.Maha.sp)
    
    # verify 
    print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Mahalanobis', replica ", 
                 ifelse(r < 10, paste0("0", r), r), " it's done!"))
    
    
    ## 4. maxent	
    # 4.1 calibration
    Maxent <- maxent(train[, -1], train[, 1])	
    
    # 4.2 projection
    writeRaster(predict(en.00k, Maxent), paste0(AOGCM, "_maxent_00k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
    writeRaster(predict(en.06k, Maxent), paste0(AOGCM, "_maxent_06k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
    writeRaster(predict(en.21k, Maxent), paste0(AOGCM, "_maxent_21k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
    
    # 4.3 evaluation
    eMaxent <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maxent)
    idMaxent <- which(eMaxent@t == as.numeric(threshold(eMaxent, "spec_sens")))
    eval.Maxent.sp <- c(eMaxent@t[idMaxent], eMaxent@auc, (eMaxent@TPR[idMaxent] + eMaxent@TNR[idMaxent] - 1))
    eval.Maxent <- rbind(eval.Maxent, eval.Maxent.sp)
    
    # verify 
    print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Maxent', replica ", 
                 ifelse(r < 10, paste0("0", r), r), " it's done!"))
    
    
    
    ## 5. svm	
    # 5.1 calibration
    SVM <- ksvm(pb ~ bio02 + bio04 + bio10 + bio16 + bio17, data = train)	
    
    # 5.2 projection
    writeRaster(predict(en.00k, SVM), paste0(AOGCM, "_svm_00k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
    writeRaster(predict(en.06k, SVM), paste0(AOGCM, "_svm_06k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
    writeRaster(predict(en.21k, SVM), paste0(AOGCM, "_svm_21k_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
    
    # 5.3 evaluation
    eSVM <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = SVM)
    idSVM <- which(eSVM@t == as.numeric(threshold(eSVM, "spec_sens")))
    eval.SVM.sp <- c(eSVM@t[idSVM], eSVM@auc, (eSVM@TPR[idSVM] + eSVM@TNR[idSVM] - 1))
    eval.SVM <- rbind(eval.SVM, eval.SVM.sp)
    
    eval.names <- c(eval.names, paste0(id.specie, ifelse(r < 10, paste0("0", r), r)))	
    
    
    # verify 
    print(paste0("Yeh! The model of ", id.specie, ", algorithm 'SVM', replica ", 
                 ifelse(r < 10, paste0("0", r), r), " it's done!"))
    
    
} # ends for "r"
  
  dimnames(eval.Bioclim) <- list(eval.names, c("thrs", "AUC", "TSS"))
  dimnames(eval.Gower) <- list(eval.names, c("thrs", "AUC", "TSS"))  
  dimnames(eval.Maha) <- list(eval.names, c("thrs", "AUC", "TSS"))  
  dimnames(eval.Maxent) <- list(eval.names, c("thrs", "AUC", "TSS"))
  dimnames(eval.SVM) <- list(eval.names, c("thrs", "AUC", "TSS"))
  
  write.table(eval.Bioclim, paste0("zEval_", AOGCM, "_bioclim_", id.specie, ".txt"))
  write.table(eval.Gower, paste0("zEval_", AOGCM, "_gower_", id.specie, ".txt"))
  write.table(eval.Maha, paste0("zEval_", AOGCM, "_mahalanobis_", id.specie, ".txt"))
  write.table(eval.Maxent, paste0("zEval_", AOGCM, "_maxent_", id.specie, ".txt"))
  write.table(eval.SVM, paste0("zEval_", AOGCM, "_svm_", id.specie, ".txt"))
  
} # ends for"i"

###----------------------------------------------------------------------------###

