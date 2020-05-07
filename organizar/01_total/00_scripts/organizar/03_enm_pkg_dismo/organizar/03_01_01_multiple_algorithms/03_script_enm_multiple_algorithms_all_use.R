### script enm dismo - multiple algorithms use ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 2018-10-27

###---------------------------------------------------------------------------###

# memory
rm(list = ls())

# packages
library(sp)
library(raster)
library(rgdal)
library(spocc)
library(usdm)
library(dismo)
library(randomForest)
library(kernlab)
library(rJava)
library(viridis)
library(tidyverse)

###---------------------------------------------------------------------------###

## data

# occurrences
setwd("")
po <- readr::read_csv("00_occ.csv")
po

po <- spocc::occ(query = "Haddadus binotatus", date = c("1990-01-01", "2018-10-10"), has_coords = TRUE) %>% 
  spocc::occ2df() %>% 
  dplyr::distinct(name, longitude, latitude)

plot(po$longitude, po$latitude, main = unique(po$name), pch = 20)

# variables
setwd("")
en <- raster::stack(dir(patt = ".tif"))
en

en <- raster::getData("worldclim", var = "bio", res = 10, download = FALSE)
en

br <- raster::getData("GADM", country = "BRA", level = 0, download = FALSE)
br

en <- raster::mask(raster::crop(en, br), br)

raster::plot(en[[1]], col = viridis::viridis(100))
points(po[, 2:3], pch = 20, col = "red")

###---------------------------------------------------------------------------###

# pre-analysis

# one point per cell
ra <- tibble::tibble(id = seq_len(nrow(raster::rasterToPoints(en[[1]]))), 
                     x = raster::rasterToPoints(en[[1]])[, 1],
                     y = raster::rasterToPoints(en[[1]])[, 2])
sp::gridded(ra) <- ~ x + y
ra.r <- raster::raster(ra) 
raster::crs(ra.r) <- raster::crs(en)  
raster::plot(ra.r, col = viridis::viridis(100))
points(po[, 2:3], pch = 21)

po$oppc <- raster::extract(ra.r, po[, c(2:3)])
table(po$oppc)

setwd("..")
readr::write_csv(po, "./occ/00_occ_check_oppc.csv")
setwd("..")

po <- po %>% 
  dplyr::distinct(oppc, .keep_all = TRUE) %>% 
  na.omit
po

raster::plot(en[[1]], col = viridis::viridis(100))
points(po[, 2:3], pch = 21)

# variables selecion
vi <- en %>% 
  raster::values() %>% 
  na.omit %>% 
  usdm::vifcor(th = .7, maxobservations = 10000)
vi

en <- en[[as.character(vi@results$Variables)]]
en

plot(en, col = viridis::viridis(100))

###---------------------------------------------------------------------------###

## background coordinates
bc <- tibble::as.tibble(raster::rasterToPoints(en)[, 1:2])
colnames(bc) <- c("long", "lat")

plot(en[[1]], col = viridis::viridis(100))
points(bc[sample(1:nrow(bc), 1000), ], pch = 20, cex = .5, col = "blue")
points(po[, 2:3], pch = 20, cex = .5, col = "red")

###---------------------------------------------------------------------------###

# verify maxent
file.exists(paste0(system.file(package = "dismo"), "/java/maxent.jar"))

###---------------------------------------------------------------------------###

### enms ###

# directory
setwd("")
dir.create("02_enm")
setwd("02_enm")

for(i in 1:length(unique(occ[, 1]))){ # for to each specie
  
  # graphics
  dir.create("graphics")
  
  # variables for evaluate
  eval.Bioclim <- NULL
  eval.Gower <- NULL
  eval.GLM <- NULL
  eval.RandomForest <- NULL
  eval.Maxent <- NULL
  eval.SVM <- NULL
  eval.names <- NULL
  maxent.results <- matrix()
  
  # selecting presence and absence
  id.specie <- as.character(unique(occ[, 1]))[i]
  pr.specie <- occ[which(occ[, 1] == id.specie), 2:3]
  id.background <- sample(nrow(bc), nrow(pr.specie))
  bc.specie <- bc[id.background, ]
  
  # export points
  readr::write_csv(tibble::tibble(lon = pr.specie[[1]], lat = pr.specie[[2]]), 
                   paste0("00_", id.specie, "_presence_points.csv"))
  readr::write_csv(tibble::tibble(lon = bc.specie[[1]], lat = bc.specie[[2]]), 
                   paste0("01_", id.specie, "_background_points.csv"))
  
  
  for(r in 1:10){	# number of replicas
    
    ## preparing the models
    # train and test data	
    pr.sample.train <- sample(nrow(pr.specie), round(0.7 * nrow(pr.specie)))
    bc.sample.train <- sample(nrow(bc.specie), round(0.7 * nrow(bc.specie)))
    train <- na.omit(dismo::prepareData(x = var, p = pr.specie[pr.sample.train, ], b = bc.specie[bc.sample.train, ]))
    test <- na.omit(dismo::prepareData(x = var, p = pr.specie[-pr.sample.train, ], b = bc.specie[-bc.sample.train, ]))
    
    
    ### algorithms ###
    
    ## 1. bioclim
    print(paste(id.specie, "Bioclim", ifelse(r < 10, paste0("0", r), r)))
    Bioclim <- dismo::bioclim(train[which(train[, 1] == 1), -1])
    raster::writeRaster(dismo::predict(var, Bioclim, progress = "text"), paste0("bioclim_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), options = c("COMPRESS=DEFLATE"), format = "GTiff")
    eBioclim <- dismo::evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Bioclim)
    idBioclim <- which(eBioclim@t == as.numeric(threshold(eBioclim, "spec_sens")))
    eval.Bioclim.sp <- c(id.specie, ifelse(r < 10, paste0("0", r), r), "bioclim", eBioclim@t[idBioclim], eBioclim@auc, (eBioclim@TPR[idBioclim] + eBioclim@TNR[idBioclim] - 1))
    eval.Bioclim <- rbind(eval.Bioclim, eval.Bioclim.sp)
    
    setwd("graphics")
    tiff(paste0("bioclim_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); dismo::response(Bioclim); dev.off()
    tiff(paste0("bioclim_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eBioclim, "ROC"); dev.off()
    setwd("..")
    
    
    ## 2. gower
    print(paste(id.specie, "Gower", ifelse(r < 10, paste0("0", r), r)))
    Gower <- dismo::domain(train[which(train[, 1] == 1), -1])	
    raster::writeRaster(dismo::predict(var, Gower, progress = "text"), paste0("gower_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), options = c("COMPRESS=DEFLATE"), format = "GTiff") 
    eGower <- dismo::evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Gower)
    idGower <- which(eGower@t == as.numeric(threshold(eGower, "spec_sens")))
    eval.Gower.sp <- c(id.specie, ifelse(r < 10, paste0("0", r), r), "gower", eGower@t[idGower], eGower@auc, (eGower@TPR[idGower] + eGower@TNR[idGower] - 1))
    eval.Gower <- rbind(eval.Gower, eval.Gower.sp)
    
    setwd("graphics")
    tiff(paste0("gower_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); dismo::response(Gower); dev.off()
    tiff(paste0("gower_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eGower, "ROC"); dev.off()
    setwd("..")
    
    
    ## 3. glm
    print(paste(id.specie, "GLM", ifelse(r < 10, paste0("0", r), r)))
    GLM <- glm(pb ~ ., data = train)	
    raster::writeRaster(dismo::predict(var, GLM, progress = "text"), paste0("glm_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), options = c("COMPRESS=DEFLATE"), format = "GTiff") 
    eGLM <- dismo::evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = GLM)
    idGLM <- which(eGLM@t == as.numeric(threshold(eGLM, "spec_sens")))
    eval.GLM.sp <- c(id.specie, ifelse(r < 10, paste0("0", r), r), "glm", eGLM@t[idGLM], eGLM@auc, (eGLM@TPR[idGLM] + eGLM@TNR[idGLM] - 1))
    eval.GLM <- rbind(eval.GLM, eval.GLM.sp)
    
    setwd("graphics")
    tiff(paste0("glm_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); dismo::response(GLM); dev.off()
    tiff(paste0("glm_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eGLM, "ROC"); dev.off()
    setwd("..")
    
    
    ## 4. random forest
    print(paste(id.specie, "Random Forest", ifelse(r < 10, paste0("0", r), r)))
    RandomForest <- randomForest::randomForest(pb ~ ., data = train)
    writeRaster(dismo::predict(var, RandomForest, progress = "text"), paste0("randomforest_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), options = c("COMPRESS=DEFLATE"), format = "GTiff") 
    eRandomForest <- dismo::evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = RandomForest)
    idRandomForest <- which(eRandomForest@t == as.numeric(threshold(eRandomForest, "spec_sens")))
    eval.RandomForest.sp <- c(id.specie, ifelse(r < 10, paste0("0", r), r), "random_forest", eRandomForest@t[idRandomForest], eRandomForest@auc, (eRandomForest@TPR[idRandomForest] + eRandomForest@TNR[idRandomForest] - 1))
    eval.RandomForest <- rbind(eval.RandomForest, eval.RandomForest.sp)
    
    setwd("graphics")
    tiff(paste0("random_forest_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eRandomForest, "ROC"); dev.off()
    setwd("..")
    
    
    ## 5. maxent	
    print(paste(id.specie, "Maxent", ifelse(r < 10, paste0("0", r), r)))
    Maxent <- dismo::maxent(train[, -1], train[, 1])	
    raster::writeRaster(dismo::predict(var, Maxent, progress = "text"), paste0("maxent_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), options = c("COMPRESS=DEFLATE"), format = "GTiff") 
    eMaxent <- dismo::evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maxent)
    idMaxent <- which(eMaxent@t == as.numeric(threshold(eMaxent, "spec_sens")))
    eval.Maxent.sp <- c(id.specie, ifelse(r < 10, paste0("0", r), r), "maxent", eMaxent@t[idMaxent], eMaxent@auc, (eMaxent@TPR[idMaxent] + eMaxent@TNR[idMaxent] - 1))
    eval.Maxent <- rbind(eval.Maxent, eval.Maxent.sp)
    
    setwd("graphics")
    tiff(paste0("maxent_response_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); dismo::response(Maxent); dev.off()
    tiff(paste0("maxent_contribution_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(Maxent); dev.off()
    tiff(paste0("maxent_auc_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eMaxent, "ROC"); dev.off()
    maxent.results <- tibble::as.tibble(data.frame(maxent.results, as.matrix(Maxent@results)))
    setwd("..")
    
    
    ## 6. svm	
    print(paste(id.specie, "SVM", ifelse(r < 10, paste0("0", r), r)))
    SVM <- kernlab::ksvm(pb ~ ., data = train)
    raster::writeRaster(dismo::predict(var, SVM, progress = "text"), paste0("svm_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), options = c("COMPRESS=DEFLATE"), format = "GTiff") 
    eSVM <- dismo::evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = SVM)
    idSVM <- which(eSVM@t == as.numeric(threshold(eSVM, "spec_sens")))
    eval.SVM.sp <- c(id.specie, ifelse(r < 10, paste0("0", r), r), "svm", eSVM@t[idSVM], eSVM@auc, (eSVM@TPR[idSVM] + eSVM@TNR[idSVM] - 1))
    eval.SVM <- rbind(eval.SVM, eval.SVM.sp)
    
    setwd("graphics")
    tiff(paste0("svm_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eSVM, "ROC"); dev.off()
    setwd("..")
    
    
    eval.names <- c(eval.names, paste0(id.specie, ifelse(r < 10, paste0("0", r), r)))	
    
  } # ends for "r"
  
  # maxent results
  setwd("graphics")
  na <- attributes(Maxent@results)[[2]][[1]]
  maxent.results <- tibble::as.tibble(data.frame(na, maxent.results[, -1]))
  colnames(maxent.results) <- c("names", paste0("rep", 1:r))
  readr::write_csv(maxent.results, paste0("_maxent_results", id.specie, ".csv"))
  setwd("..")
  
  # evaluations
  dimnames(eval.Bioclim) <- list(eval.names, c("species", "replica", "algorithm", "thrs", "AUC", "TSS"))
  dimnames(eval.Gower) <- list(eval.names, c("species", "replica", "algorithm", "thrs", "AUC", "TSS"))  
  dimnames(eval.GLM) <- list(eval.names, c("species", "replica", "algorithm", "thrs", "AUC", "TSS"))  
  dimnames(eval.RandomForest) <- list(eval.names, c("species", "replica", "algorithm", "thrs", "AUC", "TSS"))
  dimnames(eval.Maxent) <- list(eval.names, c("species", "replica", "algorithm", "thrs", "AUC", "TSS"))
  dimnames(eval.SVM) <- list(eval.names, c("species", "replica", "algorithm", "thrs", "AUC", "TSS"))
  
  write.csv(eval.Bioclim, paste0("zEval_", "bioclim_", id.specie, ".csv"))
  write.csv(eval.Gower, paste0("zEval_", "gower_", id.specie, ".csv"))
  write.csv(eval.GLM, paste0("zEval_", "glm_", id.specie, ".csv"))
  write.csv(eval.RandomForest, paste0("zEval_", "randomforest_", id.specie, ".csv"))
  write.csv(eval.Maxent, paste0("zEval_", "maxent_", id.specie, ".csv"))
  write.csv(eval.SVM, paste0("zEval_", "svm_", id.specie, ".csv"))
  
} # ends for"i"


###----------------------------------------------------------------------------###

