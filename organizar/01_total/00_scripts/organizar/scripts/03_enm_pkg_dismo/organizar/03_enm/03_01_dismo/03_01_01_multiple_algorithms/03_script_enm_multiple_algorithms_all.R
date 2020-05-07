### script enm dismo - multiple algorithms ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 2018-05-04

###---------------------------------------------------------------------------###

# memory
rm(list = ls())
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, dismo, gam, kernlab, randomForest, gbm, rJava, vegan, 
               spocc, usdm, tidyverse, data.table, viridis, RStoolbox, RCurl)

###---------------------------------------------------------------------------###

## data

# occurrences
ha <- occ2df(occ(query = "Haddadus binotatus", 
                 from = c("gbif", "idigbio", "inat"),
                 has_coords = T))[, 1:4] %>% 
  distinct
  

barplot(table(ha$prov), col = 1:length(unique(ha$prov)))

po <- data.table(sp = sub(" ", "_", unique(tolower(ha$name))), 
                 lon = as.numeric(ha$longitude), 
                 lat = as.numeric(ha$latitude))
po

plot(po$lon, po$lat, pch = 20)



## variables
en <- getData(name = "worldclim", var = "bio", res = 10, download = TRUE)
en

plot(en[[1]], col = viridis(100))

# limite
br <- getData("GADM", country = "BRA", level = 0)
br

plot(br)

# adjust to mask
en.br <- crop(mask(en, br), br)
en.br
plot(en.br[[1]], col = viridis(100))
lines(br)

###---------------------------------------------------------------------------###

## variable selection

# vif
en.co <- vifcor(en.br[], th = .6)
en.co

# raster pca
en.pca <- rasterPCA(en.br, spca = TRUE)
en.pca

plot(en.pca$map$PC1, col = viridis(100))

su <- summary(en.pca$model)
su

# axis number
n.pca <- length(su$sdev[su$sdev > 1])
n.pca

# loadings
l.pca <- abs(round(en.pca$model$loadings[, 1:n.pca], 2))
l.pca

# selecting
va <- l.pca[row.names(l.pca) %in% en.co@results$Variables, ]
va

va.max <- row.names(va)[apply(va, 2, which.max)]
va.max 

en <- en.br[[va.max]]
en  

plot(en, col = viridis(100))

###---------------------------------------------------------------------------###

## occurrences selection

# output
setwd("E:/github_mauriciovancine/R-ENM")
dir.create("output")
setwd("output")

# one point per cell
ra <- data.table(rasterToPoints(en.br[[1]])[, 1:2])
ra <- data.table(id = 1:nrow(ra), ra)
gridded(ra) <- ~ x + y
ra.r <- raster(ra) 
crs(ra.r) <- crs(en)  
plot(ra.r, col = viridis(100))
points(po$lon, po$lat, pch = 20)

po$oppc <- raster::extract(ra.r, po[, c(2:3)])
table(po$oppc)
write.csv(po, "_po_check_oppc.csv")

po <- na.omit(distinct(po, oppc, .keep_all = TRUE))
po

plot(en[[1]], col = viridis(100))
points(po$lon, po$lat, pch = 20)


###---------------------------------------------------------------------------###

## background coordinates
bc <- rasterToPoints(en)[, 1:2]
colnames(bc[, -3]) <- c("long", "lat")

plot(en[[1]], col = viridis(100))
points(bc[sample(1:nrow(bc), 1000), ], pch = 20, cex = .5, col = "blue")
points(po[, 2:3], pch = 20, cex = .5, col = "red")

###---------------------------------------------------------------------------###

# verify maxent
file.exists(paste0(system.file(package = "dismo"), "/java/maxent.jar"))

###---------------------------------------------------------------------------###

### enms ###

# enms
for(i in 1:length(unique(po[, 1]))){ # for to each specie
  
  # graphics
  dir.create("graphics")
  
  # variables for evaluate
  eval.GLM <- NULL
  eval.GAM <- NULL
  eval.Bioclim <- NULL
  eval.Gower <- NULL
  eval.Maha <- NULL
  # eval.BRT <- NULL
  eval.Maxent <- NULL
  eval.RandomForest <- NULL
  eval.SVM <- NULL
  eval.names <- NULL
  maxent.results <- matrix()

  # selecting presence and absence
	id.specie <- as.character(unique(po[, 1]))[i]
	pr.specie <- po[which(po[, 1] == id.specie), 2:3]
	id.background <- sample(nrow(bc), nrow(pr.specie))
	bc.specie <- bc[id.background, ]
	
	# export points
	fwrite(data.table(pr.specie), paste0("_", id.specie, "_presence_points.csv"))
	fwrite(data.table(bc.specie), paste0("_", id.specie, "_background_points.csv"))
	

  for(r in 1:10){	# number of replicas
    
	  ## preparing the models
    # train and test data	
	  pr.sample.train <- sample(nrow(pr.specie), round(0.7 * nrow(pr.specie)))
	  bc.sample.train <- sample(nrow(bc.specie), round(0.7 * nrow(bc.specie)))
	  train <- na.omit(prepareData(x = en, p = pr.specie[pr.sample.train, ], b = bc.specie[bc.sample.train, ]))
  	test <- na.omit(prepareData(x = en, p = pr.specie[-pr.sample.train, ], b = bc.specie[-bc.sample.train, ]))
  	
  	
  	### algorithms ###
  	
  	## 1. glm
  	GLM <- glm(pb ~ ., family = "binomial", data = train)
  	writeRaster(predict(en, GLM), paste0("glm_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")	
  	eGLM <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = GLM)
  	idGLM <- which(eGLM@t == as.numeric(threshold(eGLM, "spec_sens")))
  	eval.GLM.sp <- c(eGLM@t[idGLM], eGLM@auc, (eGLM@TPR[idGLM] + eGLM@TNR[idGLM] - 1))
  	eval.GLM <- rbind(eval.GLM, eval.GLM.sp)
  	
  	setwd("graphics")
  	tiff(paste0("glm_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); response(GLM); dev.off()
  	tiff(paste0("glm_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eGLM, "ROC"); dev.off()
  	setwd("..")
  	
  	print(paste0("Yeh! The model of ", id.specie, ", algorithm 'GLM', replica ", 
  	             ifelse(r < 10, paste0("0", r), r), " it's done!"))
  	
  	
  	## 2. gam
  	GAM <- gam(as.formula(paste0(colnames(train)[1], "~", paste0("s(", colnames(train)[-1], ")", collapse = "+"))), 
  	           family = "binomial", data = train)
  	writeRaster(predict(en, GAM), paste0("gam_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")	
  	eGAM <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = GAM)
  	idGAM <- which(eGAM@t == as.numeric(threshold(eGAM, "spec_sens")))
  	eval.GAM.sp <- c(eGAM@t[idGAM], eGAM@auc, (eGAM@TPR[idGAM] + eGAM@TNR[idGAM] - 1))
  	eval.GAM <- rbind(eval.GAM, eval.GAM.sp)
  	
  	setwd("graphics")
  	tiff(paste0("gam_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eGAM, "ROC"); dev.off()
  	setwd("..")
  	
  	print(paste0("Yeh! The model of ", id.specie, ", algorithm 'GAM', replica ", 
  	             ifelse(r < 10, paste0("0", r), r), " it's done!"))
  	
  	
  	## 3. bioclim
  	Bioclim <- bioclim(train[which(train[, 1] == 1), -1])
  	writeRaster(predict(en, Bioclim), paste0("bioclim_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")	
  	eBioclim <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Bioclim)
  	idBioclim <- which(eBioclim@t == as.numeric(threshold(eBioclim, "spec_sens")))
  	eval.Bioclim.sp <- c(eBioclim@t[idBioclim], eBioclim@auc, (eBioclim@TPR[idBioclim] + eBioclim@TNR[idBioclim] - 1))
  	eval.Bioclim <- rbind(eval.Bioclim, eval.Bioclim.sp)
  	
  	setwd("graphics")
  	tiff(paste0("bioclim_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); response(Bioclim); dev.off()
  	tiff(paste0("bioclim_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eBioclim, "ROC"); dev.off()
  	setwd("..")
	  
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Bioclim', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))
	  

    ## 4. gower
	  Gower <- domain(train[which(train[, 1] == 1), -1])	
	  writeRaster(predict(en, Gower), paste0("gower_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
	  eGower <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Gower)
  	idGower <- which(eGower@t == as.numeric(threshold(eGower, "spec_sens")))
	  eval.Gower.sp <- c(eGower@t[idGower], eGower@auc, (eGower@TPR[idGower] + eGower@TNR[idGower] - 1))
	  eval.Gower <- rbind(eval.Gower, eval.Gower.sp)

	  setwd("graphics")
	  tiff(paste0("gower_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); response(Gower); dev.off()
	  tiff(paste0("gower_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eGower, "ROC"); dev.off()
	  setwd("..")
	  
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Gower', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))
	  
	  
	  ## 5. mahalanobis	
	  Maha <- mahal(train[which(train[, 1] == 1), -1])	
	  writeRaster(predict(en, Maha), paste0("mahalanobis_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
	  eMaha <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maha)
	  idMaha <- which(eMaha@t == as.numeric(threshold(eMaha, "spec_sens")))
	  eval.Maha.sp <- c(eMaha@t[idMaha], eMaha@auc, (eMaha@TPR[idMaha] + eMaha@TNR[idMaha] - 1))
	  eval.Maha <- rbind(eval.Maha, eval.Maha.sp)

	  setwd("graphics")
	  tiff(paste0("mahalanobis_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); response(Maha); dev.off()
	  tiff(paste0("mahalanobis_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eMaha, "ROC"); dev.off()
	  setwd("..")
	  
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Mahalanobis', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))
	  
	  
	  ## 6. brt
	  # BRT <- mahal(train[which(train[, 1] == 1), -1])	
	  # writeRaster(predict(en, BRT), paste0("brt_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
	  # eBRT <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = BRT)
	  # idBRT <- which(eBRT@t == as.numeric(threshold(eBRT, "spec_sens")))
	  # eval.BRT.sp <- c(eBRT@t[idBRT], eBRT@auc, (eBRT@TPR[idBRT] + eBRT@TNR[idBRT] - 1))
	  # eval.BRT <- rbind(eval.BRT, eval.BRT.sp)
	  # 
	  # setwd("graphics")
	  # tiff(paste0("brt_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); response(BRT); dev.off()
	  # tiff(paste0("brt_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eBRT, "ROC"); dev.off()
	  # setwd("..")
	  # 
	  # print(paste0("Yeh! The model of ", id.specie, ", algorithm 'BRT', replica ", 
	  #              ifelse(r < 10, paste0("0", r), r), " it's done!"))
	  

	 
	  ## 7. random forest
	  RandomForest <- randomForest(pb ~ ., data = train)
	  writeRaster(predict(en, RandomForest), paste0("randomforest_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
	  eRandomForest <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = RandomForest)
	  idRandomForest <- which(eRandomForest@t == as.numeric(threshold(eRandomForest, "spec_sens")))
	  eval.RandomForest.sp <- c(eRandomForest@t[idRandomForest], eRandomForest@auc, (eRandomForest@TPR[idRandomForest] + eRandomForest@TNR[idRandomForest] - 1))
	  eval.RandomForest <- rbind(eval.RandomForest, eval.RandomForest.sp)
	  
	  setwd("graphics")
	  tiff(paste0("random_forest_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eRandomForest, "ROC"); dev.off()
	  setwd("..")
	  
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Random Forest', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))
	  
	  
	  ## 8. maxent	
	  Maxent <- maxent(train[, -1], train[, 1])	
	  writeRaster(predict(en, Maxent), paste0("maxent_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
	  eMaxent <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maxent)
	  idMaxent <- which(eMaxent@t == as.numeric(threshold(eMaxent, "spec_sens")))
	  eval.Maxent.sp <- c(eMaxent@t[idMaxent], eMaxent@auc, (eMaxent@TPR[idMaxent] + eMaxent@TNR[idMaxent] - 1))
	  eval.Maxent <- rbind(eval.Maxent, eval.Maxent.sp)
	  
	  setwd("graphics")
	  tiff(paste0("maxent_response_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); response(Maxent); dev.off()
	  tiff(paste0("maxent_contribution_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(Maxent); dev.off()
	  tiff(paste0("maxent_auc_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eMaxent, "ROC"); dev.off()
	  maxent.results <- data.table(maxent.results, as.matrix(Maxent@results[1:50]))
	  setwd("..")
	  
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Maxent', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))
	        
	        
	  ## 9. svm	
	  SVM <- ksvm(pb ~ ., data = train)
	  writeRaster(predict(en, SVM), paste0("svm_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
	  eSVM <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = SVM)
	  idSVM <- which(eSVM@t == as.numeric(threshold(eSVM, "spec_sens")))
	  eval.SVM.sp <- c(eSVM@t[idSVM], eSVM@auc, (eSVM@TPR[idSVM] + eSVM@TNR[idSVM] - 1))
	  eval.SVM <- rbind(eval.SVM, eval.SVM.sp)
	  
	  setwd("graphics")
	  tiff(paste0("svm_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eSVM, "ROC"); dev.off()
	  setwd("..")
	  
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'SVM', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))
	  

	  eval.names <- c(eval.names, paste0(id.specie, ifelse(r < 10, paste0("0", r), r)))	
	  
  } # ends for "r"
	
	# maxent results
	setwd("graphics")
	na <- attributes(Maxent@results)[[2]][[1]]
	maxent.results <- data.table(na, maxent.results[, -1])
	colnames(maxent.results) <- c("names", paste0("rep", 1:r))
	fwrite(maxent.results, paste0("_maxent_results", id.specie, ".csv"))
	setwd("..")
	
	# evaluations
  dimnames(eval.GLM) <- list(eval.names, c("thrs", "AUC", "TSS"))
  dimnames(eval.GAM) <- list(eval.names, c("thrs", "AUC", "TSS"))
  dimnames(eval.Bioclim) <- list(eval.names, c("thrs", "AUC", "TSS"))
  dimnames(eval.Gower) <- list(eval.names, c("thrs", "AUC", "TSS"))  
  dimnames(eval.Maha) <- list(eval.names, c("thrs", "AUC", "TSS"))  
  # dimnames(eval.BRT) <- list(eval.names, c("thrs", "AUC", "TSS"))
  dimnames(eval.RandomForest) <- list(eval.names, c("thrs", "AUC", "TSS"))
  dimnames(eval.Maxent) <- list(eval.names, c("thrs", "AUC", "TSS"))
  dimnames(eval.SVM) <- list(eval.names, c("thrs", "AUC", "TSS"))
  
  
  write.table(eval.GLM, paste0("zEval_", "glm_", id.specie, ".txt"))
  write.table(eval.GAM, paste0("zEval_", "gam_", id.specie, ".txt"))
  write.table(eval.Bioclim, paste0("zEval_", "bioclim_", id.specie, ".txt"))
  write.table(eval.Gower, paste0("zEval_", "gower_", id.specie, ".txt"))
  write.table(eval.Maha, paste0("zEval_", "mahalanobis_", id.specie, ".txt"))
  # write.table(eval.BRT, paste0("zEval_", "brt_", id.specie, ".txt"))
  write.table(eval.RandomForest, paste0("zEval_", "randomforest_", id.specie, ".txt"))
  write.table(eval.Maxent, paste0("zEval_", "maxent_", id.specie, ".txt"))
  write.table(eval.SVM, paste0("zEval_", "svm_", id.specie, ".txt"))

} # ends for"i"

###----------------------------------------------------------------------------###


#Euclidean <- vegdist(decostand(train[which(train[, 1] == 1), -1], "norm"), 
 #                    method = "euclidean")
