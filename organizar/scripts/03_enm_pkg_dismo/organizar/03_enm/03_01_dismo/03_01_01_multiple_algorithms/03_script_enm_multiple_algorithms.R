### script enm dismo - multiple algorithms ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 01/02/2018

###---------------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, dismo, kernlab, rJava, vegan, dplyr,
	      data.table, spocc, ggplot2, usdm, viridis, RCurl, RStoolbox)

# temp
setwd("E:/github_mauriciovancine/R-ENM/data")
dir.create("temp")
tempdir <- function() "E:/github_mauriciovancine/R-ENM/data/temp"
unlockBinding("tempdir", baseenv())
assignInNamespace("tempdir", tempdir, ns = "base", envir = baseenv())
assign("tempdir", tempdir, baseenv())
lockBinding("tempdir", baseenv())
tempdir()

# functions
eval(parse(text = getURL(
  "https://gist.githubusercontent.com/mauriciovancine/498e34ddb89f0a7ecf991174cb9893d0/raw/900ecb7c4604fb4604c2df364a441e279582f695/script_%2520eliminate_biases_environmental_space.R",
  ssl.verifypeer = F)))


###---------------------------------------------------------------------------###

## data

## occurrences
ha <- distinct(occ2df(occ(query = "Haddadus binotatus", 
                          from = c("gbif", "idigbio", "inat"),
                          has_coords = T))[, 1:4])

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

# resampling
en.re <- aggregate(en, fact = 6, fun = "mean", expand = TRUE)
en.re
plot(en.re[[1]], col = viridis(100))

# limite
br <- getData("GADM", country = "BRA", level = 0)
br

# adjust to mask
en.br <- crop(mask(en.re, br), br)
en.br
plot(en.br[[1]], col = viridis(100))

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

# biases in the environmental space
da <- data.table(extract(en.pca$map, po[, 2:3]))
da

co <- na.omit(data.table(po, da))[, 2:3]
co

da <- na.omit(da)
da


range(da$PC1)
range(da$PC2)

# selection
r <- 1

po.se <- data.frame(sp = paste0(unique(po$sp), "_all"),
                 envSample(coord = co, 
                           filters = list(da$PC1, da$PC2), 
                           res = list(diff(range(da$PC1)) / r, diff(range(da$PC2)) / r), 
                           do.plot = TRUE))

po <- as.data.frame(rbind(po, po.all, po.part))
po


###---------------------------------------------------------------------------###

## background coordinates
bc <- rasterToPoints(en)[, 1:2]
colnames(bc[, -3]) <- c("long", "lat")

plot(en[[1]], col = viridis(100))
points(bc, pch = 20, cex = .5, col = "blue")
points(po[, 2:3], pch = 20, cex = .5, col = as.factor(po$sp))

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
  eval.Bioclim <- NULL
  eval.Gower <- NULL
  eval.Maha <- NULL
  eval.Maxent <- NULL
  eval.SVM <- NULL
  eval.names <- NULL
  max.res <- matrix()

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

    ### algorithms
  	
    ## 1. bioclim
  	# 1.1 calibration
	  Bioclim <- bioclim(train[which(train[, 1] == 1), -1])
	  
	  # 1.2 projection
    writeRaster(predict(en, Bioclim), paste0("bioclim_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")	
   
    # 1.3 evaluation
	  eBioclim <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Bioclim)
	  idBioclim <- which(eBioclim@t == as.numeric(threshold(eBioclim, "spec_sens")))
	  eval.Bioclim.sp <- c(eBioclim@t[idBioclim], eBioclim@auc, (eBioclim@TPR[idBioclim] + eBioclim@TNR[idBioclim] - 1))
	  eval.Bioclim <- rbind(eval.Bioclim, eval.Bioclim.sp)

	  # 1.4 graphics
	  setwd("graphics")
	  tiff(paste0("bioclim_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); response(Bioclim); dev.off()
	  tiff(paste0("bioclim_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eBioclim, "ROC"); dev.off()
	  setwd("..")
	  
	  # verify 
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Bioclim', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))
	  
	  
    ## 2. gower
	  # 2.1 calibration
	  Gower <- domain(train[which(train[, 1] == 1), -1])	
	  
	  # 2.2 projection
    writeRaster(predict(en, Gower), paste0("gower_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 

    # 2.3 evaluation
	  eGower <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Gower)
  	idGower <- which(eGower@t == as.numeric(threshold(eGower, "spec_sens")))
	  eval.Gower.sp <- c(eGower@t[idGower], eGower@auc, (eGower@TPR[idGower] + eGower@TNR[idGower] - 1))
	  eval.Gower <- rbind(eval.Gower, eval.Gower.sp)

	  # 2.4 graphics
	  setwd("graphics")
	  tiff(paste0("gower_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); response(Gower); dev.off()
	  tiff(paste0("gower_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eGower, "ROC"); dev.off()
	  setwd("..")
	  
	  # verify 
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Gower', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))
	  
	  
    ## 3. mahalanobis	
	  # 3.1 calibration
	  Maha <- mahal(train[which(train[, 1] == 1), -1])	
	
	  # 3.2 projection
    writeRaster(predict(en, Maha), paste0("mahalanobis_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 

    # 3.3 evaluation
	  eMaha <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maha)
	  idMaha <- which(eMaha@t == as.numeric(threshold(eMaha, "spec_sens")))
	  eval.Maha.sp <- c(eMaha@t[idMaha], eMaha@auc, (eMaha@TPR[idMaha] + eMaha@TNR[idMaha] - 1))
	  eval.Maha <- rbind(eval.Maha, eval.Maha.sp)
	
	  # 3.4 graphics
	  setwd("graphics")
	  tiff(paste0("mahalanobis_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); response(Maha); dev.off()
	  tiff(paste0("mahalanobis_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eMaha, "ROC"); dev.off()
	  setwd("..")
	  
	  
	  # verify 
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Mahalanobis', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))

    ## 4. maxent	
	  # 4.1 calibration
	  Maxent <- maxent(train[, -1], train[, 1])	

	  # 4.2 projection
    writeRaster(predict(en, Maxent), paste0("maxent_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 

    # 4.3 evaluation
	  eMaxent <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maxent)
	  idMaxent <- which(eMaxent@t == as.numeric(threshold(eMaxent, "spec_sens")))
	  eval.Maxent.sp <- c(eMaxent@t[idMaxent], eMaxent@auc, (eMaxent@TPR[idMaxent] + eMaxent@TNR[idMaxent] - 1))
	  eval.Maxent <- rbind(eval.Maxent, eval.Maxent.sp)
	  
	  # 4.4 grapihcs
	  setwd("graphics")
	  tiff(paste0("maxent_response_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); response(Maxent); dev.off()
	  tiff(paste0("maxent_contribution_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(Maxent); dev.off()
	  tiff(paste0("maxent_auc_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eMaxent, "ROC"); dev.off()
	  max.res <- data.table(max.res, as.matrix(Maxent@results[1:50]))
	  setwd("..")
	  
    # verify 
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Maxent', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))

    ## 5. svm	
	  # 5.1 calibration
	  SVM <- ksvm(pb ~ ., data = train)
	  
	  # 5.2 projection
    writeRaster(predict(en, SVM), paste0("svm_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 

    # 5.3 evaluation
	  eSVM <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = SVM)
	  idSVM <- which(eSVM@t == as.numeric(threshold(eSVM, "spec_sens")))
	  eval.SVM.sp <- c(eSVM@t[idSVM], eSVM@auc, (eSVM@TPR[idSVM] + eSVM@TNR[idSVM] - 1))
	  eval.SVM <- rbind(eval.SVM, eval.SVM.sp)
	  
	  # 5.4 graphics
	  setwd("graphics")
	  tiff(paste0("svm_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eSVM, "ROC"); dev.off()
	  setwd("..")
	  
	  
	  # verify 
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'SVM', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))
	  

	  eval.names <- c(eval.names, paste0(id.specie, ifelse(r < 10, paste0("0", r), r)))	
	  
  } # ends for "r"
	
	# maxent results
	setwd("graphics")
	na <- attributes(Maxent@results)[[2]][[1]]
	max.res <- data.table(na, max.res[, -1])
	colnames(max.res) <- c("names", paste0("rep", 1:r))
	fwrite(max.res, paste0("_maxent_results", id.specie, ".csv"))
  setwd("..")
	
	# evaluations
  dimnames(eval.Bioclim) <- list(eval.names, c("thrs", "AUC", "TSS"))
  dimnames(eval.Gower) <- list(eval.names, c("thrs", "AUC", "TSS"))  
  dimnames(eval.Maha) <- list(eval.names, c("thrs", "AUC", "TSS"))  
  dimnames(eval.Maxent) <- list(eval.names, c("thrs", "AUC", "TSS"))
  dimnames(eval.SVM) <- list(eval.names, c("thrs", "AUC", "TSS"))

  write.table(eval.Bioclim, paste0("zEval_", "bioclim_", id.specie, ".txt"))
  write.table(eval.Gower, paste0("zEval_", "gower_", id.specie, ".txt"))
  write.table(eval.Maha, paste0("zEval_", "mahalanobis_", id.specie, ".txt"))
  write.table(eval.Maxent, paste0("zEval_", "maxent_", id.specie, ".txt"))
  write.table(eval.SVM, paste0("zEval_", "svm_", id.specie, ".txt"))

} # ends for"i"

###----------------------------------------------------------------------------###

