### script enm projections past ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 05/06/2017

###---------------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, dismo, gam, randomForest, kernlab, rJava, vegan, colorRamps,
               data.table, dplyr, colorRamps, spocc, ggplot2, RCurl, usdm, viridis)
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
en.co <- vifcor(en.br[[1:19]], th = .7)
en.co

en.pca <- prcomp(na.omit(en.br[[1:19]][]), scale = T)
en.pca

su <- summary(en.pca)
su

n.pca <- length(su$sdev[su$sdev > 1])
n.pca

l.pca <- abs(round(en.pca$rotation[, 1:n.pca], 2))
l.pca

va <- l.pca[row.names(l.pca) %in% en.co@results$Variables, ]
va

va.max <- row.names(va)[apply(va, 2, which.max)]
va.max 


# selection
en.p <- en.br[[1:19]][[va.max]]
en.p

en.45.50 <- en.br[[grep("45bi50", names(en.br))]]
names(en.45.50) <-  names(en.br[[1:19]])
en.45.50 <- en.45.50[[va.max]]
en.45.50

en.45.70 <- en.br[[grep("45bi70", names(en.br))]]
names(en.45.70) <-  names(en.br[[1:19]])
en.45.70 <- en.45.70[[va.max]]
en.45.70

en.85.50 <- en.br[[grep("85bi50", names(en.br))]]
names(en.85.50) <-  names(en.br[[1:19]])
en.85.50 <- en.85.50[[va.max]]
en.85.50

en.85.70 <- en.br[[grep("85bi70", names(en.br))]]
names(en.85.70) <-  names(en.br[[1:19]])
en.85.70 <- en.85.70[[va.max]]
en.85.70


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

# copy maxent.jar (https://biodiversityinformatics.amnh.org/open_source/maxent/) in 
# the folder "C:\Users\john01\Documents\R\win-library\3.4\dismo\java"

file.exists(paste0(system.file(package = "dismo"), "/java/maxent.jar"))

###---------------------------------------------------------------------------###

### ENMs ###

# diretory
setwd("..")
getwd()
dir.create("output_future")
setwd("output_future")
getwd()

# export occurrences
fwrite(po, "_occurrences_points.csv")

# aogcms
AOGCM <- "ACCESS"
AOGCM

# enms
for(i in 1:length(unique(po[, 1]))){ # for to each specie
  
  # variables for evaluate
  eval.Bioclim <- NULL
  eval.Gower <- NULL
  eval.Maha <- NULL
  eval.Maxent <- NULL
  eval.SVM <- NULL
  eval.names <- NULL

  # selecting presence and absence
	id.specie <- as.character(unique((po[, 1])))[i]
	pr.specie <- po[which(po[, 1] == id.specie), 2:3]
	id.background <- sample(nrow(bc), nrow(pr.specie))
	bc.specie <- bc[id.background, ]
	
	# export background points
	fwrite(data.table(back = "background", bc.specie), "_background_points.csv")
	

  for(r in 1:5){   # number of replicas
    
	  ## preparing the models
    # train and test data	
	  pr.sample.train <- sample(nrow(pr.specie), round(0.7 * nrow(pr.specie)))
	  bc.sample.train <- sample(nrow(bc.specie), round(0.7 * nrow(bc.specie)))
	  test <- na.omit(prepareData(x = en.p, p = pr.specie[-pr.sample.train, ], b = bc.specie[-bc.sample.train, ]))
  	train <- na.omit(prepareData(x = en.p, p = pr.specie[pr.sample.train, ], b = bc.specie[bc.sample.train, ]))


  	
    ### algorithms
  	
  	
    ## 1. bioclim
  	
  	# verify 
  	print(paste0("The model of ", id.specie, ", algorithm 'Bioclim', replica ", 
  	             ifelse(r < 10, paste0("0", r), r), " started!"))	
  	
  	# 1.1 calibration
	  Bioclim <- bioclim(train[which(train[, 1] == 1), -1])	
	 
	  # 1.2 projection
    writeRaster(predict(en.p, Bioclim), paste0(AOGCM, "_bioclim_pres_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")	
    writeRaster(predict(en.45.50, Bioclim), paste0(AOGCM, "_bioclim_rcp45_2050_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
    writeRaster(predict(en.85.50, Bioclim), paste0(AOGCM, "_bioclim_rcp85_2050_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
    writeRaster(predict(en.45.70, Bioclim), paste0(AOGCM, "-bioclim_rcp45_2070_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
    writeRaster(predict(en.85.70, Bioclim), paste0(AOGCM, "_bioclim_rcp85_2070_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
    
    # 1.3 evaluation
	  eBioclim <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Bioclim)
	  idBioclim <- which(eBioclim@t == as.numeric(threshold(eBioclim, "spec_sens")))
	  eval.Bioclim.sp <- c(eBioclim@t[idBioclim], eBioclim@auc, (eBioclim@TPR[idBioclim] + eBioclim@TNR[idBioclim] - 1))
	  eval.Bioclim <- rbind(eval.Bioclim, eval.Bioclim.sp)

	  # verify 
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Bioclim', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))  
	  
	  
	  
    ## 2. gower
	  
	  # verify 
	  print(paste0("The model of ", id.specie, ", algorithm 'Gower', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " started!"))	
	  
	  # 2.1 calibration
	  Gower <- domain(train[which(train[, 1] == 1), -1])	

	  # 2.2 projection
	  writeRaster(predict(en.p, Gower), paste0(AOGCM, "_gower_pres_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.45.50, Gower), paste0(AOGCM, "_gower_rcp45_2050_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.45.70, Gower), paste0(AOGCM, "_gower_rcp85_2050_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.85.50, Gower), paste0(AOGCM, "_gower_rcp45_2070_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.85.70, Gower), paste0(AOGCM, "_gower_rcp85_2070_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  
    # 2.3 evaluation
	  eGower <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Gower)
  	idGower <- which(eGower@t == as.numeric(threshold(eGower, "spec_sens")))
	  eval.Gower.sp <- c(eGower@t[idGower], eGower@auc, (eGower@TPR[idGower] + eGower@TNR[idGower] - 1))
	  eval.Gower <- rbind(eval.Gower, eval.Gower.sp)
	  
	  # verify 
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Gower', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))  
	  

	  
    ## 3. mahalanobis	
	  
	  # verify 
	  print(paste0("The model of ", id.specie, ", algorithm 'Mahalanobis', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " started!"))	
	  
	  # 3.1 calibration
	  Maha <- mahal(train[which(train[, 1] == 1), -1])	
	
	  # 3.2 projection
	  writeRaster(predict(en.p, Maha), paste0(AOGCM, "_maha_pres_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.45.50, Maha), paste0(AOGCM, "_maha_rcp45_2050_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.45.70, Maha), paste0(AOGCM, "_maha_rcp85_2050_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.85.50, Maha), paste0(AOGCM, "_maha_rcp45_2070_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.85.70, Maha), paste0(AOGCM, "_maha_rcp85_2070_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  
    # 3.3 evaluation
	  eMaha <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maha)
	  idMaha <- which(eMaha@t == as.numeric(threshold(eMaha, "spec_sens")))
	  eval.Maha.sp <- c(eMaha@t[idMaha], eMaha@auc, (eMaha@TPR[idMaha] + eMaha@TNR[idMaha] - 1))
	  eval.Maha <- rbind(eval.Maha, eval.Maha.sp)
	
	  # verify
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Mahalanobis', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))  

	  
	  
    ## 4. maxent	
	  
	  # verify 
	  print(paste0("The model of ", id.specie, ", algorithm 'Maxent', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " started!"))	
	  
	  # 4.1 calibration
	  Maxent <- maxent(train[, -1], train[, 1])	

	  # 4.2 projection
	  writeRaster(predict(en.p, Maxent), paste0(AOGCM, "_maxent_pres_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.45.50, Maxent), paste0(AOGCM, "_maxent_rcp45_2050_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.45.70, Maxent), paste0(AOGCM, "_maxent_rcp85_2050_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.85.50, Maxent), paste0(AOGCM, "_maxent_rcp45_2070_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.85.70, Maxent), paste0(AOGCM, "_maxent_rcp85_2070_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  
    # 4.3 evaluation
	  eMaxent <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maxent)
	  idMaxent <- which(eMaxent@t == as.numeric(threshold(eMaxent, "spec_sens")))
	  eval.Maxent.sp <- c(eMaxent@t[idMaxent], eMaxent@auc, (eMaxent@TPR[idMaxent] + eMaxent@TNR[idMaxent] - 1))
	  eval.Maxent <- rbind(eval.Maxent, eval.Maxent.sp)

	  # verify 
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Maxent', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))  
	  
	  
	  
    ## 5. svm	
	  
	  # verify 
	  print(paste0("The model of ", id.specie, ", algorithm 'SVM', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " started!"))	
	  
	  # 5.1 calibration
	  SVM <- ksvm(pb ~ bio2 + bio4 + bio14, data = train)	

	  # 5.2 projection
	  writeRaster(predict(en.p, SVM), paste0(AOGCM, "_svm_pres_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.45.50, SVM), paste0(AOGCM, "_svm_rcp45_2050_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.45.70, SVM), paste0(AOGCM, "_svm_rcp85_2050_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.85.50, SVM), paste0(AOGCM, "_svm_rcp45_2070_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  writeRaster(predict(en.85.70, SVM), paste0(AOGCM, "_svm_rcp85_2070_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")
	  
    # 5.3 evaluation
	  eSVM <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = SVM)
	  idSVM <- which(eSVM@t == as.numeric(threshold(eSVM, "spec_sens")))
	  eval.SVM.sp <- c(eSVM@t[idSVM], eSVM@auc, (eSVM@TPR[idSVM] + eSVM@TNR[idSVM] - 1))
	  eval.SVM <- rbind(eval.SVM, eval.SVM.sp)
	  
	  # verify 
	  print(paste0("Yeh! The model of ", id.specie, ", algorithm 'SVM', replica ", 
	               ifelse(r < 10, paste0("0", r), r), " it's done!"))  
	  

	  eval.names <- c(eval.names, paste0(id.specie, ifelse(r < 10, paste0("0", r), r)))	
	  
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
