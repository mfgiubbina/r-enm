### enm r package - function build enm ###

# mauricio humberto vancine - mauricio.vancine@gmail.com
# 21-12-2018

###---------------------------------------------------------------------------###

# function
enm <- function(occ, var_nic, var_proj, alg, part, rep, path){
  
  # information
  print("Installing and loading packages")
  
  # packages
  if(!require(dismo)) install.packages("dismo")
  if(!require(kernlab)) install.packages("kernlab")
  if(!require(randomForest)) install.packages("randomForest")
  if(!require(raster)) install.packages("raster")
  if(!require(tidyverse)) install.packages("tidyverse")
  
  # occurrences
  occ <- dir(patt = ".csv") %>% 
    readr::read_csv()
  
  # variables
  var <- dir(patt = ".tif") %>% 
    stack()
  
  # background coordinates
  bc <- rasterToPoints(en)[, 1:2]
  colnames(bc[, -3]) <- c("long", "lat")
  
  # diretory
  setwd(path)
  
  # enms
  for(i in 1:length(unique(po[, 1]))){ # for to each specie
    
    # null objects for evaluate
    eval.Bioclim <- NULL
    eval.Gower <- NULL
    eval.Maha <- NULL
    eval.GLM <- NULL
    eval.GAM <- NULL
    eval.RandomForest <- NULL
    eval.SVM <- NULL
    eval.Maxent <- NULL
    eval.names <- NULL
    
    # selecting presence and absence
    id.specie <- as.character(unique(po[, 1]))[i]
    pr.specie <- po[which(po[, 1] == id.specie), 2:3]
    id.background <- sample(nrow(bc), nrow(pr.specie))
    bc.specie <- bc[id.background, ]
    
    for(r in 1:rep){	# number of replicas
      
      ## preparing the models
      # train and test data	
      pr.sample.train <- sample(nrow(pr.specie), round(trn * nrow(pr.specie)))
      bc.sample.train <- sample(nrow(bc.specie), round(trn * nrow(bc.specie)))
      train <- na.omit(prepareData(x = en, p = pr.specie[pr.sample.train, ], b = bc.specie[bc.sample.train, ]))
      test <- na.omit(prepareData(x = en, p = pr.specie[-pr.sample.train, ], b = bc.specie[-bc.sample.train, ]))
      
      
      ### algorithms ###
      
      ## 1. bioclim
      Bioclim <- bioclim(train[which(train[, 1] == 1), -1])
      writeRaster(predict(en, Bioclim), paste0("bioclim_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")	
      eBioclim <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Bioclim)
      idBioclim <- which(eBioclim@t == as.numeric(threshold(eBioclim, "spec_sens")))
      eval.Bioclim.sp <- c(eBioclim@t[idBioclim], eBioclim@auc, (eBioclim@TPR[idBioclim] + eBioclim@TNR[idBioclim] - 1))
      eval.Bioclim <- rbind(eval.Bioclim, eval.Bioclim.sp)

      print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Bioclim', replica ", 
                   ifelse(r < 10, paste0("0", r), r), " it's done!"))
      
      ## 2. gower
      Gower <- domain(train[which(train[, 1] == 1), -1])	
      writeRaster(predict(en, Gower), paste0("gower_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
      eGower <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Gower)
      idGower <- which(eGower@t == as.numeric(threshold(eGower, "spec_sens")))
      eval.Gower.sp <- c(eGower@t[idGower], eGower@auc, (eGower@TPR[idGower] + eGower@TNR[idGower] - 1))
      eval.Gower <- rbind(eval.Gower, eval.Gower.sp)
      
      print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Gower', replica ", 
                   ifelse(r < 10, paste0("0", r), r), " it's done!"))
      
      ## 3. mahalanobis	
      Maha <- mahal(train[which(train[, 1] == 1), -1])	
      writeRaster(predict(en, Maha), paste0("mahalanobis_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
      eMaha <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maha)
      idMaha <- which(eMaha@t == as.numeric(threshold(eMaha, "spec_sens")))
      eval.Maha.sp <- c(eMaha@t[idMaha], eMaha@auc, (eMaha@TPR[idMaha] + eMaha@TNR[idMaha] - 1))
      eval.Maha <- rbind(eval.Maha, eval.Maha.sp)
      
      print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Mahalanobis', replica ", 
                   ifelse(r < 10, paste0("0", r), r), " it's done!"))
      
      ## 4. glm
      GLM <- glm(pb ~ ., family = "binomial", data = train)
      writeRaster(predict(en, GLM), paste0("glm_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")	
      eGLM <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = GLM)
      idGLM <- which(eGLM@t == as.numeric(threshold(eGLM, "spec_sens")))
      eval.GLM.sp <- c(eGLM@t[idGLM], eGLM@auc, (eGLM@TPR[idGLM] + eGLM@TNR[idGLM] - 1))
      eval.GLM <- rbind(eval.GLM, eval.GLM.sp)
      
      print(paste0("Yeh! The model of ", id.specie, ", algorithm 'GLM', replica ", 
                   ifelse(r < 10, paste0("0", r), r), " it's done!"))
      
      ## 5. gam
      GAM <- gam(as.formula(paste0(colnames(train)[1], "~", paste0("s(", colnames(train)[-1], ")", collapse = "+"))), 
                 family = "binomial", data = train)
      writeRaster(predict(en, GAM), paste0("gam_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff")	
      eGAM <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = GAM)
      idGAM <- which(eGAM@t == as.numeric(threshold(eGAM, "spec_sens")))
      eval.GAM.sp <- c(eGAM@t[idGAM], eGAM@auc, (eGAM@TPR[idGAM] + eGAM@TNR[idGAM] - 1))
      eval.GAM <- rbind(eval.GAM, eval.GAM.sp)
      
      print(paste0("Yeh! The model of ", id.specie, ", algorithm 'GAM', replica ", 
                   ifelse(r < 10, paste0("0", r), r), " it's done!"))
      
      ## 6. random forest
      RandomForest <- randomForest(pb ~ ., data = train)
      writeRaster(predict(en, RandomForest), paste0("randomforest_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
      eRandomForest <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = RandomForest)
      idRandomForest <- which(eRandomForest@t == as.numeric(threshold(eRandomForest, "spec_sens")))
      eval.RandomForest.sp <- c(eRandomForest@t[idRandomForest], eRandomForest@auc, (eRandomForest@TPR[idRandomForest] + eRandomForest@TNR[idRandomForest] - 1))
      eval.RandomForest <- rbind(eval.RandomForest, eval.RandomForest.sp)

      print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Random Forest', replica ", 
                   ifelse(r < 10, paste0("0", r), r), " it's done!"))
      
      ## 7. svm	
      SVM <- ksvm(pb ~ ., data = train)
      writeRaster(predict(en, SVM), paste0("svm_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
      eSVM <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = SVM)
      idSVM <- which(eSVM@t == as.numeric(threshold(eSVM, "spec_sens")))
      eval.SVM.sp <- c(eSVM@t[idSVM], eSVM@auc, (eSVM@TPR[idSVM] + eSVM@TNR[idSVM] - 1))
      eval.SVM <- rbind(eval.SVM, eval.SVM.sp)

      print(paste0("Yeh! The model of ", id.specie, ", algorithm 'SVM', replica ", 
                   ifelse(r < 10, paste0("0", r), r), " it's done!"))
      
      ## 8. maxent	
      Maxent <- maxent(train[, -1], train[, 1])	
      writeRaster(predict(en, Maxent), paste0("maxent_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), format = "GTiff") 
      eMaxent <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maxent)
      idMaxent <- which(eMaxent@t == as.numeric(threshold(eMaxent, "spec_sens")))
      eval.Maxent.sp <- c(eMaxent@t[idMaxent], eMaxent@auc, (eMaxent@TPR[idMaxent] + eMaxent@TNR[idMaxent] - 1))
      eval.Maxent <- rbind(eval.Maxent, eval.Maxent.sp)
      
      print(paste0("Yeh! The model of ", id.specie, ", algorithm 'Maxent', replica ", 
                   ifelse(r < 10, paste0("0", r), r), " it's done!"))
      
      
      eval.names <- c(eval.names, paste0(id.specie, ifelse(r < 10, paste0("0", r), r)))	
      
    } # ends for "r"
    
    # evaluations
    dimnames(eval.GLM) <- list(eval.names, c("thrs", "AUC", "TSS"))
    dimnames(eval.GAM) <- list(eval.names, c("thrs", "AUC", "TSS"))
    dimnames(eval.Bioclim) <- list(eval.names, c("thrs", "AUC", "TSS"))
    dimnames(eval.Gower) <- list(eval.names, c("thrs", "AUC", "TSS"))  
    dimnames(eval.Maha) <- list(eval.names, c("thrs", "AUC", "TSS"))  
    dimnames(eval.RandomForest) <- list(eval.names, c("thrs", "AUC", "TSS"))
    dimnames(eval.SVM) <- list(eval.names, c("thrs", "AUC", "TSS"))
    dimnames(eval.Maxent) <- list(eval.names, c("thrs", "AUC", "TSS"))
    
    write.table(eval.GLM, paste0("zEval_", "glm_", id.specie, ".txt"))
    write.table(eval.GAM, paste0("zEval_", "gam_", id.specie, ".txt"))
    write.table(eval.Bioclim, paste0("zEval_", "bioclim_", id.specie, ".txt"))
    write.table(eval.Gower, paste0("zEval_", "gower_", id.specie, ".txt"))
    write.table(eval.Maha, paste0("zEval_", "mahalanobis_", id.specie, ".txt"))
    write.table(eval.RandomForest, paste0("zEval_", "randomforest_", id.specie, ".txt"))
    write.table(eval.SVM, paste0("zEval_", "svm_", id.specie, ".txt"))
    write.table(eval.Maxent, paste0("zEval_", "maxent_", id.specie, ".txt"))
    
  } # ends for"i"
  
}

###----------------------------------------------------------------------------###
  