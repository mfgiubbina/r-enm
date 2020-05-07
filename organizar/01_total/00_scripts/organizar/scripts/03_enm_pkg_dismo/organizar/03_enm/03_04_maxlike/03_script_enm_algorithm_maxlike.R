################################################################################
# script to model ant distributions in New England using maxent & malike
# 
# written by MC Fitzpatrick
# at UMCES-Appalachian Lab, Frostburg, MD 
# 9/2012-3/2013
#
# Results described in:
# Fitzpatrick et al. (2013) MAXENT vs. MAXLIKE: Empirical Comparisons with 
# Ant Species Distributions. Ecosphere
#
# DESCRIPTION & NOTES
# The code needs the following to run:
# (1) climate & elevation rasters (provided as neClim)
# (2) ant distribution data (provided as antData_use4R_snappedToClim.csv). Note
# that as the file name suggests these data have been snapped to cell centriods.
#
# Code is provided as is, without support 
################################################################################

################################################################################
# CHUNK 1: Read in & prep data
################################################################################
library(dismo)
library(raster)
library(maxlike)
library(foreach)
library(doParallel)
library(dismo)
library(raster)
library(sp)
library(sm)

setwd("/.....") # setwd to where data files are stored

antsGeoXY <- read.csv("antData_use4R_snappedToClim.csv")
neClim <- stack("neClim.grd") # read in climate & elev data

# scale climate layers per Royle et al. suggestion
neClimTrans <- neClim
for(i in 1:dim(neClim)[3]){
  neClimTrans[[i]] <- (neClim[[i]]-cellStats(neClim[[i]],mean))/cellStats(neClim[[i]],sd)
}
mask <- neClimTrans[[1]]>-1000

# develop KDE sampling bias surface
bias <- cellFromXY(mask, antsGeoXY[,-1])
cells <- unique(sort(bias))
kernelXY <- xyFromCell(mask, cells)
samps <- as.numeric(table(bias))

# code to make KDE raster
KDEsur <- sm.density(kernelXY, weights=samps, display="none", ngrid=812, 
                     ylim=c(40,48), xlim=c(-75,-65), nbins=0)
KDErast=SpatialPoints(expand.grid(x=KDEsur$eval.points[,1], y=KDEsur$eval.points[,2]))
KDErast = SpatialPixelsDataFrame(KDErast, data.frame(kde = array(KDEsur$estimate, 
                                                                 length(KDEsur$estimate))))
KDErast <- raster(KDErast)
KDErast <- resample(KDErast, mask)
KDErast <- KDErast*mask
KDEpts <- rasterToPoints(KDErast)
################################################################################


################################################################################
# CHUNK 2: Run models
################################################################################
# extract data for ant spp
antSpp <- c("camher", "camnov", "forint", "monema", "phepil", "preimp")

# loop to run models for each species
for(a in 1:length(antSpp)){
  antGeoXY <- antsGeoXY[antsGeoXY$spcode==antSpp[a],-1]
  
  ########## make sets of training/test (t/t) data
  ttSplit = 0.25 # ttSplit = test/train split percentage
  # function to partition data into t/t
  fold <- function(ttSplit){ 
    k = round(1/ttSplit, 0)
    fold <- kfold(antGeoXY, k=k)
  }
  
  # make sets of t/t data
  sets = 50 # number of t/t sets
  folds <- replicate(sets, fold(ttSplit))
  
  # now loop through to build lists of t/t data
  antTrain <- list()
  antTest <- list()
  for(h in 1:sets){
    antTrain[[h]] <- antGeoXY[folds[,h]!=1,]
    antTest[[h]] <- antGeoXY[folds[,h]==1,]
  }
  
  # set up parallel computing
  cl <- makeCluster(24) # number of cores to use
  registerDoParallel(cl)
  
  # run & predict (in parallel) maxlike models for k randomizations
  antlikeMods <- foreach(k=1:sets, .verbose=T, .packages="maxlike") %dopar%
    maxlike(~bio_1 + bio_12 + elev, neClimTrans, antTrain[[k]], 
            control=list(maxit=10000), method="SANN", removeDuplicates=TRUE)
  
  # predict to geography
  antlikeStack <- predict(antlikeMods[[1]], neClimTrans)
  for(j in 2:sets){
    mod <- predict(antlikeMods[[j]], neClimTrans)
    antlikeStack <- stack(antlikeStack, mod)
  }
  
  #### MAXENT - LINEAR FEATURES
  # run & predict (in parallel) maxent models for k randomizations
  antmaxMods_LF <- foreach(k=1:sets, .verbose=T, .packages=c("dismo", "rJava", "rgdal")) %dopar% 
    maxent(neClimTrans, antTrain[[k]], args=c(c("-h", "-q", "-p", "-P", "nothreshold"),c("-m", 10000)))
  
  antmaxStack_LF <- predict(antmaxMods_LF[[1]], neClimTrans)
  antmaxStackRAW_LF <- predict(antmaxMods_LF[[1]], neClimTrans, args='outputformat=raw')
  for(j in 2:sets){
    mod_LF <- predict(antmaxMods_LF[[j]], neClimTrans)
    modRAW_LF <- predict(antmaxMods_LF[[j]], neClimTrans, args='outputformat=raw')
    antmaxStack_LF <- stack(antmaxStack_LF, mod_LF)
    antmaxStackRAW_LF <- stack(antmaxStackRAW_LF, modRAW_LF)
  }
  
  #### MAXENT - DEFAULT FEATURES
  # run & predict (in parallel) maxent models for k randomizations
  antmaxMods_allF <- foreach(k=1:sets, .verbose=T, .packages=c("dismo", "rJava", "rgdal")) %dopar% 
    maxent(neClimTrans, antTrain[[k]], args=c("-P",c("-m", 10000)))
  
  antmaxStack_allF <- predict(antmaxMods_allF[[1]], neClimTrans)
  antmaxStackRAW_allF <- predict(antmaxMods_allF[[1]], neClimTrans, args='outputformat=raw')
  for(j in 2:sets){
    mod_allF <- predict(antmaxMods_allF[[j]], neClimTrans)
    modRAW_allF <- predict(antmaxMods_allF[[j]], neClimTrans, args='outputformat=raw')
    antmaxStack_allF <- stack(antmaxStack_allF, mod_allF)
    antmaxStackRAW_allF <- stack(antmaxStackRAW_allF, modRAW_allF)
  }
  
  # 10,000 locations selected using probabilistic target-group sampling
  # from KDE bias surface
  antmaxMods_LF_bias <- foreach(k=1:sets, .verbose=T, .packages=c("dismo", "rJava", "rgdal")) %dopar% 
    maxent(neClimTrans, antTrain[[k]], a=KDEpts[sample(seq(1:nrow(KDEpts)), size=10000, replace=T, prob=KDEpts[,"layer"]),1:2], args=c(c("-h", "-q", "-p", "-P", "nothreshold"),c("-m", 10000)))
  
  antmaxStack_LF_bias <- predict(antmaxMods_LF_bias[[1]], neClimTrans)
  antmaxStackRAW_LF_bias <- predict(antmaxMods_LF_bias[[1]], neClimTrans, args='outputformat=raw')
  for(j in 2:sets){
    mod_LF_bias <- predict(antmaxMods_LF_bias[[j]], neClimTrans)
    modRAW_LF_bias <- predict(antmaxMods_LF_bias[[j]], neClimTrans, args='outputformat=raw')
    antmaxStack_LF_bias <- stack(antmaxStack_LF_bias, mod_LF_bias)
    antmaxStackRAW_LF_bias <- stack(antmaxStackRAW_LF_bias, modRAW_LF_bias)
  }
  
  #### MAXENT - DEFAULT FEATURES - BIAS CORRECTED BACKGROUND
  # run & predict (in parallel) maxent models for k randomizations
  # 10,000 locations selected using probabilistic target-group sampling
  # from KDE bias surface
  antmaxMods_allF_bias <- foreach(k=1:sets, .verbose=T, .packages=c("dismo", "rJava", "rgdal")) %dopar% 
    maxent(neClimTrans, antTrain[[k]], a=KDEpts[sample(seq(1:nrow(KDEpts)), size=10000, replace=T, prob=KDEpts[,"layer"]),1:2], args=c("-P",c("-m", 10000)))
  
  antmaxStack_allF_bias <- predict(antmaxMods_allF_bias[[1]], neClimTrans)
  antmaxStackRAW_allF_bias <- predict(antmaxMods_allF_bias[[1]], neClimTrans, 
                                      args='outputformat=raw')
  for(j in 2:sets){
    mod_allF_bias <- predict(antmaxMods_allF_bias[[j]], neClimTrans)
    modRAW_allF_bias <- predict(antmaxMods_allF_bias[[j]], neClimTrans, 
                                args='outputformat=raw')
    antmaxStack_allF_bias <- stack(antmaxStack_allF_bias, mod_allF_bias)
    antmaxStackRAW_allF_bias <- stack(antmaxStackRAW_allF_bias, modRAW_allF_bias)
  }
  
  stopCluster(cl)
  save.image(file=paste(antSpp[a], ".RData", sep=""))
}
################################################################################


################################################################################
# CHUNK 3: Write rasters of predictions for evaluation, mapping, etc
################################################################################
library(raster)

setwd("/.....")

antFiles <- list.files(pattern=".RData", recursive=T)

for(jj in 1:length(antFiles)){
  load(antFiles[jj])
  fileName <- strsplit(antFiles[jj], split="_")[[1]][1]
  
  #maxlike
  writeRaster(sum(antlikeStack)/dim(antlikeStack)[3], paste(fileName, 
                                                            "_maxlike_prob.tiff", sep=""), type="GTiff", overwrite=T)
  writeRaster(calc(antlikeStack, sd), paste(fileName, "_maxlike_sd.tiff", 
                                            sep=""), type="GTiff", overwrite=T)
  
  #maxent - LINEAR FEATURES
  writeRaster(sum(antmaxStack_LF)/dim(antmaxStack_LF)[3], paste(fileName, 
                                                                "_maxent_prob_LF.tiff", sep=""), type="GTiff", overwrite=T)
  writeRaster(calc(antmaxStack_LF, sd), paste(fileName, "_maxent_sd_LF.tiff", 
                                              sep=""), type="GTiff", overwrite=T)  
  
  #maxent - ALL FEATURES
  writeRaster(sum(antmaxStack_allF)/dim(antmaxStack_allF)[3], paste(fileName, 
                                                                    "_maxent_prob_allF.tiff", sep=""), type="GTiff", overwrite=T)
  writeRaster(calc(antmaxStack_allF, sd), paste(fileName, "_maxent_sd_allF.tiff", 
                                                sep=""), type="GTiff", overwrite=T)
  
  #maxent - LINEAR FEATURES - ***bias corrected***
  writeRaster(sum(antmaxStack_LF_bias)/dim(antmaxStack_LF_bias)[3], paste(fileName, 
                                                                          "_maxent_prob_LF_biasCorrected.tiff", sep=""), type="GTiff", overwrite=T)
  writeRaster(calc(antmaxStack_LF_bias, sd), paste(fileName, 
                                                   "_maxent_sd_LF_biasCorrected.tiff", sep=""), type="GTiff", overwrite=T)  
  
  #maxent - ALL FEATURES - ***bias corrected***
  writeRaster(sum(antmaxStack_allF_bias)/dim(antmaxStack_allF_bias)[3], 
              paste(fileName, "_maxent_prob_allF_biasCorrected.tiff", sep=""), 
              type="GTiff", overwrite=T)
  writeRaster(calc(antmaxStack_allF_bias, sd), paste(fileName, 
                                                     "_maxent_sd_allF_biasCorrected.tiff", sep=""), type="GTiff", 
              overwrite=T)
}
################################################################################


################################################################################
# CHUNK 4: Calculate AICc for maxlike & maxent models
################################################################################
library(PresenceAbsence)
library(raster)
library(dismo)
library(zoo)

setwd("/.....")

antFiles <- list.files(pattern=".RData")
maxlikeAICc <- list()
maxentAICc <- list()
maxentAICc_bias <- list()

#loop through each model and calculate AICc
for(jj in 1:length(antFiles)){
  load(antFiles[jj])
  antlikeAICc <- antmaxAICc <- antmaxBiasAICc <- NULL
  for(ii in 1:length(antlikeMods)){
    #calculate AICc for maxlike, need to do small sample adjust
    k <- nrow(antlikeMods[[ii]]$Est) # number of parameters
    n <- nrow(antGeoXY) # number of data points
    smallCorrect <- ((2*k)*(k+1))/(n-k-1)
    antlikeAICc[ii] <- antlikeMods[[ii]]$AIC+smallCorrect
    
    # Caclulate AIC following Warren & Seifert 2011, page 337
    # First, standardize raw scores so they sum to 1
    rawStand <- antmaxStackRAW_LF[[ii]]/cellStats(antmaxStackRAW_LF[[ii]], sum)
    rawPts <- as.numeric(na.omit(extract(rawStand, antGeoXY)))
    # Second, calculate the likelihood at both training & test localities
    logLik <- sum(log(rawPts))
    # Third, bunch of hoops to count number of parameters from lambdas
    k1 <- strsplit(antmaxMods_LF[[ii]]@lambdas[1],",")[[1]]
    k1 <- length(which(as.numeric(k1[2])!=0))
    k2 <- strsplit(antmaxMods_LF[[ii]]@lambdas[2],",")[[1]]
    k2 <- length(which(as.numeric(k2[2])!=0))
    k3 <- strsplit(antmaxMods_LF[[ii]]@lambdas[3],",")[[1]]
    k3 <- length(which(as.numeric(k3[2])!=0))
    k <- k1+k2+k3   
    smallCorrect <- ((2*k)*(k+1))/(n-k-1)
    antmaxAICc[ii] <- 2*k-(2*logLik)+smallCorrect
    
    # Calculate AIC following Warren & Seifert 2001, page 337
    # same as above, but for models that account for sampling bias
    rawStand <- antmaxStackRAW_LF_bias[[ii]]/cellStats(antmaxStackRAW_LF_bias[[ii]], sum)
    rawPts <- as.numeric(na.omit(extract(rawStand, antGeoXY)))
    logLik <- sum(log(rawPts))
    k1 <- strsplit(antmaxMods_LF_bias[[ii]]@lambdas[1],",")[[1]]
    k1 <- length(which(as.numeric(k1[2])!=0))
    k2 <- strsplit(antmaxMods_LF_bias[[ii]]@lambdas[2],",")[[1]]
    k2 <- length(which(as.numeric(k2[2])!=0))
    k3 <- strsplit(antmaxMods_LF_bias[[ii]]@lambdas[3],",")[[1]]
    k3 <- length(which(as.numeric(k3[2])!=0))
    k <- k1+k2+k3   
    smallCorrect <- ((2*k)*(k+1))/(n-k-1)
    antmaxBiasAICc[ii] <- 2*k-(2*logLik)+smallCorrect   
  }    
  maxlikeAICc[[jj]] <- antlikeAICc
  maxentAICc[[jj]] <- antmaxAICc
  maxentAICc_bias[[jj]] <- antmaxBiasAICc
}

dput(maxlikeAICc, "maxlikeAICc.txt")
dput(maxentAICc, "maxentAICc_LF.txt")
dput(maxentAICc_bias, "maxentAICc_LF_biasCorrected.txt")

###############

maxlikeAICc <- dget("maxlikeAICc.txt")
maxentAICc <- dget("maxentAICc_LF.txt")
maxentAICc_bias <- dget("maxentAICc_LF_biasCorrected.txt")

# calculate AIC weights
library(MuMIn)
aicWeights <- out <- delAIC <- NULL
for(w in 1:length(maxlikeAICc)){
  lik <- maxlikeAICc[[w]]
  maxE <- maxentAICc[[w]]
  maxE_bias <- maxentAICc_bias[[w]]
  delAIC <- round(rbind(delAIC, c(mean(lik-maxE), mean(lik-maxE_bias))),2)
  for(ww in 1:length(lik)){    
    aicWeights <- rbind(aicWeights, round(Weights(c(lik[ww], maxE[ww], 
                                                    maxE_bias[ww])),3))
  }
  # mean AIC weight across 50 runs for each model/species
  out <- rbind(out,apply(aicWeights, 2, mean))
}
################################################################################


################################################################################
# CHUNK 5: Evaluate models
################################################################################
library(PresenceAbsence)
library(raster)
library(dismo)
library(zoo)

setwd("/.....")

antFiles <- list.files(pattern=".RData")
maxlikeEval <- list()
maxentEval_LF <- list()
maxentEval_allF <- list()
maxentEval_LF_bias <- list()
maxentEval_allF_bias <- list()  

#loop through each model and calculate evaluation measures & thresholds
for(jj in 1:length(antFiles)){
  load(antFiles[jj])
  AUCmod <- AVI <- thresh95 <- predArea95 <- threshMPA <- MPA <- meanProb <- meanBG <- NULL
  area <- cellStats(antlikeStack[[1]]<=1, sum)
  
  # predicted probability at random background points
  probBG <- extract(antlikeStack, randomPoints(neClimTrans, 10000))
  
  for(ii in 1:dim(antlikeStack)[3]){  
    probTest <- as.numeric(na.omit(extract(antlikeStack[[ii]], antTest[[ii]])))
    evalDismo <- evaluate(p=probTest, a=probBG[,ii])
    AUCmod[[ii]] <- evalDismo@auc
    meanProb[[ii]] <- mean(probTest)
    meanBG[[ii]] <- mean(probBG[,ii])
    AVI[[ii]] <- sum(probTest>0.5)/length(probTest)
    thresh95[ii] <- sort(probTest, decreasing=T)[round(length(probTest)*0.95,0)]
    predArea95[[ii]] <- count(antlikeStack[[ii]]>=thresh95[ii],1, digits=3)/area
  }
  
  maxlikeEval[[jj]] <- rbind(thresh95, predArea95, threshMPA, MPA, AVI, AUCmod,
                             meanProb, meanBG)
  
  ######### maxent LF ############# 
  AUCmod <- corrMod <- AVI <- thresh95 <- predArea95 <- threshMPA <- MPA <- meanProb <- meanBG <- NULL
  
  # predicted probability at random background points
  probBG <- extract(antmaxStack_LF, randomPoints(neClimTrans, 10000))
  
  for(ii in 1:dim(antmaxStack_LF)[3]){    
    # predicted probability at test points
    probTest <- as.numeric(na.omit(extract(antmaxStack_LF[[ii]], antTest[[ii]])))
    evalDismo <- evaluate(p=probTest, a=probBG[,ii])
    AUCmod[[ii]] <- evalDismo@auc
    meanProb[[ii]] <- mean(probTest)
    meanBG[[ii]] <- mean(probBG[,ii])
    AVI[[ii]] <- sum(probTest>0.5)/length(probTest)
    thresh95[ii] <- sort(probTest, decreasing=T)[round(length(probTest)*0.95,0)]
    predArea95[[ii]] <- count(antmaxStack_LF[[ii]]>=thresh95[ii], 1, digits=3)/area
  }
  
  maxentEval_LF[[jj]] <- rbind(thresh95, predArea95, threshMPA, MPA, AVI, AUCmod,
                               meanProb, meanBG)
  
  ######### maxent allF #############  
  AUCmod <- AVI <- thresh95 <- predArea95 <- threshMPA <- MPA <- meanProb <- meanBG <- NULL
  
  # predicted probability at random background points
  probBG <- extract(antmaxStack_allF, randomPoints(neClimTrans, 10000))
  
  for(ii in 1:dim(antmaxStack_allF)[3]){    
    # predicted probability at test points
    probTest <- as.numeric(na.omit(extract(antmaxStack_allF[[ii]], antTest[[ii]])))
    evalDismo <- evaluate(p=probTest, a=probBG[,ii])
    AUCmod[[ii]] <- evalDismo@auc
    meanProb[[ii]] <- mean(probTest)
    meanBG[[ii]] <- mean(probBG[,ii])
    AVI[[ii]] <- sum(probTest>0.5)/length(probTest)
    thresh95[ii] <- sort(probTest, decreasing=T)[round(length(probTest)*0.95,0)]
    predArea95[[ii]] <- count(antmaxStack_allF[[ii]]>=thresh95[ii],1, digits=3)/area
  }
  
  maxentEval_allF[[jj]] <- rbind(thresh95, predArea95, threshMPA, MPA, AVI, AUCmod,
                                 meanProb, meanBG)
  
  ######### maxent LF bias corrected ############# 
  AUCmod <- AVI <- thresh95 <- predArea95 <- threshMPA <- MPA <- meanProb <- meanBG <- NULL
  
  # predicted probability at random background points
  probBG <- extract(antmaxStack_LF_bias, randomPoints(neClimTrans, 10000))
  
  for(ii in 1:dim(antmaxStack_LF_bias)[3]){    
    # predicted probability at test points
    probTest <- as.numeric(na.omit(extract(antmaxStack_LF_bias[[ii]], antTest[[ii]])))
    evalDismo <- evaluate(p=probTest, a=probBG[,ii])
    AUCmod[[ii]] <- evalDismo@auc
    meanProb[[ii]] <- mean(probTest)
    meanBG[[ii]] <- mean(probBG[,ii])
    AVI[[ii]] <- sum(probTest>0.5)/length(probTest)
    thresh95[ii] <- sort(probTest, decreasing=T)[round(length(probTest)*0.95,0)]
    predArea95[[ii]] <- count(antmaxStack_LF_bias[[ii]]>=thresh95[ii], 1, digits=3)/area
  }
  
  maxentEval_LF_bias[[jj]] <- rbind(thresh95, predArea95, threshMPA, MPA, AVI, AUCmod,
                                    meanProb, meanBG)
  
  ######### maxent allF bias corrected #############  
  AUCmod <- AVI <- thresh95 <- predArea95 <- threshMPA <- MPA <- meanProb <- meanBG <- NULL
  
  # predicted probability at random background points
  probBG <- extract(antmaxStack_allF_bias, randomPoints(neClimTrans, 10000))
  
  for(ii in 1:dim(antmaxStack_allF_bias)[3]){    
    # predicted probability at test points
    probTest <- as.numeric(na.omit(extract(antmaxStack_allF_bias[[ii]], antTest[[ii]])))
    evalDismo <- evaluate(p=probTest, a=probBG[,ii])
    AUCmod[[ii]] <- evalDismo@auc
    meanProb[[ii]] <- mean(probTest)
    meanBG[[ii]] <- mean(probBG[,ii])
    AVI[[ii]] <- sum(probTest>0.5)/length(probTest)
    thresh95[ii] <- sort(probTest, decreasing=T)[round(length(probTest)*0.95,0)]
    predArea95[[ii]] <- count(antmaxStack_allF_bias[[ii]]>=thresh95[ii],1, digits=3)/area
  }
  
  maxentEval_allF_bias[[jj]] <- rbind(thresh95, predArea95, threshMPA, MPA, AVI, AUCmod,
                                      meanProb, meanBG)
  
  cat(round(jj/length(antFiles)*100,2), "%", sep="", fill=T)
}

dput(maxlikeEval, "maxlikeEval.txt")
dput(maxentEval_LF, "maxentEval_LF.txt")
dput(maxentEval_allF, "maxentEval_allF.txt")
dput(maxentEval_LF_bias, "maxentEval_LF_bias.txt")
dput(maxentEval_allF_bias, "maxentEval_allF_bias.txt") 
################################################################################