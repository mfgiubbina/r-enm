# memory
rm(list = ls())

# packages
library(broom)
library(raster)
library(randomForest)
library(lattice)
library(pdp)
library(RColorBrewer)
library(PresenceAbsence)
library(tidyverse)

# directory
path <- "/home/mude/Downloads/zurell_sdm_introduction"
setwd(path)

# data --------------------------------------------------------------------
# occurrence data
avi_dat <- readr::read_csv("https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fjbi.13608&file=jbi13608-sup-0001-DataS1.csv")
avi_dat

dplyr::glimpse(avi_dat)

# select columns
avi_df <- avi_dat %>% 
  dplyr::select(Turdus_torquatus, bio_5, bio_2, bio_14, std, rad, blockCV_tile)

dplyr::glimpse(avi_df)

# covariates environmental
bio_curr <- raster::getData("worldclim", var = "bio", res = .5, lon = 5.5, lat = 45.5)[[c(2, 5, 14)]]
bio_curr

bio_fut <- raster::getData("CMIP5", var = "bio", res = .5, lon = 5.5, lat = 45.5, rcp = 45, model = "NO", year = 50, download = TRUE)[[c(2, 5, 14)]]
bio_fut

# mask
bg <- raster('/vsicurl/https://damariszurell.github.io/SDM-Intro/CH_mask.tif')
bg

ch_ext <- c(5, 11, 45, 48)
ch_ext

bio_curr <- raster::crop(bio_curr, ch_ext)
bio_curr <- raster::projectRaster(bio_curr, bg)
bio_curr <- raster::resample(bio_curr, bg)
bio_curr <- raster::mask(bio_curr, bg)

names(bio_curr) <- c('bio_2', 'bio_5', 'bio_14')
bio_curr[[1]] <- bio_curr[[1]]/10
bio_curr[[2]] <- bio_curr[[2]]/10

plot(bio_curr)

bio_fut <- raster::crop(bio_fut, ch_ext)
bio_fut <- raster::projectRaster(bio_fut, bg)
bio_fut <- raster::resample(bio_fut, bg)
bio_fut <- raster::mask(bio_fut, bg)

names(bio_fut) <- c('bio_2', 'bio_5', 'bio_14')
bio_fut[[1]] <- bio_fut[[1]]/10
bio_fut[[2]] <- bio_fut[[2]]/10

plot(bio_fut)

# scale
scale_attrib <- attributes(scale(avi_df[, 2:6]))[3:4]
scale_attrib

avi_dfst <- avi_df
avi_dfst[, 2:6] <- scale(avi_df[, 2:6])

values(bio_curr) <- scale(values(bio_curr), 
                          center = scale_attrib$`scaled:center`[names(bio_curr)], 
                          scale = scale_attrib$`scaled:scale`[names(bio_curr)])
bio_curr

values(bio_fut) <- scale(values(bio_fut),
                         center = scale_attrib$`scaled:center`[names(bio_fut)], 
                         scale = scale_attrib$`scaled:scale`[names(bio_fut)])
bio_fut

# model fit ---------------------------------------------------------------
# glm
# fit
m_glm <- glm(Turdus_torquatus ~ bio_2 + I(bio_2 ^ 2) + bio_5 + I(bio_5 ^ 2) + bio_14 + I(bio_14 ^ 2), family = "binomial", data = avi_dfst)
m_glm

summary(m_glm)

broom::tidy(m_glm)
broom::glance(m_glm)

# partial response curves
pred <- c('bio_2', 'bio_5', 'bio_14')

par(mfrow=c(1,3)) 
for (i in 1:3) {
  xz <- data.frame(sapply(colMeans(avi_dfst[, pred]), rep, each = 50))
  xz[, pred[i]] <- seq(min(avi_dfst[, pred[i]]), max(avi_dfst[, pred[i]]), length = 50)
  xz$z <- predict(m_glm, newdata = xz, type = 'response')
  plot(xz[, i], xz$z, type = 'l', lwd = 2, col = "red", xlab = pred[i], ylab = 'Occurrence probability')
}

# 3d
# We prepare the response surface:
xyz <- data.frame(expand.grid(seq(min(avi_dfst[, pred[1]]), max(avi_dfst[, pred[1]]), length = 50), 
                              seq(min(avi_dfst[, pred[2]]), max(avi_dfst[, pred[2]]), length = 50)), 
                  mean(pull(avi_dfst[, pred[3]])))
xyz
names(xyz) <- pred

xyz$z <- predict(m_glm, xyz, type='response')
xyz$z
summary(xyz)

cls <- colorRampPalette(rev(brewer.pal(11, 'RdYlBu')))(100)
cls

wireframe(z ~ bio_2 + bio_5, data = xyz, zlab = list("Occurrence prob.", rot=90),
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), zlim = c(0, 1), 
          main='GLM', xlab='bio_2', ylab='bio_5', screen=list(z = 120, x = -70, y = 3))

# Get source code for inflated response curves:
script<-readLines("https://raw.githubusercontent.com/damariszurell/Rcodes_MapNovelEnvironments_SDMs/master/appendixS1_functions.r")
eval(parse(text  = script)) 

# Plot inflated response curves:
par(mfrow=c(1,3)) 
inflated.response(m_glm, predictors = as.data.frame(avi_dfst[,pred]), method = "stat6", lwd = 3, main='GLM') 

# random forest
# Fit RF
(m_rf <- randomForest(x = avi_dfst[, 2:4], y = pull(avi_dfst[, 1]), ntree = 1000, nodesize = 10, importance = TRUE))

# Variable importance:
importance(m_rf, type = 1)
varImpPlot(m_rf)

# Look at single trees:
head(getTree(m_rf, 1, TRUE))

# Now, we plot response curves:
par(mfrow=c(1,3)) 
for (i in 1:3) {
  xz <- data.frame(sapply(colMeans(avi_dfst[,pred]),rep,each=50))
  xz[,pred[i]] <- seq(min(avi_dfst[,pred[i]]),max(avi_dfst[,pred[i]]),length=50)
  xz$z <- predict(m_rf, newdata=xz)
  plot(xz[,i],xz$z,type='l', xlab=pred[i], 
       ylab='Occurrence probability',ylim=c(0,1), main='Random forest')
}

# Plot the response surface:
xyz$z <- predict(m_rf, xyz)
wireframe(z ~ bio_2 + bio_5, data = xyz, zlab = list("Occurrence prob.", rot=90),
          drape = TRUE, col.regions = cls, scales = list(arrows = FALSE), zlim = c(0, 1), 
          main='RF', xlab='bio_2', ylab='bio_5', screen=list(z = 120, x = -70, y = 3))

# Plot inflated response curves:
par(mfrow=c(1,3)) 
inflated.response(m_rf, predictors = as.data.frame(avi_dfst[,pred]), method = "stat6", lwd = 3, main='RF') 

# assessement -------------------------------------------------------------
# Function to make predictions:
make.preds <- function(model, newdata) {
  switch(class(model)[1],
         glm = predict(model, newdata, type='response'),
         randomForest = predict(model, newdata))
}

# Function to make cross-validated predictions:
crossval.preds <- function(model, dat, pred, cv.tiles) {
  
  # Make k-fold data partitions
  kfold <- length(unique(cv.tiles))
  
  cross.val.preds <- data.frame(row = row.names(dat), 
                                cross.val.preds = numeric(length = nrow(dat))) 
  
  for(i in seq_len(kfold)){
    cv.train <- dat[cv.tiles!=i,]
    cv.test <- dat[cv.tiles ==i,]
    
    # We update the model for the new training data
    modtmp <- switch(class(model)[1],
                     glm = update(model, data=cv.train),
                     randomForest = update(model, data=cv.train))
    
    # We make predictions for k-fold:
    cross.val.preds[which(cv.tiles ==i),2] <- make.preds(modtmp, cv.test[, pred])
  }
  
  return(cross.val.preds[,2])
}

# Function to calculate model performances:
calc.eval <- function(obs, predictions, thresh.method='MaxSens+Spec'){
  require(PresenceAbsence)
  
  # Helper functions:
  # True Skill Statistic:
  TSS = function(cmx){
    PresenceAbsence::sensitivity(cmx, st.dev=F) + 
      PresenceAbsence::specificity(cmx, st.dev=F) - 1
  }
  
  thresh.dat <- data.frame(ID=length(obs), 
                           obs = obs,
                           pred = predictions)
  
  thresh <- optimal.thresholds(DATA= thresh.dat)
  cmx.maxSSS <- cmx(DATA= thresh.dat, threshold=thresh[thresh$Method==thresh.method,2])
  
  data.frame(AUC = PresenceAbsence::auc(thresh.dat, st.dev=F),
             TSS = TSS(cmx.maxSSS), 
             Sens = PresenceAbsence::sensitivity(cmx.maxSSS, st.dev=F),
             Spec = PresenceAbsence::specificity(cmx.maxSSS, st.dev=F),
             thresh = thresh[thresh$Method==thresh.method,2])
}


# Make cross-validated predictions for GLM:
crosspred_glm <- crossval.preds(m_glm, dat = avi_dfst[!is.na(avi_dfst$blockCV_tile),], 
                                pred = pred, cv.tiles = avi_dfst[!is.na(avi_dfst$blockCV_tile),'blockCV_tile'])
crosspred_glm

# Make cross-validated predictions for RF:
crosspred_rf <- crossval.preds(m_rf, dat = avi_dfst[!is.na(avi_dfst$blockCV_tile),], 
                               pred = pred, cv.tiles = avi_dfst[!is.na(avi_dfst$blockCV_tile),'blockCV_tile'])
crosspred_rf

# Look at correlation between GLM and RF predictions:
plot(crosspred_glm, crosspred_rf, pch = 19, col = 'grey35')


(eval_glm <- calc.eval(obs = avi_dfst[!is.na(avi_dfst$blockCV_tile), 1], predictions = crosspred_glm))
(eval_rf <- calc.eval(obs = avi_dfst[!is.na(avi_dfst$blockCV_tile),1], predictions = crosspred_rf))

# Derive median predictions:
crosspred_ens <- apply(data.frame(crosspred_glm, crosspred_rf),1,median)

# Evaluate ensemble predictions
(eval_ens <- calc.eval(obs = avi_dfst[!is.na(avi_dfst$blockCV_tile),1], predictions = crosspred_ens))


# prediction --------------------------------------------------------------
# present
# Make predictions to current climate:
bio_curr_df <- data.frame(rasterToPoints(bio_curr))
bio_curr_df$pred_glm <- make.preds(m_glm, bio_curr_df)
bio_curr_df$pred_rf <- make.preds(m_rf, bio_curr_df)
bio_curr_df$pred_ens <- apply(bio_curr_df[,-c(1:5)],1,median)

# Make binary predictions:
bio_curr_df$bin_glm <- ifelse(bio_curr_df$pred_glm > eval_glm$thresh, 1, 0)
bio_curr_df$bin_rf <- ifelse(bio_curr_df$pred_rf > eval_rf$thresh, 1, 0)
bio_curr_df$bin_ens <- ifelse(bio_curr_df$pred_ens > eval_ens$thresh, 1, 0)

# Make raster stack of predictions:
r_pred_curr <- rasterFromXYZ(bio_curr_df[,-c(3:5)])
r_pred_curr
plot(r_pred_curr)

# future
# Assess novel environments in future climate layer:
bio_fut_df <- data.frame(rasterToPoints(bio_fut))

# Values of 1 in the eo.mask will indicate novel environmental conditions
bio_fut_df$eo.mask <- eo.mask(avi_dfst[,pred], bio_fut_df[,pred])
plot(rasterFromXYZ(bio_fut_df[,-c(3:5)]), main='Environmental novelty')

# Make predictions to future climate:
bio_fut_df$pred_glm <- make.preds(m_glm, bio_fut_df)
bio_fut_df$pred_rf <- make.preds(m_rf, bio_fut_df)
bio_fut_df$pred_ens <- apply(bio_fut_df[,-c(1:5)],1,median)

# Make binary predictions:
bio_fut_df$bin_glm <- ifelse(bio_fut_df$pred_glm > eval_glm$thresh, 1, 0)
bio_fut_df$bin_rf <- ifelse(bio_fut_df$pred_rf > eval_rf$thresh, 1, 0)
bio_fut_df$bin_ens <- ifelse(bio_fut_df$pred_ens > eval_ens$thresh, 1, 0)

# Make raster stack of predictions:
r_pred_fut <- rasterFromXYZ(bio_fut_df[,-c(3:5)])
r_pred_fut
plot(r_pred_fut[[-1]])

# Predictions to analogous climates:
bio_analog_df <- bio_fut_df[,c('x','y','pred_glm','pred_rf')]
bio_analog_df[bio_fut_df$eo.mask>0,c('pred_glm','pred_rf')] <- NA
plot(rasterFromXYZ(bio_analog_df))

# Predictions to novel climates:
bio_novel_df <- bio_fut_df[,c('x','y','pred_glm','pred_rf')]
bio_novel_df[bio_fut_df$eo.mask==0,c('pred_glm','pred_rf')] <- NA
plot(rasterFromXYZ(bio_novel_df))
