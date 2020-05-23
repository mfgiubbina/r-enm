install.packages("maxlike", dep = T)

library(maxlike)


# Carolina Wren data used in Royle et. al (2012)
data(carw)

# Covert data.frame to a list of rasters
rl <- lapply(carw.data$raster.data, function(x) {
  m <- matrix(x, nrow=carw.data$dim[1], ncol=carw.data$dim[2], byrow=TRUE)
  r <- raster(m)
  extent(r) <- carw.data$ext
  r
})

# Create a raster stack and add layer names
rs <- stack(rl[[1]], rl[[2]], rl[[3]], rl[[4]], rl[[5]], rl[[6]])
names(rs) <- names(carw.data$raster.data)

plot(rs)


# Fit a model
fm <- maxlike(~pcMix + I(pcMix^2) + pcDec + I(pcDec^2)+ pcCon +
                I(pcCon^2) + pcGr + I(pcGr^2) +
                Lat + I(Lat^2) + Lon + I(Lon^2), rs, carw.data$xy1,
              method="BFGS", removeDuplicates=TRUE, savedata=TRUE)

summary(fm)
confint(fm)
AIC(fm)
logLik(fm)


# Produce species distribution map (ie, expected probability of occurrence)
psi.hat <- predict(fm) # Will warn if savedata=FALSE
plot(psi.hat)
points(carw.data$xy1, pch=16, cex=0.1)



# MAXENT sets "default prevalence" to an arbitrary value, 0.5.
# We could do something similar by fixing the intercept at logit(0.5)=0.
# However, it seems more appropriate to estimate this parameter.

fm.fix <- update(fm, fixed=c(0, rep(NA,length(coef(fm))-1)))

# Predict data.frame
presenceData <- as.data.frame(extract(rs, carw.data$xy1))
presenceData <- presenceData[complete.cases(presenceData), ]
presence.predictions <- predict(fm, newdata=presenceData)
summary(presence.predictions)

# Calibrate with data.frames
PresenceUniqueCells <- unique(cellFromXY(rs, xy=carw.data$xy1))
PresenceUnique <- xyFromCell(rs, PresenceUniqueCells)
presenceData <- as.data.frame(extract(rs, PresenceUnique))

library(dismo)
background <- randomPoints(rs, n=ncell(rs), extf=1.00)
backgroundData <- as.data.frame(extract(rs, y=background))
backgroundData <- backgroundData[complete.cases(backgroundData), ]
fm2 <- maxlike(~pcMix + I(pcMix^2) + pcDec + I(pcDec^2)+ pcCon +
                 I(pcCon^2) + pcGr + I(pcGr^2) +
                 Lat + I(Lat^2) + Lon + I(Lon^2), 
               rasters=NULL, points=NULL,
               x=presenceData, z=backgroundData,
               method="BFGS", removeDuplicates=TRUE, savedata=TRUE)

summary(fm2)

fm2$rasters <- rs
psi.hat2 <- predict(fm2)




## Not run: 
# 
# # Simulation example
# 
# set.seed(131)
# x1 <- sort(rnorm(100))
# x1 <- raster(outer(x1, x1), xmn=0, xmx=100, ymn=0, ymx=100)
# 
# x2 <- raster(matrix(runif(1e4), 100, 100), 0, 100, 0, 100)
# 
# # Code factors as dummy variables.
# # Note, using asFactor(x3) will not help
# x3 <- raster(matrix(c(0,1), 100, 100), 0, 100, 0, 100)
# 
# logit.psi <- -1 + 1*x1 + 0*x2
# psi <- exp(logit.psi)/(1+exp(logit.psi))
# plot(psi)
# 
# r <- stack(x1, x2, x3)
# names(r) <- c("x1", "x2", "x3")
# plot(r)
# 
# pa <- matrix(NA, 100, 100)
# pa[] <- rbinom(1e4, 1, as.matrix(psi))
# str(pa)
# table(pa)
# 
# pa <- raster(pa, 0, 100, 0, 100)
# plot(pa)
# 
# xy <- xyFromCell(pa, sample(Which(pa==1, cells=TRUE), 1000))
# 
# plot(x1)
# points(xy)
# 
# fm2 <- maxlike(~x1 + x2 + x3, r, xy)
# 
# summary(fm2)
# confint(fm2)
# AIC(fm2)
# logLik(fm2)
# 
# ## End(Not run)