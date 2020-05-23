
############################################################
####                                                    ####  
####  NRES 746, SDMs                                    ####
####                                                    ####
####  Corey Mitchell & Lauren Phillips                  #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Species distribution modeling!                    ####
############################################################



#install.packages(c("dismo","rgdal","raster","sp","ggmap","ggplot","mgcv","nlme","biomod2","gstat"))


library(dismo)
chuck1 <-  gbif(genus = "Sauromalus", species = 'obesus', geo=T)
chuck2 <-  gbif(genus = "Sauromalus", species = 'ater', geo=T)


dim(chuck1)
dim(chuck2)

keepcols <- c('genus','specificEpithet','eventDate','lon','lat','elevation','geodeticDatum')
keepcols %in% names(chuck1)
keepcols %in% names(chuck2)

allchck <- rbind(chuck1[,c(keepcols)], chuck2[,keepcols])

## Check out the data and remove any duplicates
chuckNoDup <- allchck[!duplicated(allchck),]


plot(chuckNoDup$lon, chuckNoDup$lat)


chuckNoDup <- chuckNoDup[chuckNoDup$lon < -105,]
plot(chuckNoDup$lon, chuckNoDup$lat)
chuck.sp <- chuckNoDup
summary(chuck.sp$lat)
summary(chuck.sp$lon)


chuck.sp <- chuck.sp[!is.na(chuck.sp$lon),]


coordinates(chuck.sp) <- c('lon','lat')
proj4string(chuck.sp) <- CRS('+proj=longlat + datum=WGS84')
plot(chuck.sp)


library(rgdal)
library(sp)
e = extent(chuck.sp)
e
buf = .5
chuck.df<- as.data.frame(chuck.sp)

library(ggmap)
myMap <- get_stamenmap(bbox = c(left = e[1]-buf,
                                bottom = e[3] -buf,
                                right = e[2]+buf,
                                top = e[4] +buf),
                       maptype = "terrain",
                       crop = FALSE,
                       zoom = 6)
# plot map
ggmap(myMap) +  geom_point(aes(x = lon, y = lat), data = chuck.df, alpha = .5)



chuck.sp <- chuck.sp[coordinates(chuck.sp)[,1] > -119,]
chuck.df <- as.data.frame(chuck.sp)
ggmap(myMap) +  geom_point(aes(x = lon, y = lat), data = chuck.df, alpha = .5)


library(raster)
library(sp)

blk.dens <- raster("ofr20091102 Environmental Layers/BlkDensity.asc") # Bulk density (soil)
pct.cov <-raster("ofr20091102 Environmental Layers/pctCov.asc") #% Shrub cover
pct.rock <-raster("ofr20091102 Environmental Layers/pctRocks.asc") #% rocks
pct.rough <-raster("ofr20091102 Environmental Layers/pctRuf.asc") #% roughness
slope <-raster("ofr20091102 Environmental Layers/slope.asc") #slope
s.precip <- raster("ofr20091102 Environmental Layers/sp30.asc") #summer precip
w.precip <- raster("ofr20091102 Environmental Layers/wp30.asc") #winter precip


env <- stack(blk.dens, pct.cov, pct.rock, pct.rough, slope, s.precip, w.precip)

env <- raster::getData('worldclim', var='bio', res=10)
env

env <- raster::crop(env, e)
env

par(mfrow=c(1,1))          
plot(env[[5]])
points(chuck.sp, pch = 20)

# proj4string(chuck.sp)
# proj4string(env)
# chuck.sp.utm <- spTransform(chuck.sp, CRS(proj4string(env)))
# chuck.sp.utm$keep<- extract(env[['pctRocks']], chuck.sp.utm)
# summary(chuck.sp.utm$keep) 
# chuck.sp.utm <- chuck.sp.utm[!is.na(chuck.sp.utm$keep),] ### remove the points outside of your env extent 
# chuck.sp.utm$Chuckwalla = 1 # create a presence column
# chuck.sp.utm <- chuck.sp.utm[,'Chuckwalla'] # drop all other columns
# 
# 
# plot(env[['pctRocks']], main = "Percent Rocks")
# points(chuck.sp.utm, pch = 19)


library(biomod2)
set.seed(24)

chuck.sp.utm.pa <- BIOMOD_FormatingData(resp.var = chuck.sp$,
                     expl.var = env,
                     resp.xy = coordinates(chuck.sp.utm),
                     resp.name = 'Chuckwalla',
                     #eval.resp.var = chuck.sp.utm.ex.tst$Chuckwalla,
                     #eval.expl.var = env,
                     #eval.resp.xy = coordinates(chuck.sp.utm.ex.tst),
                     PA.nb.rep = 1,
                     PA.nb.absences = dim(chuck.sp.utm)[1],
                     PA.strategy = 'sre',
                     PA.dist.min = NULL, # for 'disk' strategy
                     PA.dist.max = NULL, # for 'disk' strategy
                     PA.sre.quant = 0.1,
                     PA.table = NULL,
                     na.rm = TRUE)
 chuck.sp.utm.pa
 

dim(chuck.sp.utm)
str(chuck.sp.utm.pa) 
summary(chuck.sp.utm.pa@data.species)


chuck.sp.all <- SpatialPointsDataFrame(coords = chuck.sp.utm.pa@coord, data = data.frame(Chuckwalla = chuck.sp.utm.pa@data.species), proj = CRS(proj4string(chuck.sp.utm)))


chuck.sp.all$Chuckwalla[is.na(chuck.sp.all$Chuckwalla)] <- 0
chuck.sp.all


plot(env[[3]])
points(chuck.sp.all, col='red') 
points(chuck.sp.utm, col='black')


howmanychucks <- dim(chuck.sp.all)[1]/2

## 70 % of those
pct70 <- round(howmanychucks * 0.7)


chuck.sp.Pres <- chuck.sp.all[chuck.sp.all$Chuckwalla == 1,]
chuck.sp.Pa <- chuck.sp.all[chuck.sp.all$Chuckwalla == 0,]


trnchuckrows <- sample(1:howmanychucks, size = pct70, replace = F)


## select those rows
chuck.sp.Pres.trn <- chuck.sp.Pres[trnchuckrows,'Chuckwalla']
chuck.sp.Pa.trn <- chuck.sp.Pa[trnchuckrows,'Chuckwalla']

## select the opposite for testing
chuck.sp.Pres.tst <- chuck.sp.Pres[-trnchuckrows,'Chuckwalla']
chuck.sp.Pa.tst <- chuck.sp.Pa[-trnchuckrows,'Chuckwalla']

## Combine rows #### 
chuck.sp.trn <- rbind(chuck.sp.Pres.trn, chuck.sp.Pa.trn)
chuck.sp.tst <- rbind(chuck.sp.Pres.tst, chuck.sp.Pa.tst)


plot(env[['sp30']], main = " Summer Precipitation: Training Data")
points(chuck.sp.trn[chuck.sp.trn$Chuckwalla == 1,], pch=19)
points(chuck.sp.trn[chuck.sp.trn$Chuckwalla == 0,], pch=24, col='red')

plot(env[['sp30']], main = " Summer Precipitation: Testing Data")
points(chuck.sp.tst[chuck.sp.tst$Chuckwalla == 1,], pch=19)
points(chuck.sp.tst[chuck.sp.tst$Chuckwalla == 0,], pch=24, col='red')


library(gstat)
c.variog <- variogram(Chuckwalla ~1, chuck.sp.trn, xlab= "Distance (m)", ylab="Semivariance")
plot(c.variog) #check training data for SA (it there)

c.variog2 <- variogram(Chuckwalla ~1, chuck.sp.tst, xlab= "Distance (m)", ylab="Semivariance")
plot(c.variog2) #check testing data for SA (it there)

### Zooming in a bit to focus on local effects
c.variog3 <- variogram(Chuckwalla ~1, chuck.sp.trn,cutoff = 50000)
c.variog3.f <- fit.variogram(c.variog3, vgm(psill = 0.1, "Sph", range = 25000, nugget = 0))
plot(c.variog3, model = c.variog3.f)


## Lets make a quick grid for sampling

minx <- min(bbox(chuck.sp.utm)[1,])
maxx <- max(bbox(chuck.sp.utm)[1,])
miny <- min(bbox(chuck.sp.utm)[2,])
maxy <- max(bbox(chuck.sp.utm)[2,])
sidel <- 35000  # determines grid spacing (based on where SA tapers off)

proj <- CRS(proj4string(chuck.sp.utm))
x <- seq(from = minx, to = maxx, by = sidel) ## sequence of x centroids
y <- seq(from = miny, to = maxy, by = sidel) ## sequence of y for centroids


xy <- expand.grid(x = x, y = y)
grid.pts<-SpatialPointsDataFrame(coords= xy,data = data.frame(id = 1:dim(xy)[1]), proj4string = proj)
plot(grid.pts)
gridded(grid.pts) <- TRUE
grid <- as(grid.pts, "SpatialPolygons")


plot(grid)
points(grid.pts, col='red')


gridspdf <- SpatialPolygonsDataFrame(grid, data=data.frame(id=row.names(grid), row.names=row.names(grid), values = rep(1,length(grid))))
names.grd<-sapply
proj4string(gridspdf)
gridspdf <- spTransform(gridspdf, proj)
plot(gridspdf)
points(chuck.sp.utm, col='red')


getgrid <- over(gridspdf, chuck.sp.utm)
head(getgrid)
grids2.occ <- gridspdf[!is.na(getgrid$Chuckwalla),]
plot(grids2.occ)
points(chuck.sp.utm, col='red')


set.seed(24)
## find five per grid
nper = 5
keepchucks <- chuck.sp.utm[1,]
for(i in 1:length(grids2.occ)){
  tmp.poly <- grids2.occ[i,]
  #plot(tmp.poly)
  tmp.over <- over(chuck.sp.utm, tmp.poly)
  tmp.chucks <- chuck.sp.utm[!is.na(tmp.over$id),]
  #points(tmp.chucks)
  length(tmp.chucks)
  if(length(tmp.chucks) > nper){
  tmp.chucks <- tmp.chucks[sample(size = nper,x = 1:length(tmp.chucks)),]
  }
  #points(tmp.chucks, col='red')
  keepchucks <- rbind(keepchucks,tmp.chucks)
}
## Get rid of first row that you used to build the df
keepchucks <- keepchucks[-1,]

plot(gridspdf)
points(chuck.sp.utm, pch=19,col='blue')
points(keepchucks, pch=19, col='red')


set.seed(24)
keepchuck.pa.bm <- BIOMOD_FormatingData(resp.var = keepchucks$Chuckwalla,
                     expl.var = env,
                     resp.xy = coordinates(keepchucks),
                     resp.name = 'Chuckwalla',
                     #eval.resp.var = chuck.sp.utm.ex.tst$Chuckwalla,
                     #eval.expl.var = env,
                     #eval.resp.xy = coordinates(chuck.sp.utm.ex.tst),
                     PA.nb.rep = 1,
                     PA.nb.absences = dim(keepchucks)[1],
                     PA.strategy = 'sre',
                     #PA.dist.min = 1000,
                     #PA.dist.max = 20000,
                     PA.sre.quant = 0.1,
                     PA.table = NULL,
                     na.rm = TRUE)
keepchuck.pa.bm


## Grab the PA data from the first object
keepchuck.all <- SpatialPointsDataFrame(coords = keepchuck.pa.bm@coord, data = data.frame(Chuckwalla = keepchuck.pa.bm@data.species), proj = CRS(proj4string(chuck.sp.utm)))

## replace the NA from the PA with 0 to feed into next data selection

keepchuck.all$Chuckwalla[is.na(keepchuck.all$Chuckwalla)] <- 0
keepchuck.all

#plot(keepchuck.all[keepchuck.all$Chuckwalla == 0,], pch = 19, col='red', cex=0.5)
#points(keepchuck.all[keepchuck.all$Chuckwalla == 1,], pch = 19)

#summary(keepchuck.all)


 ## How many records are left?
howmanychucks.kc <- dim(keepchuck.all)[1]/2

## 70 % of those
pct70.kc <- round(howmanychucks.kc * 0.7)

## split pres from PA ####
k.chuck.Pres <- keepchuck.all[keepchuck.all$Chuckwalla == 1,]
k.chuck.Pa <- keepchuck.all[keepchuck.all$Chuckwalla == 0,]


## sample 70 pct from the whole length withouth replacement
trnchuckrows.kc <- sample(1:howmanychucks.kc, size = pct70.kc, replace = F)


## select those rows
k.chuck.Pres.trn <- k.chuck.Pres[trnchuckrows.kc,'Chuckwalla']
k.chuck.Pa.trn <- k.chuck.Pa[trnchuckrows.kc,'Chuckwalla']

## select the opposite for testing
k.chuck.Pres.tst <- k.chuck.Pres[-trnchuckrows.kc,'Chuckwalla']
k.chuck.Pa.tst <- k.chuck.Pa[-trnchuckrows.kc,'Chuckwalla']

## Combine rows #### 
k.chuck.trn <- rbind(k.chuck.Pres.trn, k.chuck.Pa.trn)
k.chuck.tst <- rbind(k.chuck.Pres.tst, k.chuck.Pa.tst)


### spatial autocorrelation
kc.variog <- variogram(Chuckwalla ~1, k.chuck.trn, xlab= "Distance (m)", ylab="Semivariance")
plot(kc.variog)

kc.variog2 <- variogram(Chuckwalla ~1, k.chuck.tst, xlab= "Distance (m)", ylab="Semivariance")
plot(kc.variog2) #check testing data for SA (it there)

### Zooming in a bit to focus on local effects
kc.variog3 <- variogram(Chuckwalla ~1, k.chuck.trn, cutoff = 50000, xlab= "Distance (m)", ylab="Semivariance")
kc.variog3.f <- fit.variogram(kc.variog3, vgm(psill = 0.1, "Sph", range = 25000, nugget = 0))
plot(kc.variog3, model = kc.variog3.f)


## create a new dataset with the testing data assigned in the eval slots
set.seed(24)
chuck.sp.bm <- BIOMOD_FormatingData(resp.var = k.chuck.trn$Chuckwalla,
                     expl.var = env,
                     resp.xy = coordinates(k.chuck.trn),
                     resp.name = 'Chuckwalla',
                     eval.resp.var = k.chuck.tst$Chuckwalla,
                     eval.expl.var = env,
                     eval.resp.xy = coordinates(k.chuck.tst),
                     PA.nb.rep = 0,
                     PA.nb.absences = NULL,
                     PA.strategy = NULL,
                     PA.dist.min = NULL,
                     PA.dist.max = NULL,
                     PA.sre.quant = NULL,
                     PA.table = NULL,
                     na.rm = TRUE)
 chuck.sp.bm
 
myBiomodOption <- BIOMOD_ModelingOptions() 
myBiomodOption

set.seed(28)
library(mgcv)
library(nlme)
# NOTE: We don't usually use the suppress warnings function, but needed it to spruce up the html file for the presentation.
suppressWarnings(system.time( myBiomodModelOut <- BIOMOD_Modeling( chuck.sp.bm, 
               models = c('SRE','GAM','GBM','RF'), # four algorithms
               models.options = myBiomodOption, # where to find algorithms
               NbRunEval=5, # number of iterations
               DataSplit=80, # used for internal data calibration
               VarImport=10, # num of bootstraps to determine var importance
               models.eval.meth = c('TSS','ROC','ACCURACY'), # performance metrics
               do.full.models=FALSE, # run using all training data
               rescal.all.models = T, # need for ensemble compatibility
               modeling.id="test")))

                                                           

myBiomodModelOut 


myBiomodModelEval <- get_evaluations(myBiomodModelOut)
myBiomodModelEval


dimnames(myBiomodModelEval)   # we specify two algorithms, and run 5
 myBiomodModelEval[c("ROC", "TSS","ACCURACY"),c("Testing.data","Evaluating.data", "Sensitivity","Specificity"),c("RF","GBM"),"RUN5",]


MyBiomodModelVarImp <- get_variables_importance(myBiomodModelOut)
dimnames(MyBiomodModelVarImp)
MyBiomodModelVarImp[,,"RUN5",]


str(MyBiomodModelVarImp)
rf.imp <- MyBiomodModelVarImp[1:7,4,1:5,1] #pulling out the rf data

##Have a look at the average value....
rfimp.m <- melt(rf.imp) #condenses data to more readable 
rfimp.av <- aggregate(value~ X1, data =rfimp.m, FUN = mean) #take average value of all runs for each covariate, mean importance 
rfimp.av.o <- rfimp.av[order(rfimp.av$value, decreasing = T),] #order them, highest at top 
## Plot means 
library(gplots)
levels(rfimp.m$X1)
rfimp.m$X1 <- factor(rfimp.m$X1, levels = rfimp.av.o$X1) #refactor to make levels equal to those we just made that are in order
plotmeans(value~ X1, data =rfimp.m, connect=F, n.label=F, xlab = '', ylab = 'Chuck', main = 'Chuck RF Models', las=2)


gbm.imp <- MyBiomodModelVarImp[1:7,3,1:5,1] #pulling out the rf data

##Have a look at the average value....
gbmimp.m <- melt(gbm.imp) #condenses data to more readable 
gbmimp.av <- aggregate(value~ X1, data =gbmimp.m, FUN = mean) #take average value of all runs for each covariate, mean importance 
gbmimp.av.o <- gbmimp.av[order(gbmimp.av$value, decreasing = T),] #order them, highest at top 
## Plot means 

levels(gbmimp.m$X1)
gbmimp.m$X1 <- factor(gbmimp.m$X1, levels = gbmimp.av.o$X1) #refactor to make levels equal to those we just made that are in order
plotmeans(value~ X1, data =gbmimp.m, connect=F, n.label=F, xlab = '', ylab = 'Chuck', main = 'Chuck GBM Models', las=2)


myMods <- BIOMOD_LoadModels(myBiomodModelOut, models= c('RF','GBM'), run.eval="RUN5")
response.plot2(models = myMods,
               Data = get_formal_data(myBiomodModelOut,'expl.var'),
               show.variables=       get_formal_data(myBiomodModelOut,'expl.var.names'),
               do.bivariate = FALSE,
               fixed.var.metric = 'median',
               col = c("blue", "red"),
               legend = TRUE,
               data_species = get_formal_data(myBiomodModelOut,'resp.var'))


myBiomodProj <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = env,
  proj.name = 'Chuckwalla_proj', 
  selected.models = c('Chuckwalla_AllData_RUN5_GBM','Chuckwalla_AllData_RUN5_RF'), 
  binary.meth = NULL, 
  compress = 'xz', 
  clamping.mask = F, 
  output.format = '.img')

myBiomodProj


myCurrentProj <- get_predictions(myBiomodProj)
class(myCurrentProj)
plot(myCurrentProj)


GBM_pred <-myCurrentProj[[1]]/1000
RF_pred <-myCurrentProj[[2]]/1000

plot(GBM_pred)
plot(RF_pred)


#writeRaster(GBM_pred, "")  ### insert file name to save 
#writeRaster(RF_pred, "")


save.image("Chuckspace.RData")

