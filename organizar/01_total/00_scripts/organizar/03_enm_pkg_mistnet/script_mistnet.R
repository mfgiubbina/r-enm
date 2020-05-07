

# packages
library(spocc)
library(maptools)
library(raster)
library(rgdal)
library(rgeos)
library(ENMeval)
library(dismo)


# Search GBIF for occurrence data.
bv <- occ('Bradypus variegatus', 'gbif', limit=300, has_coords=TRUE)
bv

# Get the latitude/coordinates for each locality. Also convert the tibble that occ() outputs
# to a data frame for compatibility with ENMeval functions.
occs <- as.data.frame(bv$gbif$data$Bradypus_variegatus[,2:3])

# Remove duplicate rows (Note that you may or may not want to do this).
occs <- occs[!duplicated(occs),]

# First, load some predictor rasters from the dismo folder:
files <- list.files(path=paste(system.file(package='dismo'), '/ex', sep=''), pattern='grd', full.names=TRUE)

# Put the rasters into a RasterStack:
envs <- stack(files)

# Plot first raster in the stack, bio1
plot(envs[[1]], main=names(envs)[1])

# Add points for all the occurrence points onto the raster
points(occs)

# There are some points all the way to the south-east, far from all others. Let's say we know that this represents a subpopulation that we don't want to include, and want to remove these points from the analysis. We can find them by first sorting the occs table by latitude.
head(occs[order(occs$latitude),])
#>     longitude  latitude
#> 208 -43.40630 -23.00829
#> 228 -42.99930 -22.49430
#> 230 -43.00060 -22.49420
#> 226 -43.00000 -22.49400
#> 225 -42.73640 -22.40630
#> 295 -40.06526 -19.39466

# We see there are two such points, and we can find them by specifying a logical statement that says to find all records with latitude less than -20.
index <- which(occs$latitude < (-20))

# Next, let's subset our dataset to remove them by using the negative assignment on the index vector.
occs <- occs[-index,]

# Let's plot our new points over the old ones to see what a good job we did.
points(occs, col='red')

# Make a SpatialPoints object
occs.sp <- SpatialPoints(occs)

# Get the bounding box of the points
bb <- bbox(occs.sp)

# Add 5 degrees to each bound by stretching each bound by 10, as the resolution is 0.5 degree.
bb.buf <- extent(bb[1]-10, bb[3]+10, bb[2]-10, bb[4]+10)

# Crop environmental layers to match the study extent
envs.backg <- crop(envs, bb.buf)




# Get a simple world countries polygon
data(wrld_simpl)

# Get polygons for Central and South America
ca.sa <- wrld_simpl[wrld_simpl@data$SUBREGION==5 | wrld_simpl@data$SUBREGION==13,]

# Both spatial objects have the same geographic coordinate system with slightly different specifications, so just name the coordinate reference system (crs) for ca.sa with that of
# envs.backg to ensure smooth geoprocessing.
crs(envs.backg) <- crs(ca.sa)

# Mask envs by this polygon after buffering a bit to make sure not to lose coastline.
ca.sa <- gBuffer(ca.sa, width=1)
envs.backg <- mask(envs.backg, ca.sa)

# Let's check our work. We should see Central and South America without the Carribbean.
plot(envs.backg[[1]], main=names(envs.backg)[1])
points(occs)


# Randomly sample 10,000 background points from one background extent raster (only one per cell without replacement). Note: Since the raster has <10,000 pixels, you'll get a warning and all pixels will be used for background. We will be sampling from the biome variable because it is missing some grid cells, and we are trying to avoid getting background points with NA.
bg <- randomPoints(envs.backg[[9]], n=10000)
bg <- as.data.frame(bg)

# Notice how we have pretty good coverage (every cell).
plot(envs.backg[[1]], legend=FALSE)
points(bg, col='red')


blocks <- get.block(occs, bg)
str(blocks)
#> List of 2
#>  $ occ.grp: num [1:264] 3 3 2 1 2 1 1 4 1 3 ...
#>  $ bg.grp : num [1:5600] 2 2 2 2 2 2 2 2 2 2 ...

plot(envs.backg[[1]], col='gray', legend=FALSE)
points(occs, pch=21, bg=blocks$occ.grp)


check1 <- get.checkerboard1(occs, envs, bg, aggregation.factor=5)

plot(envs.backg[[1]], col='gray', legend=FALSE)
points(occs, pch=21, bg=check1$occ.grp)

# The partitioning method is more clearly illustrated by looking at the background points:
points(bg, pch=21, bg=check1$bg.grp)


# We can change the aggregation factor to better illustrate how this partitioning method works:
check1.large <- get.checkerboard1(occs, envs, bg, aggregation.factor=30)
plot(envs.backg[[1]], col='gray', legend=FALSE)
points(bg, pch=21, bg=check1.large$bg.grp)
points(occs, pch=21, bg=check1.large$occ.grp, col='white', cex=1.5)


check2 <- get.checkerboard2(occs, envs, bg, aggregation.factor=c(5,5))

plot(envs.backg[[1]], col='gray', legend=FALSE)
points(bg, pch=21, bg=check2$bg.grp)
points(occs, pch=21, bg=check2$occ.grp, col='white', cex=1.5)


jack <- get.jackknife(occs, bg)

plot(envs.backg[[1]], col='gray', legend=FALSE)
points(occs, pch=21, bg=jack$occ.grp)  # note that colors are repeated here


random <- get.randomkfold(occs, bg, k=5)

plot(envs.backg[[1]], col='gray', legend=FALSE)
points(occs, pch=21, bg=random$occ.grp)



ngrps <- 10
kmeans <- kmeans(occs, ngrps)
occ.grp <- kmeans$cluster

plot(envs.backg[[1]], col='gray', legend=FALSE)
points(occs, pch=21, bg=occ.grp)


bg.grp <- rep(0, nrow(bg))

plot(envs.backg[[1]], col='gray', legend=FALSE)
points(bg, pch=16, bg=bg.grp)

centers <- kmeans$center
d <- pointDistance(bg, centers, lonlat=T)
bg.grp <- apply(d, 1, function(x) which(x==min(x)))

plot(envs.backg[[1]], col='gray', legend=FALSE)
points(bg, pch=21, bg=bg.grp)


eval1 <- ENMevaluate(occs, envs, bg, method='checkerboard2', RMvalues=c(1,2), fc=c('L'), algorithm='maxent.jar')
eval1

plot(eval1@predictions[[which(eval1@results$delta.AICc==0)]], main="Relative occurrence rate")
