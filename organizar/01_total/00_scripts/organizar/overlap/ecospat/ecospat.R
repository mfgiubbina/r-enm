# Niche Quantification and Comparison with Ordination techniques

# packages
library(ecospat)
library(raster)

# data
data(ecospat.testNiche.inv)
data(ecospat.testNiche.nat)

inv <- ecospat.testNiche.inv
nat <- ecospat.testNiche.nat

# maps
inv_grid <- inv
gridded(inv_grid) <- ~x + y
inv_grid
inv_raster <- raster::stack(inv_grid)
inv_raster
plot(inv_raster)

nat_grid <- nat
gridded(nat_grid) <- ~x + y
nat_grid
nat_raster <- raster::stack(nat_grid)
nat_raster
plot(nat_raster)

# PCA-ENVIRONMENT
# The PCA is calibrated on all the sites of the study area
pca_env <- ade4::dudi.pca(rbind(nat, inv)[, 3:10], scannf = FALSE , nf = 2)
pca_env

# Plot Variables Contribution with ecospat.plot.contrib()
ecospat.plot.contrib(contrib = pca_env$co, eigen = pca_env$eig)

# Predict the scores on the axes
# PCA scores for the whole study area
scores_globclim <- pca_env$li
scores_globclim

# PCA scores for the species native distribution
scores_sp_nat <- suprow(pca_env, nat[which(nat[, 11] == 1), 3:10])$li
scores_sp_nat

# PCA scores for the species invasive distribution
scores_sp_inv <- suprow(pca_env, inv[which(inv[, 11] == 1), 3:10])$li
scores_sp_inv

# PCA scores for the whole native study area
scores_clim_nat <- suprow(pca_env, nat[, 3:10])$li
scores_clim_nat

# PCA scores for the whole invaded study area
scores_clim_inv <- suprow(pca_env, inv[, 3:10])$li
scores_clim_inv

# Calculate the Occurrence Densities Grid with ecospat.grid.clim.dyn()
# gridding the native niche
grid_clim_nat <- ecospat.grid.clim.dyn(glob = scores_globclim,
                                       glob1 = scores_clim_nat,
                                       sp = scores_sp_nat, 
                                       R = 100,
                                       th.sp = 0)
grid_clim_nat
plot(grid_clim_nat$w)

# gridding the invasive niche
grid_clim_inv <- ecospat.grid.clim.dyn(glob = scores_globclim,
                                       glob1 = scores_clim_inv,
                                       sp = scores_sp_inv, 
                                       R = 100,
                                       th.sp = 0)
grid_clim_inv
plot(grid_clim_inv$w)

# Calculate Niche Overlap with ecospat.niche.overlap()
# Compute Schoener's D, index of niche overlap
D_overlap <- ecospat.niche.overlap(grid_clim_nat, grid_clim_inv, cor = TRUE)
D_overlap

# Perform the Niche Equivalency Test with ecospat.niche.equivalency.test() according to Warren et al. (2008)
eq_test <- ecospat.niche.equivalency.test(grid_clim_nat, 
                                          grid_clim_inv,
                                          rep = 1e1, 
                                          alternative = "greater")
eq_test

# Perform the Niche Similarity Test with ecospat.niche.similarity.test()
sim_test <- ecospat.niche.similarity.test(grid_clim_nat, 
                                          grid_clim_inv,
                                          rep = 1e1, 
                                          alternative = "greater",
                                          rand.type = 2)
sim_test

# Plot tests
ecospat.plot.overlap.test(eq_test, "D", "Equivalency")
ecospat.plot.overlap.test(sim_test, "D", "Similarity")

# Delimiting niche categories and quantifying niche dynamics in analogue climates with ecospat.niche.dyn.index()
niche_dyn <- ecospat.niche.dyn.index(grid_clim_nat, grid_clim_inv, intersection = .1)
niche_dyn

ecospat.plot.niche.dyn(grid_clim_nat, 
                       grid_clim_inv, 
                       quant = .25, 
                       interest = 2,
                       title = "Niche Overlap", 
                       name.axis1 = "PC1",
                       name.axis2 = "PC2")
ecospat.shift.centroids(scores_sp_nat, 
                        scores_sp_inv, 
                        scores_clim_nat, 
                        scores_clim_inv)

# gridding the native niche
grid_clim_t_nat <- ecospat.grid.clim.dyn(glob = as.data.frame(rbind(nat, inv)[, 8]),
                                         glob1 = as.data.frame(nat[, 8]),
                                         sp = as.data.frame(nat[which(nat[, 11] == 1), 8]),
                                         R = 1e3, 
                                         th.sp = 0)
grid_clim_t_nat

# gridding the invaded niche
grid_clim_t_inv <- ecospat.grid.clim.dyn(glob = as.data.frame(rbind(nat, inv)[, 8]),
                                         glob1 = as.data.frame(inv[, 8]),
                                         sp = as.data.frame(inv[which(inv[, 11] == 1), 8]),
                                         R = 1000, 
                                         th.sp = 0)
grid_clim_t_inv

t_dyn <- ecospat.niche.dyn.index (grid_clim_t_nat, 
                                  grid_clim_t_inv,
                                intersection = .1)
t_dyn

ecospat.plot.niche.dyn(grid_clim_t_nat, 
                       grid_clim_t_inv, 
                       quant = 0,
                       interest = 2, 
                       title = "Niche Overlap",
                       name.axis1 = "Average temperature")
