


# preparate r -------------------------------------------------------------
# packages
library(ENMTools)
library(tidyverse)

# directory
setwd("/home/mude/data/github/r-enm/01_enm/00_present/00_scripts/ENMTools")

# data --------------------------------------------------------------------
# creating enmtools.species objects
# var
env.files <- list.files(path = "./testdata/", pattern = "pc", full.names = TRUE)
env.files

env <- stack(env.files)
env
plot(env)

names(env) <- c("pc1", "pc2", "pc3", "pc4")
env <- raster::setMinMax(env)
env
plot(env)

# occ
ahli <- ENMTools::enmtools.species()
ahli

ahli$species.name <- "ahli"
ahli$presence.points <- read.csv("./testdata/ahli.csv")[,2:3]
ahli$range <- ENMTools::background.raster.buffer(ahli$presence.points, 50000, mask = env)
ahli$background.points <- ENMTools::background.points.buffer(points = ahli$presence.points,
                                                             radius = 20000, n = 1000, mask = env[[1]])
ahli

plot(ahli$presence.points)
plot(ahli$background.points)



allogus <- ENMTools::enmtools.species(
  species.name = "allogus", 
  presence.points = read.csv("./testdata/allogus.csv")[,2:3],
  range = ENMTools::background.raster.buffer(read.csv("./testdata/allogus.csv")[,2:3], 50000, mask = env),
  background.points = ENMTools::background.points.buffer(points = read.csv("./testdata/allogus.csv")[,2:3],
                                                         radius = 20000, n = 1000, mask = env[[1]])
  )
allogus

# sample data
data(iberolacerta.clade)
data(euro.worldclim)
monticola <- iberolacerta.clade$species$monticola
monticola
cyreni <- iberolacerta.clade$species$cyreni
cyreni
env <- euro.worldclim
env

# Building an ENM ---------------------------------------------------------
# correlation
ENMTools::raster.cor.matrix(env, method = "spearman")
ENMTools::raster.cor.plot(env)$cor.mds.plot
ENMTools::raster.cor.plot(env)$cor.heatmap

env <- env[[c("bio1", "bio12", "bio7")]]
plot(env)

raster.cor.matrix(env, method = "spearman")

# glm
monticola_glm <- ENMTools::enmtools.glm(species = monticola, env = env, f = pres ~ bio1 + bio12 + bio7, test.prop = .3)
monticola_glm <- ENMTools::enmtools.glm(species = monticola, env = env, test.prop = .3)
monticola_glm

monticola_glm <- ENMTools::enmtools.glm(species = monticola, env = env, 
                                        f = pres ~ poly(bio1, 2) + poly(bio7, 2) * poly(bio12, 2), test.prop = .3)
monticola_glm

monticola_glm$response.plots$bio1
monticola_glm$response.plots$bio7
monticola_glm$response.plots$bio12

ENMTools::visualize.enm(monticola_glm, env, layers = c("bio1", "bio12"), plot.test.data = TRUE)$background.plot
ENMTools::visualize.enm(monticola_glm, env, layers = c("bio1", "bio12"), plot.test.data = TRUE)$suit.plot

# GAM, Bioclim, Domain, and Maxent
monticola_gam <- ENMTools::enmtools.gam(monticola, env, f = pres ~ poly(bio1, 2) + poly(bio7, 2) * poly(bio12, 2), test.prop = .3)
monticola_gam

monticola_dm <- ENMTools::enmtools.dm(monticola, env, test.prop = .3)
monticola_dm

monticola_bc <- ENMTools::enmtools.bc(monticola, env, test.prop = .3)
monticola_bc

monticola_mx <- ENMTools::enmtools.maxent(monticola, env, test.prop = .3)
monticola_mx

# similarity between ENMs -------------------------------------------------
# Metrics: breadth, correlation, and overlap
ENMTools::raster.breadth(monticola_glm)

# enm
monticola_glm <- ENMTools::enmtools.glm(species = monticola, env = env, 
                                        f = pres ~ poly(bio1, 2) + poly(bio7, 2) + poly(bio12, 2), test.prop = .3)
monticola_glm

cyreni_glm <- ENMTools::enmtools.glm(species = cyreni, env = env, 
                                     = pres ~ poly(bio1, 2) + poly(bio7, 2) + poly(bio12, 2), test.prop = .3)
cyreni_glm

# raste overlap
ENMTools::raster.overlap(monticola_glm, cyreni_glm)

# env. overlap
ENMTools::env.overlap(monticola_glm, cyreni_glm, env, tolerance = .001)

# Hypothesis testing ------------------------------------------------------
# Niche identity or equivalency test
id_glm <- ENMTools::identity.test(species.1 = monticola, species.2 = cyreni, env = env, type = "glm", nreps = 4)
id_glm

id_gam <- ENMTools::identity.test(species.1 = monticola, species.2 = cyreni, env = env, type = "gam", nreps = 4)
id_gam

# Background or similarity test
bg_bc_asym <- ENMTools::background.test(species.1 = monticola, species.2 = cyreni, env = env, type = "bc", nreps = 4, test.type = "asymmetric" )
bg_bc_asym

bg_dm_sym <- ENMTools::background.test(species.1 = monticola, species.2 = cyreni, env = env, type = "dm", nreps = 4, test.type = "symmetric" )
bg_md_sym

# Ecospat tests
esp_id <- ENMTools::enmtools.ecospat.id(monticola, cyreni, env)
esp_id
esp_id$d.plot + theme_bw()
esp_id$i.plot + theme_bw()

esp_bg_sym <- ENMTools::enmtools.ecospat.bg(monticola, cyreni, env[[c("bio1", "bio12")]], test.type = "symmetric")
esp_bg_sym$d.plot + theme_bw()

esp_bg_sym <- ENMTools::enmtools.ecospat.bg(monticola, cyreni, env, test.type = "symmetric")
esp_bg_sym

# end ---------------------------------------------------------------------