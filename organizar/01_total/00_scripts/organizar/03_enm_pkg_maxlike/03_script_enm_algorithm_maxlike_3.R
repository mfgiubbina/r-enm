# 4 maxlike ---------------------------------------------------------------
# 4.1 fit
maxlike_fit <- maxlike::maxlike(formula = ~pc01 + I(pc01^2) + pc02 + I(pc02^2) + pc03 + I(pc03^2)
                                + pc04 + I(pc04^2) + pc05 + I(pc05^2) + pc06 + I(pc06^2),
                                raster = var, 
                                points = pr_specie[, c(1, 2)] %>% as.matrix,
                                savedata = TRUE)
maxlike_fit

summary(maxlike_fit)
confint(maxlike_fit)
AIC(maxlike_fit)
logLik(maxlike_fit)

# 3.1.2 projection
maxlike_proj <- raster::predict(maxlike_fit, progress = "text")	
maxlike_proj

# model export
raster::writeRaster(x = maxlike_proj, 
                    filename = "enm_haddadus_binotatus_maxlike", 
                    format = "GTiff", 
                    options = c("COMPRESS=DEFLATE"), 
                    overwrite = TRUE)

# map
landscapetools::show_landscape(maxlike_proj) +
  geom_point(data = pr_specie, aes(longitude, latitude), size = 3, alpha = .7, color = "red", pch = 20)
