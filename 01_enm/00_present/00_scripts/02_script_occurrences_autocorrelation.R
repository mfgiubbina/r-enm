# import raster
setwd("/home/mude/data/gitlab/r-sdm/00_pragmatico/00_present/01_variables/03_var")
var <- dir(pattern = ".tif$") %>% 
  raster::stack()
var
plot(var)

# transform coordinates
occ_data_taxa_temp_bias_pseudo <- occ_data_taxa_temp_bias %>% 
  dplyr::mutate(x = longitude, y = latitude) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  sf::st_transform(crs = 3857) %>% 
  sf::st_coordinates() %>% 
  tibble::as_tibble() %>% 
  dplyr::bind_cols(occ_data_taxa_temp_bias, .)
occ_data_taxa_temp_bias_pseudo

# autocorrelation
for(i in occ_data_taxa_temp_bias$species %>% unique){}

# information
print(i)

# distance matrix
dst_i_p <- occ_data_taxa_temp_bias_pseudo %>% 
  dplyr::filter(species == i) %>% 
  dplyr::select(X, Y) %>% 
  raster::pointDistance(lonlat = FALSE) %>% 
  as.vector() %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(dist_km = value/1e3) %>% 
  dplyr::select(dist_km)
dst_i_p

dst_i_l <- occ_data_taxa_temp_bias_pseudo %>% 
  dplyr::filter(species == i) %>% 
  dplyr::select(longitude, latitude) %>% 
  raster::pointDistance(lonlat = TRUE) %>% 
  as.vector() %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(dist_km = value/1e3) %>% 
  dplyr::select(dist_km)
dst_i_l

da <- data.frame(l = dst_i_l$dist_km, p = dst_i_p$dist_km)

plot(da)

cor(na.omit(da))

# histogram
mediam_dis_l <- dst_i_l$dist_km %>% median %>% round(., 0)
mediam_dis_p <- dst_i_p$dist_km %>% median %>% round(., 0)

ggplot(data = dst_i_l) +
  aes(x = dist_km) +
  geom_histogram(color = "gray30", fill = "gray50", bins = 30) +
  geom_vline(xintercept = mediam_dis, lty = 2, size = 1) +
  annotate(geom = "text", x = mediam_dis - (mediam_dis*.7), y = 1e3, label = paste0(mediam_dis, " km")) +
  labs(x = "Distance (km)", y = "Frequency") +
  theme_bw()

ggplot(data = dst_i_p) +
  aes(x = dist_km) +
  geom_histogram(color = "gray30", fill = "gray50", bins = 30) +
  geom_vline(xintercept = mediam_dis, lty = 2, size = 1) +
  annotate(geom = "text", x = mediam_dis - (mediam_dis*.7), y = 1e3, label = paste0(mediam_dis, " km")) +
  labs(x = "Distance (km)", y = "Frequency") +
  theme_bw()

# correlograms
da_cor <- NULL
max_dist <- NULL

for(j in 1:nlayers(var)){
  
  # information
  paste0(i, " - ", names(var[[j]])) %>% print
  
  # extract
  occ_i <- occ_data_taxa_temp_bias_pseudo %>% 
    dplyr::filter(species == i) %>% 
    dplyr::select(longitude, latitude) %>% 
    dplyr::mutate(var = raster::extract(var[[j]], .)) %>%
    dplyr::bind_cols(., occ_data_taxa_temp_bias_pseudo[occ_data_taxa_temp_bias_pseudo$species == i, c("X", "Y")]) %>% 
    dplyr::select(X, Y, var) %>% 
    na.omit %>% 
    as.data.frame
  
  colnames(occ_i) <- c("x", "y", names(var[[j]]))
  
  # correlograms
  corr <- ncf::correlog(x = occ_i[, 1],
                        y = occ_i[, 2],
                        z = occ_i[, 3], 
                        increment = 1e5,
                        # latlon = TRUE,
                        resamp = 100)
  
  da_cor_j <- data.frame(var = names(var[[j]]), 
                         dist = corr$mean.of.class %>% as.vector, 
                         morans = corr$correlation %>% as.vector)
  
  da_cor <- rbind(da_cor, da_cor_j)
  
}

da_cor %>% 
  ggplot() +
  aes(x = dist/1e3, y = morans, color = var) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, color = "red", lty = 2) +
  geom_hline(yintercept = .3, color = "red", lty = 2) +
  # xlim(c(0, 1500)) +
  ylim(c(-1, 1)) +
  labs(x = "Distance (km)", y = "Moran's I") +
  theme_bw() +
  theme(legend.position = c("top"))

ggplot() +
  geom_raster(data = raster::rasterToPoints(euc_dist$ed_haddadus_binotatus) %>% tibble::as_tibble(),
              aes(x, y, fill = ed_haddadus_binotatus)) +
  scale_fill_viridis_c() +
  geom_sf(data = li, alpha = .5) +
  geom_point(data = occ_data_taxa_temp_bias, 
             aes(x = longitude, y = latitude), 
             shape = 20, alpha = .75) +
  coord_sf(xlim = raster::bbox(euc_dist)[1, ], ylim = raster::bbox(euc_dist)[2, ]) +
  theme_bw() +
  theme(legend.position = "none")
