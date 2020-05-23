# raster euclidean distance
euc_dist_i <- distanceFromPoints(object = var, 
                                 xy = occ_data_taxa_temp_bias %>% 
                                   dplyr::filter(species == i) %>% 
                                   dplyr::select(longitude, latitude),
                                 progress = "text") 
names(euc_dist_i) <- "ed"

# stack
euc_dist <- raster::stack(var, euc_dist_i) %>% 
  raster::values() %>%
  tibble::as_tibble() %>% 
  dplyr::mutate(id = raster::ncell(var) %>% seq) %>% 
  na.omit() %>% 
  
  euc_dist
