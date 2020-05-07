setwd("D:/lilian/Mapas")

dir()

sp_name <- c("Trogon_rufus_pres.grd",
             "Trogon_surrucura_pres.grd",
             "Trogon_viridis_pres.grd",
             "Turdus_albicollis_pres.grd",
             "Turdus_amaurochalinus_pres.grd",
             "Turdus_flavipes_pres.grd",
             "Turdus_leucomelas_pres.grd",
             "Turdus_rufiventris_pres.grd",
             "Turdus_subalaris_pres.grd")

i=1

for (i in 1: length(sp_name)){

lista <- list.files(pattern = sp_name[i])

a <- raster(lista)
plot(a)

sp <- gsub(".grd", ".tif", sp_name[i])

writeRaster(a, sp, overwrite=T)

}
i

