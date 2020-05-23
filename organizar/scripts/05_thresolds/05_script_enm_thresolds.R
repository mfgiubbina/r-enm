### saguis hibridos ###

# magro et al. 2017

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 24/07/2017

###-----------------------------------------------------------------------------------------###

# memory
rm(list = ls())
gc() 
memory.limit(size = 1.75e13) 

# instalar e carregar pacotes
if(!require(pacman)) install.packages("pacman")
pacman::p_load(raster, data.table, ggplot2, prettymapr, maps, colorRamps)

###-----------------------------------------------------------------------------------------###

# diretorio
setwd("E:/dropbox/Dropbox/Modelando os saguis/ensemble_freq")

###-----------------------------------------------------------------------------------------###

# import data
po <- read.table("pontos.txt", h = T, sep = "\t")
head(po)

plot(po[, 2], po[, 3], pch = 20)

sp <- unique(po[, 1])
sp

tif <- dir(patt = ".tif$")
enm <- stack(tif)
enm

plot(enm[[grep(sp[1], names(enm))]])
points(po[po$id == sp[1], 2], po[po$id == sp[1], 3], pch = 20)
addscalebar(pos = "bottomright", plotepsg = 4326)
addnortharrow(pos = "topleft", padin = c(0.15, 0.15), scale = .7, lwd = 1, 
              border = "black", cols = c("black", "black"), text.col = "black")


###-----------------------------------------------------------------------------------------###

# thresholds
dir.create("thresholds")
setwd("thresholds")

for(i in sp){
  po.sp <- po[po$id == i, 2:3]
  enm.sp <- enm[[grep(i, names(enm))]]
  sui <- extract(enm.sp, po.sp)
  
  lpt <- min(sui[sui > 0], na.rm = T)
  p10 <- quantile(sui, p = .1, na.rm = T)
  p20 <- quantile(sui, p = .2, na.rm = T)
  
  sui.na <- data.frame(sui = c(lpt, p10, p20), na = c("lpt", "p10", "p20"))
  
  ggplot(data = as.data.frame(sui), aes(as.data.frame(sui)$sui)) + 
    geom_histogram(breaks = seq(0, 1, by = .1), 
                   col = "black", 
                   fill = "gray5", 
                   alpha = .2) + 
    geom_vline(xintercept = lpt, color = "red", lwd = 1.1) +
    geom_vline(xintercept = p10, color = "blue", lwd = 1.1) +
    geom_vline(xintercept = p20, color = "forest green", lwd = 1.1) +
    geom_text(data = sui.na, mapping = aes(x = sui, y = 0, label = na), 
              size = 6, angle = 90, vjust = -0.4, hjust = -(max(table(sui)) - 3)) +
    labs(title = paste0("Histogram for Suitability of ", i), 
         x = "Suitability", y = "Count")
  
  ggsave(paste0(i, "_00_hist_.tiff"), dpi = 300)
  
    
  tiff(paste0(i, "_01_map_total.tif"), he = 18, wi = 18, un = "cm", res = 300, comp = "lzw") 
  plot(enm.sp, col = matlab.like2(100), main = paste0("Map of ", i, " Total"))
  points(po.sp, pch = 20, cex = 1.5)
  addscalebar(pos = "bottomright", plotepsg = 4326)
  addnortharrow(pos = "topleft", padin = c(0.15, 0.15), scale = .7, lwd = 1, 
                border = "black", cols = c("black", "black"), text.col = "black")
  dev.off()
  
  writeRaster(enm.sp > lpt, paste0("enm_", i, "_lpt.tif"), format = "GTiff")
  tiff(paste0(i, "_02_map_lpt.tif"), he = 18, wi = 18, un = "cm", res = 300, comp = "lzw") 
  plot(enm.sp > lpt, main = paste0("Map of ", i, " LPT"))
  points(po.sp, pch = 20, cex = 1.5)
  addscalebar(pos = "bottomright", plotepsg = 4326)
  addnortharrow(pos = "topleft", padin = c(0.15, 0.15), scale = .7, lwd = 1, 
                border = "black", cols = c("black", "black"), text.col = "black")
  dev.off()
  
  writeRaster(enm.sp > p10, paste0("enm_", i, "_p10.tif"), format = "GTiff")
  tiff(paste0(i, "_03_map_p10.tif"), he = 18, wi = 18, un = "cm", res = 300, comp = "lzw") 
  plot(enm.sp > p10, main = paste0("Map of ", i, " P10"))
  points(po.sp, pch = 20, cex = 1.5)
  addscalebar(pos = "bottomright", plotepsg = 4326)
  addnortharrow(pos = "topleft", padin = c(0.15, 0.15), scale = .7, lwd = 1, 
                border = "black", cols = c("black", "black"), text.col = "black")
  dev.off()
  
  writeRaster(enm.sp > p20, paste0("enm_", i, "_p20.tif"), format = "GTiff")
  tiff(paste0(i, "_04_map_p20.tif"), he = 18, wi = 18, un = "cm", res = 300, comp = "lzw") 
  plot(enm.sp > p20, main = paste0("Map of ", i, " P20"))
  points(po.sp, pch = 20, cex = 1.5)
  addscalebar(pos = "bottomright", plotepsg = 4326)
  addnortharrow(pos = "topleft", padin = c(0.15, 0.15), scale = .7, lwd = 1, 
                border = "black", cols = c("black", "black"), text.col = "black")
  dev.off()
  
  print(paste0("Opa for ", i, "!"))}
  


