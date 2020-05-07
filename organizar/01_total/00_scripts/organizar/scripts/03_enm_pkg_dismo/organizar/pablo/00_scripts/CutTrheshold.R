CutTrheshold <- function(po.sp, 
                         file.model, 
                         path.model, 
                         path.result.trheshold, 
                         ncel.total,
                         periodo,
                         id.specie,
                         aogcm,
                         algoritmo,
                         rodada,
                         d18O,
                         erro_d18O,
                         eval,
                         enm.sp,
                         save.raster=F,
                         save.images=F,
                         plotpoint=F)
{
   
  da.thrs <- data.table()
  
  file.img = paste(rodada,sub(' ','_',id.specie),aogcm,resolution,algoritmo,periodo, sep='_') 
  label = as.character(paste(id.specie,aogcm,resolution,algoritmo,paste0(periodo,'Kyr'),paste0('(',rodada,')'), sep=' '))
  
  sui <- extract(enm.sp, po.sp, method = 'bilinear')
  
  max.extact.occ <- max(sui)
  min.extact.occ <- min(sui) 
  
  resumo <- summary(getValues(enm.sp))

  lpt <- min(sui[sui > 0], na.rm = T)
  p10 <- quantile(sui, p = .1, na.rm = T)
  p20 <- quantile(sui, p = .2, na.rm = T)
  
  sui.na <- data.frame(sui = c(lpt, p10, p20), na = c("lpt", "p10", "p20"))
  
  en.lpt <- enm.sp >= lpt
  en.p10 <- enm.sp >= p10
  en.p20 <- enm.sp >= p20

  ncel.lpt <- as.numeric(table(na.omit(en.lpt[]))[2])
  ncel.p10 <- as.numeric(table(na.omit(en.p10[]))[2])
  ncel.p20 <- as.numeric(table(na.omit(en.p20[]))[2])
  
  perc.neo.lpt <- percent(ncel.lpt/ncel.total) 
  perc.neo.p10 <- percent(ncel.p10/ncel.total) 
  perc.neo.p20 <- percent(ncel.p20/ncel.total)
 
  da.thrs <- rbind(da.thrs, data.table(trheshold = 'lpt',
                                       periodo = periodo,
                                       sp = id.specie,
                                       aogcm = aogcm,
                                       algoritmo = algoritmo,
                                       rodada = rodada,
                                       ncel = ncel.lpt,
                                       perc.neo = perc.neo.lpt,
                                       trheshold_value = lpt,
                                       d18O = d18O,
                                       erro_d18O = erro_d18O,
                                       max_extact_occ = max.extact.occ,
                                       min_extact_occ = min.extact.occ,
                                       Min  =   round(as.numeric(resumo[1]),2),
                                       Qu1st  = round(as.numeric(resumo[2]),2),
                                       Median = round(as.numeric(resumo[3]),2),
                                       Mean  =  round(as.numeric(resumo[4]),2),
                                       Qu3rd  = round(as.numeric(resumo[5]),2),    
                                       Max =    round(as.numeric(resumo[6]),2),    
                                       NumNAs = as.numeric(resumo[7]),
                                       thrs_modelo = eval[1],
                                       AUC_modelo =  eval[2],
                                       TSS_modelo =  eval[3]))
  
  da.thrs <- rbind(da.thrs, data.table(trheshold = 'p10',
                                       periodo = periodo,
                                       sp = id.specie,
                                       aogcm = aogcm,
                                       algoritmo = algoritmo,
                                       rodada = rodada,
                                       ncel = ncel.p10,
                                       perc.neo = perc.neo.p10,
                                       trheshold_value = p10,
                                       d18O = d18O,
                                       erro_d18O = erro_d18O,
                                       max_extact_occ = max.extact.occ,
                                       min_extact_occ = min.extact.occ,
                                       Min  =   round(as.numeric(resumo[1]),2),
                                       Qu1st  = round(as.numeric(resumo[2]),2),
                                       Median = round(as.numeric(resumo[3]),2),
                                       Mean  =  round(as.numeric(resumo[4]),2),
                                       Qu3rd  = round(as.numeric(resumo[5]),2),    
                                       Max =    round(as.numeric(resumo[6]),2),    
                                       NumNAs = as.numeric(resumo[7]),
                                       thrs_modelo = eval[1],
                                       AUC_modelo =  eval[2],
                                       TSS_modelo =  eval[3]))
  
  da.thrs <- rbind(da.thrs, data.table(trheshold = 'p20',
                                       periodo = periodo,
                                       sp = id.specie,
                                       aogcm = aogcm,
                                       algoritmo = algoritmo,
                                       rodada = rodada,
                                       ncel = ncel.p20,
                                       perc.neo = perc.neo.p20,
                                       trheshold_value = p20,
                                       d18O = d18O,
                                       erro_d18O = erro_d18O,
                                       max_extact_occ = max.extact.occ,
                                       min_extact_occ = min.extact.occ,
                                       Min  =   round(as.numeric(resumo[1]),2),
                                       Qu1st  = round(as.numeric(resumo[2]),2),
                                       Median = round(as.numeric(resumo[3]),2),
                                       Mean  =  round(as.numeric(resumo[4]),2),
                                       Qu3rd  = round(as.numeric(resumo[5]),2),    
                                       Max =    round(as.numeric(resumo[6]),2),    
                                       NumNAs = as.numeric(resumo[7]),
                                       thrs_modelo = eval[1],
                                       AUC_modelo =  eval[2],
                                       TSS_modelo =  eval[3]))
  # resultados  
  setwd(path.result.trheshold)
  path.result.trheshold <- paste0(path.result.trheshold,'/trheshold')
  if(!dir.exists(path.result.trheshold)){dir.create("trheshold")}
  setwd(path.result.trheshold)
  path.lpt <- paste0(path.result.trheshold,'/lpt')
  path.p10 <- paste0(path.result.trheshold,'/p10')
  path.p20 <- paste0(path.result.trheshold,'/p20')
  if(!dir.exists(path.lpt)){dir.create("lpt")}
  if(!dir.exists(path.p10)){dir.create("p10")}
  if(!dir.exists(path.p20)){dir.create("p20")}
  
  if (save.images==T){
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
      #Histogram for Suitability of 
      labs(title = paste0("Hist. Suitability ", label), 
           x = "Suitability", y = "Count")
    ggsave(paste0('hist_',file.save,".tiff"), dpi = 300)
    
    tiff(paste0('total_',file.img,'.tif'), he = 18, wi = 18, un = "cm", res = 300, comp = "lzw") 
    plot(enm.sp, col = matlab.like2(100), main = paste0("Map of ", label, " Total"))
    points(po.sp, pch = 20, cex = 1.5)
    addscalebar(pos = "bottomright", plotepsg = 4326)
    addnortharrow(pos = "topleft", padin = c(0.15, 0.15), scale = .7, lwd = 1, 
                  border = "black", cols = c("black", "black"), text.col = "black")
    dev.off()}
  
  setwd(path.lpt)
  if (save.raster==T){writeRaster(en.lpt, paste0('raster_lpt_',file.img), format = "GTiff",overwrite=T)}
  if (save.images==T){
    tiff(paste0('map_lpt_',file.img,'.tif'), he = 18, wi = 18, un = "cm", res = 300, comp = "lzw") 
    plot(en.lpt, main = paste0("Map of ", label, " LPT (",as.character(perc.neo.lpt),")"))
    if (plotpoint==T) {points(po.sp, pch = 20, cex = 1.5)}
    #points(po.sp.model, pch = 1, cex = 1.5)
    addscalebar(pos = "bottomright", plotepsg = 4326)
    addnortharrow(pos = "topleft", padin = c(0.15, 0.15), scale = .7, lwd = 1, 
                  border = "black", cols = c("black", "black"), text.col = "black")
    dev.off()}
  
  setwd(path.p10)
  if (save.raster==T){writeRaster(en.p10, paste0('raster_p10_',file.img), format = "GTiff", overwrite=T)}
  if (save.images==T){
    tiff(paste0('map_p10_',file.img,'.tif'), he = 18, wi = 18, un = "cm", res = 300, comp = "lzw") 
    plot(en.p10, main = paste0("Map of ", label, " P10 (",perc.neo.p10,")"))
    if (plotpoint==T) {points(po.sp, pch = 20, cex = 1.5)}
    addscalebar(pos = "bottomright", plotepsg = 4326)
    addnortharrow(pos = "topleft", padin = c(0.15, 0.15), scale = .7, lwd = 1, 
                  border = "black", cols = c("black", "black"), text.col = "black")
    dev.off()}
  
  setwd(path.p20)
  if (save.raster==T){writeRaster(en.p20, paste0('raster_p20_',file.img), format = "GTiff", overwrite=T)}
  if (save.images==T){
    tiff(paste0('map_p20_',file.img,'.tif'), he = 18, wi = 18, un = "cm", res = 300, comp = "lzw") 
    plot(en.p20, main = paste0("Map of ", label, " P20 (",perc.neo.p20,")"))
    if (plotpoint==T) {points(po.sp, pch = 20, cex = 1.5)}
    addscalebar(pos = "bottomright", plotepsg = 4326)
    addnortharrow(pos = "topleft", padin = c(0.15, 0.15), scale = .7, lwd = 1, 
                  border = "black", cols = c("black", "black"), text.col = "black")
    dev.off()}
  
  print(paste0(" ok - ",file.model," !"))
  
  retorno = list(thrs= da.thrs,
                 lpt = en.lpt,
                 p10 = en.lpt,
                 p20 = en.lpt)
  
  return(retorno)}


