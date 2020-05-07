### script tabela com as media dos dados para os municipios do estado do parana ###

# mauricio humberto vancine

# limpar memoria
rm(list = ls())

# pacotes 
library(raster)
library(rgdal)
library(foreign)


###---------------------------------------------------------------------------------###

### paisagem

# diretorio
setwd('D:/_________parcerias/__________extrair_dados')

# shapefile dos municipios do parana
parana <- shapefile('municipios_parana_ibge.shp')
plot(parana)

# tabela de atributos
head(parana@data)

# importar tabelas de atributo
setwd('D:/_________parcerias/__________extrair_dados/tables')
dbf <- list.files(pattern = '.dbf')
dbf

table <- NULL
for (i in dbf){
  temp <- read.dbf(i)
  table <- cbind(table, temp$MEAN)}

colnames(table) <- dbf

head(table)

# associar a tabela de atributos
parana.table <- parana
parana.table@data <- cbind(parana.table@data, table)

head(parana.table@data)

# corrigindo a projecao
am <- shapefile('am_sul.shp')
plot(am)
crs(am)

crs(parana.table) <- crs(am)
crs(parana.table)


# exportar o shapefile
setwd('D:/_________parcerias/__________extrair_dados')
writeOGR(obj = parana.table, dsn = 'shp', layer = 'parana_table', driver = 'ESRI Shapefile', over = T)



###---------------------------------------------------------------------------------###


### uso da terra

# diretorio
setwd('D:/_________parcerias/__________extrair_dados')

# importar raster
uso <- raster('uso_da_terra_2013_gcs_wgs84.tif')
plot(uso, axes = T)

par.t <- shapefile('D:/_________parcerias/__________extrair_dados/shp/parana_table.shp')
lines(parana.table, col = 'gray')


# exportar municipios separadamente
head(par.t@data)

nome <- read.table('D:/_________parcerias/__________extrair_dados/municipios/_municipios.txt', h = T)
head(nome)
par.t@data <- cbind(par.t@data, nome)
head(par.t@data)

list.mun <- par.t@data$nome_munic
list.mun
class(list.mun)

# exportar shapefiles dos municipios
for(i in list.mun){
  mun <- subset(par.t, par.t@data$nome_munic == i)
  writeOGR(mun, 'municipios', paste(i), driver = 'ESRI Shapefile')}


###---------------------------------------------------------------------------------###


# calcular as porcentagens de uso para cada municipio
classes <-  data.frame(classes = as.numeric(names(table(values(uso)))))

classes.n <- classes

for(i in list.mun){
  mun <- subset(par.t, par.t@data$nome_munic == i)
  cr <- crop(uso, mun)
  ma <- mask(cr, mun)
  cl <- table(values(ma)) / length(na.omit(values(ma)))
  df <- data.frame(classes = as.numeric(names(cl)), round(as.vector(cl), 4))
  me <- merge(df, classes, all = T)
  classes.n <- cbind(classes.n, me[2])}

head(classes.n)
dim(classes.n)
colnames(classes.n)

colSums(classes.n, na.rm = T)


# alterar os nomes das colunas das classes
colnames(classes.n) <- 1:400
colnames(classes.n) <- c(classes, as.character(list.mun))
colnames(classes.n)

head(classes.n)

uso.cob <- t(classes.n[, -1])

colnames(uso.cob) <- classes.n[, 1]

colnames(uso.cob) <- c('agua', 'urbano', 'pastagem', 'pastagem_em_AP', 'savanas', 'savanas_em_AP', 'florestas', 
				'florestas_em_AP', 'soja', 'cana_de_acucar', 'milho', 'algodao', 'arroz', 'feijao', 
				'cafe_arabica', 'cafe_robusta', 'laranja', 'mandioca', 'banana', 'cacau', 'fumo', 
				'floresta_plantada', 'soja_milho_safrinha', 'soja_trigo', 'milho_trigo', 'soja_feijao', 
				'milho_feijao', 'feijao_feijao')

head(uso.cob)

par.t2 <- par.t
par.t2

par.t2@data <- cbind(par.t2@data, uso.cob)

head(par.t2@data)

# selecionar apenas as colunas para exportar
par.t3 <- par.t2

par.t3@data <- par.t2@data[, c(38, 1, 9, 10, 12:37, 39:66)]

head(par.t3@data)

# exportar raster
writeOGR(par.t3, '__shp_final_clima_paisagem', 'municipios_parana_media_clima_paisagem', driver = 'ESRI Shapefile')

###---------------------------------------------------------------------------------###


