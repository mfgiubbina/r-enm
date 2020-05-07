### script enm - download vector - brazil ##

# mauricio vancine
# 17-04-2018

# bases cartograficas continuas - ibge
# https://mapas.ibge.gov.br/bases-e-referenciais

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(rvest)
library(tidyverse)
library(xml2)

# directory
path <- "/media/mude/data/spatial_data/vector/base_ibge_1_250_2017"
setwd(path)
dir()

# download ----------------------------------------------------------------
# url
site <- "ftp://geoftp.ibge.gov.br/cartas_e_mapas/bases_cartograficas_continuas/bc250/versao2017/shapefile/"
site

# list
li <- c("Abastecimento_Saneamento_v2017.zip", "Energia_Comunicacao_v2017.zip",
        "Estrutura_Economica_v2017.zip", "Hidrografia_v2017.zip", 
        "Indicacao_Geografica_2017.zip", "Limites_v2017.zip", "Localidades_v2017.zip",
        "Relevo_v2017.zip", "Transporte_v2017.zip")
li

# download
purrr::map2(paste0(site, "/", li)[6], li[6], download.file)

# unzip
purrr::map(dir(pattern = ".zip"), unzip)

# end ---------------------------------------------------------------------