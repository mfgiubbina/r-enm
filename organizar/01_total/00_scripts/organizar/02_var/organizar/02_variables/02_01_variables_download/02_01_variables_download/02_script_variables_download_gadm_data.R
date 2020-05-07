### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 01/06/2017

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(downloader)

###----------------------------------------------------------------------------------###

# diretorio
setwd("D:/limites/base_gadm")
dir.create("data")
setwd("data")

# url 
url <- "http://biogeo.ucdavis.edu/data/diva/"

# code
co <- c("ABW", "AFG", "AGO", "AIA", "ALA", "ALB", "AND", "ANT", "ARE", "ARG", 
        "ARM", "ASM", "ATA", "ATF", "ATG", "AUS", "AUT", "AZE", "BDI", "BEL", 
        "BEN", "BFA", "BGD", "BGR", "BHR", "BHS", "BIH", "BLM", "BLR", "BLZ", 
        "BMU", "BOL", "BRA", "BRB", "BRN", "BTN", "BVT", "BWA", "CAF", "CAN", 
        "CCK", "CHE", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COK", "COL", 
        "COM", "CPV", "CRI", "CUB", "CXR", "CYM", "CYP", "CZE", "DEU", "DJI", 
        "DMA", "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESH", "ESP", "EST", 
        "ETH", "FIN", "FJI", "FLK", "FRA", "FRO", "FSM", "GAB", "GBR", "GEO", 
        "GGY", "GHA", "GIB", "GIN", "GLP", "GMB", "GNB", "GNQ", "GRC", "GRD", 
        "GRL", "GTM", "GUF", "GUM", "GUY", "HKG", "HMD", "HND", "HRV", "HTI", 
        "HUN", "IDN", "IMN", "IND", "IOT", "IRL", "IRN", "IRQ", "ISL", "ISR", 
        "ITA", "JAM", "JEY", "JOR", "JPN", "KAZ", "KEN", "KGZ", "KHM", "KIR", 
        "KNA", "KOR", "KWT", "LAO", "LBN", "LBR", "LBY", "LCA", "LIE", "LKA", 
        "LSO", "LTU", "LUX", "LVA", "MAC", "MAF", "MAR", "MCO", "MDA", "MDG", 
        "MDV", "MEX", "MHL", "MKD", "MLI", "MLT", "MMR", "MNE", "MNG", "MNP", 
        "MOZ", "MRT", "MSR", "MTQ", "MUS", "MWI", "MYS", "MYT", "NAM", "NCL", 
        "NER", "NFK", "NGA", "NIC", "NIU", "NLD", "NOR", "NPL", "NRU", "NZL", 
        "OMN", "PAK", "PAN", "PCN", "PER", "PHL", "PLW", "PNG", "POL", "PRI", 
        "PRK", "PRT", "PRY", "PSE", "PYF", "QAT", "REU", "ROU", "RUS", "RWA", 
        "SAU", "SDN", "SEN", "SGP", "SGS", "SHN", "SJM", "SLB", "SLE", "SLV", 
        "SMR", "SOM", "SPM", "SRB", "STP", "SUR", "SVK", "SVN", "SWE", "SWZ", 
        "SYC", "SYR", "TCA", "TCD", "TGO", "THA", "TJK", "TKL", "TKM", "TLS", 
        "TON", "TTO", "TUN", "TUR", "TUV", "TWN", "TZA", "UGA", "UKR", "UMI", 
        "URY", "USA", "UZB", "VAT", "VCT", "VEN", "VGB", "VIR", "VNM", "VUT", 
        "WLF", "WSM", "YEM", "ZAF", "ZMB", "ZWE")
co


da <- c("adm", "alt", "cov", "gaz", "msk_alt", "msk_cov", "msk_pop", "pop", 
        "rds", "rrd", "wat")
da

###----------------------------------------------------------------------------------###

# download
for(i in co){
  dir.create(i)
  setwd(i)
  
  for(j in da){
    
    tryCatch({
      download(paste0(url, j, "/", i, "_", j, ".zip"), paste0(i, "_", j, ".zip"), 
               mode = "wb")
    }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
    unzip(paste0(i, "_", j, ".zip"))
    unlink(paste0(i, "_", j, ".zip"))
    
  }
  
  setwd("..")
  
}

###----------------------------------------------------------------------------------###