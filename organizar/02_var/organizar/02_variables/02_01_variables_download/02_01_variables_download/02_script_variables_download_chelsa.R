### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 26/10/2017

###-----------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(downloader, xml2, rvest)


###-----------------------------------------------------------------------------###
###                                 chelsa
###-----------------------------------------------------------------------------###

## v 1.0
# directory
setwd("D:/environmental_data/chelsa")
dir.create("v1_0")
setwd("v1_0")
getwd()

# list of url
url <- "http://www.wsl.ch/lud/chelsa/archive/version1.0/"
url

pg <- read_html(url)
pg

list <- grep("CHELSA", html_attr(html_nodes(pg, "a"), "href"), value = T)
list

# download
for(i in list){
  download(paste0(url, i), paste0(i), mode = "wb")
  }


###------------------------------------------------------------------------------###

## v 1.1
# directory
setwd("..")
dir.create("v1_1")
setwd("v1_1")
getwd()

# list of url
url <- "http://www.wsl.ch/lud/chelsa/archive/version1.1/"
url

pg <- read_html(url)
pg

list <- grep("CHELSA", html_attr(html_nodes(pg, "a"), "href"), value = T)
list

# download
for(i in list){
  download(paste0(url, i), paste0(i), mode = "wb")
  }


###------------------------------------------------------------------------------###

### v1.2
# directory
setwd("..")
dir.create("v1_2")
setwd("v1_2")
getwd()

# lists
url <- "https://www.wsl.ch/lud/chelsa/data/"
pg <- read_html(url)
li <- grep("/$", html_attr(html_nodes(pg, "a"), "href"), value = T)[-1]


# download
for(i in li){
  
  ## bioclim ##
  if(i == "bioclim/"){
    
    print(sub("/", "", i))
    
    dir.create(sub("/", "", i))
    setwd(sub("/", "", i))
    
    url.i <- paste0(url, i)
    pg.i <- read_html(url.i)
    li.i <- grep("/$", html_attr(html_nodes(pg.i, "a"), "href"), value = T)[-1]
    
    for(j in li.i){
      dir.create(sub("/", "", j))
      setwd(sub("/", "", j))
      
      url.j <- paste0(url, i, j)
      pg.j <- read_html(url.j)
      li.j <- grep("[.]", html_attr(html_nodes(pg.j, "a"), "href"), value = T)
      
      for(k in li.j){
        
        download(paste0(url, i, j, k), paste0(k), mode = "wb")
        
      }
      
      setwd("..")
      
    }
    
    setwd("..")
    
  }
  
  
  ## climatologies ##
  if(i == "climatologies/"){
    
    print(sub("/", "", i))
    
    dir.create(sub("/", "", i))
    setwd(sub("/", "", i))
    
    url.i <- paste0(url, i)
    pg.i <- read_html(url.i)
    li.i <- grep("/$", html_attr(html_nodes(pg.i, "a"), "href"), value = T)[-1]
    
    for(j in li.i){
    
      if(j == "prec/"){
        dir.create(sub("/", "", j))
        setwd(sub("/", "", j))
    
        url.j <- paste0(url, i, j)
        pg.j <- read_html(url.j)
        li.j <- grep("[.]", html_attr(html_nodes(pg.j, "a"), "href"), value = T)
        
        for(k in li.j){
          
          download(paste0(url, i, j, k), paste0(k), mode = "wb")
          
        }
        
      }
      
      setwd("..")
      
    }
      
      if(j == "temp/"){
        dir.create(sub("/", "", j))
        setwd(sub("/", "", j))
      
        url.j <- paste0(url, i, j)
        pg.j <- read_html(url.j)
        li.j <- grep("/$", html_attr(html_nodes(pg.j, "a"), "href"), value = T)[-1]
      
        for(k in li.j){
          dir.create(sub("/", "", k))
          setwd(sub("/", "", k))
        
          url.k <- paste0(url, i, j, k)
          pg.k <- read_html(url.k)
          li.k <- grep("/$", html_attr(html_nodes(pg.k, "a"), "href"), value = T)[-1]
        
          for(l in li.k){
            dir.create(sub("/", "", l))
            setwd(sub("/", "", l))
          
            url.l <- paste0(url, i, j, k, l)
            pg.l <- read_html(url.l)
            li.l <- grep("[.]", html_attr(html_nodes(pg.l, "a"), "href"), value = T)
        
            for(m in li.l){
            
              download(paste0(url, i, j, k, l, m), paste0(m), mode = "wb")
            
            }
          
          setwd("..")
          
          }
        
        setwd("..")
        
        }
      
      setwd("..")
      
    }
    
    setwd("..")
      
    }
    

  ## cmip5 ##
  if(i == "cmip5/"){
    
    print(sub("/", "", i))
    
    
    dir.create(sub("/", "", i))
    setwd(sub("/", "", i))
    
    url.i <- paste0(url, i)
    pg.i <- read_html(url.i)
    li.i <- grep("/$", html_attr(html_nodes(pg.i, "a"), "href"), value = T)[-1]
    
    for(j in li.i){
      dir.create(sub("/", "", j))
      setwd(sub("/", "", j))
      
      url.j <- paste0(url, i, j)
      pg.j <- read_html(url.j)
      li.j <- grep("/$", html_attr(html_nodes(pg.j, "a"), "href"), value = T)[-1]
      
      for(k in li.j){
        dir.create(sub("/", "", k))
        setwd(sub("/", "", k))
        
        url.k <- paste0(url, i, j, k)
        pg.k <- read_html(url.k)
        li.k <- grep("[.]", html_attr(html_nodes(pg.k, "a"), "href"), value = T)
        
        for(l in li.k){
          
          download(paste0(url, i, j, k, l), paste0(l), mode = "wb")
            
          }
          
          setwd("..")
          
        }
        
        setwd("..")
        
      }
      
      setwd("..")
      
  }
      

  ## pmip3 ##
  if(i == "pmip3/"){
    
    print(sub("/", "", i))
    
    dir.create(sub("/", "", i))
    setwd(sub("/", "", i))
    
    url.i <- paste0(url, i)
    pg.i <- read_html(url.i)
    li.i <- grep("/$", html_attr(html_nodes(pg.i, "a"), "href"), value = T)[-1]
    
    for(j in li.i){
      dir.create(sub("/", "", j))
      setwd(sub("/", "", j))
      
      url.j <- paste0(url, i, j)
      pg.j <- read_html(url.j)
      li.j <- grep("[.]", html_attr(html_nodes(pg.j, "a"), "href"), value = T)
      
      for(k in li.j){
        
        download(paste0(url, i, j, k), paste0(k), mode = "wb")
        
      }
      
      setwd("..")
      
    }
    
    setwd("..")
  }


  ## timeseries ##
  if(i == "timeseries/"){
    
    print(sub("/", "", i))
    
    dir.create(sub("/", "", i))
    setwd(sub("/", "", i))
    
    url.i <- paste0(url, i)
    pg.i <- read_html(url.i)
    li.i <- grep("/$", html_attr(html_nodes(pg.i, "a"), "href"), value = T)[-1]
    
    for(j in li.i){
      dir.create(sub("/", "", j))
      setwd(sub("/", "", j))
      
      url.j <- paste0(url, i, j)
      pg.j <- read_html(url.j)
      li.j <- grep("[.]", html_attr(html_nodes(pg.j, "a"), "href"), value = T)
      
      for(k in li.j){
        
        download(paste0(url, i, j, k), paste0(k), mode = "wb")
        
      }
      
      setwd("..")
      
    }
    
    setwd("..")
  }
          
}
 

###------------------------------------------------------------------------------###
 
