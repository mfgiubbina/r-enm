euclidean <- function(p, 
                      x, 
                      method = "mean", 
                      decostand.method = "standardize",
                      suitability = FALSE){
  
  # packages
  if(!require(raster)) install.packages("raster")
  if(!require(vegan)) install.packages("vegan")
  
  # conditions
  if(class(x) != "raster" & class(x) != "RasterStack" & class(x) != "RasterBrick"){
    stop("x has to be a raster")
  }
  
  if(class(p) != "matrix" & class(p) != "data.frame" & class(p) != "tbl_df"){
    stop("p has to be a matrix or data.frame")
  }
  
  if (ncol(p) != 2) {
    stop("p has to be a matrix or data.frame of 2 columns (x and y)")
  }
  
  # values
  val <- raster::values(x)
  val <- apply(values, 2, decostand, method = decostand.method, na.rm = TRUE)
  raster::values(x) <- val
  val_p <- raster::extract(x, p)
  pos <- is.na(val[, 1])
  val2 <- val[!pos, ]
  n <- nrow(val2)
  eu <- numeric(n)
  val_p <- na.omit(val_p)
  
  # euclidean distance
  for(i in 1:n){
    
    for(j in 1:ncol(val_p)){
      
      temp <- eu[i] + ((val2[i, j] - val_p[, j]) ^ 2)
      
      if(method == "mean"){
        
        eu[i] <- mean(temp, na.rm = TRUE)
        
      }
      
      if(method == "min"){
        
        eu[i] <-  min(temp, na.rm = TRUE)  
        
      }
      
    }
    
    eu[i] <- sqrt(eu[i])
    
  }
  
  x <- raster(x, 1)
  
  if(!suitability){
    
    raster::values(x)[!pos] <- eu
    
    return(x)
    
  } 
  
  if(suitability){
    
    raster::values(x)[!pos] <- decostand(-eu, method = "range", na.rm = TRUE)
    
    return(x)
    
  }
  
}
