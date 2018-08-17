# function to transform data frame into sf data frame making sure to separate UTM zones

ConverttoSFDF <- function (x){
  library(sf)
  # First remove missing values
  x <- x[which(!(x$EASTING == 0)),]
  x <- x[which(!(x$EASTING == "NA")),]
  x$EASTING2 <- x$EASTING
  x$NORTHING2 <- x$NORTHING
  x12 <- x[which(x$Zone == 12),]
  x13 <- x[which(x$Zone == 13),]
  x12 <- st_as_sf(x = x12,
                             coords = c("EASTING2", "NORTHING2"),
                             crs = "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  x13 <- st_as_sf(x = x13,
                             coords = c("EASTING2", "NORTHING2"),
                             crs = "+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  # transform coordinates
  x12 <- st_transform(x12, crs = 4326)
  x13 <- st_transform(x13, crs = 4326)
  
  x <- rbind(x12,x13)
  x <- st_transform(x, 4326)
  xm <- matrix(unlist(st_geometry(x)),ncol = 2, byrow = T)
  names(xm) <- c('long','lat')
  x$long <- xm[,1]
  x$lat <- xm[,2]
  return(x)
}

