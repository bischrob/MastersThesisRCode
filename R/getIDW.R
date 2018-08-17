# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script is a function to create IDW rasters
# !diagnostics off

getIDW <- function(sf, boundary, variable, pwr = 3, crsP = 26912,
                   useEmptyPoints = T, myWidth = 18000){
  # sites must be sf, 
  # boundary, is a spatial polygon showing the extent to be used,
  # variable is the column name to be interpolated,
  # pwr is the IDW power, 
  # myWidth is the buffer around sites to make empty points,
  # crs is the projected coordinate system to be used 
  # useEmptyPoints determines whether artificial empty points should be used
  library(rRJB)
  myLibrary(c("rgdal","lubridate","tidyverse",'sf','rgeos','maptools','raster','gstat'))
  sites <- st_transform(sf,crsP)
  if(!isS4(boundary))  boundary <- as(boundary, "Spatial")
  sites$X<- st_coordinates(sites)[,1]
  sites$Y <- st_coordinates(sites)[,2]
  df <- sites %>% dplyr::select(variable)
  df <- as(df,"Spatial")
  colnames(df@coords)<- c("X","Y")
  st_geometry(sites) <- NULL
  if(useEmptyPoints == T){
    emptyPoints <- makeEmptyPoints(df,boundary = boundary, variable = variable, myWidth = myWidth)
    sites <- bind_rows(sites,emptyPoints)
  }
  sites <- st_as_sf(sites, coords = c("X","Y"), crs = crsP,
                          remove = F)
  sites <- as(sites, "Spatial")
  sites@bbox <- boundary@bbox
  grd              <- as.data.frame(spsample(sites, "regular", n=50000))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  proj4string(grd) <- proj4string(sites)
  sitesIDW <- gstat::idw(get(variable) ~ 1, sites, newdata=grd, idp=pwr) 
  r <- raster(sitesIDW)
  return(r)  
}

# create empty points for interpolation

makeEmptyPoints <- function(df, boundary, variable, myWidth = 18000){
  # first buffer existing sites
  buffer <- gBuffer(df, byid = T, width = myWidth)
  # Create an empty grid where n is the total number of cells
  grd <- as.data.frame(spsample(boundary, "regular", n=5000))
  names(grd) <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  grd@proj4string <- df@proj4string
  emptypoints <- gDifference(grd,buffer)
  x <- as.tibble(emptypoints@coords)
  names(x) <- c("X","Y")
  x[[variable]] <- 0
  return(x)
}
