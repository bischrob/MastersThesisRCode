# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#' This script is designed to adapt the nhdflowlines west for use in mapping my
#' data.
library(rRJB)
myLibrary(c('rgdal', 'raster'))

fp <- myFilePath()
projectRivers <- readOGR("GIS",'FourCornersPlusRiversLakesNHDFlow')
projectRivers <- spTransform(projectRivers, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
saveRDS(projectRivers,"GIS/rivers.Rds")
library(mapview)
mapview(projectRivers)

