# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script is designed to plots sites by period
library(rRJB)
myLibrary(c('tidyverse'))

# load data
DataMasterMyPeriods <- readRDS('Data/DataMasterSf.Rds')

# ggplot
borderEnlarged <- readRDS('GIS/BorderEnlarged.Rds')
borderEnlargedSf <- st_as_sf(borderEnlarged)
projectRivers <- readRDS('GIS/projectRivers.Rds')
projectRivers <- as(projectRivers, "Spatial")
countiesSP <- readRDS('GIS/CountiesSP.Rds')
projectMountainsSP <- readRDS("GIS/projectMountainsSP.Rds")
mountainCoords <- gCentroid(projectMountainsSP, byid = T)
mountainlocs <- data.frame(ID = 1:length(mountainCoords), Name = rep("",length(mountainCoords)))
mountainlocs <- cbind(mountainlocs,mountainCoords@coords)
mountainlocs$Name <- as.character(mountainlocs$Name)
mountainlocs[mountainlocs$ID == 18,2] <- "Abajo Mountains"
mountainlocs[mountainlocs$ID == 15,2] <- "Chuska Mountains"
rownames(mountainlocs) <- 1:nrow(mountainlocs)
Abajo <- mountainlocs[18,]
Chuska <- mountainlocs[15,]
ggplot() + 
  geom_polygon(data = projectMountainsSP, aes(long,lat,group = group), fill = "gray") +
  geom_path(data = projectRivers, aes(long,lat, group = group),
            color = "lightgray", size = .5) +
  geom_polygon(data = countiesSP, aes(long, lat, group = group),
               fill = NA, color = "black") +
  geom_point(data = DataMasterMyPeriods, aes(x = long, y = lat), color = "blue", size = .7) +
  theme_bw() + coord_fixed(1.3, xlim = c(-110.6,-106.9), ylim = c(35.4,38)) +
  xlab("Longitude") + ylab("Latitude") + 
  geom_text(data = Abajo, aes(x,y), label = Abajo$Name, size = 2.8) +
  geom_text(data = Chuska, aes(x,y), label = Chuska$Name, angle = -65, size = 2.8)


ggsave(filename = "GeneralFigures/ProjectMap.pdf", width = 6.5, units = "in")