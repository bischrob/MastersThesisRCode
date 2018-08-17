# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script is designed to combine site data and to map it.
# !diagnostics off
library(rRJB)
myLibrary(c('pkgconfig','rio','tidyverse','sf','leaflet','raster','rgeos','ggpubr','ggsn', 'rgdal', 'grid'))

# convert Data Master to SF
DataMaster <- read.csv('Data/DataMaster.csv')
DataMasterSf <- st_as_sf(DataMaster, coords = c("EASTING","NORTHING"),
                         remove = F, crs = 26912)
DataMasterSf <- st_transform(DataMasterSf,4326)
DataMasterSf$long <- st_coordinates(DataMasterSf)[,1]
DataMasterSf$lat <- st_coordinates(DataMasterSf)[,2]
saveRDS(DataMasterSf,"Data/DataMasterSf.Rds")

# plot project area with all sites
DataMasterSf <- readRDS('Data/DataMasterSf.Rds')
source('R/getBasePlot.R')

basegLabels <- getBasePlot(labels = T, scalebar = T, sizeF = 1, smallbase = F)
# basegLabels + 
#   geom_point(data = DataMasterSf, aes(x = long, y = lat), color = "lightblue", size = .1)

ggsave(filename = "GeneralFigures/ProjectMap.png", width = 6.5, units = "in", dpi = 600)
ggsave(filename = "GeneralFigures/ProjectMap.svg")

# print by periods

DataMasterSf <- readRDS('Data/DataMasterSf.Rds')
DataMasterSf <- DataMasterSf %>% filter(Date %in% 750:999)
source('R/getBasePlot.R')
if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = F, sizeF = .8)
# NArrow <- png::readPNG('North Arrow.png')
# NArrow <- rasterGrob(NArrow)
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
mCols <- gg_color_hue(8)
mCols <- c(unlist(mCols[c(1,3:8)]),"#000000")
mShape <- c(0,1,2,5,6)
myPeriods <- sort(unique(DataMasterSf$Period))
# dev.new(width = 3, height = 2.5)
for(j in 1:length(myPeriods)){
  plotdf <- DataMasterSf %>% filter(Period == myPeriods[j])
  p1 <- baseg + geom_point(data = plotdf, aes(long,lat, color = Period),
                        shape = mShape[j], size = .1) +
      ggtitle(myPeriods[j]) + guides(color = F) + 
    scale_color_manual(values = mCols[j+1]) +
    theme(plot.title = element_text(size=9)) +
    scalebar(location = "bottomleft", dist = 50, dd2km = TRUE, model = 'WGS84',
             st.size = 2, x.min =-110 , x.max =-107, y.min = 35.4 , y.max = 37.5) 
    # annotation_custom(NArrow, ymin = 35.45, ymax = 35.67, xmax = -108.1) 
  ggsave(paste0("GeneralFigures/Period ",myPeriods[j],".png"), dpi = 600,
         width = 3, height = 2.5, units = "in", plot =p1)
  ggsave(paste0("GeneralFigures/Period ",myPeriods[j],".svg"),
         plot = p1)
}

# zoom in on Eastern Mesa Verde region for comparison
regions <- st_read("GIS/EarlyPuebloRegionsAdjusted.geojson")
regions <- as(regions,"Spatial")
regionsdf <- fortify(regions, region ="id")
regionsdf <- merge(regionsdf,regions@data, by = "id")
subregions <- st_read("GIS/EasternMesaVerdeSubregions.geojson")
subregions <- as(subregions,"Spatial")
subregionsdf <- fortify(subregions, region ="id")
subregionsdf <- merge(subregionsdf,subregions@data, by = "id")
subregionText <- gCentroid(subregions, byid = T)
subregionText <- tibble(id = subregions@data$id,
                        long = subregionText@coords[,1],
                        lat = subregionText@coords[,2])
subregionText <- merge(subregionText, subregions@data, by="id")
# NArrow <- png::readPNG('North Arrow.png')
# NArrow <- rasterGrob(NArrow)
source('R/getBasePlot.R')
if(!exists('baseP')) baseP <- getBasePlot(labels = F, scalebar = F, sizeF = 1)

baseP <- baseP + 
  geom_polygon(data = regionsdf, aes(long, lat, group = group, fill = Name),
               color = "#A9A9A9", alpha = .2) + 
  geom_polygon(data = subregionsdf, aes(long, lat, group = group),
               fill = NA, linetype = 'longdash', color = "black", size = .25) + 
  theme_bw() + 
  coord_fixed(1.3, xlim = c(-108.35,-107.15), ylim = c(36.65,37.55)) +
  guides(fill = F) + theme(axis.title = element_blank(),
                           axis.text = element_blank(),
                           axis.ticks = element_blank()) +
  scalebar(location = "bottomleft", dist = 10, dd2km = TRUE, model = 'WGS84',
           st.size = 2, x.min =-108.2 , x.max =-106.2 , y.min =36.65 , y.max =37.55 ) 
  # annotation_custom(NArrow, ymin = 36.68, ymax = 36.76, xmax = -107.777) 

baseP + 
  geom_text(data = subregionText, aes(x = long,y = lat), label = subregionText$Name, size = 1.75,
          fontface = "bold") 
ggsave(paste0("GeneralFigures/EasternMesaVerdeSubRegions.svg"))

DataMasterSf <- readRDS('Data/DataMasterSf.Rds')
DataMasterSf <- DataMasterSf %>% filter(Date %in% 750:999)

# look at the number of sites per district per period
EMV <- DataMasterSf %>% filter(Region == "East Mesa Verde")
st_crs(EMV)
subregions <- st_read("GIS/EasternMesaVerdeSubregions.geojson")
st_crs(subregions)
regions <- st_read("GIS/EarlyPuebloRegionsAdjusted.geojson")
st_crs(regions)
EMV <- st_intersection(EMV,subregions)
# dir.create("Tables")
write.csv(table(EMV$Period,EMV$Name),"Tables/Eastern Mesa Verde by district.csv")
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
mCols <- gg_color_hue(8)
mCols <- c(unlist(mCols[c(1,3:8)]),"#000000")
mShape <- c(0,1,2,5,6)
myPeriods <- sort(unique(DataMasterSf$Period))
j = 1
for(j in 1:length(myPeriods)){
  plotdf <- DataMasterSf %>% filter(Period == myPeriods[j])
  p1 <- baseP + geom_point(data = plotdf, aes(long,lat, color = Period),
                           shape = mShape[j], size = .1) +
    ggtitle(myPeriods[j]) + guides(color = F) + 
    scale_color_manual(values = mCols[j+1]) +
    theme(plot.title = element_text(size=9))
  ggsave(paste0("GeneralFigures/EasternMesaVerdePeriod ",myPeriods[j],".png"), dpi = 600,
         width = 3, height = 2.5, units = "in", plot =p1)
  ggsave(paste0("GeneralFigures/EasternMesaVerdePeriod ",myPeriods[j],".svg"),plot = p1)
}

################################################################################################
################################################################################################

# map Crucible of Pueblos regions
regions <- st_read('GIS/EarlyPuebloRegions.geojson')
regions <- as(regions,"Spatial")
regions <- spTransform(regions, CRS("+init=epsg:4326"))
regiondf <- fortify(regions, region ="id")
regiondf <- merge(regiondf, regions@data, by="id")
# regionText <- gCentroid(regions, byid = T)
# regionText <- tibble(id = regions@data$id, long = regionText@coords[,1],lat = regionText@coords[,2])
# regionText <- merge(regionText, regions@data, by="id")
# regionText[5,3] <- 35.9
# regionText[2,3] <- 37.355
# saveRDS(regionText, 'GIS/regionText.Rds')
regionText <- readRDS('GIS/regionText.Rds')
regionText$Name <- gsub("East","Eastern", regionText$Name)
source('R/getBasePlot.R')
if(!exists('baseL')) baseL <- getBasePlot(labels = F, scalebar = T, sizeF = 1)
DataMasterSf <- readRDS('Data/DataMasterSf.Rds')
border <- readRDS('GIS/projectBoundary.RDS')
border <- st_transform(border, 4326)
border <- as(border,"Spatial")
borderdf <- fortify(border)
citylocs <- readRDS("GIS/Citylocs.Rds")
Abajo <- readRDS('GIS/Abajo.Rds')
Chuska <- readRDS('GIS/Chuska.Rds')
SanJuan <- readRDS('GIS/SanJuan.Rds')

baseL + 
  geom_polygon(data = regiondf, aes(long, lat, group = group, fill = Name),
               color = "#A9A9A9", alpha = .35) + 
  geom_polygon(data = borderdf, aes(long, lat, group = group),
               color = "black", fill = NA, size = .35) + 
  guides(fill = F) +
  geom_point(data = citylocs, aes(lon, lat), color = "black", size = 1, shape = 15) +
  geom_text(data = regionText, aes(long,lat), label = regionText$Name, size = 4,
            nudge_y = -.1, fontface = "bold") +
  geom_text(data = Abajo, aes(x,y), label = Abajo$Name, size = 3, fontface = "bold") +
  geom_text(data = Chuska, aes(x,y), label = Chuska$Name, angle = -65, size = 3,
            fontface = "bold") +
  geom_text(data = SanJuan, aes(x = -107.55,y = 37.7), label = SanJuan$Name, size = 3,
            fontface = "bold") +
  geom_text(data = citylocs, aes(lon,lat), label = citylocs$Name, size = 2.5,
            nudge_y = -.03, fontface = "bold") 

ggsave(filename = "GeneralFigures/ProjectMapRegionswithBorder.png", width = 6.5, units = "in", dpi = 600)
ggsave(filename = "GeneralFigures/ProjectMapRegionswithBorder.svg")

# plot adjusted regions

regionsAdj <- st_read('GIS/EarlyPuebloRegionsAdjusted.geojson')
regionsAdj <- as(regionsAdj,"Spatial")
regionsAdj <- spTransform(regionsAdj, CRS('+init=epsg:4326'))
regionAdjdf <- fortify(regionsAdj, region ="id")
regionAdjdf <- merge(regionAdjdf, regionsAdj@data, by="id")
regionAdjdf$Name <- gsub("East","Eastern",regionAdjdf$Name)

baseL + 
  geom_polygon(data = regionAdjdf, aes(long, lat, group = group, fill = Name),
               color = "#A9A9A9", alpha = .35) + 
  # geom_point(data = DataMasterSf, aes(x = long, y = lat), color = "#FD694C", size = .35) +
  guides(fill = F) +
  geom_text(data = regionText, aes(long,lat), label = regionText$Name, size = 4,
            nudge_y = -.1, fontface = "bold") 

ggsave(filename = "GeneralFigures/ProjectMapRegionsAdjusted.png", width = 6.5, units = "in",
       dpi = 600)
ggsave(filename = "GeneralFigures/ProjectMapRegionsAdjusted.svg")

# plot sites by region and by size
source('R/getBasePlot.R')
baseg <- getBasePlot(labels = F,scalebar = F)
DataMasterSf <- readRDS('Data/DataMasterSf.Rds')
DataMasterSf <- DataMasterSf %>% filter(Date %in% 750:999)
DataMasterSf <- DataMasterSf %>% filter(Total >= 100)
myRegions <- unique(DataMasterSf$Region)
myPeriods <- sort(unique(DataMasterSf$Period))

baseg +
  geom_polygon(data = regionAdjdf, aes(long, lat, group = group, fill = Name),
               color = "#A9A9A9", alpha = .35) + 
  guides(fill = F) + 
  theme(plot.title = element_text(size = 9)) +
  scalebar(location = "bottomleft", dist = 50, dd2km = TRUE, model = 'WGS84',
           st.size = 2, x.min =-110 , x.max =-107, y.min = 35.4 , y.max = 37.5) +
  # annotation_custom(NArrow, ymin = 35.45, ymax = 35.67, xmax = -108.1)  +
  geom_text(data = regionText, aes(long,lat), label = regionText$Name, size = 2,
            nudge_y = -.1, fontface = "bold") 
ggsave("GeneralFigures/Sitesmorethan100Main.png",
       dpi = 600, width = 3, height = 2.5, units = "in")
ggsave("GeneralFigures/Sitesmorethan100Main.svg")

for(j in 1:length(myPeriods)){
  plotdf <- DataMasterSf %>% filter(Period == myPeriods[j])
    p1 <- baseg + geom_point(data = plotdf, aes(long,lat), size = 1.25) +
      geom_polygon(data = regionAdjdf, aes(long, lat, group = group, fill = Name),
                   color = "#A9A9A9", alpha = .35) + 
      ggtitle(myPeriods[j]) + guides(fill = F) + 
      theme(plot.title = element_text(size = 9)) 
    ggsave(paste0("GeneralFigures/Sitesmorethan100-",myPeriods[j],".png"),
    dpi = 600, width = 3, height = 2.5, units = "in")
    ggsave(paste0("GeneralFigures/Sitesmorethan100-",myPeriods[j],".svg"))
}

# add location map
rRJB::myLibrary(c("sf","tidyverse"))
states <- st_read("R:/AnthroPubs/Robert B/GIS Data/cb_2015_us_state_20m/WesternStates.shp")
states <- states %>% filter(STUSPS %in% c("UT","CO","AZ","NM"))
statesSP <- as(states,"Spatial")
statesSP <- fortify(statesSP)
boundary <- readRDS("GIS/ProjectBoundary.Rds")
boundary <- as(boundary,"Spatial")
boundary <- fortify(boundary)
# get coordinates for labels
df <- locator()
df <- tibble(x = df$x, y = df$y, name = c("Utah", "Colorado","New Mexico", "Arizona"))
df$x <- c(-112, -106, -106, -112)
df$y <- c(39.5,39.5,35,35)

ggplot(statesSP) + geom_polygon(aes(long-.35,lat-.35, group = group)) + 
  geom_polygon(aes(long,lat, group = group), fill = "white", color = "darkgray") +
  geom_text(data = df, aes(x,y), label = df$name, color = "#464849", size = 6) +
  geom_polygon(data = boundary, aes(long,lat,group = group), color = "#464849", fill = "firebrick",
               size = 2, alpha = .8) +
  theme_void() + coord_fixed(1.1)  
  
ggsave("GeneralFigures/ProjectLocation.svg", bg = "transparent")
ggsave("GeneralFigures/ProjectLocation.png", dpi = 600, bg = "transparent")
