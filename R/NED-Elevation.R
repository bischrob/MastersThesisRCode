# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#' The purpose of this script is to download NED information for my project 
#' area and examine the elevation differences by cross-section
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(rRJB)
myLibrary(c('FedData','raster','rgeos','tidyverse','sf', 'ggspatial'))
# boundary <- readRDS('GIS/BorderEnlarged.Rds')
# bbox <- st_bbox(boundary)
# boundaryP <- as(raster::extent(bbox[1],bbox[3],bbox[2],bbox[4]), "SpatialPolygons")
# boundaryP@proj4string <- boundary@proj4string
# projectBoundaryNED <- FedData::get_ned(template = boundaryP,
#                                        label = "MastersThesisBoundaryEnlarged", raw.dir = ".GIS/RAW/NED")
# mapview(projectBoundaryNED)
# saveRDS(projectBoundaryNED, "GIS/projectBoundaryNEDLarge.Rds")
# projectBoundaryNED <- readRDS("GIS/projectBoundaryNED.Rds")
# thesisNEDLarge <- aggregate(projectBoundaryNED, fact = 3, fun = mean)
# saveRDS(thesisNEDLarge,"GIS/thesisNEDLarge90m.Rds")
# thesisNED <- readRDS('GIS/thesisNED90m.Rds')
# thesisNEDProj <- projectRaster(thesisNED, crs= "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

# # examine the DEM profile
# # south to north
# x <- rep((thesisNEDProj@extent@xmax-thesisNEDProj@extent@xmin)/2 + thesisNEDProj@extent@xmin,1000)
# yVal <- thesisNEDProj@extent@ymin
# ySpread <-  (thesisNEDProj@extent@ymax - thesisNEDProj@extent@ymin) / 1050
# y <- 1:1000
# y[1] <- yVal
# for (i in 2:1200) {
#   y[i] <- y[i-1] + ySpread
# }
# elevSP <- data.frame(x = x, y = y[26:1025])
# elevSP <- SpatialPointsDataFrame(elevSP, elevSP, proj4string = thesisNEDProj@crs)
# elevSPlonglat <- spTransform(elevSP, thesisNED@crs)
# elevdf1 <- raster::extract(thesisNED, elevSPlonglat, df = T)
# dist <- 1:nrow(elevdf1)
# dist[1] <- 0
# for (i in 2:nrow(elevdf1)){
#   dist[i] <- dist[i-1] + sqrt((elevSP@coords[i,1] - elevSP@coords[i-1,1])^2 +
#                   (elevSP@coords[i,2] - elevSP@coords[i-1,2])^2)
# }
# elevdf1$Distance <- dist / 1000
# names(elevdf1)[2] <- "Elevation"
# elevdf1$lat <- elevSPlonglat@coords[,2]
# elevdf1$long <- elevSPlonglat@coords[,1]
#
# # get data for annotations
# elevdf1 <- data.table(elevdf1)
# lowestPoint <- elevdf1[ , .SD[which.min(Elevation)]]
# highestPoint <- elevdf1[ , .SD[which.max(Elevation)]]
# Chuska <- elevdf1[ , .SD[which.max(Elevation[1:500])]]
# CDBSitesSF <- readRDS("GIS/CDBSitesSF.Rds")
# PBonito <- CDBSitesSF[CDBSitesSF$SWSN_Site == "Pueblo Bonito",]
# sp <- sf::as_Spatial(PBonito$geometry)
# splonglat <- spTransform(sp, thesisNED@crs)
# spelev <- extract(thesisNED, splonglat, df = T)
# spdist <- sqrt((sp@coords[1,1] - elevSP@coords[1,1])^2 +
#                       (sp@coords[1,2] - elevSP@coords[1,2])^2) / 1000
# PBonito <- data.frame(Elevation = spelev$layer, Distance = spdist)
#
# McPhee <- CDBSitesSF[CDBSitesSF$SWSN_Site == "McPhee Pueblo",]
# sp <- sf::as_Spatial(McPhee$geometry)
# splonglat <- spTransform(sp, thesisNED@crs)
# spelev <- extract(thesisNED, splonglat, df = T)
# spdist <- sqrt((sp@coords[1,1] - elevSP@coords[1,1])^2 +
#                  (sp@coords[1,2] - elevSP@coords[1,2])^2) / 1000
# McPhee <- data.frame(Elevation = spelev$layer, Distance = spdist)
#
# # plot
# g1 <- ggplot(elevdf1, aes(x = Distance, y = Elevation)) + geom_path(color = "darkgray") +
#   theme_light() + xlab("Distance (km)") + ylab("Elevation (m)") +
#   annotate("text", label = "San Juan River",
#            x = lowestPoint$Distance, y = lowestPoint$Elevation) +
#   annotate("text", label = "San Juan Mountains",
#            x = highestPoint$Distance - 21, y = highestPoint$Elevation) +
#   annotate("text", label = "Chuska Mountains",
#            x = Chuska$Distance, y = Chuska$Elevation + 45) +
#   annotate("text", label = "Pueblo Bonito",
#            x = PBonito$Distance, y = PBonito$Elevation + 45) +
#   geom_point(data = PBonito, aes(x = Distance, y = Elevation)) +
#   annotate("text", label = "McPhee Village",
#            x = McPhee$Distance, y = McPhee$Elevation + 45) +
#   geom_point(data = McPhee, aes(x = Distance, y = Elevation))
#
# ggsave(plot = g1, filename = "GeneralFigures/ElevationPlotSouthtoNorth.png",
#        width = 6.5, height = 2.5, units = "in", dpi = 600)
#
# # map data east to west
# y <- rep((thesisNEDProj@extent@ymax-thesisNEDProj@extent@ymin)/2 + thesisNEDProj@extent@ymin,1000)
# xVal <- thesisNEDProj@extent@xmin
# xSpread <-  (thesisNEDProj@extent@xmax - thesisNEDProj@extent@xmin) / 1050
# x <- 1:1000
# x[1] <- xVal
# for (i in 2:1200) {
#   x[i] <- x[i-1] + xSpread
# }
# elevSP <- data.frame(x = x[26:1025], y = y)
# elevSP <- SpatialPointsDataFrame(elevSP, elevSP, proj4string = thesisNEDProj@crs)
# elevSPlonglat <- spTransform(elevSP, thesisNED@crs)
# elevdf1 <- raster::extract(thesisNED, elevSPlonglat, df = T)
# dist <- 1:nrow(elevdf1)
# dist[1] <- 0
# for (i in 2:nrow(elevdf1)){
#   dist[i] <- dist[i-1] + sqrt((elevSP@coords[i,1] - elevSP@coords[i-1,1])^2 +
#                                 (elevSP@coords[i,2] - elevSP@coords[i-1,2])^2)
# }
# elevdf1$Distance <- dist / 1000
# names(elevdf1)[2] <- "Elevation"
# elevdf1$lat <- elevSPlonglat@coords[,2]
# elevdf1$long <- elevSPlonglat@coords[,1]
#
# # get data for annotations
# elevdf1 <- data.table(elevdf1)
# lowestPoint <- elevdf1[ , .SD[which.min(Elevation)]]
# highestPoint <- elevdf1[ , .SD[which.max(Elevation)]]
# SaltPoint <- CDBSitesSF[CDBSitesSF$SWSN_Site == "Salt Point",]
# sp <- sf::as_Spatial(SaltPoint$geometry)
# splonglat <- spTransform(sp, thesisNED@crs)
# spelev <- extract(thesisNED, splonglat, df = T)
# spdist <- sqrt((sp@coords[1,1] - elevSP@coords[1,1])^2 +
#                  (sp@coords[1,2] - elevSP@coords[1,2])^2) / 1000
# SaltPoint <- data.frame(Elevation = spelev$layer, Distance = spdist)
#
# Salmon <- CDBSitesSF[CDBSitesSF$SWSN_Site == "Salmon",]
# sp <- sf::as_Spatial(Salmon$geometry)
# splonglat <- spTransform(sp, thesisNED@crs)
# spelev <- extract(thesisNED, splonglat, df = T)
# spdist <- sqrt((sp@coords[1,1] - elevSP@coords[1,1])^2 +
#                  (sp@coords[1,2] - elevSP@coords[1,2])^2) / 1000
# Salmon <- data.frame(Elevation = spelev$layer, Distance = spdist)
#
# DancingRock <- CDBSitesSF[CDBSitesSF$SWSN_Site == "Dancing Rock GH",]
# sp <- sf::as_Spatial(DancingRock$geometry)
# splonglat <- spTransform(sp, thesisNED@crs)
# spelev <- extract(thesisNED, splonglat, df = T)
# spdist <- sqrt((sp@coords[1,1] - elevSP@coords[1,1])^2 +
#                  (sp@coords[1,2] - elevSP@coords[1,2])^2) / 1000
# DancingRock <- data.frame(Elevation = spelev$layer, Distance = spdist)
#
# # plot
# g2 <- ggplot(elevdf1, aes(x = Distance, y = Elevation)) + geom_path(color = "darkgray") +
#   theme_light() + xlab("Distance (km)") + ylab("Elevation (m)") +
#   annotate("text", label = "Carrizo Mountains",
#            x = Chuska$Distance, y = Chuska$Elevation + 45) +
#   annotate("text", label = "Salt Point",
#            x = SaltPoint$Distance, y = SaltPoint$Elevation + 45) +
#   geom_point(data = SaltPoint, aes(x = Distance, y = Elevation)) +
#   annotate("text", label = "Salmon Ruins",
#            x = Salmon$Distance, y = Salmon$Elevation + 45) +
#   geom_point(data = Salmon, aes(x = Distance, y = Elevation)) +
#   annotate("text", label = "Dancing Rock GH",
#            x = DancingRock$Distance, y = DancingRock$Elevation + 45) +
#   geom_point(data = DancingRock, aes(x = Distance, y = Elevation))
#
# g2
# ggsave(plot = g2, filename = "GeneralFigures/ElevationPlotEasttoWest.png",
#        width = 6.5, height = 2.5, units = "in", dpi = 600)
#
# # map data east to west along the northern half
# # y <- rep((thesisNEDProj@extent@ymax-thesisNEDProj@extent@ymin) * .75 + thesisNEDProj@extent@ymin,1000)
# y <- rep(4161266,1000)
# xVal <- thesisNEDProj@extent@xmin
# xSpread <-  (thesisNEDProj@extent@xmax - thesisNEDProj@extent@xmin) / 1050
# x <- 1:1000
# x[1] <- xVal
# for (i in 2:1200) {
#   x[i] <- x[i-1] + xSpread
# }
# elevSP <- data.frame(x = x[26:1025], y = y)
# elevSP <- SpatialPointsDataFrame(elevSP, elevSP, proj4string = thesisNEDProj@crs)
# elevSPlonglat <- spTransform(elevSP, thesisNED@crs)
# elevdf1 <- raster::extract(thesisNED, elevSPlonglat, df = T)
# dist <- 1:nrow(elevdf1)
# dist[1] <- 0
# for (i in 2:nrow(elevdf1)){
#   dist[i] <- dist[i-1] + sqrt((elevSP@coords[i,1] - elevSP@coords[i-1,1])^2 +
#                                 (elevSP@coords[i,2] - elevSP@coords[i-1,2])^2)
# }
# elevdf1$Distance <- dist / 1000
# names(elevdf1)[2] <- "Elevation"
# elevdf1$lat <- elevSPlonglat@coords[,2]
# elevdf1$long <- elevSPlonglat@coords[,1]
#
# # get data for annotations
# elevdf1 <- data.table(elevdf1)
# lowestPoint <- elevdf1[ , .SD[which.min(Elevation)]]
# highestPoint <- elevdf1[ , .SD[which.max(Elevation)]]
# highestpoint2 <- elevdf1[ , .SD[which.max(Elevation[1:400])]]
#
# P1Sites <- readRDS('GIS/P1SitesSf.Rds')
# Site13 <- P1Sites[433,]
# sp <- sf::as_Spatial(Site13$geometry)
# splonglat <- spTransform(sp, thesisNED@crs)
# spelev <- extract(thesisNED, splonglat, df = T)
# spdist <- sqrt((sp@coords[1,1] - elevSP@coords[1,1])^2 +
#                  (sp@coords[1,2] - elevSP@coords[1,2])^2) / 1000
# Site13 <- data.frame(Elevation = spelev$layer, Distance = spdist)
#
# # plot
# g3 <- ggplot(elevdf1, aes(x = Distance, y = Elevation)) + geom_path(color = "darkgray") +
#   theme_light() + xlab("Distance (km)") + ylab("Elevation (m)") +
#   annotate("text", label = "San Juan River",
#            x = lowestPoint$Distance + 12, y = lowestPoint$Elevation) +
#   annotate("text", label = "McPhee Village",
#            x = McPhee$Distance, y = McPhee$Elevation + 120) +
#   geom_point(data = McPhee, aes(x = Distance, y = Elevation)) +
#   annotate("text", label = "San Juan Mountains",
#            x = highestPoint$Distance, y = highestPoint$Elevation - 20) +
#   annotate("text", label = "Alkali Ridge",
#            x = Site13$Distance, y = Site13$Elevation + 120) +
#   geom_point(data = Site13, aes(x = Distance, y = Elevation))
# ggsave(plot = g3, filename = "GeneralFigures/ElevationPlotEasttoWestNorthernHalf.png",
#        width = 6.5, height = 2.5, units = "in", dpi = 600)


# plot raster
# source('R/getBasePlot.R')
# thesisNED <- readRDS('GIS/thesisNED90m.Rds')
# NEDdf <- fortify(thesisNED)
# max(NEDdf$band1)
# min(NEDdf$band1)
# thesisNED <- ?aggregate(x = thesisNED, fact = 4, fun = mean)
# baseg <- getBasePlot(labels = T, scalebar = T)
# rdf <- fortify(thesisNED)
# rdf <- rdf[which(!is.na(rdf$band1)),]
# baseg + 
#   geom_raster(data = rdf,aes(x,y, fill = band1), alpha = .9) +
#   scale_fill_gradient2(low = "red", mid = "brown", high = "green") + 
#   guides(fill = F) 
#   geom_point(data = sites, aes(long, lat, fill = posteriormean), color = "black",
#              pch = 21, size = 1.25)
# 
# ggsave(paste0('GeneralFigures/ThesisNED.png'), dpi = 600,
#        width = 6.5, units = 'in')
# 
# plot vegetative zones
# rc <- reclassify(thesisNED, c(2440,Inf,4,  1524,2440,3,  1402,1524,2,  0,1402,1))
# rc <- ratify(rc)
# rat <- levels(rc[[1]])
# levels(rc) <- rat
# rat$zone <- c("San Juan River corridor", "Lowland benches", "Upland", "Highland")
# myColors <- c('#f2db73', '#d5e5b8', '#c3d490', '#c0dcdf')
# myKey <- list(text=list(lab=rat$zone),
#               rectangles=list(col = myColors),
#               space='left',
#               columns=4)
# 
# levelplot(rc, col.regions = myColors, colorkey = FALSE, key = myKey, margin = F) 
# myTheme
# levelplot(rc, margin = F, col.regions = c('#f2db73', '#d5e5b8', '#c3d490', '#c0dcdf'))
# png(filename = "GeneralFigures/ElevationZones.png", res = 600, width = 6.5, height = 6.5, units = "in")
# plot(rc, legend = F, box = F, axes = F, col = c('#edcea0', '#d5e5b8', '#c3d490', '#c0dcdf'),
#      interpolate = T)
# legend("bottomleft", c("San Juan River corridor", "Lowland benches", "Upland", "Highland"), 
#         fill = c('#edcea0', '#d5e5b8', '#c3d490', '#c0dcdf'), inset = c(0.01,.09,0,0))
# dev.off()
# 
# rc <- rasterToPoints(rc)
# rcdf <- data.frame(rc)
# colnames(rcdf) <- c("Longitude", "Latitude", "Zone")
# rcPlot <- ggplot(data=rcdf, aes(y=Latitude, x=Longitude)) +
#   geom_raster(aes(fill=Zone), interpolate = T) +
#   theme_bw() +
#   coord_equal() +
#   scale_fill_discrete(labels = c("San Juan River corridor", "Lowland benches", "Upland", "Highland")) +
#   theme(axis.title.x = element_text(size=16),
#         axis.title.y = element_text(size=16, angle=90),
#         axis.text.x = element_text(size=14),
#         axis.text.y = element_text(size=14),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position = "right",
#         legend.key = element_blank()
#   )
# rcPlot
# ggsave(plot = p, filename = "GeneralFigures/AnnualPrecipitation.png",
#        width = 6.5, height = 6.5, units = "in", dpi = 600)


# get landsat cover
# Get the NLCD (USA ONLY)
# Returns a raster
# NLCD <- get_nlcd(template = boundaryP,
#                  year = 2011,
#                  dataset = "landcover",
#                  label = "VEPIIN")
# levels(NLCD@data@attributes[[1]]$NLCD.2011.Land.Cover.Class)
# Plot with raster::plot
# raster::plot(NLCD, interpolate = T)
# sites <- readRDS('Data/SiteMaster.Rds')
# NLCD <- readRDS('GIS/NLCD.Rds')
# sitesSP <- SpatialPoints 
# points(sites$geometry, cex = .3, pch = 17)
# saveRDS(NLCD, "GIS/NLCD.Rds")
NLCD <- readRDS('GIS/NLCD.Rds')
# NLCD <- aggregate(NLCD, fact = 3, mean)
# saveRDS(NLCD,"GIS/NLCDReduced.Rds")
countiesSP <- readRDS('GIS/CountiesSP.Rds')
# NLCDt <- projectRaster(NLCD, crs = "+init=epsg:4326")
countiesSP <- spTransform(countiesSP, NLCD@crs)
countiesSP <- crop(countiesSP,NLCD)
png(filename = 'GeneralFigures/NLCDPlotwithStateLines.png', width = 3900, height = 3900, units = "px")
  plot(NLCD, interpolate = T)
  plot(countiesSP, fill = NA, add = T, lwd = 8)
dev.off()


# precip <- read_sf("GIS/PrecipitationAnnualPRISM19611990/PrecipitationAnnualPRISM19611990.shp")
precip <- raster("GIS/PRISM_ppt_30yr_normal_4kmM2_annual_asc/PRISM_ppt_30yr_normal_4kmM2_annual_asc.asc")
border <- readRDS('GIS/BorderEnlarged.Rds')
border <- spTransform(border, precip@crs)
precip <- crop(precip,border)
sites <- readRDS('Data/DataMasterSf.Rds')
countiesSP <- readRDS('GIS/CountiesSP.Rds')
projectRivers <- readRDS('GIS/projectRivers.Rds')
projectRivers <- as(projectRivers, "Spatial")

source('R/getBasePlot.R')
baseg <- getBasePlot(labels = F, scalebar = F)
precip <- disaggregate(precip, fact = 3, method = 'bilinear')
rc <- reclassify(precip, c(498.2,Inf,4,  361.5,498.2,3,  260.5,361.5,2,  0,260.5,1))
rdf <- fortify(rc)

baseg + 
  geom_raster(data = rdf,aes(x,y, fill = band1), alpha = .6) +
  geom_path(data = projectRivers, aes(long,lat, group = group),
            color = "lightgray", size = .25) +
  geom_polygon(data = countiesSP, aes(long, lat, group = group),
               fill = NA, color = "black", size = .3) +
  geom_point(data = sites, aes(long, lat), color = "white",
           pch = 21, size = .25) + 
  scalebar(location = "bottomleft", dist = 25, dd2km = TRUE, model = 'WGS84',
           st.size = 2.5, st.dist = .04, x.min =-110 , x.max =-107 , y.min =35.4 , y.max =37.5)+
  theme(legend.title = element_text(size = 12),
        plot.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.position = "bottom") +
  scale_fill_gradient("Annual Precipitation (mm)",
                      limits=c(min(rdf$band1),max(rdf$band1)),
                      labels = c(0,250,350,"500+"))

ggsave(filename = "GeneralFigures/AnnualPrecipitation.png",
       width = 6.5, height = 6.5, units = "in", dpi = 600)

hist(precip,
     xlab = "Elevation (meters)", ylab = "Frequency",
     col = "springgreen")
ggplot(df, aes(x = breaks, y = counts)) + 
  geom_bar(stat = "identity",fill='blue',alpha = 0.8) + 
  theme_bw()



precip <- rasterToPoints(precip)
precipdf <- data.frame(precip)
colnames(precipdf) <- c("Longitude", "Latitude", "Precipitation")
p <- ggplot(data=precipdf, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=Precipitation), interpolate = T) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradient("Annual Precipitation (mm)",
                      limits=c(min(precipdf$Precipitation),max(precipdf$Precipitation)),
                      low = "yellow", high = "blue") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank()
  )
ggsave(plot = p, filename = "GeneralFigures/AnnualPrecipitation.png",
       width = 6.5, height = 6.5, units = "in", dpi = 600)
