#################################################################################
# Name: Interpolation for San Juan Red Ware
# Project: Master's Thesis
# Author: Robert J. Bischoff
# Date: 12/11/2017
# Purpose: Interpolate the density of red ware. Based on tutorial at 
# https://mgimond.github.io/Spatial/interpolation-in-r.html
# License: NA
# Packages Used: rgdal, tmap, gstat, sp 
#################################################################################

library(rRJB)
myLibrary(c("rgdal","lubridate","tidyverse",
            'sf','rgeos','maptools','raster','gstat','ggspatial', 'egg','grid','rgdal'))

# Load data
sitesMaster <- readRDS("Data/DataMasterPosterior.Rds")
sitesMaster$posteriormean <- round(sitesMaster$posteriormean * 100,1)
periods <- sort(unique(sitesMaster$Period))
boundary <- readRDS("GIS/BorderEnlarged.Rds")
boundary <- spTransform(boundary, CRS("+init=epsg:26912"))
countiesSP <- readRDS('GIS/CountiesSP.Rds')

i = 3
for (i in 1:length(periods)){
  p <- periods[i]
  sites <- sitesMaster %>% filter(Period == p)
  
  # Get IDW object then clip to boundary
  source("R/getIDW.R")
  r <- getIDW(sf = sites, boundary = boundary, variable = "posteriormean",
              pwr = 3, myWidth = 36000, crsP = 26912, useEmptyPoints = T)
  rg <- projectRaster(r, crs = "+init=epsg:4326")
  # saveRDS(rg,paste0('GIS/IDW-',p,'.Rds'))
  
  # Plots
  rg <- disaggregate(rg, fact = 3, method = 'bilinear')
  rdf <- ggspatial::spatial_fortify(rg)
  names(rdf) <- c("x","y","band1")
  rdf <- rdf[which(!is.na(rdf$band1)),]
  # rdf <- rdf %>% filter(band1 > )

  source('R/getBasePlot.R')
  if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = F)

  baseg +
    geom_raster(data = rdf,aes(x,y, fill = band1), alpha = .6, interpolate = T) +
    geom_polygon(data = countiesSP, aes(long, lat, group = group),
                 fill = NA, color = "black", size = .1) +
    geom_contour(data = rdf, aes(x,y,z = band1), color = "black", binwidth = 5, size = .15) +
    ggtitle(p) +
    scale_fill_gradientn(limits = c(0,100),
                         colours=c("white","red","firebrick","firebrick","firebrick"),
                         breaks= c(0,25,50,75,100)) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 10)) + labs(caption = "5% San Juan Red Ware contour intervals") +
    ggsn::scalebar(location = "bottomleft", dist = 25, dd2km = TRUE, model = 'WGS84',
                   st.size = 1.7, st.dist = .04, x.min =-110 , x.max =-107 , y.min =35.4 , y.max =37.5)


  ggsave(paste0('GeneralFigures/Distribution/IDW-',p,'.png'), dpi = 600,
         width = 3.2, height = 3.72, units = 'in')
  # ggsave(paste0('GeneralFigures/IDW-',p,'.svg'),
  #        width = 3.2, height = 3.72, units = 'in')

  # baseg +
  #   geom_raster(data = rdf,aes(x,y, fill = band1), alpha = .6, interpolate = T) +
  #   geom_contour(data = rdf, aes(x,y,z = band1), color = "#383832", binwidth = 5, size = .15) +
  #   ggtitle(p) + guides(color = F) +
  #   geom_point(data = sites, aes(long,lat, color = posteriormean), size = 2,
  #              pch = 19) +
  #   geom_point(data = sites, aes(long,lat), size = 2,
  #              pch = 21, fill = NA, color = "black", stroke = .1) +
  #   scale_color_gradientn(limits = c(0,100),
  #                         colours=c("white","red","firebrick","firebrick","firebrick"),
  #                         breaks= c(0,25,50,75,100)) +
  #   scale_fill_gradientn(limits = c(0,100),
  #                        colours=c("white","red","firebrick","firebrick","firebrick"),
  #                        breaks= c(0,25,50,75,100)) +
  #   theme(legend.position = "bottom",
  #         legend.title = element_text(size = 10)) +
  #   labs(caption = "5% San Juan Red Ware contour intervals") +
  #   ggsn::scalebar(location = "bottomleft", dist = 25, dd2km = TRUE, model = 'WGS84',
  #                  st.size = 2.5, st.dist = .04, x.min =-110 , x.max =-107 , y.min =35.4,
  #                  y.max =37.5)
  # 
  # ggsave(paste0('GeneralFigures/IDW-',p,'w-sites.png'), dpi = 600,
  #        width = 3.2, height = 3.72, units = 'in')
  # ggsave(paste0('GeneralFigures/IDW-',p,'w-sites.svg'),
  #        width = 3.2, height = 3.72, units = 'in')
}

## inspect graphs using plotly
# p = periods[3]
# r <- readRDS(paste0('GIS/IDW-',p,'.Rds'))
# writeRaster(r,paste0('GIS/IDW-',p,'.tif'))
# rg <- disaggregate(rg, fact = 3, method = 'bilinear')
# rdf <- fortify(r)
# rdf <- rdf[which(!is.na(rdf$band1)),]
# rdf <- rdf %>% filter(band1 > 1)
# sites <- sitesMaster %>% filter(Period == p)
# sites <- sites %>% dplyr::select(c(1,2,3,7,8,9,10,11,12,13,16,17,28))
# st_write(sites, paste0('GIS/Sites-',p,'v2.geojson'))

# g <- ggplot() +
#   geom_raster(data = rdf,aes(x,y, fill = band1), alpha = .9, interpolate = T) +
#   scale_fill_gradient(low = "white", high = "red") + 
#   geom_contour(data = rdf, aes(x,y,z = band1), color = "#383832", binwidth = 10, size = .15) +
#   geom_point(data = sites, aes(long, lat, fill = posteriormean, label = ProjectNumber),
#                                color = "black",
#              pch = 21, size = 1.25) + geom_jitter()
# 
# plotly::ggplotly(p = g)
################################################################################################
################################################################################################

# plot all at once
# Load data
# DMPosterior <- readRDS("Data/DataMasterPosterior.Rds")
# DMPosterior$posteriormean <- round(DMPosterior$posteriormean * 100,1)
# sites <- DMPosterior
# # sites <- sites %>% filter(Total >= 200) # use only large sites  
# # Get IDW object then clip to boundary
#   source("R/getIDW.R")
#   r <- getIDW(sites, pwr = 5, myWidth = 18000)
#   rg <- projectRaster(r, crs = "+init=epsg:4326")
#   # saveRDS(rg,paste0('GIS/IDW-All.Rds'))
#   
#   # Plots
#   rg <- disaggregate(rg, fact = 3, method = 'bilinear')
#   rdf <- fortify(rg)
#   rdf <- rdf[which(!is.na(rdf$band1)),]
#   rdf <- rdf %>% filter(band1 > 1)
#   
#   source('R/getBasePlot.R')
#   if(!exists('baseg')){
#     NArrow <- png::readPNG('North Arrow.png')
#     NArrow <- rasterGrob(NArrow)
#     baseg <- getBasePlot(labels = F, scalebar = F)
#   } 
#   
#   baseg + geom_raster(data = rdf,aes(x,y, fill = band1), alpha = .8) +
#     geom_point(data = sites, aes(long, lat), fill = "black",
#                pch = 21, size = .25) +
#     scale_fill_gradient(low = "#FFF5F0", high = "#EF3B2C") + 
#     geom_contour(data = rdf, aes(x,y,z = band1), color = "black", binwidth = 10, size = .15) +
#     ggtitle('All') + guides(fill = guide_colorbar(barwidth = .5, barheight = 4,
# title = "SJRW %")) +
#     scalebar(location = "bottomleft", dist = 50, dd2km = TRUE, model = 'WGS84',
#              st.size = 2, x.min =-110 , x.max =-107 , y.min =35.5 , y.max =37.5, st.dist = .06) +
#     annotation_custom(NArrow, ymin = 35.3, ymax = 35.7, xmax = -109.65) +
#     theme(legend.title = element_text(size = 8),
#           plot.title = element_text(size = 11),
#           legend.text = element_text(size = 6),
#           legend.position = c(1.1,.6),
#           legend.justification = "right")
# 
  # ggsave(paste0('GeneralFigures/IDW-All-w-sites.png'), dpi = 600,
  #        width = 3.25, height = 2.5, units = 'in')
# 
# save raster for further exploration in GIS
# writeRaster(r,'GIS/IDWAllSites.tif')
# DMPosterior <- DMPosterior %>% dplyr::select(c(1:17,28,31))
# DMPosterior <- st_transform(DMPosterior, 26912)
# st_write(DMPosterior,'GIS/SitesPosterior.geojson')

# allsitescontours <- rasterToContour(r)
# contoursSf <- st_as_sf(allsitescontours)
# contoursSf50 <- contoursSf[which(contoursSf$level == 50),]
# contoursseparate <- st_cast(contoursSf50,"LINESTRING")
# productionCenter <- st_centroid(contoursseparate)
# library(mapview)
# coords <- st_coordinates(productionCenter)
# x1 <- mean(coords[,1])
# x2 <-mean(coords[,2])
# x <- tibble(x = x1, y = x2)
# productionCenter <- st_as_sf(x, coords = c("x","y"), crs = 26912)
# mapview(productionCenter)+ contoursSf50
# saveRDS(productionCenter,'GIS/productionCenter.Rds')
# st_write(productionCenter,'GIS/productionCenter.geojson')

#########################################################################################
## Explore IDW
# r <- readRDS('GIS/IDW-AD 900.Rds')
# DMPosterior <- readRDS("Data/DataMasterPosterior.Rds")
# allsitescontours <- rasterToContour(r)
# contoursSf <- st_as_sf(allsitescontours)
# library(mapview)
# mapview(contoursSf) + r

#########################################################################################

# decorated

library(rRJB)
myLibrary(c("rgdal","lubridate","tidyverse",
            'sf','rgeos','maptools','raster','gstat','ggspatial', 'egg','grid','rgdal'))

# Load data
sitesMaster <- readRDS("Data/DataMasterPosterior.Rds")
sitesMaster <- sitesMaster %>% filter(!is.na(priormeanDec))
sitesMaster$posteriormeanDec <- round(sitesMaster$posteriormeanDec * 100,1)
periods <- sort(unique(sitesMaster$Period))
boundary <- readRDS("GIS/BorderEnlarged.Rds")
boundary <- spTransform(boundary, CRS("+init=epsg:26912"))

# i = 3
for (i in 1:length(periods)){
  p <- periods[i]
  sites <- sitesMaster %>% filter(Period == p)
  
  # Get IDW object then clip to boundary
  source("R/getIDW.R")
  r <- getIDW(sf = sites, boundary = boundary, variable = "posteriormeanDec",
              pwr = 3, myWidth = 36000, crsP = 26912, useEmptyPoints = T)
  rg <- projectRaster(r, crs = "+init=epsg:4326")
  saveRDS(rg,paste0('GIS/IDW-',p,'-decorated.Rds'))

  # Plots
  rg <- disaggregate(rg, fact = 3, method = 'bilinear')
  rdf <- ggspatial::spatial_fortify(rg)
  names(rdf) <- c("x","y","band1")
  rdf <- rdf[which(!is.na(rdf$band1)),]
  # rdf <- rdf %>% filter(band1 > )

  source('R/getBasePlot.R')
  if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = F)
  if(!exists("NArrow")) {
    NArrow <- png::readPNG('North Arrow.png')
    NArrow <- rasterGrob(NArrow)
  }
  
  baseg +
    geom_raster(data = rdf,aes(x,y, fill = band1), alpha = .6, interpolate = T) +
    geom_contour(data = rdf, aes(x,y,z = band1), color = "black", binwidth = 10, size = .15) +
    ggtitle(p) +
    scale_fill_gradientn(limits = c(0,100),
                colours=c("white","red","firebrick"),
                          breaks= c(0,25,50,75,100)) +
    theme(legend.position = "bottom") + labs(caption = "10% San Juan Red Ware contour intervals") +
    ggsn::scalebar(location = "bottomleft", dist = 25, dd2km = TRUE, model = 'WGS84',
             st.size = 2.5, st.dist = .04, x.min =-110 , x.max =-107 , y.min =35.4 , y.max =37.5) +
    annotation_custom(NArrow, ymin = 35.5, ymax = 35.7, xmax = -108.65) 
  
  ggsave(paste0('GeneralFigures/IDW-',p,'-decorated.png'), dpi = 600,
         width = 6.5, units = 'in')
  ggsave(paste0('GeneralFigures/IDW-',p,'-decorated.svg'),
         height = 3, units = 'in')

  baseg +
    geom_raster(data = rdf,aes(x,y, fill = band1), alpha = .6, interpolate = T) +
    geom_contour(data = rdf, aes(x,y,z = band1), color = "#383832", binwidth = 10, size = .15) +
    ggtitle(p) + guides(color = F) +
    geom_point(data = sites, aes(long,lat, color = posteriormeanDec), size = 2,
             pch = 19) +
    geom_point(data = sites, aes(long,lat), size = 2,
               pch = 21, fill = NA, color = "black", stroke = .1) +
    scale_color_gradientn(limits = c(0,100),
                         colours=c("white","red","firebrick"),
                         breaks= c(0,25,50,75,100)) +
    scale_fill_gradientn(limits = c(0,100),
                          colours=c("white","red","firebrick"),
                          breaks= c(0,25,50,75,100)) +
    theme(legend.position = "bottom") + labs(caption = "10% San Juan Red Ware contour intervals") +
    ggsn::scalebar(location = "bottomleft", dist = 25, dd2km = TRUE, model = 'WGS84',
                   st.size = 2.5, st.dist = .04, x.min =-110 , x.max =-107 , y.min =35.4,
                   y.max =37.5) +
    annotation_custom(NArrow, ymin = 35.5, ymax = 35.7, xmax = -108.65) 
 
   ggsave(paste0('GeneralFigures/IDW-',p,'w-sites-decorated.png'), dpi = 600,
         width = 6.5, units = 'in')
  ggsave(paste0('GeneralFigures/IDW-',p,'w-sites-decorated.svg'),
         height = 3, units = 'in')
}

