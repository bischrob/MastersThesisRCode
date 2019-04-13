# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script is designed to make my base plot callable via function

getBasePlot <- function(labels = F, scalebar = T, sizeF = 1, smallbase = F){
  library(rRJB)
  myLibrary(c('rio','tidyverse','sf','raster','rgeos','ggpubr','ggsn', 'rgdal','grid'))
  # sizeF = factor for resizing labels
  # smallbase is for plotting the area in the first map
  citylocs <- readRDS("GIS/Citylocs.Rds")
  Abajo <- readRDS('GIS/Abajo.Rds')
  Chuska <- readRDS('GIS/Chuska.Rds')
  SanJuan <- readRDS('GIS/SanJuan.Rds')
  projectRivers <- readRDS('GIS/projectRivers.Rds')
  projectRivers <- as(projectRivers, "Spatial")
  countiesSP <- readRDS('GIS/CountiesSP.Rds')
  projectMountainsSP <- readRDS("GIS/projectMountainsSP.Rds")
  # mountainCoords <- gCentroid(projectMountainsSP, byid = T)
  # mountainlocs <- data.frame(ID = 1:length(mountainCoords), Name = rep("",length(mountainCoords)))
  # mountainlocs <- cbind(mountainlocs,mountainCoords@coords)
  # mountainlocs$Name <- as.character(mountainlocs$Name)
  # mountainlocs[mountainlocs$ID == 18,2] <- "Abajo Mountains"
  # mountainlocs[mountainlocs$ID == 15,2] <- "Chuska Mountains"
  # mountainlocs[mountainlocs$ID == 27,2] <- "San Juan Mountains"
  # rownames(mountainlocs) <- 1:nrow(mountainlocs)
  # Abajo <- mountainlocs[18,]
  # Chuska <- mountainlocs[15,]
  # SanJuan <- mountainlocs[27,]
  # saveRDS(Abajo,'GIS/Abajo.Rds')
  # saveRDS(Chuska,'GIS/Chuska.Rds')
  # saveRDS(SanJuan,'GIS/SanJuan.Rds')
  # NArrow <- png::readPNG('North Arrow.png')
  # NArrow <- rasterGrob(NArrow)
  
  if(smallbase == T){
    baseg <- ggplot() + 
      geom_polygon(data = projectMountainsSP, aes(long,lat,group = group), fill = "gray") +
      geom_path(data = projectRivers, aes(long,lat, group = group),
                color = "lightgray", size = .25) +
      geom_polygon(data = countiesSP, aes(long, lat, group = group),
                   fill = NA, color = "#A9A9A9", size = .1) +
      geom_point(data = citylocs, aes(lon, lat), color = "black", size = 1, shape = 15) +
      theme_bw() + coord_fixed(1.3, xlim = c(-110.6,-106.9), ylim = c(35.4,38)) +
      theme(axis.title = element_blank(),
            axis.text = element_blank(), 
            legend.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"))
  } else if(labels == F){
    baseg <- ggplot() + 
      geom_polygon(data = projectMountainsSP, aes(long,lat,group = group), fill = "gray") +
      geom_path(data = projectRivers, aes(long,lat, group = group),
                color = "lightgray", size = .25) +
      geom_polygon(data = countiesSP, aes(long, lat, group = group),
                   fill = NA, color = "#A9A9A9", size = .1) +
      theme_bw() + coord_fixed(1.3, xlim = c(-110.6,-106.9), ylim = c(35.4,38)) +
      theme(axis.title = element_blank(),
            axis.text = element_blank(), 
            legend.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"))
  } else {
    baseg <- ggplot() + 
      geom_polygon(data = projectMountainsSP, aes(long,lat,group = group), fill = "gray") +
      geom_path(data = projectRivers, aes(long,lat, group = group),
                color = "lightgray", size = .5) +
      geom_polygon(data = countiesSP, aes(long, lat, group = group),
                   fill = NA, color = "#A9A9A9") +
      geom_point(data = citylocs, aes(lon, lat), color = "black", size = 1, shape = 15) +
      theme_bw() + coord_fixed(1.3, xlim = c(-110.6,-106.9), ylim = c(35.4,38)) +
      xlab("Longitude") + ylab("Latitude") + 
      geom_text(data = Abajo, aes(x,y), label = Abajo$Name, size = 3*sizeF, fontface = "bold") +
      geom_text(data = Chuska, aes(x,y), label = Chuska$Name, angle = -65, size = 3*sizeF,
                fontface = "bold") +
      geom_text(data = SanJuan, aes(x = -107.55,y = 37.7), label = SanJuan$Name, size = 3*sizeF,
                fontface = "bold") +
      geom_text(data = citylocs, aes(lon,lat), label = citylocs$Name, size = 2.5*sizeF,
                nudge_y = -.03, fontface = "bold") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank())
  }
  if(scalebar == T){
    baseg <- baseg + 
      scalebar(location = "bottomleft", dist = 25, dd2km = TRUE, model = 'WGS84',
               st.size = 2.5*sizeF, st.dist = .04, x.min =-110 , x.max =-107 , y.min =35.4 , y.max =37.5) 
    # annotation_custom(NArrow, ymin = 35.5, ymax = 35.7, xmax = -108.65) 
  }
  return(baseg)  
}

getSEUtahbaseg <- function(){
  pacman::p_load(tidyverse,sf)
  #load map data
  crs1 <- st_crs(4326)
  # create more uniform bounding box for SE Utah
  boundarySEU <- st_as_sfc(st_bbox(SEUtahdf))
  boundarySEU <- st_transform(boundarySEU, crs1)
  boundarySEU <- st_buffer(boundarySEU, .1)
  boundaryBigSEU <- st_buffer(boundarySEU, 1)
  citylocs <- readRDS("GIS/Citylocs.Rds")
  citylocs <- st_as_sf(citylocs, coordinates = c(long,lat), remove = F, crs = 4326)
  citylocs <- st_transform(citylocs, crs1)
  mountains <- readRDS('GIS/SEUtahMountains.Rds')
  mountains <- st_transform(mountains, crs1)
  projectRivers <- readRDS('GIS/projectRivers.Rds')
  projectRivers <- st_transform(projectRivers, crs1)
  counties <- readRDS('GIS/Counties.Rds')
  counties <- st_transform(counties, crs1)
  rivers <- st_read('GIS/SEUtahRivers.geojson')
  rivers <- st_transform(rivers, crs1)
  rivers <- rivers %>% filter(GNIS_ID %in% c(1450314,1443567,1426595))
  projectRivers <- st_crop(projectRivers,boundaryBigSEU)
  
  # base plot
  g <- ggplot() + 
    geom_sf(data = mountains, fill = 'lightgray', color = "gray") + 
    geom_sf(data = projectRivers,
            color = "gray", size = 1) +
    geom_sf(data = rivers, color = "gray", size = .75) +
    geom_sf(data = citylocs, color = "gray", size = 1.5, shape = 15) +
    # geom_text_repel(data = citylocs, aes(lon, lat, label = Name)) +
    # geom_sf(data = SEUtahdf, aes(color = posteriormeanDec)) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          legend.title = element_blank(),
          panel.grid.major = element_line(color = NA),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"),
          panel.border = element_rect(colour = "#373737", fill=NA, size=2))
  return(g)
}

getSAA2019FullBaseMap <- function(){
  
  pacman::p_load(tidyverse,sf)
  #load map data
  crs1 <- st_crs(4326)
  # create more uniform bounding box for SE Utah
  citylocs <- readRDS("GIS/Citylocs.Rds")
  citylocs <- st_as_sf(citylocs, coordinates = c(long,lat), remove = F, crs = 4326)
  citylocs <- st_transform(citylocs, crs1)
  mountains <- readRDS("GIS/projectMountains.Rds")
  mountains <- st_transform(mountains, crs1)
  projectRivers <- readRDS('GIS/projectRivers.Rds')
  projectRivers <- st_transform(projectRivers, crs1)
  counties <- readRDS('GIS/Counties.Rds')
  counties <- st_transform(counties, crs1)
  rivers <- st_read('GIS/SEUtahRivers.geojson')
  rivers <- st_transform(rivers, crs1)
  rivers <- rivers %>% filter(GNIS_ID %in% c(1450314,1443567,1426595, 936834))
  
  # base plot
  g <- ggplot() + 
    geom_sf(data = mountains, fill = 'lightgray', color = "gray") + 
    geom_sf(data = projectRivers,
            color = "gray", size = 1) +
    geom_sf(data = rivers, color = "gray", size = .75) +
    geom_sf(data = counties, fill = NA) +
    geom_sf(data = citylocs, color = "gray", size = 1.5, shape = 15)


  return(g)
}