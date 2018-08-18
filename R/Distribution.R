#' Hexagon distributions
# !diagnostics off

# Hexagons

set.seed(1010)
rRJB::myLibrary(c("sf",'tidyverse','ggspatial','tidyr','raster','rgeos','rgbif','viridis','gridExtra'))

# get project diversity (number of decorated types found at each site)
DMP <- readRDS("Data/DataMasterPosterior.Rds")
source('R/CeramicTypeandRangeFunctions.R')
diversity <- NULL
i = 0
for (x in DMP$ProjectNumber){
  i = i + 1
  diversity[i] <- nrow(getCeramicTypes(x, onlyMCDtypes = F, onlyFormaltypes = T))
}

periodDiversity <- tibble(ProjectNumber = DMP$ProjectNumber,
                          Diversity = diversity,
                          Region = DMP$Region,
                          Period = DMP$Period,
                          SJRWP = round(DMP$posteriormean*100,1))
saveRDS(periodDiversity, "Data/PeriodDiversity.Rds")
ggplot(periodDiversity,aes(diversity,SJRWP)) + geom_point()
regiondiversity <- tibble(Diversity = diversity,
                          Region = DMP$Region,
                          SJRWP = round(DMP$posteriormean*100,1))
regionSum <- regiondiversity %>% group_by(Region) %>% summarise_all(mean)

# load data
DMP <- readRDS('Data/DataMasterPosterior.Rds')

boundary <- readRDS('GIS/projectBoundary.Rds')
boundary <- st_transform(boundary, 26912)
boundarySP <- as(boundary, "Spatial")

# also get diversity
periodDiversity <- readRDS("Data/PeriodDiversity.Rds")
periodDiversity <- periodDiversity %>% dplyr::select(ProjectNumber,Diversity)
DMP <- merge(DMP,periodDiversity, by = "ProjectNumber")

size <- 9000
hexPoints <- spsample(boundarySP, type = "hexagonal", cellsize = size, offset = c(0, 0))
hexGrid <- HexPoints2SpatialPolygons(hexPoints, dx = size)
hexGridSF <- st_as_sf(hexGrid)
saveRDS(hexGridSF, "Data/hexGridSF.Rds")
hexPointSf <- st_as_sf(hexPoints)
hexPointSf$x <- st_coordinates(hexPointSf)[,1]
hexPointSf$y <- st_coordinates(hexPointSf)[,2]
saveRDS(hexPointSf, "Data/HexPoints.Rds")
hexPointSf <- readRDS("Data/HexPoints.Rds")
hexGridSF <- readRDS("Data/hexGridSF.Rds")

# plot regions with the confidence intervals
regionsAdj <- st_read('GIS/EarlyPuebloRegionsAdjusted.geojson')
regionsAdj <- as(regionsAdj,"Spatial")
regionsAdj <- spTransform(regionsAdj, CRS('+init=epsg:4326'))
regionAdjdf <- fortify(regionsAdj, region ="id")
regionAdjdf <- merge(regionAdjdf, regionsAdj@data, by="id")
regionAdjdf$Name <- gsub("East","Eastern",regionAdjdf$Name)

periods <- sort(unique(DMP$Period))
p = periods[2]
for(p in periods){
  DMPsub <- DMP %>% filter(Period == p)
  DMPsub <- st_transform(DMPsub, 26912)
  ConfInts <- list()
  regions <- unique(DMPsub$Region)
  for (r in 1:length(regions)){
    DMPs1 <- DMPsub %>% filter(Region == regions[r])
    bt <- binom.test(x = round(sum(DMPs1$SJRWTotal),0), 
                   n = round(sum(DMPs1$Total),0), 
                   p = DMPs1$priormean[1]/100, alternative = "t",
                   conf.level = .999)
    ConfInts[[r]] <- tibble(Region = regions[r], mean = bt$estimate*100,
                            cLow = bt$conf.int[1]*100,
                 cHigh = bt$conf.int[2]*100)
  }
  ConfInts <- bind_rows(ConfInts)
  grdPts <- list()
  # n = 692
  for (n in 1:nrow(hexGridSF)) {
    g <- hexGridSF[n,]
    pts <- DMPsub[g,]
    if(!nrow(pts) == 0){
    cInt <- ConfInts %>% filter(Region == pts$Region[1]) 
    grdPts[[n]] <- tibble(id = n, wm = weighted.mean(pts$posteriormean,pts$Total)*100,
                          sd = nrow(pts),
                          diversity = mean(pts$Diversity),
                          x = st_coordinates(hexPointSf[n,])[,1],
                          y = st_coordinates(hexPointSf[n,])[,2],
                          cLow = cInt$cLow,
                          cHigh = cInt$cHigh)
    }
  }
  grdPts <- bind_rows(grdPts)
  confVal <- NULL
  for (i in 1:nrow(grdPts)) {
    confVal[i] <- ifelse(grdPts$wm[i] > grdPts$cHigh[i],"High",
                         ifelse(grdPts$wm[i] < grdPts$cLow[i],"Low",
                                "Expected"))  
  }
  grdPts$confVal <- confVal
  hexGridSF2 <- hexGridSF
  hexGridSF2$id = 1:nrow(hexGridSF2)
  hexGridSF2 <- merge(hexGridSF2,grdPts, by = "id")
  hexGridSP <- as(hexGridSF, "Spatial")
  hexGridSP <- spTransform(hexGridSP, CRS('+init=epsg:4326'))
  hexGriddf <- fortify(hexGridSP)
  st_geometry(hexGridSF2) <- NULL
  hexGriddf <- merge(hexGriddf,hexGridSF2, by = "id")
  hexGriddf <- arrange(hexGriddf,group,order)
  
  source('R/getBasePlot.R')
  if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = F)
  
  baseg +
    geom_polygon(data = hexGriddf, aes(long,lat, group = group, fill = wm), alpha = .75,
                 color = "darkgray") +
    ggtitle(p) +
    scale_fill_gradientn(limits = c(0,100),
                         colours=c("white","red","firebrick","firebrick","firebrick"),
                         breaks= c(0,25,50,75,100)) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 10)) + 
    ggsn::scalebar(location = "bottomleft", dist = 25, dd2km = TRUE, model = 'WGS84',
                   st.size = 1.7, st.dist = .04, x.min =-110 , x.max =-107 , y.min =35.4 , y.max =37.5)
  
  
  ggsave(paste0('GeneralFigures/Hexagon',p,'.png'), dpi = 600,
         width = 3.2, height = 3.72, units = 'in')
  # ggsave(paste0('GeneralFigures/Hexagon-',p,'.svg'),
  #        width = 3.2, height = 3.72, units = 'in')

#confidence intervals
baseg +
  geom_polygon(data = regionAdjdf, aes(long,lat, group = group), fill = NA, color = "#4b4d4f",
               lwd = 1.25) +
  geom_polygon(data = hexGriddf, aes(long,lat, group = group, fill = confVal),
               alpha = .75, color = "darkgray") +
  scale_fill_manual(values = c('white','firebrick','cyan')) +
  ggtitle(p) +     theme(legend.position = "bottom",
                         plot.title = element_text(size = 10)) + 
  ggsn::scalebar(location = "bottomleft", dist = 25, dd2km = TRUE, model = 'WGS84',
                 st.size = 1.7, st.dist = .04, x.min =-110 , x.max =-107 , y.min =35.4 , y.max =37.5)

ggsave(paste0('GeneralFigures/HexagonConfidence',p,'-9k.png'), dpi = 600,
       width = 3.2, height = 3.72, units = 'in')
# ggsave(paste0('GeneralFigures/HexagonConfidence',p,'-9k.svg'),
#        width = 3.2, height = 3.72, units = 'in')

    
  # baseg +
  #   geom_polygon(data = hexGriddf, aes(long,lat, group = group, fill = sd), alpha = .75,
  #                color = "darkgray") +
  #   ggtitle(p) + theme(legend.position = "bottom") +
  #   scale_fill_gradientn(limits = c(0,225),
  #               colours=c("white","#240496","#240496","#240496","#380059","#380059","#380059"),
  #                        breaks= c(5,50,100,225))
  # 
  # ggsave(paste0('GeneralFigures/HexagonSiteDensity',p,'-9k.png'), dpi = 600,
  #        width = 6.5, units = 'in')
  
  # baseg +
  #   geom_polygon(data = hexGriddf, aes(long,lat, group = group, fill = diversity), alpha = .75,
  #                color = "darkgray") +
  #   scale_fill_gradient(low = "white", high = "#3a7513") +
  #   ggtitle(p) + theme(legend.position = "bottom")
  # 
  # ggsave(paste0('GeneralFigures/HexagonCeramicDiversity',p,'-9k.png'), dpi = 600,
  #        width = 6.5, units = 'in')
}  

# look closer at the DAP region
DMPosterior <- readRDS("Data/DataMasterPosterior.Rds")
DAP <- st_read('GIS/DAPArea.geojson')
DMPosterior <- DMPosterior[DAP,]
summary(DMPosterior$posteriormean)
# mapview::mapview(DMPosterior)
source('R/getBasePlot.R')
if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = T)
periods <- sort(unique(DMPosterior$Period))
p = periods[5]
for(p in periods){
  plotdf <- DMPosterior %>% filter(Period == p)
  baseg + 
  geom_point(data = plotdf, aes(long,lat, fill = posteriormeanDec), size = 1.8,
             stroke = .25, pch = 21, color = "black") + 
    coord_cartesian(xlim = c(-108.625,-108.475), ylim = c(37.44,37.59)) +
    ggtitle(p) +
    scale_fill_gradientn(limits = c(0,1),
                         colours=c("white","orange","firebrick","firebrick","firebrick"),
                         breaks= c(0,.5,1)) +
    ggsn::scalebar(location = "bottomleft", dist = 1, height = .05, dd2km = TRUE, model = 'WGS84',
             st.size = 2.5, st.dist = .045, x.min = -108.625 , x.max = -108.475,
             y.min = 37.45 , y.max = 37.59) + 
    theme(legend.position = "bottom",
          plot.title = element_text(size=10))
  
  # ggsave(paste0('GeneralFigures/DAPArea',p,'-decorated.png'), dpi = 600)
  # ggsave(paste0('GeneralFigures/DAPArea',p,'-decorated.svg'), height = 3.1, width = 3.2)
  mapview::mapview(plotdf)
}

# assemblage size vs SJRWP
ggplot(data = DMPosterior, aes(Total,posteriormean, color = Period)) + 
  geom_point() + theme_bw() + ylab("SJRW %") + xlab("Total Ceramic Assemblage") +
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave("GeneralFigures/DAPSJRWvsTotal.png", dpi = 600, width = 6.5, height = 3.5, units = "in")

# location map
if(!exists('basep')) basep <- getBasePlot(labels = T, scalebar = T)
DAP <- as(DAP, "Spatial")
DAP <- fortify(DAP)

basep + 
  geom_polygon(data = DAP, aes(long,lat, group = group), fill = "firebrick")
ggsave(filename = "GeneralFigures/DAPLocation.png", dpi = 600,
       width = 6.5, units = 'in')  


# look closer at the ALP region
DMPosterior <- readRDS("Data/DataMasterPosterior.Rds")
ALP <- st_read('GIS/AnimasLaPlata.geojson')
DMPosterior <- DMPosterior[ALP,]
summary(DMPosterior$posteriormean)
mapview::mapview(DMPosterior) + ALP
# mapview::mapview(DMPosterior)
source('R/getBasePlot.R')
if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = T)
periods <- sort(unique(DMPosterior$Period))
p = periods[2]
for(p in periods){
  plotdf <- DMPosterior %>% filter(Period == p)
  baseg + 
    geom_point(data = plotdf, aes(long,lat, fill = posteriormean), size = 3,
               pch = 21, color = "black") + 
    coord_cartesian(xlim = c(-107.97,-107.86), ylim = c(37.2,37.26)) +
    ggtitle(p) +
    scale_fill_gradientn(limits = c(0,1),
                         colours=c("white","orange","firebrick","firebrick","firebrick"),
                         breaks= c(0,.25,.5,.75,1))
  
  ggsave(paste0('GeneralFigures/ALPArea',p,'.png'), dpi = 600,
         width = 6.5, units = 'in')
}

# location map
if(!exists('basep')) basep <- getBasePlot(labels = T, scalebar = T)
ALP <- as(ALP, "Spatial")
ALP <- fortify(ALP)

basep + 
  geom_polygon(data = ALP, aes(long,lat, group = group), fill = "firebrick")
ggsave(filename = "GeneralFigures/ALPLocation.png", dpi = 600,
       width = 6.5, units = 'in')  



# explore variability in SJRW by site
DMP <- readRDS('Data/DataMasterPosterior.Rds')
periods <- sort(unique(DMP$Period))
p = periods[3]
for(p in periods){
  DMPsub <- DMP %>% filter(Period == p)
  ggplot(data = DMPsub, aes(Region, posteriormean, fill = Region)) +
    geom_boxplot() + ggtitle(p)
  ggsave(paste("GeneralFigures/Site Variability/",p,"SJRWPBoxPlots.png"))
}

# top sites
p = periods[5]
DMPsub <- DMP %>% filter(Period == p) %>% arrange(desc(posteriormean))
x = DMPsub[1:20,]


##########################################################################################
# Hexagons for Decorated

set.seed(1010)
rRJB::myLibrary(c("sf",'tidyverse','ggspatial','tidyr','raster','rgeos','rgbif','viridis','gridExtra'))

# load data
DMP <- readRDS('Data/DataMasterPosterior.Rds')
hexGridSF <- readRDS("Data/hexGridSF.Rds")
hexPointSf <- readRDS("Data/HexPoints.Rds")

periods <- sort(unique(DMP$Period))
p = periods[2]
for(p in periods){
  DMPsub <- DMP %>% filter(Period == p, !is.na(priormeanDec))
  DMPsub <- st_transform(DMPsub, 26912)
  ConfInts <- list()
  regions <- unique(DMPsub$Region)
  for (r in 1:length(regions)){
    DMPs1 <- DMPsub %>% filter(Region == regions[r])
    bt <- binom.test(x = round(sum(DMPs1$SJRWTotal),0), 
                     n = round(sum(DMPs1$DecoratedTotal),0), 
                     p = DMPs1$priormeanDec[1]/100, alternative = "t",
                     conf.level = .999)
    ConfInts[[r]] <- tibble(Region = regions[r], mean = bt$estimate*100,
                            cLow = bt$conf.int[1]*100,
                            cHigh = bt$conf.int[2]*100)
  }
  ConfInts <- bind_rows(ConfInts)
  grdPts <- list()
  # n = 692
  for (n in 1:nrow(hexGridSF)) {
    g <- hexGridSF[n,]
    pts <- DMPsub[g,]
    if(!nrow(pts) == 0){
      cInt <- ConfInts %>% filter(Region == pts$Region[1]) 
      grdPts[[n]] <- tibble(id = n, wm = weighted.mean(pts$posteriormeanDec,pts$DecoratedTotal)*100,
                            sd = nrow(pts),
                            diversity = mean(pts$Diversity),
                            x = st_coordinates(hexPointSf[n,])[,1],
                            y = st_coordinates(hexPointSf[n,])[,2],
                            cLow = cInt$cLow,
                            cHigh = cInt$cHigh)
    }
  }
  grdPts <- bind_rows(grdPts)
  confVal <- NULL
  for (i in 1:nrow(grdPts)) {
    confVal[i] <- ifelse(grdPts$wm[i] > grdPts$cHigh[i],"High",
                         ifelse(grdPts$wm[i] < grdPts$cLow[i],"Low",
                                "Expected"))  
  }
  grdPts$confVal <- confVal
  hexGridSF2 <- hexGridSF
  hexGridSF2$id = 1:nrow(hexGridSF2)
  hexGridSF2 <- merge(hexGridSF2,grdPts, by = "id")
  hexGridSF2$sd <- log(hexGridSF2$sd+.1)
  hexGridSP <- as(hexGridSF, "Spatial")
  hexGridSP <- spTransform(hexGridSP, CRS('+init=epsg:4326'))
  hexGriddf <- fortify(hexGridSP)
  st_geometry(hexGridSF2) <- NULL
  hexGriddf <- merge(hexGriddf,hexGridSF2, by = "id")
  hexGriddf <- arrange(hexGriddf,group,order)
  source('R/getBasePlot.R')
  if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = T)
  
  baseg +
    geom_polygon(data = hexGriddf, aes(long,lat, group = group, fill = wm), alpha = .75,
                 color = "darkgray") +
    scale_fill_gradientn(limits = c(0,100),
                         colours=c("white","orange","firebrick","firebrick","firebrick"),
                         breaks= c(0,25,50,75,100)) +
    ggtitle(p) + theme(legend.position = "bottom")
  plotly::ggplotly()
  ggsave(paste0('GeneralFigures/HexagonDist',p,'-9k-Decorated.png'), dpi = 600,
         width = 6.5, units = 'in')
  # # ggsave(paste0('GeneralFigures/HexagonDist',p,'-9k-Decorated.svg'),
  # #        width = 6.5, units = 'in')
  # 
  # confidence intervals
  baseg +
    geom_polygon(data = hexGriddf, aes(long,lat, group = group, fill = confVal),
                 alpha = .75, color = "darkgray") +
    scale_fill_manual(values = c('white','firebrick','cyan')) +
    ggtitle(p) + theme(legend.position = "bottom")

  ggsave(paste0('GeneralFigures/HexagonConfidence',p,'-9k-Decorated.png'), dpi = 600,
         width = 6.5, units = 'in')
  ggsave(paste0('GeneralFigures/HexagonConfidence',p,'-9k.svg'),
         width = 6.5, units = 'in')

  
  baseg +
    geom_polygon(data = hexGriddf, aes(long,lat, group = group, fill = sd), alpha = .75,
                 color = "darkgray") +
    scale_fill_gradient(low = "white", high = "#240496") +
    ggtitle(p) + theme(legend.position = "bottom")

  ggsave(paste0('GeneralFigures/HexagonSiteDensity',p,'-9k-log-decorated.png'), dpi = 600,
         width = 6.5, units = 'in')
  
  baseg +
    geom_polygon(data = hexGriddf, aes(long,lat, group = group, fill = diversity), alpha = .75,
                 color = "darkgray") +
    scale_fill_gradient(low = "white", high = "#3a7513") +
    ggtitle(p) + theme(legend.position = "bottom")

  ggsave(paste0('GeneralFigures/HexagonCeramicDiversity',p,'-9k-Decorated.png'), dpi = 600,
         width = 6.5, units = 'in')
}  

