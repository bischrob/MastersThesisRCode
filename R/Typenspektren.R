################################################################################################
# This script is adapated from the script_01.R from the Mosaic conference and calculates 
# Typenspektren
# !diagnostics off

# make sure random effects are the same
set.seed(1010)

library(rRJB)
myLibrary(c("tidyverse","sp","proj4","spatstat","maptools",'raster',
            'rgdal', 'reshape2',"splitstackshape",'rgeos','sf','ggspatial'))

# Euclidean distance function
cdist <- function(a,b){sqrt(sum((a-b) ^ 2))} # function for distance

################################################################################################

# standard Typenspektren

DMAll <- readRDS('Data/DataMasterAdjusted.Rds')

# load spatial data
DMSF <- readRDS('Data/DataMasterSf.Rds')

# DMSP <- st_transform(DMSF, 26912)
# DMSP <- as(DMSP,"Spatial")
# bb <- bbox(DMSP)
# bb[1:2] <- bb[1:2] - 10000
# bb[3:4] <- bb[3:4] + 10000
# spext <- as(raster::extent(bb), "SpatialPolygons")
# spext@proj4string <- DMSP@proj4string
# win <- owin(xrange=c(bb[1,1],bb[1,2]), yrange= c(bb[2,1],bb[2,2]), unitname="m")
# s.points <- spsample(spext, 3000,  type="regular")
# saveRDS(win,"data/win.Rds")
# saveRDS(s.points,"data/s.points.Rds")
win <- readRDS("data/win.Rds")
s.points <- readRDS("data/s.points.Rds")
sdev <- 9000

results <- list()
z <- 0
periods <- sort(unique(DMAll$Period))
# p = periods[3]
for(p in periods){
  z <- z + 1
  DMAllSub <- DMAll %>% filter(Period == p)
  rs <- rowSums(DMAllSub[,4:ncol(DMAllSub)])
  DMAllSub <- DMAllSub %>% mutate(Total = rs)
  DMAllSub <- DMAllSub %>% filter(Total >=10)
  
  # keep only formal types
  Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
  Formal <- Ceramics %>% filter(Formal == "Y")
  Formal <- as.character(Formal$Ceramic_Type)
  DMAllSubinfo <- DMAllSub %>% dplyr::select(1:3)
  DMAllSub <- DMAllSub %>% dplyr::select(which(names(DMAllSub) %in% Formal))
  
  # only keep ceramic types with more than 50
  cs <- as.tibble(round(colSums(DMAllSub),0))
  csindx <- names(DMAllSub)[which(cs >= 50)]
  DMAllSub <- DMAllSub %>% dplyr::select(which(names(DMAllSub) %in% csindx))
  
  # load spatial data
  DMSF <- readRDS('Data/DataMasterSf.Rds')
  DMSP <- merge(DMAllSubinfo,DMSF, by = "ProjectNumber")
  DMC <- DMSP %>% dplyr::select(c("ProjectNumber","long","lat"))
  
  # change to wide format with coordinates
  rs <- rowSums(DMAllSub)
  DMAllSub <- round(DMAllSub,0)
  DMAllSub$ProjectNumber <- DMAllSubinfo$ProjectNumber
  DMLong <- melt(DMAllSub, id.vars = "ProjectNumber")
  DMLong <- DMLong %>% filter(value > 0)
  DMLong <- expandRows(DMLong, "value")
  DMLongSF <- merge(DMLong, DMC, by = "ProjectNumber")
  DMLongSF <- st_as_sf(DMLongSF, coords = c("long","lat"), remove = F, crs = 4326)
  
  crs1 <- "+init=epsg:4326" 
  crs2 <- "+init=epsg:26912" 
  DMSP <- st_transform(DMLongSF, 26912)
  DMSP <- as(DMSP,"Spatial")
  
  type.list <- unique(DMSP$variable)
  type.n <- length(type.list)
  samp <- list() 
  i=0
  # t = type.list[30]
  for (t in type.list) {
    i <- i+1
    finds <- DMSP[which(DMSP@data$variable==t),]
    ppp_w <-  ppp(finds@coords[,1], finds@coords[,2], window=win)  
    dens <- density(ppp_w, kernel="gaussian", sigma=sdev, dimyx=c(100,100), w=win,
                    edge=TRUE, at="pixels")
    sgdf_w_dens   <- as.SpatialGridDataFrame.im(dens)
    proj4string(sgdf_w_dens) <- DMSP@proj4string
    # plot(sgdf_w_dens, main = t)
    meg_dens_samp   <- over(s.points,  sgdf_w_dens)
    samp[[i]] <- meg_dens_samp
  }
  
  # str(samp)
  # samp            # list of Typenspektren
  
  ts <- data.frame(matrix(unlist(samp), nrow=length(samp), byrow=T))# points rows and types columns
  ts <- t(ts)
  colnames(ts) <- type.list
  ts[1:4,1:3]
  
  # convert to sf
  tsSF <- cbind.data.frame(ts,s.points@coords)
  tsSF <- st_as_sf(tsSF, coords = c("x1","x2"), crs = 26912)
  rownames(tsSF) <- 1:length(s.points)
  # saveRDS(tsSF,'Data/TypenspektrenSf.Rds')
  
  # # save plot so I can determine which points to use
  # source("R/GetBasePlot.R")
  # if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = F)
  # s.pointsG <- spTransform(s.points, CRS("+init=epsg:4326"))
  # plotdf <- as.data.frame(s.pointsG)
  # baseg +
  #   geom_point(data = DMSF, aes(long,lat), color = "cyan", size = .1) +
  #   geom_text(data = plotdf, aes(x1,x2), label = row.names(plotdf), size = .75)
  # 
  # ggsave('GeneralFigures/TypenspektrenPoints-rawcounts.pdf')

  # the following are determined by visual inspection of the above plot

  lineA <- tsSF[2608:2631,]
  lineB <- tsSF[2197:2242,]
  lineC <- tsSF[1733:1766,]
  lineD <- tsSF[seq(152,2748,59),]
  lineE <- tsSF[seq(399,2346,59),]
  
  plot(tsSF$geometry)
  plot(lineA$geometry, add = T, pch = 17, col = "red")
  plot(lineB$geometry, add = T, pch = 17, col = "green")
  plot(lineC$geometry, add = T, pch = 17, col = "blue")
  plot(lineD$geometry, add = T, pch = 17, col = "orange")
  plot(lineE$geometry, add = T, pch = 17, col = "magenta")
  
  # create list of all directions
  nm <- factor(c("lineA","lineB","lineC","lineD","lineE"),
               levels = c("lineA","lineB","lineC","lineD","lineE"))
  dircols <- c("red","green","blue","orange","Magenta")
  
  lineA$Direction <- nm[1]
  lineB$Direction <- nm[2]
  lineC$Direction <- nm[3]
  lineD$Direction <- nm[4]
  lineE$Direction <- nm[5]
  
  dirlist <- list()
  dirlist[[1]] <- lineA
  dirlist[[2]] <- lineB
  dirlist[[3]] <- lineC
  dirlist[[4]] <- lineD
  dirlist[[5]] <- lineE
  
  dd <- list()
  # i = 1
  for(i in 1:5){
    plotdf <- plotpts <- dirlist[[i]]
    st_geometry(plotdf) <- NULL
    n <- nrow(plotdf)
    x <- 1:n
    df.d <- data.frame(p1=rep(1,n),p2=x,d1=x,d2=x)
    j = 10
    for (j in x) {
      df.d[j,3] <- st_distance(plotpts[1,],plotpts[j,]) /1000
      df.d[j,4] <- cdist(as.numeric(plotdf[1,]),as.numeric(plotdf[j,]))
    }
    df.d$Direction <- nm[i]
    dd[[i]] <- df.d
  }
  dd <- bind_rows(dd)
  
  # get IDW data for comparison
  r <- readRDS(paste0('GIS/IDW-',p,'.Rds'))
  r <- projectRaster(r,crs = CRS("+init=epsg:26912"))
  allLines <- bind_rows(dirlist)
  c <- matrix(unlist(allLines$geometry), ncol = 2, byrow = T)
  allLines <- as.data.frame(allLines)
  allLines$geometry <- NULL
  tempSP <- SpatialPointsDataFrame(c, as.data.frame(c), proj4string = CRS(crs2))
  allLines$Extract <- extract(r,tempSP)
  allLines$Easting <- c[,1]
  allLines$Northing <- c[,2]
  tempSP <- spTransform(tempSP,crs1)
  allLines$long <- tempSP@coords[,1]
  allLines$lat <- tempSP@coords[,2]
  allLines <- st_as_sf(allLines, coords = c("long","lat"), crs = crs1, remove = F)
  allLines <- bind_cols(allLines,dd)
  allLines$Period <- p
  results[[z]] <- allLines
  
  # plot directions on base map
  DMSF <- readRDS('Data/DataMasterSf.Rds')
  source("R/GetBasePlot.R")
  if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = T)
  baseg +
    geom_point(data = DMSF, aes(long,lat), size = .1, color = "cyan")+
    geom_path(data = allLines, aes(long,lat, color = Direction), size = 3) +
    scale_color_manual(values = dircols, labels = 
                         c("Line A", "Line B", "Line C", "Line D", "Line E"))

  ggsave(filename = "GeneralFigures/Typenspektren-rawcounts-linesmapped.png",
         width = 6.5, height = 4.5, units = "in", dpi = 600)
  
  ##################################################### 
  
  # # Plot differences from "average point"
  # plotdf <- tsSF
  # st_geometry(plotdf) <- NULL
  # avgpt <- apply(plotdf,2,mean)
  # n <- nrow(plotdf) 
  # x <- 1:n 
  # df.d <- data.frame(p1=rep(1,n),p2=x,d1=x)
  # for (j in x) {
  #   df.d[j,3] <- cdist(avgpt,plotdf[j,])
  # }
  # cs <- as.tibble(st_coordinates(tsSF))
  # rdf <- as.tibble(cbind(cs,df.d$d1))
  # r <- rasterFromXYZ(rdf, crs = crs2)
  # r <- projectRaster(r, crs = crs1)
  # rdf <- fortify(r)
  # rdf <- rdf %>% filter(!is.na(band1))
  # baseg +
  #   geom_raster(data = rdf, aes(x,y, fill = band1), alpha = .5, interpolate = T) +
  #   scale_fill_gradientn(colours = c("white","cyan","darkblue", "firebrick"))
  # # plotly::ggplotly()
  # ggsave(filename = "GeneralFigures/TypenspektrenDistancefromAverageStandardizedHex.png",
  #        width = 6.5, height = 4, units = "in", dpi = 600)
  # 
  # # identity different hotspots values using buffer and calculate distance from center of these 
  # # points
  # areas <- tibble(area = c("Southeast Utah", "Central Mesa Verde", "Eastern Mesa Vesa","Chaco"),
  #                 long = c(-109.37027, -108.60123, -107.42981, -107.992029),
  #                 lat = c(37.52498, 37.38107, 37.00, 36.060793))
  # areaSF <- st_as_sf(areas,coords = c("long","lat"), crs = 4326)
  # areaSF <- st_transform(areaSF, 26912)
  # areabuffer <- st_buffer(areaSF, 12000)
  # cpoints <- tibble (id = 1:4, x = 1:4, y = 1:4)
  # for(i in 1:4){
  #   temp <- tsSF[areabuffer[i,],]
  #   id <- row.names(tsSF[areabuffer[i,],])
  #   temp <- temp[1,]
  #   cpoints$id[i] <- id[1]
  #   cpoints[i,2:3] <- st_coordinates(temp)
  # }
  # tsSFG <- st_transform(tsSF, 4326)
  # # plot(st_coordinates(tsSFG), col = "white")
  # # points(areas$long, areas$lat, pch = 19, col = "red")
  # # text(st_coordinates(tsSFG), label = row.names(tsSFG), cex = .4)
  # # visually identified closest points
  # cpoints <- cpoints$id
  # 
  # # plot differences from each center point identified
  # areaDens <- list()
  # # c = cpoints[3]
  # i = 0
  # for(c in cpoints){
  #   i = i + 1
  #   plotdf <- tsSF
  #   st_geometry(plotdf) <- NULL
  #   cdf <- plotdf[c,]
  #   n <- nrow(plotdf) 
  #   x <- 1:n 
  #   df.d <- data.frame(p1=rep(1,n),p2=x,d1=x)
  #   for (j in x) {
  #     df.d[j,3] <- cdist(cdf,plotdf[j,])
  #   }
  #   cs <- as.tibble(st_coordinates(tsSF))
  #   rdf <- as.tibble(cbind(cs,df.d$d1))
  #   r <- rasterFromXYZ(rdf, crs = crs2)
  #   r <- projectRaster(r, crs = crs1)
  #   rdf <- fortify(r)
  #   rdf <- rdf %>% filter(!is.na(band1))
  #   rdf$area <- areas$area[i]
  #   areaDens[[i]] <- rdf
  # }
  # # areaDens <- bind_rows(areaDens)
  # adf <- tsSFG[cpoints,]
  # adf$long <- st_coordinates(adf)[,1]
  # adf$lat <- st_coordinates(adf)[,2]
  # 
  # i = 1
  # for(i in 1:4){
  # pdf1 <- adf[i,]
  # df <- areaDens[[i]]
  # df <- df[which(df$band1 > summary(df$band1)[5]),]
  # pdf <- areas[i,]
  # source("R/GetBasePlot.R")
  # if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = T)
  # 
  # baseg +
  #   geom_raster(data = df, aes(x,y, fill = band1), alpha = .5, interpolate = T) +
  #   geom_point(data = pdf1, aes(long,lat), shape = 88, size = 5) +
  #   scale_fill_gradientn(colours = c("white","cyan","darkblue", "firebrick")) +
  #   ggtitle(paste(pdf$area,p))
  # ggsave(filename = paste0("GeneralFigures/TypenspektrenDistancefromAreasStandardizedHex",
  #                          pdf$area,"-",p,".png"),
  #        width = 6.5, height = 4, units = "in", dpi = 600)
  # }
  options(warn = 0)
}

# plot results

resultsDF <- bind_rows(results)
resultsDF <- as.data.frame(resultsDF)
lines <- unique(resultsDF$Direction)
linesNames <- c("Line A", "Line B", "Line C", "Line D", "Line E")

# l <- lines[1]
i = 0
for(l in as.character(lines)){
  i = i +1
  plotdf <- resultsDF[which(resultsDF$Direction == l),]
  ggplot(plotdf, aes(d1,d2, color = Period), size = 2) +
    geom_path(alpha = .5) +
    geom_point(size = .5) + scale_color_manual(values = dircols) +
    theme_light() + xlab("Spatial distance (km)") + ylab("Density distance") +
    theme(legend.position = "bottom") + ggtitle(linesNames[i])

  ggsave(filename = paste0("GeneralFigures/",l,"Typenspektren-rawcounts-sd-",sdev,".png"),
         width = 6.5, height = 3, units = "in", dpi = 600)
  
  ggplot(plotdf, aes(d1,Extract, color = Period), size = 2) +
    geom_path(alpha = .5) +
    geom_point(size = .5) + scale_color_manual(values = dircols) +
    theme_light() + xlab("Spatial distance (km)") + ylab("SJRW %") +
    theme(legend.position = "bottom") + ggtitle(linesNames[i])

  ggsave(filename = paste0("GeneralFigures/",l,"IDW-rawcounts.png"),
         width = 6.5, height = 3, units = "in", dpi = 600)
}


##################################################### 

# Plot differences from "average point"
plotdf <- tsSF
st_geometry(plotdf) <- NULL
avgpt <- apply(plotdf,2,mean)
n <- nrow(plotdf) 
x <- 1:n 
df.d <- data.frame(p1=rep(1,n),p2=x,d1=x)
for (j in x) {
  df.d[j,3] <- cdist(avgpt,plotdf[j,])
}
cs <- as.tibble(st_coordinates(tsSF))
rdf <- as.tibble(cbind(cs,df.d$d1))
r <- rasterFromXYZ(rdf, crs = crs2)
r <- projectRaster(r, crs = crs1)
rdf <- fortify(r)
rdf <- rdf %>% filter(!is.na(band1))
baseg +
  geom_raster(data = rdf, aes(x,y, fill = band1), alpha = .5, interpolate = T) +
  scale_fill_gradientn(colours = c("cyan","darkblue"))

ggsave(filename = "GeneralFigures/TypenspektrenDistancefromAverageStandardized.png",
  width = 6.5, height = 4, units = "in", dpi = 600)

##################################################### 
cpoint <- data.frame(id = "center",x = -108.7964, y = 36.77902)
source("R/GetBasePlot.R")
if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = T)
# baseg +  
#   geom_point(data = DMSF, aes(long,lat), size = .1, color = "cyan") +
#   geom_path(data = dirdf, aes(long,lat, color = Direction), size = 2, linetype = 'F1') +
#   geom_point(data = cpoint, aes(x,y), size = 6) 
# 
# ggsave(filename = "GeneralFigures/TypenspektrenalldirectionsMapped.png",
#   width = 6.5, height = 4.5, units = "in", dpi = 600)

##################################################### 

  # plot distributions
crs1 <- "+init=epsg:4326" 
crs2 <- "+init=epsg:26912" 
# sdf <- readRDS('Data/TypenspektrenSf.Rds')
sdf <- tsSF
cs <- as.tibble(st_coordinates(sdf))
st_geometry(sdf) <- NULL
rstack <- stack()
for(i in 1:ncol(sdf)){
    rDF <- cbind(cs,sdf[,i])
    names(rDF) <- c("x","y","band1")
    r <- rasterFromXYZ(rDF,crs = crs2)
    r <- projectRaster(r, crs = crs1)
    rstack <- stack(rstack,r)
}

source('R/getBasePlot.R')
if(!exists("baseg")) baseg <- getBasePlot(labels = F,scalebar = F)

type.list <- names(sdf)

for(i in 1:length(type.list)){
  rDF <- fortify(rstack[[i]])
  rDF <- rDF %>% filter(!is.na(band1))
  rDF <- rDF[rDF$band1 > 1.0e-8,]
  baseg +
      geom_raster(data = rDF,aes(x,y,fill = band1), alpha = .75, interpolate = T) +
      scale_fill_gradientn(colours = c("#ffffe0","orange","red")) +
      ggtitle(type.list[i]) +
      guides(fill = F) + 
      theme(plot.title = element_text(size = 9)) 
  ggsave(filename = paste0('GeneralFigures/TypeDensity/',type.list[i],"9km.png"),
                   dpi = 300, width = 3, height = 2.5, units = "in")
}

##################################################### 

cpoint <- data.frame(id = "center",x = -108.7964, y = 36.77902)
source("R/GetBasePlot.R")
if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = T)
baseg +  
  geom_point(data = DMSF, aes(long,lat), size = .1, color = "cyan") +
  geom_path(data = dirdf, aes(long,lat, color = Direction), size = 2, linetype = 'F1') +
  geom_point(data = cpoint, aes(x,y), size = 6) 

ggsave(filename = "GeneralFigures/TypenspektrenalldirectionsMapped.png",
  width = 6.5, height = 4.5, units = "in", dpi = 600)

##################################################### 

# # plot distributions
# rstack <- stack()
# for(i in 1:ncol(ts)){
#   rDF <- cbind(s.points@coords,ts[,i])
#   names(rDF) <- c("x","y","z")
#   r <- rasterFromXYZ(rDF,crs = s.points@proj4string)
#   rstack <- stack(rstack,r)
# }
# 
# for(n in 1:type.n){
#   png(filename = paste0('GeneralFigures/TypeDensity/',type.list[[n]],'.png'),
#     res = 300, width = 6.5, height = 6, units = 'in')
#     par(mai=c(.5,.5,0.5,.5))
#     plot(rstack[[n]], main = type.list[[n]])
#   dev.off()
# }

##################################################### 
cpoint <- data.frame(id = "center",x = -108.7964, y = 36.77902)
source("R/GetBasePlot.R")
if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = T)
baseg +  
  geom_point(data = DMSF, aes(long,lat), size = .1, color = "cyan") +
  geom_path(data = dirdf, aes(long,lat, color = Direction), size = 2, linetype = 'F1') +
  geom_point(data = cpoint, aes(x,y), size = 6) 

ggsave(filename = "GeneralFigures/TypenspektrenalldirectionsMapped.png",
  width = 6.5, height = 4.5, units = "in", dpi = 600)

##################################################### 

##################################################### 

  # plot distributions
crs1 <- "+init=epsg:4326" 
crs2 <- "+init=epsg:26912" 
# sdf <- readRDS('Data/TypenspektrenSf.Rds')
sdf <- tsSF
cs <- as.tibble(st_coordinates(sdf))
st_geometry(sdf) <- NULL
rstack <- stack()
for(i in 1:ncol(sdf)){
    rDF <- cbind(cs,sdf[,i])
    names(rDF) <- c("x","y","band1")
    r <- rasterFromXYZ(rDF,crs = crs2)
    r <- projectRaster(r, crs = crs1)
    rstack <- stack(rstack,r)
}

source('R/getBasePlot.R')
if(!exists("baseg")) baseg <- getBasePlot(labels = F,scalebar = F)

type.list <- names(sdf)

for(i in 1:length(type.list)){
  rDF <- fortify(rstack[[i]])
  rDF <- rDF %>% filter(!is.na(band1))
  rDF <- rDF[rDF$band1 > 1.0e-8,]
  baseg +
      geom_raster(data = rDF,aes(x,y,fill = band1), alpha = .75, interpolate = T) +
      scale_fill_gradientn(colours = c("#ffffe0","orange","red")) +
      ggtitle(type.list[i]) +
      guides(fill = F) + 
      theme(plot.title = element_text(size = 9)) 
  ggsave(filename = paste0('GeneralFigures/TypeDensity/',type.list[i],"9kmStandardized.png"),
                   dpi = 300, width = 3, height = 2.5, units = "in")
}

##############################################################################################

# create hexagon distributions showing the different types using simple mean
# load data
DMAll <- readRDS('Data/DataMasterAdjusted.Rds')

# DMAll <- DMAll %>% filter(Date %in% 949:999)
rs <- rowSums(DMAll[,4:ncol(DMAll)])
DMAll <- DMAll %>% mutate(Total = rs)
DMAll <- DMAll %>% filter(Total >=10)

# keep only formal types
Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
Formal <- Ceramics %>% filter(Formal == "Y")
Formal <- as.character(Formal$Ceramic_Type)
DMAllinfo <- DMAll %>% dplyr::select(1:3)
DMAll <- DMAll %>% dplyr::select(which(names(DMAll) %in% Formal))

# only keep ceramic types with more than 50
cs <- as.tibble(round(colSums(DMAll),0))
csindx <- names(DMAll)[which(cs >= 50)]
DMAll <- DMAll %>% dplyr::select(which(names(DMAll) %in% csindx))

# load spatial data
DMSF <- readRDS('Data/DataMasterSf.Rds')
DMSP <- merge(DMAllinfo,DMSF, by = "ProjectNumber")
DMC <- DMSP %>% dplyr::select(c("ProjectNumber","long","lat"))

# change to wide format with coordinates
rs <- rowSums(DMAll)
DMProps <- apply(DMAll, 2, function(x) round(x/rs*100,0))
DMProps[is.nan(DMProps)] <- 0
DMProps <- as.tibble(DMProps)
DMProps$ProjectNumber <- DMAllinfo$ProjectNumber
DMLong <- melt(DMProps, id.vars = "ProjectNumber")
DMLong <- DMLong %>% filter(value > 0)
DMSF <- merge(DMLong, DMC, by = "ProjectNumber")
DMSF <- as.tibble(DMSF)
DMSF <- st_as_sf(DMSF, coords = c("long","lat"), remove = F, crs = 4326)
hexgrid <- st_read('GIS/HexGrid9k.geojson')
hexgrid <- hexgrid %>% mutate( id = 1:nrow(hexgrid), mean = 0)
type.list <- sort(names(DMAll))
source('R/getBasePlot.R')
if(!exists("baseg")) baseg <- getBasePlot(labels = F,scalebar = F)

# i = 2
# j = 921
for(i in 1:length(type.list)){
  plotdf <- DMSF %>% filter(variable == type.list[i])
  hexgrid$mean <- 0
    for(j in 1:nrow(hexgrid)){
    g <- hexgrid[j,]
    df <- plotdf[g,]
    if(nrow(df) > 0){
      hexgrid$mean[j] <- mean(df$value)
    } 
  }
  plotSP <- as(hexgrid,"Spatial")
  plotSP <- fortify(plotSP)
  plotSP <- merge(plotSP, hexgrid, by = "id")
  plotSP <- arrange(plotSP, id, order)
  plotSP <- plotSP %>% filter(mean > 0)
  baseg +
    geom_polygon(data = plotSP, aes(long, lat, group = group, fill = mean), alpha = .75) +
    ggtitle(type.list[i]) +
    scale_fill_gradient2(low = "white", high = "blue")

  ggsave(paste0('GeneralFigures/SitesbyType/',type.list[i],'-hexgrid.png'), dpi = 600,
       width = 6.5, height = 6.5, units = "in")  
}

##########################################################################################

# use mean values for each type per hexagon to create equally spaced points (the centroids
# of the hexagons) to use for Typenspektren
# create hexagon distributions showing the different types using simple mean

# load data
set.seed(1010)
DMAll <- readRDS('Data/DataMasterAdjusted.Rds')
periods <- sort(unique(DMAll$Period))
hexgrid <- st_read('GIS/HexGrid9k.geojson')
hexgrid <- hexgrid %>% mutate( id = 1:nrow(hexgrid))
hexpoints <- st_centroid(hexgrid)
cdist <- function(a,b){sqrt(sum((a-b) ^ 2))} # function for distance
hexSP1 <- st_transform(hexgrid, 26912)
hexSP1 <- as(hexSP1,"Spatial")
bb <- bbox(hexSP1)
bb[1:2] <- bb[1:2] - 10000
bb[3:4] <- bb[3:4] + 10000
spext <- as(raster::extent(bb), "SpatialPolygons")
spext@proj4string <- hexSP1@proj4string
win <- owin(xrange=c(bb[1,1],bb[1,2]), yrange= c(bb[2,1],bb[2,2]), unitname="m")
s.points <- spsample(spext, 1000,  type="regular")

results <- list()
z <- 0
# p = periods[2]
for(p in periods){
  z <- z + 1
  DMAllSub <- DMAll %>% filter(Period == p)
  rs <- rowSums(DMAllSub[,4:ncol(DMAllSub)])
  DMAllSub <- DMAllSub %>% mutate(Total = rs)
  DMAllSub <- DMAllSub %>% filter(Total >=10)
  
  # keep only formal types
  Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
  Formal <- Ceramics %>% filter(Formal == "Y")
  Formal <- as.character(Formal$Ceramic_Type)
  DMAllSubinfo <- DMAllSub %>% dplyr::select(1:3)
  DMAllSub <- DMAllSub %>% dplyr::select(which(names(DMAllSub) %in% Formal))
  
  # only keep ceramic types with more than 50
  cs <- as.tibble(round(colSums(DMAllSub),0))
  csindx <- names(DMAllSub)[which(cs >= 50)]
  DMAllSub <- DMAllSub %>% dplyr::select(which(names(DMAllSub) %in% csindx))
  
  # load spatial data
  DMSF <- readRDS('Data/DataMasterSf.Rds')
  DMSP <- merge(DMAllSubinfo,DMSF, by = "ProjectNumber")
  DMC <- DMSP %>% dplyr::select(c("ProjectNumber","long","lat"))
  
  # change to wide format with coordinates
  rs <- rowSums(DMAllSub)
  DMProps <- apply(DMAllSub, 2, function(x) round(x/rs*100,0))
  DMProps[is.nan(DMProps)] <- 0
  DMProps <- as.tibble(DMProps)
  DMProps$ProjectNumber <- DMAllSubinfo$ProjectNumber
  DMProps <- merge(DMProps,DMC, by = "ProjectNumber")
  DMProps <- st_as_sf(DMProps, coords = c("long","lat"), crs = 4326)
  DMProps$ProjectNumber <- NULL
  hexTypesdf <- as.tibble(matrix(ncol = ncol(DMProps)-1,nrow = nrow(hexgrid)))
  names(hexTypesdf) <- names(DMProps)[1:ncol(DMProps)-1]
  options(warn = -1)
  # j = 921
  for(j in 1:nrow(hexTypesdf)){
    g <- hexgrid[j,]
    df <- DMProps[g,]
    dfM <- df
    st_geometry(dfM)  <- NULL
    dfM <- as.matrix(dfM) 
    hexTypesdf[j,] <- colMeans(dfM)
  } 
  is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
  
  hexTypesdf[is.nan(hexTypesdf)] <- 0
  hexTypesdf <- as.tibble(hexTypesdf)
  hexTypesdf$id <- hexpoints$id
  hexLong <- melt(hexTypesdf, id.vars = "id")
  hexLong <- hexLong %>% filter(value > 0)
  hexLong <- expandRows(hexLong, "value")
  hexSF <- merge(hexLong, hexpoints, by = "id")
  hexSF <- st_as_sf(hexSF)
  
  crs1 <- "+init=epsg:4326" 
  crs2 <- "+init=epsg:26912" 
  hexSP <- st_transform(hexSF, 26912)
  hexSP <- as(hexSP,"Spatial")
  
  type.list <- unique(hexSP$variable)
  type.n <- length(type.list)
  sdev <- 9000
  samp <- list() 
  i=0
  # t = type.list[30]
  for (t in type.list) {
    i <- i+1
    finds <- hexSP[which(hexSP@data$variable==t),]
    ppp_w <-  ppp(finds@coords[,1], finds@coords[,2], window=win)  
    dens <- density(ppp_w, kernel="gaussian", sigma=sdev, dimyx=c(100,100), w=win,
                    edge=TRUE, at="pixels")
    sgdf_w_dens   <- as.SpatialGridDataFrame.im(dens)
    proj4string(sgdf_w_dens) <- hexSP@proj4string
    # plot(sgdf_w_dens, main = t)
    meg_dens_samp   <- over(s.points,  sgdf_w_dens)
    samp[[i]] <- meg_dens_samp
  }
  
  # str(samp)
  # samp            # list of Typenspektren
  
  ts <- data.frame(matrix(unlist(samp), nrow=length(samp), byrow=T))# points rows and types columns
  ts <- t(ts)
  colnames(ts) <- type.list
  ts[1:4,1:3]
  
  # convert to sf
  tsSF <- cbind.data.frame(ts,s.points@coords)
  tsSF <- st_as_sf(tsSF, coords = c("x1","x2"), crs = 26912)
  rownames(tsSF) <- 1:length(s.points)
  # saveRDS(tsSF,'Data/TypenspektrenSf.Rds')
  
  # # save plot so I can determine which points to use
  # source("R/GetBasePlot.R")
  # if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = F)
  # s.pointsG <- spTransform(s.points, CRS("+init=epsg:4326"))
  # plotdf <- as.data.frame(s.pointsG)
  # baseg + 
  #   geom_point(data = DMSF, aes(long,lat), color = "cyan", size = .1) +
  #   geom_text(data = plotdf, aes(x1,x2), label = row.names(plotdf), size = 2.2) 
  # 
  # ggsave('GeneralFigures/TypenspektrenPoints.pdf')
  
  # the following are determined by visual inspection of the above plot

  lineA <- tsSF[764:779,]
  lineB <- tsSF[698:724,]
  lineC <- tsSF[573:591,]
  lineD <- tsSF[seq(151,844,33),]
  lineE <- tsSF[seq(156,750,33),]
  
  plot(tsSF$geometry)
  plot(lineA$geometry, add = T, pch = 17, col = "red")
  plot(lineB$geometry, add = T, pch = 17, col = "green")
  plot(lineC$geometry, add = T, pch = 17, col = "blue")
  plot(lineD$geometry, add = T, pch = 17, col = "orange")
  plot(lineE$geometry, add = T, pch = 17, col = "magenta")

  # plot above directions
  cdist <- function(a,b){sqrt(sum((a-b) ^ 2))}

  # create list of all directions
  nm <- factor(c("lineA","lineB","lineC","lineD","lineE"),
               levels = c("lineA","lineB","lineC","lineD","lineE"))
  dircols <- c("red","green","blue","orange","Magenta")

  lineA$Direction <- nm[1]
  lineB$Direction <- nm[2]
  lineC$Direction <- nm[3]
  lineD$Direction <- nm[4]
  lineE$Direction <- nm[5]

  dirlist <- list()
  dirlist[[1]] <- lineA
  dirlist[[2]] <- lineB
  dirlist[[3]] <- lineC
  dirlist[[4]] <- lineD
  dirlist[[5]] <- lineE

  dd <- list()
  i = 1
  for(i in 1:5){
    plotdf <- plotpts <- dirlist[[i]]
    st_geometry(plotdf) <- NULL
    n <- nrow(plotdf)
    x <- 1:n
    df.d <- data.frame(p1=rep(1,n),p2=x,d1=x,d2=x)
    j = 10
    for (j in x) {
      df.d[j,3] <- st_distance(plotpts[1,],plotpts[j,]) /1000
      df.d[j,4] <- cdist(as.numeric(plotdf[1,]),as.numeric(plotdf[j,]))
    }
    df.d$Direction <- nm[i]
    dd[[i]] <- df.d
  }
  dd <- bind_rows(dd)

  # get IDW data for comparison
  r <- readRDS(paste0('GIS/IDW-',p,'.Rds'))
  r <- projectRaster(r,crs = CRS("+init=epsg:26912"))
  allLines <- bind_rows(dirlist)
  c <- matrix(unlist(allLines$geometry), ncol = 2, byrow = T)
  allLines <- as.data.frame(allLines)
  allLines$geometry <- NULL
  tempSP <- SpatialPointsDataFrame(c, as.data.frame(c), proj4string = CRS(crs2))
  allLines$Extract <- extract(r,tempSP)
  allLines$Easting <- c[,1]
  allLines$Northing <- c[,2]
  tempSP <- spTransform(tempSP,crs1)
  allLines$long <- tempSP@coords[,1]
  allLines$lat <- tempSP@coords[,2]
  allLines <- st_as_sf(allLines, coords = c("long","lat"), crs = crs1, remove = F)
  allLines <- bind_cols(allLines,dd)
  allLines$Period <- p
  results[[z]] <- allLines
  # plot directions on base map

  # DMSF <- readRDS('Data/DataMasterSf.Rds')
  # source("R/GetBasePlot.R")
  # if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = T)
  # baseg +
  #   geom_point(data = DMSF, aes(long,lat), size = .1, color = "cyan")+
  #   geom_path(data = allLines, aes(long,lat, color = Direction), size = 3) +
  #   scale_color_manual(values = dircols)
  # 
  # ggsave(filename = "GeneralFigures/TypenspektrenLinesMappedHex.png",
  #        width = 6.5, height = 4.5, units = "in", dpi = 600)
  
  ##################################################### 
  
  # # Plot differences from "average point"
  # plotdf <- tsSF
  # st_geometry(plotdf) <- NULL
  # avgpt <- apply(plotdf,2,mean)
  # n <- nrow(plotdf) 
  # x <- 1:n 
  # df.d <- data.frame(p1=rep(1,n),p2=x,d1=x)
  # for (j in x) {
  #   df.d[j,3] <- cdist(avgpt,plotdf[j,])
  # }
  # cs <- as.tibble(st_coordinates(tsSF))
  # rdf <- as.tibble(cbind(cs,df.d$d1))
  # r <- rasterFromXYZ(rdf, crs = crs2)
  # r <- projectRaster(r, crs = crs1)
  # rdf <- fortify(r)
  # rdf <- rdf %>% filter(!is.na(band1))
  # baseg +
  #   geom_raster(data = rdf, aes(x,y, fill = band1), alpha = .5, interpolate = T) +
  #   scale_fill_gradientn(colours = c("white","cyan","darkblue", "firebrick"))
  # # plotly::ggplotly()
  # ggsave(filename = "GeneralFigures/TypenspektrenDistancefromAverageStandardizedHex.png",
  #        width = 6.5, height = 4, units = "in", dpi = 600)
  # 
  # # identity different hotspots values using buffer and calculate distance from center of these 
  # # points
  # areas <- tibble(area = c("Southeast Utah", "Central Mesa Verde", "Eastern Mesa Vesa","Chaco"),
  #                 long = c(-109.37027, -108.60123, -107.42981, -107.992029),
  #                 lat = c(37.52498, 37.38107, 37.00, 36.060793))
  # areaSF <- st_as_sf(areas,coords = c("long","lat"), crs = 4326)
  # areaSF <- st_transform(areaSF, 26912)
  # areabuffer <- st_buffer(areaSF, 12000)
  # cpoints <- tibble (id = 1:4, x = 1:4, y = 1:4)
  # for(i in 1:4){
  #   temp <- tsSF[areabuffer[i,],]
  #   id <- row.names(tsSF[areabuffer[i,],])
  #   temp <- temp[1,]
  #   cpoints$id[i] <- id[1]
  #   cpoints[i,2:3] <- st_coordinates(temp)
  # }
  # tsSFG <- st_transform(tsSF, 4326)
  # # plot(st_coordinates(tsSFG), col = "white")
  # # points(areas$long, areas$lat, pch = 19, col = "red")
  # # text(st_coordinates(tsSFG), label = row.names(tsSFG), cex = .4)
  # # visually identified closest points
  # cpoints <- cpoints$id
  # 
  # # plot differences from each center point identified
  # areaDens <- list()
  # # c = cpoints[3]
  # i = 0
  # for(c in cpoints){
  #   i = i + 1
  #   plotdf <- tsSF
  #   st_geometry(plotdf) <- NULL
  #   cdf <- plotdf[c,]
  #   n <- nrow(plotdf) 
  #   x <- 1:n 
  #   df.d <- data.frame(p1=rep(1,n),p2=x,d1=x)
  #   for (j in x) {
  #     df.d[j,3] <- cdist(cdf,plotdf[j,])
  #   }
  #   cs <- as.tibble(st_coordinates(tsSF))
  #   rdf <- as.tibble(cbind(cs,df.d$d1))
  #   r <- rasterFromXYZ(rdf, crs = crs2)
  #   r <- projectRaster(r, crs = crs1)
  #   rdf <- fortify(r)
  #   rdf <- rdf %>% filter(!is.na(band1))
  #   rdf$area <- areas$area[i]
  #   areaDens[[i]] <- rdf
  # }
  # # areaDens <- bind_rows(areaDens)
  # adf <- tsSFG[cpoints,]
  # adf$long <- st_coordinates(adf)[,1]
  # adf$lat <- st_coordinates(adf)[,2]
  # 
  # i = 1
  # for(i in 1:4){
  # pdf1 <- adf[i,]
  # df <- areaDens[[i]]
  # df <- df[which(df$band1 > summary(df$band1)[5]),]
  # pdf <- areas[i,]
  # source("R/GetBasePlot.R")
  # if(!exists('baseg')) baseg <- getBasePlot(labels = F, scalebar = T)
  # 
  # baseg +
  #   geom_raster(data = df, aes(x,y, fill = band1), alpha = .5, interpolate = T) +
  #   geom_point(data = pdf1, aes(long,lat), shape = 88, size = 5) +
  #   scale_fill_gradientn(colours = c("white","cyan","darkblue", "firebrick")) +
  #   ggtitle(paste(pdf$area,p))
  # ggsave(filename = paste0("GeneralFigures/TypenspektrenDistancefromAreasStandardizedHex",
  #                          pdf$area,"-",p,".png"),
  #        width = 6.5, height = 4, units = "in", dpi = 600)
  # }
  options(warn = 0)
}

# plot results

resultsDF <- bind_rows(results)
resultsDF <- as.data.frame(resultsDF)
lines <- unique(resultsDF$Direction)
linesNames <- c("Line A", "Line B", "Line C", "Line D", "Line E")

# l <- lines[1]
i = 0
for(l in as.character(lines)){
  i = i +1
  plotdf <- resultsDF[which(resultsDF$Direction == l),]
  ggplot(plotdf, aes(d1,d2, color = Period), size = 2) +
    geom_path(alpha = .5) +
    geom_point(size = .5) + scale_color_manual(values = dircols) +
    theme_light() + xlab("Spatial distance (km)") + ylab("Density distance") +
    theme(legend.position = "bottom") + ggtitle(linesNames[i])

  ggsave(filename = paste0("GeneralFigures/",l,"Typenspektren.png"),
         width = 6.5, height = 3, units = "in", dpi = 600)
  
  ggplot(plotdf, aes(d1,Extract, color = Period), size = 2) +
    geom_path(alpha = .5) +
    geom_point(size = .5) + scale_color_manual(values = dircols) +
    theme_light() + xlab("Spatial distance (km)") + ylab("SJRW %") +
    theme(legend.position = "bottom") + ggtitle(linesNames[i])
  
  ggsave(filename = paste0("GeneralFigures/",l,"IDW.png"),
                           width = 6.5, height = 3, units = "in", dpi = 600)
}

