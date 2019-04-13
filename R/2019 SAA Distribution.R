#################################################################################
#' Get San Juan Red Ware distribution for Southeastern Utah
#' Analysis for 2019 SAA presentation
if(!require(pacman)){install.packages("pacman")}
p_load(tidyverse,ggspatial,sf)

# use EPSG 4326 for all projections
crs1 <- st_crs(4326)

# # Load data
# sitesMaster <- readRDS("Data/DataMasterPosterior.Rds")
# sitesMaster <- st_sf(sitesMaster, geometry = sitesMaster$geometry)
# sitesMaster <- st_transform(sitesMaster, crs1)
# sitesMaster <- sitesMaster %>%
#   mutate(posteriormean = round(posteriormean * 100,1),
#          posteriormeanDec = round(posteriormeanDec * 100,1))
# regionsAdj <- readRDS('GIS/regionsAdj.Rds')
# regionsAdj <- st_as_sf(regionsAdj)
# regionsAdj <- st_transform(regionsAdj, crs1)
# SEUtah <- regionsAdj %>% filter(Name == "Southeast Utah")
# SEUtahdf <- sitesMaster[SEUtah,]
# SEUtahdf <- SEUtahdf %>% filter(DecoratedTotal > 5)
# SEUtahdf <- SEUtahdf %>% filter(!ProjectNumber %in% c(59,60))  # remove bad data
# saveRDS(SEUtahdf, "Data/SEUtahdf.Rds")
# boundary <- readRDS("GIS/BorderEnlarged.Rds")
# boundary <- st_transform(boundary, crs1)
SEUtahdf <- readRDS("Data/SEUtahdf.Rds")
SEUtahdf <- SEUtahdf %>% filter(DecoratedTotal >= 10)

# SEUtahdf <- SEUtahdf %>% filter(!ProjectNumber %in% c(59,60))

periods <- sort(unique(SEUtahdf$Period))

# create more uniform bounding box for SE Utah
boundarySEU <- st_as_sfc(st_bbox(SEUtahdf))
boundarySEU <- st_transform(boundarySEU, crs1)
boundarySEU <- st_buffer(boundarySEU, .1)
boundaryBigSEU <- st_buffer(boundarySEU, 1)

#Create a custom color scale
grp <- levels(as.factor(SEUtahdf$Ceramic_Type))
myColors <- c("orange","red","cyan","darkred","darkgray","black","white")
names(myColors) <- grp
fillScaleCeramics <- scale_fill_manual(name = "grp",values = myColors)

fillscale <- scale_fill_gradientn(name = "San Juan \nRed Ware %",
                                  colors = rev(c("#6d1911", "#933016", "#b84a17",
                                                 "#dd6613", "#ff8500","white"))) 

# fillscale <-  scale_fill_gradientn(limits = c(0,100),
#                                     colors = c("white","orange","red","red","red"),
#                                     breaks= seq(0,100,25)) 


# get basemap
source('R/getBasePlot.R')
g <- getSEUtahbaseg()

# variable <- "posteriormean"
variable <- "posteriormeanDec"
contourlevel <- 10
source("R/getIDW.R")
# p = periods[3]
r <- NULL
cl <- NULL
for(p in periods){
  sites <- SEUtahdf %>% filter(Period == p)
  
  # Get IDW object then clip to boundary
  tmp <- getIDW(sf = sites, boundary = boundaryBigSEU, variable = variable,
                pwr = 3, myWidth = 36000, crsP = 26912, useEmptyPoints = T,
                contourlevel = contourlevel, filterLow = 5, p = p)
  
  r <- bind_rows(r,tmp$r)
  cl <- rbind(cl,tmp$cl)
}

# plot
g1 <- g +
  geom_sf(data = SEUtahdf, color = "black", size = .5) +
  geom_raster(data = r,aes(x,y, fill = band1), alpha = .6, interpolate = F) +
  geom_sf(data = cl) +
  fillscale +
  coord_sf(xlim = c(-110.1,-108.9),
           ylim = c( 36.95,37.95)) +
  theme(legend.position = "bottom") +
  labs(caption = paste0(contourlevel,"% San Juan Red Ware contour intervals")) +
    guides(fill = guide_legend("San Juan \nRed Ware %")) +
  facet_wrap(~Period)
ggsave(plot = g1,
       'SAA2019Figures/Distribution/IDW-FacetWrap.png', dpi = 600,
       width = 10, height = 6, units = 'in')


#' Hexagon distributions

# load data
periodDiversity <- readRDS("Data/PeriodDiversity.Rds")
periodDiversity <- periodDiversity %>% dplyr::select(ProjectNumber,Diversity)
SEUtahdf <- merge(SEUtahdf,periodDiversity, by = "ProjectNumber")

# Create hex grid
# set.seed(1010)
# size <- 9000
# boundary <- st_transform(boundaryBigSEU, 26912)
# hexGrid <- st_make_grid(boundary, cellsize = size, square = FALSE)
# saveRDS(hexGrid, "Data/hexGrid.Rds")
hexGrid <- readRDS("Data/hexGrid.Rds")

# variable = "posteriormean"
variable = "posteriormeanDec"
# Totalval = "Total"
Totalval = "DecoratedTotal"

# test out facet wrap
SEUtahdf <- st_transform(SEUtahdf, 26912)
bt <- binom.test(x = round(sum(SEUtahdf$SJRWTotal),0), 
                 n = round(sum(as.data.frame(SEUtahdf)[,Totalval]),0), 
                 p = SEUtahdf$priormean[1]/100, alternative = "t",
                 conf.level = .999)
ConfInts <- tibble(mean = bt$estimate*100,
                   cLow = bt$conf.int[1]*100,
                   cHigh = bt$conf.int[2]*100)
df <- tibble(id = 1:length(hexGrid), 
             cLow = ConfInts$cLow,
             cHigh = ConfInts$cHigh,
             geometry = hexGrid)
hexGriddf <- st_as_sf(df)
hexGriddf <- st_join(hexGriddf,SEUtahdf) %>% filter(!is.na(ProjectNumber))
hexGrid1 <- hexGriddf %>%
  group_by(id, Period) %>%
  mutate(wm = weighted.mean(get(variable),get(Totalval)),
         sd = n(),
         diversity = mean(Diversity),
         confval = case_when(wm > cHigh ~ "High",
                             wm < cLow ~ "Low",
                             TRUE ~ "Expected")) %>%
  distinct(id,Period,wm,sd,diversity,confval,geometry)

# plot weighted means
g2 <- g +
  geom_sf(data = hexGrid1, aes(fill = wm), alpha = .75, color = "darkgray") +
  fillscale +
  coord_sf(xlim = c(-110.1,-108.9),
           ylim = c( 36.95,37.95)) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10)) +
  facet_wrap(~Period)

ggsave(plot = g2, 'SAA2019Figures/Distribution/Hex-FacetWrap.png', dpi = 600,
       width = 10, height = 6, units = 'in')

# # plot most popular ceramic types
# hexGrid2 <- hexGriddf %>% 
#   group_by(id, Period, Ceramic_Type) %>% 
#   mutate(count = n()) %>%
#   group_by(id, Period) %>% 
#   mutate(Max = max(count)) %>% 
#   group_by(id, Period) %>% 
#   filter(count == Max) %>% 
#   distinct(id,Ceramic_Type,geometry) 
# 
# g3 <- g + 
#   geom_sf(data = hexGrid2, aes(fill = Ceramic_Type), alpha = .75, color = "darkgray") +
#   coord_sf(xlim = c(-110.1,-108.9),
#            ylim = c( 36.95,37.95)) +
#   theme(legend.position = "bottom",
#         plot.title = element_text(size = 10)) +
#   fillScaleCeramics + 
#   facet_wrap(~Period)
# 
# ggsave(plot = g3,
#        'SAA2019Figures/Distribution/Hex-FacetWrap-CommonCeramicType.png', dpi = 600,
#        width = 10, height = 6, units = 'in')


#################################################################################
#' Get San Juan Red Ware distribution for full area
#' Analysis for 2019 SAA presentation

pacman::p_load(tidyverse,ggspatial,sf)

# use EPSG 4326 for all projections
crs1 <- st_crs(4326)

# Load data
DMP <- readRDS("Data/DMPC.Rds") %>% 
  mutate(posteriormeanDec = posteriormeanDec * 100)

periods <- sort(unique(DMP$Period))

# boundary
boundary <- readRDS("GIS/BorderEnlarged.Rds")
boundary <- st_transform(boundary, crs1)

#Create a custom color scale
grp <- levels(as.factor(DMP$Ceramic_Type))
myColors <- read_csv("Data/CeramicTypeColors.csv")
myColors <- myColors$Hex
names(myColors) <- grp
fillScaleCeramics <- scale_fill_manual(name = "grp",values = myColors)

# get basemap
source('R/getBasePlot.R')
if(!exists('baseg2')) baseg2 <- getSAA2019FullBaseMap()

# variable <- "posteriormean"
variable <- "posteriormeanDec"
contourlevel <- 10
source("R/getIDW.R")
# p = periods[3]
r <- NULL
cl <- NULL
for(p in periods){
  sites <- DMP %>% filter(Period == p)
  
  # Get IDW object then clip to boundary
  tmp <- getIDW(sf = sites, boundary = boundary, variable = variable,
                pwr = 3, myWidth = 36000, crsP = 26912, useEmptyPoints = T,
                contourlevel = contourlevel, filterLow = 5, p = p)
  
  r <- bind_rows(r,tmp$r)
  cl <- rbind(cl,tmp$cl)
}

# plot
g1 <-  baseg2 +
  geom_sf(data = DMP, color = "black", size = .5) +
  geom_raster(data = r,aes(x,y, fill = band1), alpha = .6, interpolate = F) +
  geom_sf(data = cl) +
  coord_sf(xlim = c(-110.6,-106.9),
           ylim = c(35.4,38)) +
  fillscale +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_line(color = NA),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"),
        panel.border = element_rect(colour = "#373737", fill=NA, size=2),
        legend.position = "bottom") +
  labs(caption = paste0(contourlevel,"% San Juan Red Ware contour intervals")) +
  facet_wrap(~Period)

ggsave(plot = g1,
       'SAA2019Figures/Distribution/IDW-FacetWrap-FullSites.png', dpi = 600,
       width = 10, height = 6, units = 'in')


#' Hexagon distributions

# load data
periodDiversity <- readRDS("Data/PeriodDiversity.Rds")
periodDiversity <- periodDiversity %>% dplyr::select(ProjectNumber,Diversity)
DMP <- merge(DMP,periodDiversity, by = "ProjectNumber")

# Create hex grid
# set.seed(1010)
# size <- 9000
# boundary <- st_transform(boundaryBigSEU, 26912)
# hexGrid <- st_make_grid(boundary, cellsize = size, square = FALSE)
# saveRDS(hexGrid, "Data/hexGrid.Rds")
hexGrid <- readRDS("Data/hexGridSF.Rds")

# variable = "posteriormean"
variable = "posteriormeanDec"
# Totalval = "Total"
Totalval = "DecoratedTotal"

# test out facet wrap
DMP <- st_transform(DMP, 26912)
bt <- binom.test(x = round(sum(DMP$SJRWTotal),0), 
                 n = round(sum(as.data.frame(DMP)[,Totalval]),0), 
                 p = DMP$priormean[1]/100, alternative = "t",
                 conf.level = .999)
ConfInts <- tibble(mean = bt$estimate*100,
                   cLow = bt$conf.int[1]*100,
                   cHigh = bt$conf.int[2]*100)
df <- tibble(id = 1:nrow(hexGrid), 
             cLow = ConfInts$cLow,
             cHigh = ConfInts$cHigh,
             geometry = hexGrid$geometry)
hexGriddf <- st_as_sf(df)
hexGriddf <- st_join(hexGriddf,DMP) %>% filter(!is.na(ProjectNumber))
hexGrid1 <- hexGriddf %>%
  group_by(id, Period, Region) %>%
  mutate(wm = weighted.mean(get(variable),get(Totalval)),
         sd = n(),
         diversity = mean(Diversity),
         confval = case_when(wm > cHigh ~ "High",
                             wm < cLow ~ "Low",
                             TRUE ~ "Expected")) %>%
  distinct(id,Period,wm,sd,diversity,confval,geometry)

# plot weighted means
g2 <- baseg2 +
  geom_sf(data = hexGrid1, aes(fill = wm), alpha = .75, color = "darkgray") +
  coord_sf(xlim = c(-110.6,-106.9),
           ylim = c(35.4,38)) +
  fillscale +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_line(color = NA),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"),
        panel.border = element_rect(colour = "#373737", fill=NA, size=2),
        legend.position = "bottom") +
  facet_wrap(~Period)
g2
ggsave(plot = g2, 'SAA2019Figures/Distribution/Hex-FacetWrap-FullSites.png', dpi = 600,
       width = 10, height = 6, units = 'in')

# # plot most popular ceramic types
# hexGrid2 <- hexGriddf %>% 
#   group_by(id, Period, Ceramic_Type) %>% 
#   mutate(count = n()) %>%
#   group_by(id) %>% 
#   mutate(Max = max(count)) %>% 
#   group_by(id, Period) %>% 
#   filter(count == Max) %>% 
#   distinct(id,Ceramic_Type,geometry, Region) 
# 
# g3 <- baseg2 + 
#   geom_sf(data = hexGrid2, aes(fill = Ceramic_Type), alpha = .75, color = "darkgray") +
#   coord_sf(xlim = c(-110.6,-106.9),
#            ylim = c(35.4,38)) +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         panel.grid.major = element_line(color = NA),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.ticks = element_blank(),
#         plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"),
#         panel.border = element_rect(colour = "#373737", fill=NA, size=2),
#         legend.position = "bottom") +
#   theme(legend.position = "bottom",
#         plot.title = element_text(size = 10)) +
#   fillScaleCeramics +
#   facet_wrap(~Period)
# 
# ggsave(plot = g3,
#        'SAA2019Figures/Distribution/Hex-FacetWrap-CommonCeramicType-FullSites.png', dpi = 600,
#        width = 10, height = 6, units = 'in')
