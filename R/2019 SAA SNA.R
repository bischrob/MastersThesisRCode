################################################################################################
# SNA Analysis (code from http://www.mattpeeples.net/netstats.html)
if(!require(pacman)){install.packages("pacman")}
p_load(statnet,tnet,tidyverse,sf, ggthemes, ggnetwork)

################################################################################################

# source functions 
source('R/SNAFunctions.R')

# load data
DMAll <- readRDS('Data/DataMasterAdjusted.Rds') %>% 
  select(-Date) %>% mutate_if(is.numeric,round,0)
SEUtahdf <- readRDS("data/SEUtahdf.Rds")
# SEUtahdf <- SEUtahdf %>% filter(!ProjectNumber %in% c(59,60))
SEUtahdf <- SEUtahdf %>% filter(DecoratedTotal >= 10)
DMAll <- DMAll %>% filter(ProjectNumber %in% SEUtahdf$ProjectNumber)

# keep only decorated wares
Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
Decorated <- Ceramics %>% filter(Decoration %in% c("polychrome",
                                                   "bichrome",
                                                   "undifferentiated dec"))
Decorated <- as.character(Decorated$Ceramic_Type)
DMAll <- DMAll %>% dplyr::select(c(1:2,which(names(DMAll) %in% Decorated)))

# color scales
colscale <- scale_color_gradientn(name = "San Juan \nRed Ware %",
                                  colors = rev(c("#6d1911", "#933016", "#b84a17",
                                                 "#dd6613", "#ff8500","#ffc38f"))) 


# Filter for period
BRnetDF <- NULL # hold results
BREdges <- NULL # hold results
BRstats <- NULL
BRnet <- list()
link = .75
# edgeList <- NULL # hold results
periods <- sort(unique(DMAll$Period))
p = periods[2]
for(p in periods){
  tmp <- getNetworks(p, df = SEUtahdf, DMAll = DMAll, link = link)
  BRnetDF <- rbind(BRnetDF,tmp$tmpNet)
  BREdges <- rbind(BREdges, tmp$tmpEdges)
  BRstats <- rbind(BRstats, tmp$tmpStats)
  BRnet[[p]] <- tmp$BRnet
}

# plot(BRnet$`950-1000`)

# remove empty edges
start <- paste(BRnetDF$xend, BRnetDF$yend,BRnetDF$Period)
end <- paste(BRnetDF$x, BRnetDF$y,BRnetDF$Period)
x <- setdiff(start,end)
match <- which(!start %in% x)
BRnetDF1 <- BRnetDF %>% slice(match) %>% 
  filter(!is.na(na.y))

# plot
ggplot() +
  geom_segment(data = BRnetDF1, aes(x = x, y = y, xend = xend, yend = yend),
               color = "gray") +
  geom_point(data = BRnetDF, aes(x,y, color = posteriormeanDec), size = 3) +
  colscale +
  theme_blank() +
  theme(legend.position = "bottom")  +
  facet_wrap(~Period)

ggsave(filename = nameFun("SAA2019Figures/Networks/",
                          "NetworkGraph",
                          "SE Utah",
                          link), dpi = 600,
       width = 10, height = 6, units = 'in')

source('R/getBasePlot.R')
if(!exists('baseg')) baseg <- getSEUtahbaseg()

baseg +
  geom_segment(data = BREdges,
               aes(x = long, y = lat, xend = long2, yend = lat2),
               color = "cyan") +
  geom_sf(data = SEUtahdf, aes(color = posteriormeanDec)) +
  colscale +
  coord_sf(xlim = c(-110.1,-108.9),
           ylim = c( 36.95,37.95)) +
  theme(legend.position = "bottom") +
  facet_wrap(~Period)

ggsave(filename = 
         nameFun("SAA2019Figures/Networks/",
                 "NetworkGraph",
                 "mapped", 
                 "SE Utah",
                 link),
       dpi = 600,
       width = 10, height = 6, units = 'in')

# plot stats against SJRWP

ggplot(BRstats, aes(eg.wt, posteriormeanDec)) +
  geom_point(color = "red") +
  theme_gdocs() +
  xlab("Eigenvector Centrality") +
  ylab("San Juan Red Ware %") +
  facet_wrap(~Period)

ggsave(nameFun("SAA2019Figures/Networks/",
               "Eigenvector",
               "Weighted", 
               "SE Utah",
               link), dpi = 600,
       width = 10, height = 5, units = 'in')

################################################################################################
pacman::p_load(statnet,tnet,tidyverse,sf, ggthemes, ggnetwork)
################################################################################################
# run again for all sites

# source functions 
source('R/SNAFunctions.R')

# load data
DMAll <- readRDS('Data/DataMasterAdjusted.Rds') %>% 
  select(-Date) %>% mutate_if(is.numeric,round,0)
DMP <- readRDS("data/DataMasterPosterior.Rds") %>% 
  mutate(posteriormeanDec = posteriormeanDec * 100)


# remove small assemblages and bad data - including Lowry Ruin which is after 1000
DMP <- DMP %>% filter(!ProjectNumber %in% c(59,60, 358), DecoratedTotal >= 10)
DMAll <- DMAll %>% filter(ProjectNumber %in% DMP$ProjectNumber)

# keep only decorated wares
Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
Decorated <- Ceramics %>% filter(Decoration %in% c("polychrome",
                                                   "bichrome",
                                                   "undifferentiated dec"))
Decorated <- as.character(Decorated$Ceramic_Type)
DMAll <- DMAll %>% dplyr::select(c(1:2,which(names(DMAll) %in% Decorated)))

# color scales
colscale <- scale_color_gradientn(name = "San Juan \nRed Ware %",
                                  colors = rev(c("#6d1911", "#933016", "#b84a17",
                                                 "#dd6613", "#ff8500","#ffc38f"))) 

# Filter for period
BRnetDF <- NULL # hold results
BREdges <- NULL # hold results
BRstats <- NULL
BRnet <- list()
link = .75
# edgeList <- NULL # hold results
periods <- sort(unique(DMAll$Period))
# p = periods[2]
for(p in periods){
  tmp <- getNetworks(p, df = DMP, DMAll = DMAll, link = link)
  BRnetDF <- rbind(BRnetDF,tmp$tmpNet)
  BREdges <- rbind(BREdges, tmp$tmpEdges)
  BRstats <- rbind(BRstats, tmp$tmpStats)
  BRnet[[p]] <- tmp$BRnet
}

# plot(BRnet$`950-1000`)

# remove empty edges
start <- paste(BRnetDF$xend, BRnetDF$yend,BRnetDF$Period)
end <- paste(BRnetDF$x, BRnetDF$y,BRnetDF$Period)
x <- setdiff(start,end)
match <- which(!start %in% x)
BRnetDF1 <- BRnetDF %>% slice(match) %>% 
  filter(!is.na(na.y))

# plot
ggplot() +
  geom_segment(data = BRnetDF1, aes(x = x, y = y, xend = xend, yend = yend),
               color = "gray") +
  geom_point(data = BRnetDF, aes(x,y, color = posteriormeanDec), size = 1.5) +
  colscale +
  theme_blank() +
  theme(legend.position = "bottom")  +
  facet_wrap(~Period)

ggsave(nameFun("SAA2019Figures/Networks/",
               "NetworkGraph",
               "AllSites",
               link), dpi = 600,
       width = 10, height = 6, units = 'in')

# plot
ggplot() +
  geom_segment(data = BRnetDF1, aes(x = x, y = y, xend = xend, yend = yend),
               color = "gray") +
  geom_point(data = BRnetDF, aes(x,y, color = Region), size = 1.5) +
  theme_blank() +
  theme(legend.position = "bottom")  +
  facet_wrap(~Period)

ggsave(nameFun("SAA2019Figures/Networks/",
               "NetworkGraph",
               "AllSites",
               "Region",
               link),
       dpi = 600,
       width = 10, height = 6, units = 'in')

source('R/getBasePlot.R')
if(!exists('baseg2')) baseg2 <- getSAA2019FullBaseMap()


baseg2 +
  geom_segment(data = BREdges,
               aes(x = long, y = lat, xend = long2, yend = lat2),
               color = "cyan", size = .1) +
  geom_sf(data = DMP, aes(color = posteriormeanDec)) +
  coord_sf(xlim = c(-110.6,-106.9),
           ylim = c(35.4,38)) +
  colscale +
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

ggsave(filename = nameFun("SAA2019Figures/Networks/",
                          "NetworkGraph",
                          "Mapped",
                          "AllSites",
                          link),
       dpi = 600,
       width = 10, height = 6, units = 'in')

# plot stats against SJRWP

ggplot(BRstats, aes(eg.wt, posteriormeanDec, color = Region)) +
  geom_point() +
  theme_gdocs() +
  xlab("Eigenvector Centrality") +
  ylab("San Juan Red Ware %") +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 1)) +
  facet_wrap(~Period)

ggsave(nameFun("SAA2019Figures/Networks/",
               "Eigenvector",
               "Weighted", 
               "All Sites",
               link), dpi = 600,
       width = 10, height = 5, units = 'in')


# plotdf <- BRnetDF %>%  
#   filter(Period == "950-1000")
# 
# # explore individual network plots
# # plot
# ggplot() +
#   geom_segment(data = plotdf, aes(x = x, y = y, xend = xend, yend = yend),
#                color = "gray") +
#   geom_point(data = BRnetDF,
#              aes(x,y, color = Region,
#                  label = paste(SiteID,"\n",ProjectNumber)),
#              size = 1.5) +
#   # colscale +
#   theme_blank() +
#   theme(legend.position = "bottom")
# plotly::ggplotly()
# 
# # geographic
# plotdf <- DMP %>%  
#   filter(Period == "950-1000", Region %in% c("Southeast Utah", "Greater Chaco"))
# plotdf2 <- BREdges %>% 
#   filter(Period == "950-1000", ProjectNumber %in% plotdf$ProjectNumber)
# 
# ggplot() +
# geom_segment(data = plotdf2,
#              aes(x = long, y = lat, xend = long2, yend = lat2),
#              color = "cyan", size = .1) +
#   geom_point(data = plotdf, aes(long,lat,color = Region,
#           label = paste(SiteID,"\n",ProjectNumber))) +
#   coord_fixed(xlim = c(-110.6,-106.9),
#            ylim = c(35.4,38))
# plotly::ggplotly()


################################################################################################
# SNA Analysis (code from http://www.mattpeeples.net/netstats.html)

pacman::p_load(statnet,tnet,tidyverse,sf, ggthemes, ggnetwork)

################################################################################################
# run again but with only formal types

# source functions 
source('R/SNAFunctions.R')

# load data
DMAll <- readRDS('Data/AdjustedCeramicsFormalTypes.Rds') %>% 
  mutate_if(is.numeric,round,0)
SEUtahdf <- readRDS("data/SEUtahdf.Rds")
DMAll <- DMAll %>% filter(ProjectNumber %in% SEUtahdf$ProjectNumber)
# 
# # filter unused types and assemblages less than 10
# DMAll <- DMAll %>%
#   select(c(1,2, which(colSums(DMAll[,3:ncol(DMAll)]) > 0) +2)) %>% 
#   slice(which(rowSums(DMAll[,3:ncol(DMAll)]) >= 10))

# color scales
colscale <- scale_color_gradientn(name = "San Juan \nRed Ware %",
                                  colors = rev(c("#6d1911", "#933016", "#b84a17",
                                                 "#dd6613", "#ff8500","#ffc38f"))) 

# Filter for period
BRnetDF <- NULL # hold results
BREdges <- NULL # hold results
BRstats <- NULL
BRnet <- list()
link = .75
# edgeList <- NULL # hold results
periods <- sort(unique(DMAll$Period))
# p = periods[2]
for(p in periods){
  tmp <- getNetworks(p, df = SEUtahdf, DMAll = DMAll, link = link)
  BRnetDF <- rbind(BRnetDF,tmp$tmpNet)
  BREdges <- rbind(BREdges, tmp$tmpEdges)
  BRstats <- rbind(BRstats, tmp$tmpStats)
  BRnet[[p]] <- tmp$BRnet
}

# plot(BRnet$`950-1000`)

# remove empty edges
start <- paste(BRnetDF$xend, BRnetDF$yend,BRnetDF$Period)
end <- paste(BRnetDF$x, BRnetDF$y,BRnetDF$Period)
x <- setdiff(start,end)
match <- which(!start %in% x)
BRnetDF1 <- BRnetDF %>% slice(match) %>% 
  filter(!is.na(na.y))

# plot
ggplot() +
  geom_segment(data = BRnetDF1, aes(x = x, y = y, xend = xend, yend = yend),
               color = "gray") +
  geom_point(data = BRnetDF, aes(x,y, color = posteriormeanDec), size = 3) +
  colscale +
  theme_blank() +
  theme(legend.position = "bottom")  +
  facet_wrap(~Period)

ggsave(nameFun("SAA2019Figures/Networks/",
               "NetworkGraph",
               "Formal", 
               "SE Utah",
               link),
       dpi = 600,
       width = 10, height = 6, units = 'in')

source('R/getBasePlot.R')
if(!exists('baseg')) baseg <- getSEUtahbaseg()

baseg +
  geom_segment(data = BREdges,
               aes(x = long, y = lat, xend = long2, yend = lat2),
               color = "cyan") +
  geom_sf(data = SEUtahdf, aes(color = posteriormeanDec)) +
  colscale +
  coord_sf(xlim = c(-110.1,-108.9),
           ylim = c( 36.95,37.95)) +
  theme(legend.position = "bottom") +
  facet_wrap(~Period)

ggsave(filename = nameFun("SAA2019Figures/Networks/",
                          "NetworkGraph",
                          "Mapped",
                          "Formal", 
                          "SE Utah",
                          link),
       dpi = 600,
       width = 10, height = 6, units = 'in')

# plot stats against SJRWP

ggplot(BRstats, aes(eg.wt, posteriormeanDec)) +
  geom_point(color = "red") +
  theme_gdocs() +
  xlab("Eigenvector Centrality") +
  ylab("San Juan Red Ware %") +
  facet_wrap(~Period)

ggsave(nameFun("SAA2019Figures/Networks/",
               "Eigenvector",
               "Weighted", 
               "Formal",
               "SE Utah",
               link), dpi = 600,
       width = 10, height = 5, units = 'in')

################################################################################################
pacman::p_load(statnet,tnet,tidyverse,sf, ggthemes, ggnetwork)
################################################################################################
# run again but with only formal types

# source functions 
source('R/SNAFunctions.R')

# load data
DMAll <- readRDS('Data/AdjustedCeramicsFormalTypes.Rds') %>% 
  mutate_if(is.numeric,round,0)
DMP <- readRDS("data/DataMasterPosterior.Rds") %>% 
  mutate(posteriormeanDec = posteriormeanDec * 100)


# remove small assemblages and bad data
DMP <- DMP %>% filter(!ProjectNumber %in% c(59,60, 358), DecoratedTotal >= 10)
DMAll <- DMAll %>% filter(ProjectNumber %in% DMP$ProjectNumber)

# filter unused types and assemblages less than 10
DMAll <- DMAll %>%
  select(c(1,2, which(colSums(DMAll[,3:ncol(DMAll)]) > 0) +2)) %>% 
  slice(which(rowSums(DMAll[,3:ncol(DMAll)]) >= 10))

# color scales
colscale <- scale_color_gradientn(name = "San Juan \nRed Ware %",
                                  colors = rev(c("#6d1911", "#933016", "#b84a17",
                                                 "#dd6613", "#ff8500","#ffc38f"))) 

# Filter for period
BRnetDF <- NULL # hold results
BREdges <- NULL # hold results
BRstats <- NULL
BRnet <- list()
link = .75
# edgeList <- NULL # hold results
periods <- sort(unique(DMAll$Period))
# p = periods[3]
for(p in periods){
  tmp <- getNetworks(p, df = DMP, DMAll = DMAll, link = link)
  BRnetDF <- rbind(BRnetDF,tmp$tmpNet)
  BREdges <- rbind(BREdges, tmp$tmpEdges)
  BRstats <- rbind(BRstats, tmp$tmpStats)
  BRnet[[p]] <- tmp$BRnet
}

# plot(BRnet$`950-1000`)

# remove empty edges
start <- paste(BRnetDF$xend, BRnetDF$yend,BRnetDF$Period)
end <- paste(BRnetDF$x, BRnetDF$y,BRnetDF$Period)
x <- setdiff(start,end)
match <- which(!start %in% x)
BRnetDF1 <- BRnetDF %>% slice(match) %>% 
  filter(!is.na(na.y))


# plot
ggplot() +
  geom_segment(data = BRnetDF1, aes(x = x, y = y, xend = xend, yend = yend),
               color = "gray") +
  geom_point(data = BRnetDF, aes(x,y, color = posteriormeanDec), size = 1.5) +
  colscale +
  theme_blank() +
  theme(legend.position = "bottom")  +
  facet_wrap(~Period)

ggsave(nameFun("SAA2019Figures/Networks/",
               "NetworkGraph",
               "Formal",
               "AllSites",
               link), 
       dpi = 600,
       width = 10, height = 6, units = 'in')

# plot
ggplot() +
  geom_segment(data = BRnetDF1, aes(x = x, y = y, xend = xend, yend = yend),
               color = "gray") +
  geom_point(data = BRnetDF, aes(x,y, color = Region), size = 1.5) +
  theme_blank() +
  theme(legend.position = "bottom")  +
  facet_wrap(~Period)

ggsave(nameFun("SAA2019Figures/Networks/",
               "NetworkGraph",
               "Formal",
               "AllSites",
               "Region",
               link),
       dpi = 600,
       width = 10, height = 6, units = 'in')

source('R/getBasePlot.R')
if(!exists('baseg2')) baseg2 <- getSAA2019FullBaseMap()


gmap <- baseg2 +
  geom_segment(data = BREdges,
               aes(x = long, y = lat, xend = long2, yend = lat2),
               color = "cyan", size = .1) +
  geom_sf(data = DMP, aes(color = posteriormeanDec)) +
  coord_sf(xlim = c(-110.6,-106.9),
           ylim = c(35.4,38)) +
  colscale +
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

ggsave(plot = gmap, 
       filename = nameFun("SAA2019Figures/Networks/",
                          "Mapped",
                          "Formal",
                          "AllSites",
                          link),
       dpi = 600,
       width = 10, height = 6, units = 'in')

# plot stats against SJRWP

ggplot(BRstats, aes(eg.wt, posteriormeanDec, color = Region)) +
  geom_point() +
  theme_gdocs() +
  xlab("Eigenvector Centrality") +
  ylab("San Juan Red Ware %") +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 1)) +
  facet_wrap(~Period)

ggsave(nameFun("SAA2019Figures/Networks/",
               "Eigenvector",
               "Weighted", 
               "Formal",
               "All Sites",
               link), dpi = 600,
       width = 10, height = 5, units = 'in')


# # explore individual network plots
# plotdf <- BRnetDF %>%  
#   filter(Period == "950-1000")
# 
# # plot
# ggplot() +
#   geom_segment(data = plotdf, aes(x = x, y = y, xend = xend, yend = yend),
#                color = "gray") +
#   geom_point(data = BRnetDF,
#              aes(x,y, color = Region,
#                  label = paste(SiteID,"\n",ProjectNumber)),
#              size = 1.5) +
#   # colscale +
#   theme_blank() +
#   theme(legend.position = "bottom")
# plotly::ggplotly()
# 
# # geographic
# p <- "950-1000"
# plotdf <- DMP %>%  
#   filter(Period == p)
# # filter(Period == p, !ProjectNumber == 358)
# plotdf2 <- BREdges %>% 
#   filter(Period == p, ProjectNumber %in% plotdf$ProjectNumber,
#          ProjectNumber2 %in% plotdf$ProjectNumber)
# 
# g <- ggplot() +
#   geom_segment(data = plotdf2,
#                aes(x = long, y = lat, xend = long2, yend = lat2),
#                color = "cyan", size = .1) +
#   geom_point(data = plotdf, aes(long,lat,color = posteriormeanDec,
#                                 label = paste("SiteID:", SiteID,
#                                               "</br>", "ProjectNumber:",ProjectNumber,
#                                               "</br>", "posteriormeanDec:",posteriormeanDec))) +
#   coord_fixed(xlim = c(-110.6,-106.9),
#               ylim = c(35.4,38))
# plotly::ggplotly(g)

