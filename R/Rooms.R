#########################################################################################

# this script is designed to find single component sites from the CSNDB and VEP and
# then compare the number of rooms to the % of SJRW

rRJB::myLibrary(c('tidyverse'))

# load data
DataMaster <- readRDS("Data/DataMaster.Rds")
DMAll <- readRDS('Data/DataMasterAdjusted.Rds')
DMDated <- readRDS('Data/DataMasterDated.Rds')
DMDated <- DMDated %>% dplyr::select(ProjectNumber, Date)

# also remove all sites that have multiple components dating after AD 1000
sitesMultiple <- DataMaster %>% add_count(SiteID) %>% dplyr::select(ProjectNumber,SiteID,Sub,n)
sitesMultiple <- merge(sitesMultiple,DMDated, by = "ProjectNumber")
sitesMultiple <- dplyr::select(sitesMultiple,ProjectNumber,SiteID,Sub,n,Date)
sitesMultiple <- sitesMultiple %>% group_by(SiteID) %>% summarize(max = max(Date))
sitesMultiple <- sitesMultiple %>% filter(max < 1000)
siteID <- DataMaster %>% dplyr::select(ProjectNumber,SiteID) 
sitesMultiple <- merge(sitesMultiple,siteID, by = "SiteID")

# remove small assemblages
rs <- rowSums(DMAll[,4:ncol(DMAll)])
DMAll <- DMAll %>% mutate(Total = rs)
DMAll <- DMAll %>% filter(Total >=10, ProjectNumber %in% sitesMultiple$ProjectNumber)

# # function
# source('R/CeramicTypeandRangeFunctions.R')
# 
# dateRange <- tibble(ProjectNumber = NA, MaxStart = NA, MinEnd = NA, GAP = NA)
# for (pn in DMAll$ProjectNumber) {
#   df <- getCeramicRange(filter(DataMaster,ProjectNumber == pn))
#   dateRange <- rbind(dateRange,df)
#   if(is.na(dateRange[1,1])) dateRange <- dateRange[-1,]
#   dateRange
#   print(pn)
# }
#
# saveRDS(dateRange,'Data/DateRange.Rds')
# write.csv(dateRange,'Data/DataRange.csv', row.names = F)
dateRange <- readRDS('Data/DateRange.Rds')
hist(dateRange$GAP)
SingleOccupations <- dateRange %>% filter(GAP <= 0)
DMAll <- filter(DMAll,ProjectNumber %in% SingleOccupations$ProjectNumber)

# combine assemblages within a site
DMAll <- merge(DMAll, siteID, by = "ProjectNumber")
DMLong <- gather(DMAll, CeramicType, Total, -ProjectNumber, - Date, -Period, -SiteID, -Total)
DMLong <- DMLong %>% filter(Total > 0)
DMLong <- aggregate(Total~CeramicType + SiteID, data = DMLong, sum)
DMSpread <- DMLong %>% spread(CeramicType,Total)
DMSpread <- DMSpread %>% mutate_all(funs(replace(., is.na(.), 0)))
source('R/MCDFunction.R')
mcd <- NULL
for(nr in row.names(DMSpread)){
  mcd[nr] <- getMCD(x = DMSpread[nr,])
}
DMSpread$Date <- mcd

# # get ceramic density by year
# source('R/PeriodSherdDensityFunction.R')
# for (x in sample(SingleOccupations$ProjectNumber,100)){
#   PeriodSherdDensity(x, savePlot = T, plot = T)
# }
# visually determined there is no indication to believe that these are not single occupation
# sites

# Calc SJRW data
SJRW <- DMSpread %>% dplyr::select(`SAN JUAN RED WARE UNDIFFERENTIATED`,
                                   `ABAJO RED-ON-ORANGE`,
                                   `BLUFF BLACK-ON-RED`,
                                   `DEADMANS BLACK-ON-RED`) %>% mutate(Total = rowSums(.))
DMSpread$SJRWTotal <- SJRW$Total
DMSpread <- DMSpread %>% mutate(Total = rowSums(DMSpread[,2:68]), SJRWP = SJRWTotal/Total*100)
DMSpread$Period <- NA
DMSpread[which(DMSpread$Date %in% 750:799),"Period"] <- "750-800"
DMSpread[which(DMSpread$Date %in% 800:849),"Period"] <- "800-850"
DMSpread[which(DMSpread$Date %in% 850:899),"Period"] <- "850-900"
DMSpread[which(DMSpread$Date %in% 900:949),"Period"] <- "900-950"
DMSpread[which(DMSpread$Date %in% 950:999),"Period"] <- "950-1000"
DMSpread$Period <- factor(DMSpread$Period, levels = c("750-800","800-850","850-900","900-950","950-1000"))
DMSpread <- merge(DMSpread,siteID, by = "SiteID")
DMSpread <- DMSpread[!duplicated(DMSpread[,1:21]),]
# look at decorated percentages
Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
Decorated <- Ceramics %>% filter(Decoration %in% c("polychrome",
                                                   "bichrome",
                                                   "undifferentiated dec"))
Decorated <- as.character(Decorated$Ceramic_Type)
Decorated <- DMSpread %>% dplyr::select(which(names(DMSpread) %in% Decorated))
rs <- rowSums(Decorated)
DMSpread <- DMSpread %>% mutate(DecTotal = rs, SJRWDecP = SJRWTotal/DecTotal*100)

# load and combine room data
DM <- readRDS('Data/DataMaster.Rds')
DM <- dplyr::select(DM,1:7)
VEProoms <- readRDS("Data/VEPSites.Rds")
VEProoms <- VEProoms %>% dplyr::select(c("ProjectName","room estimate"))
names(VEProoms) <- c("SiteID","RoomEstimate")
VEProoms <- VEProoms %>% filter(SiteID %in% DM$SiteID)
DMVEP <- merge(DM,VEProoms, by = "SiteID")
DMVEP <- DMVEP %>% filter(ProjectNumber %in% SingleOccupations$ProjectNumber)
DMVEP <- DMVEP %>% filter(RoomEstimate > 0) %>% dplyr::select(2,8)
DMVEP <- plyr::ddply(DMVEP,"ProjectNumber",plyr::numcolwise(sum))
summary(DMVEP$RoomEstimate)
CSNrooms <- readRDS("Data/CDBSites.Rds")
CSNrooms <- CSNrooms %>% dplyr::select(c("SWSN_Site","Rooms"))
names(CSNrooms) <- c("SiteID","RoomEstimate")
CSNrooms <- CSNrooms %>% filter(SiteID %in% DM$SiteID)
DMCSN <- merge(DM,CSNrooms, by = "SiteID")
DMCSN <- DMCSN %>% filter(ProjectNumber %in% DMSpread$ProjectNumber) %>%
  dplyr::select(2,8)
summary(DMCSN$RoomEstimate)
DMRooms <- bind_rows(DMCSN,DMVEP)
DMRooms <- merge(DMRooms,DMSpread, by = "ProjectNumber")
DMP <- readRDS("Data/DataMasterPosterior.Rds")
DMP <- DMP %>% dplyr::select(ProjectNumber,priormean, Region)
DMRooms <- merge(DMRooms, DMP, by = "ProjectNumber")
DMRooms <- DMRooms %>% mutate(SJRWRegionDiff = SJRWP - priormean) 

table(DMRooms$Period)
table(DMRooms$Region)

# plot rooms against SJRWP
ggplot(DMRooms, aes(RoomEstimate, SJRWP,
                    label = paste(Region, ProjectNumber, SiteID), color = Period,
       shape = Region)) + 
  geom_point(size = 1.4) + 
  theme_bw() +
  xlab("Room Count") + ylab("SJRW %") +
  theme(legend.title = element_blank())

plotly::ggplotly
# ggsave(filename = paste0("GeneralFigures/RoomCounttoSJRWP.png"), dpi = 600,
#   width = 6.5, height = 3, units = 'in')

# plot rooms against SJRWP from region mean
ggplot(DMRooms, aes(RoomEstimate, SJRWRegionDiff,
                    label = paste(Region, ProjectNumber, SiteID), color = Period,
                    shape = Region)) + 
  geom_point(size = 1.4) + 
  theme_bw() +
  xlab("Room Count") + ylab("SJRW % from Region Mean") +
  theme(legend.title = element_blank())

ggsave(filename = paste0("GeneralFigures/RoomCounttoSJRWPfromRegion.png"), dpi = 600,
       width = 6.5, height = 3, units = 'in')

plotly::ggplotly(p = last_plot())

y = DMRooms %>% filter(RoomEstimate > 50)
summary(y$SJRWP)
summary(y$SJRWRegionDiff)
summary(y$SJRWDecP)

# plot rooms against decorated SJRW %
ggplot(DMRooms, aes(RoomEstimate, SJRWDecP,
                    label = paste(Region, ProjectNumber, SiteID), color = Period,
                    shape = Region)) + 
  geom_point(size = 1.4) + 
  theme_bw() +
  xlab("Room Count") + ylab("SJRW %") +
  theme(legend.title = element_blank())

# ggsave(filename = paste0("GeneralFigures/RoomCounttoSJRWDecP.png"), dpi = 600,
#        width = 6.5, height = 3, units = 'in')

# and last, look at SJRWP to Great Kivas
DM <- readRDS('Data/DataMaster.Rds')
DM <- dplyr::select(DM,1:7)
VEGK <- readRDS("Data/VEPSites.Rds")
VEGK <- VEGK %>% dplyr::select(c("ProjectName","Greatkiva"))
names(VEGK) <- c("SiteID","Greatkiva")
VEGK <- VEGK %>% filter(SiteID %in% DM$SiteID)
VEGK <- merge(DM,VEGK, by = "SiteID")
VEGK <- VEGK %>% filter(ProjectNumber %in% SingleOccupations$ProjectNumber)
VEGK <- VEGK %>% filter(Greatkiva > 0) %>% dplyr::select(2,8)
VEGK$Greatkiva <- "Y"
VEGK <- unique(VEGK)
DMGK <- merge(VEGK,DMP, by = "ProjectNumber")
DMGK <- merge(DMGK, siteID, by = "ProjectNumber")

# combine assemblages within a site
# !! Note !! I reran the DMAll section until the prior combine assemblage section
DMAll <- merge(DMAll, siteID, by = "ProjectNumber")
DMAll <- DMAll %>% filter(SiteID %in% DMGK$SiteID)
DMAll <- DMAll[-1,]
DMLong <- gather(DMAll, CeramicType, Total, -ProjectNumber, - Date, -Period, -SiteID, -Total)
DMLong <- DMLong %>% filter(Total > 0)
DMLong <- aggregate(Total~CeramicType + SiteID, data = DMLong, sum)
DMSpread <- DMLong %>% spread(CeramicType,Total)
DMSpread <- DMSpread %>% mutate_all(funs(replace(., is.na(.), 0)))
source('R/MCDFunction.R')
mcd <- NULL
for(nr in row.names(DMSpread)){
  mcd[nr] <- getMCD(x = DMSpread[nr,])
}
DMSpread$Date <- mcd

# # get ceramic density by year
# source('R/PeriodSherdDensityFunction.R')
# for (x in sample(SingleOccupations$ProjectNumber,100)){
#   PeriodSherdDensity(x, savePlot = T, plot = T)
# }
# visually determined there is no indication to believe that these are not single occupation
# sites

# Calc SJRW data
SJRW <- DMSpread %>% dplyr::select(`SAN JUAN RED WARE UNDIFFERENTIATED`,
                                   `ABAJO RED-ON-ORANGE`,
                                   `BLUFF BLACK-ON-RED`,
                                   `DEADMANS BLACK-ON-RED`) %>% mutate(Total = rowSums(.))
DMSpread$SJRWTotal <- SJRW$Total
DMSpread <- DMSpread %>% mutate(Total = rowSums(DMSpread[,2:16]), SJRWP = SJRWTotal/Total*100)
DMSpread$Period <- NA
DMSpread[which(DMSpread$Date %in% 750:799),"Period"] <- "AD 750"
DMSpread[which(DMSpread$Date %in% 800:849),"Period"] <- "AD 800"
DMSpread[which(DMSpread$Date %in% 850:899),"Period"] <- "AD 850"
DMSpread[which(DMSpread$Date %in% 900:949),"Period"] <- "AD 900"
DMSpread[which(DMSpread$Date %in% 950:999),"Period"] <- "AD 950"
DMSpread$Period <- factor(DMSpread$Period, levels = c("AD 750","AD 800","AD 850","AD 900","AD 950"))
DMSpread <- merge(DMSpread,siteID, by = "SiteID")
DMSpread <- DMSpread[!duplicated(DMSpread[,1:21]),]
DMP <- readRDS("Data/DataMasterPosterior.Rds")
DMP <- DMP %>% dplyr::select(ProjectNumber,priormean, Region, Period)
DMSpread$priormean <- 6.06
DMSpread$Region <- "Central Mesa Verde"
DMSpread <- DMSpread %>% mutate(SJRWRegionDiff = SJRWP - priormean) 
summary(DMSpread$SJRWRegionDiff)
summary(DMSpread$SJRWP)
table(DMSpread$Period)
table(DMSpread$Region)

# plot greatkivas against SJRWP
ggplot(DMSpread, aes(Period, SJRWRegionDiff,
                    label = paste(Region, ProjectNumber), color = Period)) + 
  geom_point(size = 1.4) + 
  theme_bw() +
  xlab("") + ylab("SJRW % from Region Mean") +
  theme(legend.title = element_blank(), legend.position = "bottom")

plotly::ggplotly()

# ggsave(filename = paste0("GeneralFigures/GreatKivatoSJRWP-no duplicates.png"), dpi = 600,
#        width = 6.5, height = 3, units = 'in')

# plotly::ggplotly(p = last_plot())

# look at decorated percentages
Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
Decorated <- Ceramics %>% filter(Decoration %in% c("polychrome",
                                                   "bichrome",
                                                   "undifferentiated dec"))
Decorated <- as.character(Decorated$Ceramic_Type)
Decorated <- DMSpread %>% dplyr::select(which(names(DMSpread) %in% Decorated))
rs <- rowSums(Decorated)
DMSpread <- DMSpread %>% mutate(DecTotal = rs, SJRWDecP = SJRWTotal/DecTotal*100)
DMGK <- DMSpread
