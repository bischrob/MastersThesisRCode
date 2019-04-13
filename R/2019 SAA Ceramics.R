##########################################################################
# Look at which decorated ceramics are the most common in SE Utah
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(tidyverse, janitor, formattable, rio, ggthemes, sf)

# load data
# DM <- readRDS('Data/DataMaster.Rds') # with unadjusted numbers that may
# contain later ceramics
# need to change 1:3 to 1:7 later
DM <- readRDS('Data/DataMasterAdjusted.Rds') # filtered out later ceramics
# remove Cave Canyon as the ceramic data is still in Forsyth's typology
# CaveCanyon <- DM %>% filter(`RINCON RED-ON-BLACK` > 0) %>%
#   gather(ceramictypes,totals,-ProjectNumber,
#          -SiteID,-Sub, -Zone,-EASTING,-NORTHING,-Project) %>%
#   arrange(desc(totals))
# DM <- DM %>% filter(!ProjectNumber %in% c(59,60))

# filter for SE Utah data
DMP <- readRDS("Data/SEUtahdf.Rds")
DMP <- DMP %>% filter(DecoratedTotal >= 10)
DMCeramics <- DM %>% filter(ProjectNumber %in% DMP$ProjectNumber) %>% 
  adorn_totals("row")

DMCeramics <- DMCeramics %>% dplyr::select(c(1:3,which(DMCeramics[nrow(DMCeramics),] > 0))) %>% 
  filter(!ProjectNumber == "Total")

# keep only decorated wares
Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
Decorated <- Ceramics %>% filter(Decoration %in% c("polychrome",
                                                   "bichrome",
                                                   "undifferentiated dec"))
Decorated <- as.character(Decorated$Ceramic_Type)
DMCeramics <- DMCeramics %>% 
  dplyr::select(1:3, which(names(DMCeramics) %in% Decorated))

# # add period data # not needed for adjusted data
# DMAdjusted <- readRDS('Data/DataMasterAdjusted.Rds')
# DMAdjusted <- DMAdjusted %>% dplyr::select(ProjectNumber,Period)
# DMCeramics <- left_join(DMCeramics, DMAdjusted, by = "ProjectNumber")

# convert to long
CeramicsbyPeriod <- DMCeramics %>%  dplyr::select(-ProjectNumber, -Date) %>% 
  gather(Ceramic_Type, Total, -Period) %>% group_by(Period, Ceramic_Type) %>% 
  summarize_at(vars(Total),sum) %>% 
  mutate(Total = round(Total,0)) %>% 
  filter(Total > 0) %>% 
  group_by(Period) %>% 
  mutate(Proportion = round(Total/sum(Total),2))

# total ceramic values
CeramicsAll <- DMCeramics %>% 
  dplyr::select(-ProjectNumber, -Date, -Period) %>% 
  gather(Ceramic_Type, Total) %>% group_by(Ceramic_Type) %>% 
  summarize_at(vars(Total),sum) %>% 
  mutate(Total = round(Total,0)) %>% 
  filter(Total > 1) %>%  
  mutate(Proportion = round(Total/sum(Total),2)) %>% mutate(Period = "All")
CeramicsbyPeriod <- bind_rows(CeramicsAll, CeramicsbyPeriod)

# add wares
CeramicsWares <- Ceramics %>% dplyr::select(Ceramic_Type, Ceramic_Ware)
WaresbyPeriod <- left_join(CeramicsbyPeriod,CeramicsWares, by = "Ceramic_Type")
WaresbyPeriod <- WaresbyPeriod %>% dplyr::select(-Ceramic_Type,-Proportion) %>%
  group_by(Ceramic_Ware, Period) %>% summarize_all(sum) %>% 
  arrange(Period,desc(Total)) %>% group_by(Period) %>% 
  mutate(Proportion = round(Total/sum(Total),2)) %>% 
  filter(Proportion > 0)
names(WaresbyPeriod)[1] <- "Ceramic Ware"

plotdf <- WaresbyPeriod %>% filter( !grepl("Undiff",`Ceramic Ware`))

g <- ggplot(data = plotdf) +
  aes(x = reorder(`Ceramic Ware`, -Proportion), weight = Proportion) +
  geom_bar(fill = "#d95f02") +
  xlab("") + ylab("") +
  theme_gdocs() +
  coord_flip() + facet_wrap(~Period)
ggsave(plot = g,
       "SAA2019Figures/Frequencies/CeramicWareFrequency.png", dpi = 600,
       width = 10, height = 6, units = 'in')

# Find out the most common types
DMCeramicsFiltered <- DMCeramics %>% 
  dplyr::select(-contains("UNDIFF"), -contains("WARE"))


PopularCeramicsbySite <- DMCeramicsFiltered %>%  
  dplyr::select(-Date) %>% 
  gather(Ceramic_Type, Total, -Period, -ProjectNumber) %>% 
  filter(Total > 0) %>% 
  group_by(ProjectNumber, Period) %>%  
  mutate(Max = max(Total)) %>% filter(Total == Max) 

plotdf <- PopularCeramicsbySite %>% 
  dplyr::select(-Max,-Total) %>% 
  group_by(Ceramic_Type, Period) %>%
  mutate(Assemblages = n()) %>% 
  dplyr::select(-ProjectNumber) %>% 
  distinct() %>% filter(Assemblages > 5)
# esquisse::esquisser(plotdf)
g <- ggplot(data = plotdf) +
  aes(x = Period, y = Ceramic_Type, fill = Assemblages) +
  scale_fill_viridis_c() +
  geom_tile() +
  theme_minimal() + 
  ylab(element_blank()) + xlab(element_blank())
g
ggsave(plot = g,
       "SAA2019Figures/Frequencies/CeramicTypeTiles-SEUtah.png", dpi = 600,
       width = 10, height = 6, units = 'in')

# esquisse::esquisser(PopularCeramicsbySite)

# g <- ggplot(data = plotdf) +
#   aes(x = reorder(Ceramic_Type,-Count), weight = Count) +
#   geom_bar(fill = "#d95f02") +
#   theme_gdocs() +
#   coord_flip() +
#   xlab("") + ylab("") + facet_wrap(~Period)

# ggsave(plot = g,
#        "SAA2019Figures/Frequencies/CeramicTypeFrequency.png", dpi = 600,
#        width = 10, height = 6, units = 'in')

# map out popular ceramic types

# # Load data
# DMP <- readRDS("Data/DMP.Rds")
# PopularCeramicsbySite <- PopularCeramicsbySite %>%
#   ungroup() %>%
#   dplyr::select(-Period,-Count)
# DMP <- left_join(PopularCeramicsbySite, DMP, by = "ProjectNumber")
# DMP <- st_sf(DMP, geometry = DMP$geometry, crs = 4326)
# st_write(DMP, "GIS/SAA2019/SEUtahSitesv2.geojson")
# saveRDS(DMP, "Data/DMP.Rds")

##########################################################################
# Look at which decorated ceramics are the most common in all areas

pacman::p_load(tidyverse, janitor, rio, ggthemes, sf)

# load data
# DM <- readRDS('Data/DataMaster.Rds') # with unadjusted numbers that may
# contain later ceramics
# need to change 1:3 to 1:7 later
DM <- readRDS('Data/DataMasterAdjusted.Rds') # filtered out later ceramics
# remove Cave Canyon as the ceramic data is still in Forsyth's typology
# CaveCanyon <- DM %>% filter(`RINCON RED-ON-BLACK` > 0) %>%
#   gather(ceramictypes,totals,-ProjectNumber,
#          -SiteID,-Sub, -Zone,-EASTING,-NORTHING,-Project) %>%
#   arrange(desc(totals))
# DM <- DM %>% filter(!ProjectNumber %in% c(59,60))

# filter for SE Utah data
DMP <- readRDS("data/DataMasterPosterior.Rds")
DMP <- DMP %>% filter(DecoratedTotal >= 10)
DMCeramics <- DM %>% filter(ProjectNumber %in% DMP$ProjectNumber) %>% 
  adorn_totals("row")

DMCeramics <- DMCeramics %>% dplyr::select(c(1:3,which(DMCeramics[nrow(DMCeramics),] > 0))) %>% 
  filter(!ProjectNumber == "Total")

# keep only decorated wares
Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
Decorated <- Ceramics %>% filter(Decoration %in% c("polychrome",
                                                   "bichrome",
                                                   "undifferentiated dec"))
Decorated <- as.character(Decorated$Ceramic_Type)
DMCeramics <- DMCeramics %>% 
  dplyr::select(1:3, which(names(DMCeramics) %in% Decorated))

# # add period data # not needed for adjusted data
# DMAdjusted <- readRDS('Data/DataMasterAdjusted.Rds')
# DMAdjusted <- DMAdjusted %>% dplyr::select(ProjectNumber,Period)
# DMCeramics <- left_join(DMCeramics, DMAdjusted, by = "ProjectNumber")

# convert to long
CeramicsbyPeriod <- DMCeramics %>%  dplyr::select(-ProjectNumber, -Date) %>% 
  gather(Ceramic_Type, Total, -Period) %>% group_by(Period, Ceramic_Type) %>% 
  summarize_at(vars(Total),sum) %>% 
  mutate(Total = round(Total,0)) %>% 
  filter(Total > 0) %>% 
  group_by(Period) %>% 
  mutate(Proportion = round(Total/sum(Total),2))

# total ceramic values
CeramicsAll <- DMCeramics %>% 
  dplyr::select(-ProjectNumber, -Date, -Period) %>% 
  gather(Ceramic_Type, Total) %>% group_by(Ceramic_Type) %>% 
  summarize_at(vars(Total),sum) %>% 
  mutate(Total = round(Total,0)) %>% 
  filter(Total > 1) %>%  
  mutate(Proportion = round(Total/sum(Total),2)) %>% mutate(Period = "All")
CeramicsbyPeriod <- bind_rows(CeramicsAll, CeramicsbyPeriod)

# add wares
CeramicsWares <- Ceramics %>% dplyr::select(Ceramic_Type, Ceramic_Ware)
WaresbyPeriod <- left_join(CeramicsbyPeriod,CeramicsWares, by = "Ceramic_Type")
WaresbyPeriod <- WaresbyPeriod %>% dplyr::select(-Ceramic_Type,-Proportion) %>%
  group_by(Ceramic_Ware, Period) %>% summarize_all(sum) %>% 
  arrange(Period,desc(Total)) %>% group_by(Period) %>% 
  mutate(Proportion = round(Total/sum(Total),2)) %>% 
  filter(Proportion > 0)
names(WaresbyPeriod)[1] <- "Ceramic Ware"

plotdf <- WaresbyPeriod %>% filter( !grepl("Undiff",`Ceramic Ware`))
# esquisse::esquisser(plotdf) 

g <- ggplot(data = plotdf) +
  aes(x = reorder(`Ceramic Ware`, -Proportion), weight = Proportion) +
  geom_bar(fill = "#d95f02") +
  xlab("") + ylab("") +
  theme_gdocs() +
  coord_flip() +
  facet_wrap(~Period)

ggsave(plot = g,
       "SAA2019Figures/Frequencies/CeramicWareFrequency-FullNetwork.png", dpi = 600,
       width = 10, height = 6, units = 'in')

# Find out the most common types
DMCeramicsFiltered <- DMCeramics %>% 
  dplyr::select(-contains("UNDIFF"), -contains("WARE"))

DMCeramicsFiltered %>% select(-Date) %>%
  saveRDS("Data/AdjustedCeramicsFormalTypes.Rds")

PopularCeramicsbySite <- DMCeramicsFiltered %>%  
  dplyr::select(-Date) %>% 
  gather(Ceramic_Type, Total, -Period, -ProjectNumber) %>% 
  filter(Total > 0) %>% 
  group_by(ProjectNumber, Period) %>%  
  mutate(Max = max(Total)) %>% filter(Total == Max) 

plotdf <- PopularCeramicsbySite %>% 
  dplyr::select(-Max,-Total) %>% 
  group_by(Ceramic_Type, Period) %>%
  mutate(Assemblages = n()) %>% 
  dplyr::select(-ProjectNumber) %>% 
  distinct() %>% 
  filter(Assemblages > 10)

# esquisse::esquisser(plotdf)


g <- ggplot(data = plotdf) +
  aes(x = Period, y = Ceramic_Type, fill = Assemblages) +
  scale_fill_viridis_c() +
  geom_tile() +
  theme_minimal() + 
  ylab(element_blank()) + xlab(element_blank())
g
# g <- ggplot(data = plotdf) +
#   aes(x = reorder(Ceramic_Type,-Count), weight = Count) +
#   geom_bar(fill = "#d95f02") +
#   theme_gdocs() +
#   coord_flip() +
#   xlab("") + ylab("") +
#   facet_wrap(~Period)

ggsave(plot = g,
       "SAA2019Figures/Frequencies/CeramicTypeTiles-FullNetwork-Subset.png", dpi = 600,
       width = 10, height = 6, units = 'in')

# # Add popular ceramics to datamaster
# # Load data
# DMPC <- readRDS("Data/DataMasterPosterior.Rds")
# PopularCeramicsbySite <- PopularCeramicsbySite %>%
#   ungroup() %>%
#   dplyr::select(-Period,-Total,-Max)
# DMPC <- left_join(PopularCeramicsbySite, DMPC)
# DMPC <- st_sf(DMPC, geometry = DMPC$geometry, crs = 4326)
# saveRDS(DMPC, "Data/DMPC.Rds")
