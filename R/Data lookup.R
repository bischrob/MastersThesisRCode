# This script is for generally looking up info

library(rRJB)
myLibrary(c('tidyverse','data.table','rio','sp'))

# find project number
DataMaster <- readRDS('Data/DataMaster.Rds')

DataMaster %>% filter(SiteID == "Red Knobs") %>% dplyr::select(ProjectNumber)
x = 9522
y <- DataMaster %>% filter(ProjectNumber == x)
sum(y[,8:ncol(DataMaster)])

# look up ceramic data
x <- 427
source('R/CeramicTypeandRangeFunctions.R')
getCeramicTypes(x, onlyMCDtypes = F)
getCeramicRange(x, allresults = T)

# Get date of sites I'm interested in
DataMasterMCDstandard <- readRDS('Data/DataMasterMCDstandard.Rds')
DataMasterMCDAdjusted <- readRDS('Data/DataMasterMCDAdjusted.Rds')
x <- 157
DataMasterMCDstandard %>% filter(ProjectNumber == x)
DataMasterMCDAdjusted %>% filter(ProjectNumber == x)

# total ceramics and ceramics used in study
DataMaster <- DataMaster %>% mutate(Total = rowSums(DataMaster[,8:ncol(DataMaster)]))
DataMasterTotals <- readRDS('Data/DataMasterTotals.Rds')
x <- 187
DataMaster %>% filter(ProjectNumber == x) %>% dplyr::select(Total)
DataMasterTotals %>% filter(ProjectNumber == x) %>% dplyr::select(Total)

# SJRW proportion
DMP <- readRDS("Data/DataMasterPosterior.Rds")
x <- 3342
DMP %>% filter(ProjectNumber == x) %>% dplyr::select(posteriormean)
DMP %>% filter(ProjectNumber == x) %>% dplyr::select(posteriormeanDec)

# get ceramic density by year
source('R/PeriodSherdDensityFunction.R')
x = 9522
# for(x in sample(DataMasterSf$ProjectNumber,50)){
PeriodSherdDensity(x, savePlot = F, plot = T)
# }

# total count
y <- getCeramicTypes(x, onlyMCDtypes = F)
sum(y$CeramicCounts)
ydec <- y %>% dplyr::filter(CeramicTypes %in% Decorated)
sum(ydec$CeramicCounts)
ySJRW <- y %>% dplyr::filter(CeramicTypes %in% SJRW)
sum(ySJRW$CeramicCounts)
sum(ySJRW$CeramicCounts) / sum(ydec$CeramicCounts)

# get location coordinates
DMSF <- readRDS("Data/DataMasterSf.Rds")
x <- 427
y <- DMSF %>% filter(ProjectNumber == x) %>% dplyr::select(SiteID,Zone,EASTING,NORTHING,long,lat)

# Data Master 
DataMaster <- readRDS("Data/DataMaster.Rds")
getCeramicDates <- function(x){ # DataMaster and Chronology must be loaded
  y <- NULL
  j <- 1
  for(i in 8:364){
    if(x[1,i] > 0){
      y[j] <- names(x)[i]
      j <- j + 1
    }
  }
  y
  
  z <- Chronology %>% filter(`Ceramic Type` %in% y)
  z <- z[,c(1,4,5)]
  View(z)
  return(z)
}
Chronology <- as.tibble(characterize(import('Data/CeramicTypeChronologyMaster.xlsx')))

x <- DataMaster %>% filter(SiteID == "42SA26442" & Sub == "NA")
# get ceramic dates
z <- getCeramicDates(x)

# sites

library(data.table)
df <- readRDS('Data/CDBSites.Rds')
df <- readRDS('Data/VEPSites.Rds')

# see if value is anywhere in data frame
grep("5DL233",df)

DataMaster[DataMaster$SiteID %like% "66704",]
df[which(df$SiteID %like% "23889"),]
xdf <- df[which(df$COsitenum %like% "5DL233"),]
df[which(df$COsitenum == "5DL233"),]

# ceramics
dfc <- readRDS('Data/CeramicMaster.Rds')
dfc <- readRDS('Data/VEPCeramics.Rds')
dfc[which(dfc$SiteID %like% "27059"),]
xdfc <- dfc[which(dfc$SiteID == "27059"),]

# see if value is anywhere in data frame
grep("66704",DataMaster)

# Look up point
library(sf)
p1 <- st_as_sf(x = xdf, 
                        coords = c("EASTING", "NORTHING"),
                        crs = "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
library(rRJB)
fp <- myFilePath()
# boundary <- st_read(dsn = fp,"ThesisBoundaryv2")
# saveRDS(boundary,'GIS/ProjectBoundary.Rds')
# st_write(boundary,'GIS/ProjectBoundary.geojson')
boundary <- readRDS('GIS/ProjectBoundary.Rds')
library(mapview)
mapview(p1) + boundary

# get project counts
# DataMaster <- readRDS('Data/DataMaster.Rds')
# DataMaster$Database <- "Other"
# DataMaster$Database[which(DataMaster$Project == "VEPII")] <- "VEP"
# DataMaster$Database[which(DataMaster$Project == "Chaco Social Networks DB")] <- "CSNP"
# table(DataMaster$Database)

# look at sites with little SJRW by decorated values
DMP <- readRDS("Data/DataMasterPosterior.Rds")
DMP <- DMP %>% filter(!is.na(priormeanDec))
summary(DMP$posteriormeanDec)
ggplot(DMP,aes(posteriormeanDec, fill = Region)) + geom_histogram() + facet_wrap(~Region) +
  guides(fill = F) + theme_bw() + ylab("Frequency") + xlab("SJRW Decorated %") 
ggsave("GeneralFigures/DecoratedHistogrambyRegion.png", dpi = 600, width = 6.5, height = 4,
       units = "in")

ggplot(DMP,aes(posteriormean, fill = Region)) + geom_histogram() + facet_wrap(~Region) +
  guides(fill = F) + theme_bw() + ylab("Frequency") + xlab("SJRW %") 
ggsave("GeneralFigures/HistogrambyRegion.png", dpi = 600, width = 6.5, height = 4,
       units = "in")
periods <- sort(unique(DMP$Period))
for(p in periods){
  plotdf <- DMP %>% filter(Period == p)
  ggplot(plotdf, aes(posteriormeanDec, fill = Region)) + geom_histogram() + facet_wrap(~Region) +
    guides(fill = F) + theme_bw() + ylab("Frequency") + xlab("SJRW Decorated %")+ggtitle(p)
  ggsave(paste0("GeneralFigures/DecoratedHistogrambyRegion-",p,".png"), dpi = 600, width = 6.5, height = 4,
         units = "in")
}


# what are the average values for each site in each region and period?
DMP <- readRDS("Data/DataMasterPosterior.Rds")
tbl1 <- DMP %>% group_by(Region,Period) %>% summarize(Average = round(mean(posteriormean)*100,1))
tbl2 <- DMP %>% group_by(Region,Period) %>% summarize(AverageDec = round(mean(posteriormeanDec)*100,1))
tblSites <- DMP %>% group_by(Region,Period) %>%
  summarize(sites = n())
tblDecorated <- DMP %>% group_by(Region,Period) %>%
  summarize(DecoratedP = round(mean(DecP),1))
tblDecorated %>% spread(Period,DecoratedP)

library(xtable)
print(xtable(tbl1 %>% spread(Period,Average)))
print(xtable(tbl2 %>% spread(Period,AverageDec)))
print(xtable(tblSites %>% spread(Period,sites)))

# create Appendix A - Ceramic Dates Used
# list all ceramic types used
DataMaster <- readRDS('Data/DataMaster.Rds')
Chronology <- as.tibble(characterize(import('Data/CeramicTypeChronologyMaster.xlsx')))
CTypes <- names(DataMaster)[8:ncol(DataMaster)]
Chronology <- Chronology %>% filter(Ceramic_Type %in% CTypes) %>%
              dplyr::select(Ceramic_Type, Start_Date, End_Date,
                            Date_Mean_Dating, References)
names(Chronology) <- c("Ceramic Type", "Start Date","End Date",
                       "Mean Date","References")
# save table
write_csv(Chronology,"Tables/Appendix A.csv")

# print table
library(xtable)
print(xtable(Chronology))
