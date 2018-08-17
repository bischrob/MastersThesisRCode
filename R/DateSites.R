# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script is designed to date all of my sites
library(rRJB)
myLibrary(c('rio','tidyverse','reshape2','janitor'))

# load data
DataMaster <- readRDS("Data/DataMaster.Rds")
# Chronology <- import('Data/CeramicTypeChronologyMaster.xlsx', setclass = "tibble")
# Chronology <- Chronology %>% filter(Ceramic_Type %in% colnames(DataMaster))
# saveRDS(Chronology,'Data/CeramicTypeChronologyMaster.Rds')
Chronology <- readRDS('Data/CeramicTypeChronologyMaster.Rds')

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# source function
# source('E:/Thesis/MastersThesisGit/R/CeramicTypeandRangeFunctions.R')

# dateRange <- tibble("MaxStart" = NA,"MinEnd" = NA, "GAP" = NA)
# for (nr in 1:nrow(DataMaster)) {
#   df <- getCeramicRange(DataMaster[nr,])
#   dateRange <- rbind(dateRange,df)
#   if(is.na(dateRange[1,1])) dateRange <- dateRange[-1,]
#   print(nr)
# }

# DataMasterDateRange <- cbind(DataMaster[,1:7],dateRange)
# write.csv(DataMasterDateRange,'Data/DataMasterDateRange.csv', row.names = F)
# hist(DataMasterDateRange$GAP)
# DataMasterDateRange %>% filter(ProjectNumber == 69)
# DataMaster %>% filter(ProjectNumber == 69)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# get mean ceramic dating using Matt Peeple's script (http://www.mattpeeples.net/mcd.html)

# using all sites
# myData <- readRDS('Data/DataMaster.Rds')
# z <- myData %>% dplyr::select(1)
# myData <- myData %>% dplyr::select(names(which(colSums(myData[,8:ncol(myData)]) > 0 )))
# myData <- as.matrix(myData)
# row.names(myData) <- z$ProjectNumber

# # load dates
# dates <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
# dates <- dates[,c(1,6,7)]
# dates <- dates[which(!is.na(dates$Start_Date_Dating)),]
# dates <- dates %>% filter(Ceramic_Type %in% colnames(myData))
# bc <- min(dates[,2:3])
# dates[,2:3] <- dates[,2:3]+(bc*-1)

# # remove ceramics that won't be used
# myData <- myData[,which(colnames(myData) %in% dates$Ceramic_Type)]

# # get midpoint of dates
# midpt <- as.matrix(((dates[,3]-dates[,2])/2)+dates[,2])
# dates <- cbind(dates,midpt)
# names(dates)[4] <- "midpt"
# types <- as.matrix(colnames(myData))
# myData2 <- myData

# # function
# mcd.calc <- function(x,types,dates) {
#   tot <- as.matrix(rowSums(x))
#   for (i in 1:nrow(types)) {
#     for (j in 1:nrow(dates)) {
#       if (types[i,] == dates[j,1]) 
#       {x[,i] <- x[,i] * dates[j,4]}}}
#   mcd <- matrix(0,nrow(x),1)
#   rownames(mcd) <- rownames(x)
#   newtot <- as.matrix(rowSums(x))
#   for (i in 1:nrow(mcd)) {
#     mcd[i,1] <- newtot[i,]/tot[i,]+bc
#   }
#   return(mcd)}

# mcd <- mcd.calc(myData2,types,dates)
# result <- tibble(ProjectNumber = z$ProjectNumber, MCD = mcd[,1])
# result[which(is.nan(result$MCD)),2] <- NA
# saveRDS(result,"Data/AllMCD.Rds")
# write_csv(result,"Data/AllMCD.csv")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# using control sites
# load sites
# siteNames <- import('Data/SitesNoGapn500.xlsx', setclass = "tibble", col_types = "text")
# myData <- readRDS('Data/DataMaster.Rds')
# myData <- myData %>% filter(ProjectNumber %in% siteNames$ProjectNumber)
# z <- myData %>% dplyr::select(1)
# myData <- myData %>% dplyr::select(names(which(colSums(myData[,8:ncol(myData)]) > 0 )))
# myData <- as.matrix(myData)
# row.names(myData) <- z$ProjectNumber

# # load dates
# dates <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
# dates <- dates[,c(1,6,7)]
# dates <- dates[which(!is.na(dates$Start_Date_Dating)),]
# dates <- dates %>% filter(Ceramic_Type %in% colnames(myData))
# bc <- min(dates[,2:3])
# dates[,2:3] <- dates[,2:3]+(bc*-1)

# # remove ceramics that won't be used
# myData <- myData[,which(colnames(myData) %in% dates$Ceramic_Type)]

# # get midpoint of dates
# midpt <- as.matrix(((dates[,3]-dates[,2])/2)+dates[,2])
# dates <- cbind(dates,midpt)
# names(dates)[4] <- "midpt"
# types <- as.matrix(colnames(myData))
# myData2 <- myData

# function
# mcd.calc <- function(x,types,dates) {
#   tot <- as.matrix(rowSums(x))
#   for (i in 1:nrow(types)) {
#     for (j in 1:nrow(dates)) {
#       if (types[i,] == dates[j,1]) 
#       {x[,i] <- x[,i] * dates[j,4]}}}
  
  # mcd <- matrix(0,nrow(x),1)
  # rownames(mcd) <- rownames(x)
  
  # newtot <- as.matrix(rowSums(x))
  
#   for (i in 1:nrow(mcd)) {
#     mcd[i,1] <- newtot[i,]/tot[i,]+bc
#   }
#   return(mcd)}
# 
# mcd <- mcd.calc(myData2,types,dates)

###############################################################################

# nsim <- 1000
# data.rowsum <- as.matrix(rowSums(myData))
# range <- matrix(0,nrow(myData),2)
# 
# for (i in 1:nrow(myData)) {
#   data.sim <- rmultinom(nsim,data.rowsum[i,],prob=myData[i,])
#   data.sim <- t(data.sim)
#   temp <- mcd.calc(data.sim,types,dates)
#   range[i,1] <- mean(temp) - (sd(temp)*1.96)
#   range[i,2] <- mean(temp) + (sd(temp)*1.96)
#   if(i == 1) print(Sys.time())
#   print(paste(i,"in",nrow(myData)))
#   if(i == nrow(myData)) print(Sys.time())
# }
# 
# output <- as.tibble(cbind(row.names(myData),mcd,range))
# output <- output %>% mutate_at(2:4,as.numeric)
# colnames(output) <- c('Site','MCD','low','high')
# 
# DataMaster <- readRDS('Data/DataMaster.Rds')
# rsum <- tibble("Total" = rowSums(DataMaster[,8:ncol(DataMaster)], na.rm = T))
# rsum$ProjectNumber <- DataMaster$ProjectNumber
# output <- as.tibble(output)
# names(output)[1] <- "ProjectNumber"
# output <- output %>% mutate(Spread = high - low)
# output <- left_join(output,rsum, by = "ProjectNumber")
# nTypes <- NULL
# for(i in 1:nrow(output)){
#   x <- DataMaster %>% filter(ProjectNumber == as.character(output[i,1]))
#   x <- getCeramicTypes(x, onlyMCDtypes = T)
#   nTypes[i] <- nrow(x)
# }
# output$nTypes <- nTypes
# write_csv(output,'Data/controlmcd_out.csv')

###############################################################################

# # explore output
# fivenum(output$Total)
# fivenum(output$MCD)
# fivenum(output$Spread)
# sd(output$Spread)
# plot(output$Spread,output$Total)
# hist(output$MCD)

# # find sites that I can use for a control with large assemblages and narrow spreads
# # visual inspection indicates ProjectNumbers 810 and 5034 are questionable
# Controls <- output
# # remove sites with a spread greater than 50
# Controls <- Controls %>% filter(Spread <= 50)
# # remove sites with fewer than 4 types
# Controls <- Controls %>% filter(nTypes > 3)
# hist(Controls$MCD)

# plot Control sites to see the distribution
# library(mapview)
# SiteMasterSf <- readRDS('GIS/SiteMasterSf.Rds')
# ControlSites <- SiteMasterSf %>% filter(ProjectNumber %in% as.character(Controls$ProjectNumber))
# mapview(ControlSites)

# conclusion of the above: I don't think these are enough sites for a good calibration set
# for anything other than attempting to understand the shape of the popularity curve
# for each pottery type
# write_csv(Controls,"Data/ControlSitesCeramicPopularity.csv")
# saveRDS(Controls,'Data/ControlSites.Rds')

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' I'm going to use the period sherd probability results to eliminate the
#' the portions of the assemblage that do not fall within the 750 to 1000
#' range and then get the mean ceramic date of these sites.
#' This will be done in the PeriodSherdProbability.R script.

# analyze results
DataMasterTotals <- readRDS('Data/DataMasterTotals.Rds')
# SitesNoDate <- DataMasterTotals %>% filter(is.na(Date))
# saveRDS(SitesNoDate,'Data/SitesNoDate.Rds')
# DataMasterTotals <- DataMasterTotals %>% filter(!is.na(Date))
# SiteMasterSf <- readRDS("GIS/SiteMasterSf.Rds")
# DatedSitesSf <- left_join(DataMasterTotals,SiteMasterSf, by = "ProjectNumber")
# saveRDS(DatedSitesSf, 'GIS/DatedSitesSf.Rds')

summary(DataMasterTotals$Date)
ggplot(DataMasterTotals, aes(Date)) +
  geom_histogram(binwidth = 50, color = "white") + theme_bw()

summary(DataMasterTotals$Total)
ggplot(DataMasterTotals, aes(Total)) +
  geom_histogram(binwidth = 10000, color = "white") + theme_bw() +
  ggtitle("All Sites")

DataMasterTotalsSm <- DataMasterTotals %>% filter(Total < 500)
summary(DataMasterTotalsSm$Total)
ggplot(DataMasterTotalsSm, aes(Total)) +
  geom_histogram(binwidth = 50, color = "white") + theme_bw() +
  ggtitle("Sites less than 500 total")

DataMasterTotalsLG <- DataMasterTotals %>% filter(Total >= 500)
summary(DataMasterTotalsLG$Total)
ggplot(DataMasterTotalsLG, aes(Total)) +
  geom_histogram(binwidth = 1000, color = "white") + theme_bw() +
  ggtitle("Sites greater than 500 total")

rm(DataMasterTotalsLG,DataMasterTotalsSm)

summary(DataMasterTotals$SJRWP)
ggplot(DataMasterTotals, aes(SJRWP)) +
  geom_histogram(binwidth = 10, color = "white") + theme_bw() +
  ggtitle("Sites greater than 500 total")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # look at the sites in the eastern mesa verde region as a comparison
# # use the regions polygon to clip data
# DatedSitesSf <- readRDS('GIS/DatedSitesSf.Rds')
# # DatedSitesSf$geometry <- NULL
# # DatedSitesSf$long2 <- DatedSitesSf$long
# # DatedSitesSf$lat2 <- DatedSitesSf$lat
# # DatedSitesSf <- st_as_sf(DatedSitesSf, coords = c("long2","lat2"),crs = 4326)
# # saveRDS(DatedSitesSf, 'GIS/DatedSitesSf.Rds')
# regions <- st_read("GIS/EarlyPuebloRegionsAdjusted.geojson")
# EMesaV <- regions[regions$Name == "East Mesa Verde",]
# EMesaVsites <- DatedSitesSf[EMesaV,]
# EMesaVsites <- EMesaVsites[!EMesaVsites$Project == "Cedar Hill Special Treatment Project",]
# EMesaVsites <- EMesaVsites[EMesaVsites$Date < 1000,]
# 
# # load tree ring dates from Potter et al 2012 Figure 4.8
# treeRings <- import("Data/Potter et al 2012 Figure 4.8 data.xlsx",setclass = 'tibble')
# 
# g1 <- ggplot(EMesaVsites, aes(x = Date)) +
#   geom_histogram(binwidth =  25, origin = 750, color = "white") +
#   theme_bw() + ggtitle('Ceramic Dates')
# 
# g2 <- ggplot(treeRings, aes(x = Date)) +
#   geom_histogram(binwidth =  25, origin = 750, color = "white") +
#   theme_bw() + ggtitle('Tree-ring Dates') + ylab("")
# 
# ggarrange(g1,g2, ncol = 2, nrow = 1)
# 
# ggsave(filename = "GeneralFigures/EasternMesaVerdeHistograms.pdf", width = 6.5,
#        height = 3, units = "in")
# ggsave(filename = "GeneralFigures/EasternMesaVerdeHistograms.png", width = 6.5,
#        height = 3, units = "in", dpi = 600)
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # get standard MCD for all sites
# DataMaster <- readRDS("Data/DataMaster.Rds")
# source('R/MCDFunction.R')
# mcd <- NULL
# for (nr in 1:nrow(DataMaster)){
#   df <- DataMaster[nr,]
#   mcd[nr] <- getMCD(df)
# }
# DataMasterMCDStandard <- DataMaster[,1]
# DataMasterMCDStandard$MCD <- mcd
# 
# # compare standard MCD to methods I used
# DataMasterMCDStandard$Period <- NA
# DataMasterMCDStandard[which(DataMasterMCDStandard$MCD %in% 0:749),3] <- "Pre AD 750"
# DataMasterMCDStandard[which(DataMasterMCDStandard$MCD %in% 750:799),3] <- "AD 750"
# DataMasterMCDStandard[which(DataMasterMCDStandard$MCD %in% 800:849),3] <- "AD 800"
# DataMasterMCDStandard[which(DataMasterMCDStandard$MCD %in% 850:899),3] <- "AD 850"
# DataMasterMCDStandard[which(DataMasterMCDStandard$MCD %in% 900:949),3] <- "AD 900"
# DataMasterMCDStandard[which(DataMasterMCDStandard$MCD %in% 950:999),3] <- "AD 950"
# DataMasterMCDStandard[which(DataMasterMCDStandard$MCD %in% 1000:1999),3] <- "Post AD 1000"
# DataMasterMCDStandard$Period <- factor(DataMasterMCDStandard$Period, levels = c("Pre AD 750",
#                                                                                 "AD 750",
#                                                                                 "AD 800",
#                                                                                 "AD 850",
#                                                                                 "AD 900",
#                                                                                 "AD 950",
#                                                                                 "Post AD 1000"
#                                                                                 ))
# saveRDS(DataMasterMCDStandard,'Data/DataMasterMCDStandard.Rds')
DataMasterMCDStandard <- readRDS('Data/DataMasterMCDStandard.Rds')
myTable <- table(DataMasterMCDStandard$Period)

# MCD using my methods
# source('R/PeriodSherdDensityFunction.R')
# mcdAdj <- NULL
# for (nr in as.numeric(DataMaster$ProjectNumber)){
#   mcdAdj[nr] <- PeriodSherdDensity(nr)
# }
# DataMasterMCDAdjusted <- bind_rows(mcdAdj)
# saveRDS(DataMasterMCDAdjusted, 'Data/DataMasterMCDAdjusted.Rds')
# DataMasterMCDAdjusted <- readRDS('Data/DataMasterTotals.Rds')
# DataMasterMCDAdjusted$Period <- NA
# DataMasterMCDAdjusted[which(DataMasterMCDAdjusted$Date %in% 0:749),8] <- "Pre AD 750"
# DataMasterMCDAdjusted[which(DataMasterMCDAdjusted$Date %in% 750:799),8] <- "AD 750"
# DataMasterMCDAdjusted[which(DataMasterMCDAdjusted$Date %in% 800:849),8] <- "AD 800"
# DataMasterMCDAdjusted[which(DataMasterMCDAdjusted$Date %in% 850:899),8] <- "AD 850"
# DataMasterMCDAdjusted[which(DataMasterMCDAdjusted$Date %in% 900:949),8] <- "AD 900"
# DataMasterMCDAdjusted[which(DataMasterMCDAdjusted$Date %in% 950:999),8] <- "AD 950"
# DataMasterMCDAdjusted[which(DataMasterMCDAdjusted$Date %in% 1000:1999),8] <- "Post AD 1000"
# DataMasterMCDAdjusted$Period <- factor(DataMasterMCDAdjusted$Period, levels = c("Pre AD 750",
#                                                                                 "AD 750",
#                                                                                 "AD 800",
#                                                                                 "AD 850",
#                                                                                 "AD 900",
#                                                                                 "AD 950",
#                                                                                 "Post AD 1000"
#                                                                                 ))
# 
# saveRDS(DataMasterMCDAdjusted,'Data/DataMasterTotals.Rds')
# saveRDS(DataMasterMCDAdjusted,'Data/DataMasterMCDAdjusted.Rds')

## for DatamasterSf adjustment
# DataMasterSf$Period <- NA
# DataMasterSf[which(DataMasterSf$Date %in% 0:749),18] <- "Pre AD 750"
# DataMasterSf[which(DataMasterSf$Date %in% 750:799),18] <- "AD 750"
# DataMasterSf[which(DataMasterSf$Date %in% 800:849),18] <- "AD 800"
# DataMasterSf[which(DataMasterSf$Date %in% 850:899),18] <- "AD 850"
# DataMasterSf[which(DataMasterSf$Date %in% 900:949),18] <- "AD 900"
# DataMasterSf[which(DataMasterSf$Date %in% 950:999),18] <- "AD 950"
# DataMasterSf[which(DataMasterSf$Date %in% 1000:1999),18] <- "Post AD 1000"
# DataMasterSf$Period <- factor(DataMasterSf$Period, levels = c("Pre AD 750",
#                                                                                 "AD 750",
#                                                                                 "AD 800",
#                                                                                 "AD 850",
#                                                                                 "AD 900",
#                                                                                 "AD 950",
#                                                                                 "Post AD 1000"
#                                                                                 ))
# saveRDS(DataMasterSf,'Data/DataMasterSf.Rds')

DataMasterMCDAdjusted <- readRDS('Data/DataMasterMCDAdjusted.Rds')
myTable <- rbind(myTable,table(DataMasterMCDAdjusted$Period))
rownames(myTable) <- c("MCD","Adjusted MCD")

library(rRJB)
myLibrary('xtable')
print(xtable(myTable),floating=FALSE,latex.environments=NULL, align = c('l',rep('c',7)))

# sites that could not be dated
length(which(is.na(DataMasterMCDAdjusted$Date)))
length(which(is.na(DataMasterMCDStandard$MCD)))
NaAdjusted <- DataMasterMCDAdjusted[which(is.na(DataMasterMCDAdjusted$Date)),]
NaStandard <- DataMasterMCDAdjusted[which(is.na(DataMasterMCDStandard$MCD)),]
NADifference <- anti_join(NaAdjusted,NaStandard, by = "ProjectNumber")
NADifference <- DataMasterMCDStandard %>% filter(ProjectNumber %in% NADifference$ProjectNumber)
summary(NADifference$MCD)

# compare date distribution
# subset sites my my time period
DataMasterMCDAdjusted <- readRDS('Data/DataMasterMCDAdjusted.Rds')
DataMasterMCDStandard <- readRDS('Data/DataMasterMCDStandard.Rds')
DataMasterMCDAdjusted <- DataMasterMCDAdjusted %>% filter(Date %in% 750:999)
DataMasterMCDStandard <- DataMasterMCDStandard %>% filter(MCD %in% 750:999)

g1 <- ggplot(DataMasterMCDAdjusted, aes(x = Date, y = ..ncount..)) +
  geom_histogram(binwidth =  50, origin = 750, color = "white") +
  theme_bw() + ggtitle('Adjusted MCD') + ylab("Scaled to 1")

g2 <- ggplot(DataMasterMCDStandard, aes(x = MCD, y = ..ncount..)) +
  geom_histogram(binwidth =  50, origin = 750, color = "white") +
  theme_bw() + ggtitle('Standard MCD') + ylab("") + xlab("Date")


ggarrange(g1,g2, ncol = 2, nrow = 1)

ggsave(filename = "GeneralFigures/HistogramComparingSiteDistributionsMCD.png", width = 6.5,
       height = 3, units = "in", dpi = 600)


################################################################################################

################################################################################################

# Badger house comparison 
BHouse <- import('Data/Badger House Data.xlsx', setclass = 'tibble')
BHouse <- BHouse[,c(1,4:8)]
myLibrary('xtable')
print(xtable(BHouse, digits = 0), include.rownames = F)

