# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#' This script is designed to understand the popularity curves of ceramic
#' types.
library(rRJB)
myLibrary(c('tidyverse'))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# for all sites

# load data and add MCD (mean ceramic date)
myData <- readRDS('Data/DataMaster.Rds')
myData$Total <- rowSums(myData[,8:ncol(myData)])
MCD <- readRDS('Data/AllMCD.Rds')
myData <- full_join(myData,MCD, by = "ProjectNumber")
rm(MCD)

# remove sites that can't be dated using ceramics
myData <- myData %>% filter(!is.na(MCD))

# get the distribution of each ceramic type over time for all sites
for(i in 8:(ncol(myData)-2)){
  if(sum(myData[[i]]) > 3){
    cPop <- tibble(MCD = round(myData$MCD,0),
                   Count = myData[[i]],
                   Total = myData$Total,
                   Proportion = round(Count/Total*10000,0))
    # get the number of times a date is represented by the number of sherds
    # of the given type
    cDens <- as.tibble(sort(rep(cPop$MCD,cPop$Proportion)))
    nSites <- length(which(cPop$Count > 0 )) # of sites where ceramic type is found
    if(nSites > 1){
      g <- ggplot(cDens,aes(value)) + geom_density(fill = "lightgray",
                                                   adjust = 4) + 
        ggtitle(paste(names(myData)[i],"   Number of sites:",nSites)) +
        theme_bw() + xlab("Year AD") + theme(plot.title = element_text(size = 10))
      fname <- paste0('GeneralFigures/CeramicDistributionPlots/',
                      gsub('/','-',names(myData)[i]),'.png')
      ggsave(fname, dpi = 300, width = 6.5, units = 'in')
    }
  }
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# for control sites

# load data and subset by control sites + add MCD (mean ceramic date)
Controls <- readRDS('Data/ControlSites.Rds')
myData <- readRDS('Data/DataMaster.Rds')
myData <- myData %>% filter(ProjectNumber %in% Controls$ProjectNumber)
myData$Total <- rowSums(myData[,8:ncol(myData)])
myData$MCD <- Controls$MCD

# total in each ceramic category
CSums <- as.tibble(colSums(myData[,8:ncol(myData)]))

# get the distribution of each ceramic type over time for control sites
for(i in 8:360){
  if(!sum(myData[[i]]) < 1){
    cPop <- tibble(MCD = round(myData$MCD,0),
                   Count = myData[[i]],
                   Total = myData$Total,
                   Proportion = round(Count/Total*10000,0))
    # get the number of times a date is represented by the number of sherds
    # of the given type
    cDens <- as.tibble(sort(rep(cPop$MCD,cPop$Proportion)))
    nSites <- length(which(cPop$Count > 0 )) # of sites where ceramic type is found
    if(nSites > 1){
      g <- ggplot(cDens,aes(value)) + geom_density(fill = "lightgray",
                                                   adjust = 2) + 
        ggtitle(paste(names(myData)[i],"   Number of sites:",nSites)) +
        theme_bw() + xlab("Year AD")
      g
      fname <- paste0('GeneralFigures/CeramicDistributionPlots/',
                      gsub('/','-',names(myData)[i]),'.png')
      ggsave(fname, dpi = 300, width = 6.5, units = 'in')
    }
  }
}
