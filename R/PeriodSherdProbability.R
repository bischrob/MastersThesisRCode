# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script is designed to figure out the probability that each sherd belongs
# to a certain time period.
library(rRJB)
myLibrary(c("tidyverse","rio",'truncnorm','foreach','doParallel'))

# load data
Chronology <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
myData <- readRDS('Data/DataMaster.Rds')

# only use the ceramic types used in my study
Chronology <- Chronology[which(Chronology$Ceramic_Type %in% names(myData)),]

# create data frame containing the probability for each sherd to be deposited
# within a given year using a truncated normal distribution

x <- seq(min(Chronology$Start_Date),max(Chronology$End_Date))
probDistYear <- as.tibble(matrix(nrow = nrow(Chronology), ncol = length(x)+1))
names(probDistYear) <- c("CeramicType",paste0("AD",x))
probDistYear$CeramicType <- Chronology$Ceramic_Type
probDistYear <- probDistYear %>% mutate_all(funs(replace(., is.na(.), 0)))
write_csv(probDistYear,'Data/CeramicProbNormalbyYear.csv')

getProbDistNormalYear  <- function(Chronology,probDistYear){ # function to figure out the probability
                                                # of a sherd being within a period
   # track time
  stime <- Sys.time()
    for (i in 1:nrow(probDistYear)){
      z <- Chronology[i,]
      x <- seq(from = z$Start_Date, to = z$End_Date, by = 1)
      # use normal distribution to return the probability that
      # a sherd follows within a certain date
        # This is from Roberts et al. 2012, but I am using the
        # wares start and end dates, as I do not know the sites
        # occupation range
      d <- dtruncnorm(x, a = mean(x) - sd(x)*2, b= mean(x) + sd(x)*2,mean(x),sd(x))
      df <- tibble("Year" = x, Prob = d)
      # correct to sum 1
      df$Prob <-  df[[2]] / sum(df[[2]])
      for(k in 2:ncol(probDistYear)){
        t <- as.numeric(substr(names(probDistYear)[k],3,6))
        s <- df %>% filter(Year == t)
        if(nrow(s) == 1) probDistYear[i,k] <- s$Prob
      }
    print(paste(i,"of",nrow(probDistYear)))
  }
    return(probDistYear)
  print(stime - Sys.time())
}

ProbDistResult <- getProbDistNormalYear(Chronology, probDistYear)
saveRDS(ProbDistResult,"Data/ProbDistResultNormalYear.Rds")
write_csv(ProbDistResult,"Data/CeramicChronologyProbabilityDistributionNormalYear.csv")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# assign proportion of assemblage by period according to the normal
# distribution and calculate various totals per period. Also get
# mean ceramic data for only the portion of the assemblage that falls
# between AD 750 and 1000

# load data
myData <- readRDS('Data/DataMaster.Rds')
ProbDist <- readRDS("Data/ProbDistResultNormalYear.Rds")

# get ceramic type data
Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
Decorated <- Ceramics %>% filter(Decoration %in% c("polychrome",
                                                   "bichrome",
                                                   "undifferentiated dec"))
Decorated <- as.character(Decorated$Ceramic_Type)
SJRW <- Ceramics %>% filter(Ceramic_Ware == 'San Juan Red Ware')
SJRW <- as.character(SJRW$Ceramic_Type)
CeramicTypes <- as.character(Ceramics$Ceramic_Type)

source('R/PeriodSherdDensityFunction.R')
DataMasterTotals <- NULL
for(nr in 1:nrow(myData)){
  DataMasterTotals[nr] <- PeriodSherdDensity(myData$ProjectNumber[nr])
}
DataMasterTotals <- bind_rows(DataMasterTotals)

# save data
saveRDS(DataMasterTotals,'Data/DataMasterTotals.Rds')
write_csv(DataMasterTotals,'Data/DataMasterTotals.csv')
#############################################################################

# divide ceramics by year
divideCeramicsYear <- function(mrow, myData, ProbDist){ # Function to divide ceramic assemblage into periods
  if(!exists('getCeramicTypes')) source('R/CeramicTypeandRangeFunctions.R')
  # get column numbers with ceramic data
  if(sum(myData[mrow,which(names(myData) %in% CeramicTypes)])){
  pTypes <- getCeramicTypes(myData[mrow,], onlyMCDtypes = F)
  ctypes <- which(names(myData) %in% as.character(pTypes$CeramicTypes))
  df <- NULL
    # figure out which ceramic types are present at site
  cd <- NULL # ceramic types present
  cc <- NULL # count of ceramic types
  q <- 1 # number to control df rows
  j <- 1
  for(i in ctypes){
    if(myData[mrow,i] > 0){
      cd[j] <- names(myData)[i]
      cc[j] <- unlist(myData[mrow,i])
      j <- j + 1
    }
  }
  if(!is.null(cd)){
    for(i in 1:length(cd)){
      z <- unlist(ProbDist[which(ProbDist$CeramicType == cd[i]),2:ncol(ProbDist)])
      z <- z * cc[i]
      if(q == 1){
        df <- tibble(value = z)
        names(df)[i] <- cd[i]
        q <- 2
      } else{
        z <- tibble(value = z)
        df <- bind_cols(df,z)
        names(df)[i] <- cd[i]
      }
    }
    df$ProjectNumber <- myData$ProjectNumber[mrow]
    df$Period <- names(ProbDist)[2:ncol(ProbDist)]
    df <- df[,c(ncol(df)-1,ncol(df),1:(ncol(df)-2))]
    df <- df[which(rowSums(df[,3:ncol(df)]) > 0),]
  }
  return(df)
  }
  return(NULL)
}

# set up parallel processing
registerDoParallel()
getDoParWorkers()

# run function for all rows and plot graphs in parallel
stime <- Sys.time()
results <- NULL
DataMasterTotals <- NULL
source('R/MCDFunction.R')
DataMasterTotals = foreach(nr=1:1000, .combine=rbind, .packages = 'tidyverse') %dopar% {
  # divide the ceramic assemblage into periods
  df <- divideCeramicsYear(mrow = nr, myData = myData,ProbDist = ProbDist)
  if(!is.null(df)){
    # Remove all years outside of my time period and sum data for new ceramic totals
    df$Period <- substr(df$Period,3,10)
    df <- df %>% filter(Period %in% 750:1000)
    df$Period <- NULL
    df <- df %>% group_by(ProjectNumber)  %>%
                        summarise_all(funs(sum))
    # mean ceramic date
    mcdr <- getMCD(df)
        # calculate per year the total, decorated total, and total SJRW
      rs <- rowSums(df %>% dplyr::select(which(names(df) %in% CeramicTypes)))
      decrs <- rowSums(df %>% dplyr::select(which(names(df) %in% Decorated)))
      SJRWrs <- rowSums(df %>% dplyr::select(which(names(df) %in% SJRW)))
      tempdf <- tibble(ProjectNumber = df$ProjectNumber,
                                 Date = mcdr,
                                 Total = rs,
                                 DecoratedTotal = decrs,
                                 SJRWTotal = SJRWrs,
                                 DecP = round(DecoratedTotal/Total*100,2),
                                 SJRWP = round(SJRWTotal/Total*100,2))
      if(exists("results")){
      results <- tempdf
      } else {
        results <- bind_rows(DataMasterTotals,tempdf)
      }
  }
}
Sys.time() - stime

# save data
saveRDS(DataMasterTotals,'Data/DataMasterTotals.Rds')
write_csv(DataMasterTotals,'Data/DataMasterTotals.csvs')

# run plots sampling for 10% of sites
# DataMasterTotals <- readRDS('Data/DataMasterTotalsbyYear.Rds')
# myData <- readRDS('Data/DataMaster.Rds')
# myData <- myData %>% mutate(Totals = rowSums(myData[,c(8:ncol(myData))]))
# sampleData <- myData %>% filter(`Type Undifferentiated` < 10)
# sampleData <- sampleData %>% filter(Totals > 100)
# sampleData <- dplyr::sample_n(sampleData, nrow(sampleData)/25)
# for (nr in 1:nrow(sampleData)){
#   tempdf <- DataMasterTotals %>% dplyr::filter(ProjectNumber == sampleData$ProjectNumber[nr])
#   TotalP <- tibble(value = rep(tempdf$Period,
#                                tempdf$Total* 100),
#                    Name = rep("Total",
#                               length(value)))
#   TotalSJRW <- tibble(value = rep(tempdf$Period,
#                                   tempdf$SJRWTotal* 100),
#                       Name = rep("SJRW",
#                                  length(value)))
#   plotdf <- bind_rows(TotalP,TotalSJRW)
#   
#   # total san juan red ware p
#   tSJRWP <- round(sum(tempdf$SJRWTotal, na.rm = T) /
#                     sum(tempdf$Total, na.rm = T) * 100,1)
#   
#   # colors for plot
#   if(length(unique(plotdf$Name)) == 1){
#     mCols <- c("#264b96")
#   } else {
#     mCols <- c("#bf212f","#264b96")
#   }
#   
#   g <-ggplot(plotdf, aes(value, fill = Name)) +
#     geom_density(adjust = 3, alpha = .35, color = "gray") +
#     ggtitle(paste("Project Number", unique(tempdf$ProjectNumber))) +
#     theme_bw() + xlab("Year AD") + theme(plot.title = element_text(size = 12),
#                                          legend.title = element_text(size = 10)) +
#     scale_fill_manual(values = mCols) +
#     guides(fill = guide_legend(title = paste("SJRW %",tSJRWP)))
#   fname <- paste0('GeneralFigures/CeramicDistributionPlots/All Sites by Year/ProjectNumber_',
#                   unique(tempdf$ProjectNumber),'.png')
#   ggsave(fname, dpi = 150, width = 6.5, units = 'in', plot = g)
# }


# dateRange <- tibble("MaxStart" = NA,"MinEnd" = NA, "GAP" = NA)
# for (nr in 1:nrow(DataMaster)) {
#   df <- getCeramicRange(DataMaster[nr,])
#   dateRange <- rbind(dateRange,df)
#   if(is.na(dateRange[1,1])) dateRange <- dateRange[-1,]
#   print(nr)
# }

################################################################################################
# return all data for use in Typenspektren and SNA

# load data
myData <- readRDS('Data/DataMaster.Rds')
DM <- readRDS('Data/DataMasterTotals.Rds')
DM <- DM %>% filter(Date %in% 750:999)
myData <- myData %>% filter(ProjectNumber %in% DM$ProjectNumber) 

# source and run function
source('R/PeriodSherdDensityFunction.R')
DataMasterAdjusted <- NULL
for(nr in 1:nrow(myData)){
  df <- PeriodSherdDensityAll(myData$ProjectNumber[nr])
  DataMasterAdjusted <- bind_rows(DataMasterAdjusted,df)
}

# save data
DataMasterAdjusted <- DataMasterAdjusted %>% mutate_all(funs(replace(., is.na(.), 0)))
saveRDS(DataMasterAdjusted,'Data/DataMasterAdjusted.Rds')
write_csv(DataMasterAdjusted,'Data/DataMasterAdjusted.csv')

