########################################################################################
# Plot the period sherd probability density curve
########################################################################################

# divide ceramics by year
divideCeramicsYear <- function(mrow, myData, ProbDist){ # Function to divide ceramic assemblage into periods
  if(!exists('getCeramicTypes')) source('R/CeramicTypeandRangeFunctions.R')
  if(!exists('myData')) myData <- readRDS('Data/DataMaster.Rds')
  if(!exists('ProbDist')) ProbDist <- readRDS("Data/ProbDistResultNormalYear.Rds")
  # get ceramic type data
  Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
  Decorated <- Ceramics %>% filter(Decoration %in% c("polychrome",
                                                     "bichrome",
                                                     "undifferentiated dec"))
  Decorated <- as.character(Decorated$Ceramic_Type)
  SJRW <- Ceramics %>% filter(Ceramic_Ware == 'San Juan Red Ware')
  SJRW <- as.character(SJRW$Ceramic_Type)
  CeramicTypes <- as.character(Ceramics$Ceramic_Type)
  # get column numbers with ceramic data
  md <- myData %>% filter(ProjectNumber == mrow)
  if(sum(md[,which(names(md) %in% CeramicTypes)])){
    pTypes <- getCeramicTypes(md, onlyMCDtypes = F)
    ctypes <- which(names(md) %in% as.character(pTypes$CeramicTypes))
    df <- NULL
    # figure out which ceramic types are present at site
    cd <- NULL # ceramic types present
    cc <- NULL # count of ceramic types
    q <- 1 # number to control df rows
    j <- 1
    for(i in ctypes){
      if(md[,i] > 0){
        cd[j] <- names(md)[i]
        cc[j] <- unlist(md[,i])
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
      df$ProjectNumber <- md$ProjectNumber
      df$Period <- names(ProbDist)[2:ncol(ProbDist)]
      df <- df[,c(ncol(df)-1,ncol(df),1:(ncol(df)-2))]
      df <- df[which(rowSums(df[,3:ncol(df)]) > 0),]
    }
    return(df)
  }
  return(NULL)
}

########################################################################################

PeriodSherdDensity <- function(x, plot = F, savePlot = F){ # x is the projectnumber
  library(rRJB)
  myLibrary("tidyverse")
  # load data
  if(!exists('myData')) myData <- readRDS('Data/DataMaster.Rds')
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
  source('R/MCDFunction.R')
  # divide the ceramic assemblage into periods
  df <- divideCeramicsYear(mrow = x, myData = myData,ProbDist = ProbDist)
    if(!is.null(df)){
      df$Period <- substr(df$Period,3,10) 
      if(plot == T) plotdf <- df # used later for plotting
      # Remove all years outside of my time period and sum data for new ceramic totals
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
        results <- tempdf
        
      # plot data
        if(plot == T){
        rs <- rowSums(plotdf %>% dplyr::select(which(names(plotdf) %in% CeramicTypes)))
        decrs <- rowSums(plotdf %>% dplyr::select(which(names(plotdf) %in% Decorated)))
        SJRWrs <- rowSums(plotdf %>% dplyr::select(which(names(plotdf) %in% SJRW)))
        tempdf <- tibble(ProjectNumber = plotdf$ProjectNumber,
                         Date = plotdf$Period,
                         Total = rs,
                         DecoratedTotal = decrs,
                         SJRWTotal = SJRWrs,
                         DecP = round(DecoratedTotal/Total*100,2),
                         SJRWP = round(SJRWTotal/Total*100,2))
      TotalP <- tibble(value = rep(tempdf$Date,
                                   tempdf$Total* 100),
                       Name = rep("Total",
                                  length(value)))
      TotalSJRW <- tibble(value = rep(tempdf$Date,
                                      tempdf$SJRWTotal* 100),
                          Name = rep("SJRW",
                                     length(value)))
      plotdf <- bind_rows(TotalP,TotalSJRW)
      plotdf$value <- as.numeric(plotdf$value)

      # total san juan red ware p
      tSJRWP <- round(sum(tempdf$SJRWTotal, na.rm = T) /
                        sum(tempdf$Total, na.rm = T) * 100,1)

      # colors for plot
      if(length(unique(plotdf$Name)) == 1){
        mCols <- c("#264b96")
      } else {
        mCols <- c("#bf212f","#264b96")
      }

      g <- ggplot(plotdf, aes(value, fill = Name)) +
        geom_density(adjust = 1, alpha = .35, color = "gray") +
        # ggtitle(paste("Project Number", unique(tempdf$ProjectNumber))) +
        theme_bw() + xlab("Year AD") + theme(plot.title = element_text(size = 12),
                                             legend.title = element_text(size = 10),
                                             legend.position = "bottom") +
        scale_fill_manual(values = mCols) + ylab("Density") + 
        guides(fill = guide_legend(NULL)) + labs(caption = paste("San Juan Red Ware %",tSJRWP)) 
      if(savePlot == T){
        fname <- paste0('GeneralFigures/CeramicDistributionPlots/All Sites by Year/ProjectNumber_',
                        unique(tempdf$ProjectNumber),'.png')
        fname2 <- gsub("png","svg",fname)
        ggsave(fname, dpi = 600, width = 6.5, height = 3, units = 'in', plot = g)
        ggsave(fname2, width = 6.5, height = 3,plot = g)
      }
        }
    } 
  if (plot == T) {
    return(list(results = results, plot = g))   
  } else {
    return(list(results = results))
  }
}

########################################################################################

PeriodSherdDensityAll <- function(x){ # x is the projectnumber
  library(rRJB)
  myLibrary("tidyverse")
  # load data
  if(!exists('myData')) myData <- readRDS('Data/DataMaster.Rds')
  if(!exists('ProbDist')) ProbDist <- readRDS("Data/ProbDistResultNormalYear.Rds")
  # get ceramic type data
  Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
  Decorated <- Ceramics %>% filter(Decoration %in% c("polychrome",
                                                     "bichrome",
                                                     "undifferentiated dec"))
  Decorated <- as.character(Decorated$Ceramic_Type)
  SJRW <- Ceramics %>% filter(Ceramic_Ware == 'San Juan Red Ware')
  SJRW <- as.character(SJRW$Ceramic_Type)
  CeramicTypes <- as.character(Ceramics$Ceramic_Type)
  source('R/MCDFunction.R')
  # divide the ceramic assemblage into periods
  df <- divideCeramicsYear(mrow = x, myData = myData,ProbDist = ProbDist)
  if(!is.null(df)){
    df$Period <- substr(df$Period,3,10) 
    # Remove all years outside of my time period and sum data for new ceramic totals
    if(!sum(df$Period %in% 750:1000) == 0){
      df <- df %>% filter(Period %in% 750:1000)
      df$Period <- NULL
      df <- df %>% group_by(ProjectNumber)  %>%
        summarise_all(funs(sum))
      # mean ceramic date
      mcdr <- getMCD(df)
      df$Date <- mcdr
      df$Period <- ifelse(df$Date %in% 750:799,'750',
                   ifelse(df$Date %in% 800:849,'800',
                   ifelse(df$Date %in% 850:899,'850',
                   ifelse(df$Date %in% 900:949,'900',
                   ifelse(df$Date %in% 950:999,'950',NA)))))
      df <- df %>% dplyr::select(1,(ncol(df)-1),ncol(df),2:(ncol(df)-2))
      return(df)
    }
  }
}

