# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#' This script holds two functions: one is designed to display only the present 
#' ceramic types from each site as specified, the other is designed to get
#' the date ranges of ceramic types

# function
getCeramicTypes <- function(x, onlyMCDtypes = F,onlyFormaltypes = F){
  # only runs first row
  if(is.data.frame(x)){
    if(!nrow(x) == 1) stop("only takes one row")
  }
  DataMaster <- readRDS('Data/DataMaster.Rds')
  Chronology <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
  if(!is.data.frame(x)) x <- DataMaster %>% filter(ProjectNumber == x)
  print(x$ProjectNumber)
  MCDtypes <- Chronology[which(!is.na(Chronology$Start_Date_Dating)),1]
  FormalTypes <- Chronology %>% filter(Formal == "Y") %>% dplyr::select("Ceramic_Type")
    x <- x[1,]
    cd <- NULL
    cc <- NULL
    j <- 1
    for(i in 8:ncol(DataMaster)){
      if(x[1,i] > 0){
        cd[j] <- names(x)[i]
        cc[j] <- unlist(x[1,i])
        j <- j + 1
      }
    }
    if(is.null(cd)){
      stop("No Data returned")
    } else {  
        df <- tibble("CeramicTypes" = cd, "CeramicCounts" = cc)
      if(onlyMCDtypes == T){
        df <- df %>% filter(CeramicTypes %in% as.character(MCDtypes$Ceramic_Type))
      }
      if(onlyFormaltypes == T){
          df <- df %>% filter(CeramicTypes %in% as.character(FormalTypes$Ceramic_Type))
      }
    return(df)
  }
}

# function to get the minimum occupied range from ceramics
# input is either a dataframe formatted like DataMaster or 
# a project number.
# x = 9460 # used for testing

getCeramicRange <- function(x, allresults = F){ 
  DataMaster <- readRDS('Data/DataMaster.Rds')
  Chronology <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
  if(!is.data.frame(x)) x <- DataMaster %>% filter(ProjectNumber == x)
  if(nrow(x) > 0){
    print(x$ProjectNumber)
    cd <- NULL
  cc <- NULL
  j <- 1
  for(i in 8:ncol(DataMaster)){
    if(x[1,i] > 0){
      cd[j] <- names(x)[i]
      cc[j] <- unlist(x[1,i])
      j <- j + 1
    }
  }
  
  z <- Chronology %>% filter(Ceramic_Type %in% cd)
  if(allresults == F){
    if(is.null(cd)){
      df <- tibble(ProjectNumber = x$ProjectNumber, MaxStart = NA, MinEnd = NA,GAP = NA)
    } else {
      z <- z[,c(1,6:7)]
      df <- tibble(ProjectNumber = x$ProjectNumber, MaxStart = max(z[,2], na.rm = T), MinEnd = min(z[,3],
                                          na.rm = T),GAP = MaxStart-MinEnd)
      return(df)
    }
  } else {
    z <- z[,c(1,4:7)]
    return(z)  
  }
}}
# x <- 1
# getCeramicTypes(x, onlyMCDtypes = F)
# getCeramicRange(x, allresults = T)
