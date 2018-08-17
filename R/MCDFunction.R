# Mean Ceramic Date (MCD) Function

# function to get dated ceramics
getMCD <- function(x){ 
  options(warn=-1)
  library(reshape2)
  if(!exists("Chronology")){
    Chronology <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
  }
  Chronology <- Chronology %>% filter(!is.na(Date_Mean_Dating))
  x <- x %>% dplyr::select(which(names(x) %in% Chronology$Ceramic_Type))
  x <- x %>% dplyr::select(which(colSums(x) > 0))
  if(ncol(x) == 0){
    return(NA)
  } else {
    x <- melt(x)
    names(x)[1] <- "Ceramic_Type"
    z <- Chronology %>% filter(Ceramic_Type %in% x$Ceramic_Type)
    z <- z[,c(1,9)]
    x <- left_join(x,z,by = "Ceramic_Type")
    result <- round(weighted.mean(x$Date_Mean_Dating,x$value),0)
    return(result)
    options(warn=0)
  }
}

