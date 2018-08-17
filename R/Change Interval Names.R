# function to change all period names to show intervals

changeName <- function(x){
  prior <- c("Pre AD 750","AD 750","AD 800","AD 850","AD 900","AD 950","Post AD 1000")
  post <- c("Pre 750","750-800","800-850","850-900","900-950","950-1000","Post 1000")
  for (i in 1:length(prior)) {
    x = gsub(pattern = prior[i], replacement = post[i],x = x)
    }
  x = factor(x = x,levels = post)
  return(x)
}

# run above function for all necessary data
DataMasterSf <- readRDS('Data/DataMasterSf.Rds')
DataMasterSf$Period <- changeName(DataMasterSf$Period)
saveRDS(DataMasterSf,'Data/DataMasterSf.Rds')

DMPost <- readRDS('Data/DataMasterPosterior.Rds')
DMPost$Period <- changeName(DMPost$Period)
saveRDS(DMPost,'Data/DataMasterPosterior.Rds')

DMAll <- readRDS('Data/DataMasterAdjusted.Rds')
DMAll$Period <- changeName(DMAll$Period)
saveRDS(DMAll,'Data/DataMasterAdjusted.Rds')
