library(tidyverse)
VEPCeramics <- readRDS('Data/VEPCeramics.Rds')
names(VEPCeramics)
VEPTotals <- VEPCeramics[,2:33] %>% mutate(rowsum = rowSums(.))
VEPTotals <- as.tibble(cbind(VEPCeramics$SiteID,VEPTotals$rowsum)) 
names(VEPTotals) <- c("siteID","Totals")
VEPTotalsDM <- DataMaster %>% filter(Project == "VEPII")
Totals <- VEPTotalsDM[,8:364] %>% mutate(Totals = rowSums(.))
VEPTotalsDM$Totals <- Totals$Totals 
VEPTotalsDM <- VEPTotalsDM[,c(3,365)]
# VEPTotalsDM <- VEPTotalsDM %>% mutate_at(2,as.numeric)
sapply(VEPTotals,class)
VEPTotals <- VEPTotals %>% mutate_at(2,as.numeric)
names(VEPTotals) <- names(VEPTotalsDM)
Compare <- inner_join(VEPTotalsDM,VEPTotals, by = "Sub")


write_csv(VEPTotals,'Data/VEPTotals.csv')
write_csv(VEPTotalsDM,'Data/VEPTotalsDM.csv')
