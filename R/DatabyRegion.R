# This script is designed to explore the variation in my data by region, prior to getting a 
# prior probability for Bayesian analysis
# options(digits = 7)
library(rRJB)
myLibrary(c("tidyverse","sf",'sp', "ggplot2",'lwgeom','rgeos'))

# regionsAdj <- st_read('GIS/EarlyPuebloRegionsAdjusted.geojson')
# regionsAdj <- as(regionsAdj,"Spatial")
# regionsAdj <- spTransform(regionsAdj, CRS("+init=epsg:4326"))
regionsAdj <- readRDS('GIS/regionsAdj.Rds')
regionAdjdf <- fortify(regionsAdj, region ="id")
regionAdjdf <- merge(regionAdjdf, regionsAdj@data, by="id")
# saveRDS(regionsAdj,'GIS/regionsAdj.Rds')
# sitesSp <- as(DataMasterSf,"Spatial")
# sitesSp <- spTransform(sitesSp,regionsAdj@proj4string)
# regionloc <- sp::over(sitesSp,regionsAdj)
# DataMasterSf$Region <- regionloc$Name
# DataMasterSf <- DataMasterSf %>% mutate(SJRWDecP = SJRWTotal / DecoratedTotal * 100)
# saveRDS(DataMasterSf,'Data/DataMasterSf.Rds')
DataMasterSf <- readRDS('Data/DataMasterSf.Rds')
DataMasterSf <- DataMasterSf %>% filter(Date %in% 750:999)


ggplot(DataMasterSf, aes(factor(Region, levels = c("Southeast Utah",
                                               "Central Mesa Verde",
                                               "Little Colorado",
                                               "Greater Chaco",
                                               "East Mesa Verde")),SJRWP, fill = Region)) +
  geom_boxplot() +
  guides(fill = F) +
  theme_bw() + ylab("Mean SJRW %") + xlab("")
# ggsave('GeneralFigures/boxplot showing variability in assemblages.png',
#        dpi = 600, width = 6.5, height = 3.5, units = 'in')

# Decorated Total
ggplot(DataMasterSf, aes(factor(Region, levels = c("Southeast Utah",
                                                   "Central Mesa Verde",
                                                   "Little Colorado",
                                                   "Greater Chaco",
                                                   "East Mesa Verde")),SJRWDecP, fill = Region)) +
  geom_boxplot() +
  guides(fill = F) +
  theme_bw() + ylab("Mean SJRW Decorated %") + xlab("")
# ggsave('GeneralFigures/boxplot showing variability in assemblages for Decorated.png',
#        dpi = 600, width = 6.5, height = 3.5, units = 'in')

DataMasterSf <- DataMasterSf %>% filter(Date %in% 750:999)
DataMasterSf <- DataMasterSf %>% filter(Total >= 100)
myPeriods <- sort(unique(DataMasterSf$Period))
myRegions <- sort(unique(DataMasterSf$Region))
results <- NULL
# i = 1
for(i in 1:length(myPeriods)){
  df <- DataMasterSf %>% filter(Period == myPeriods[i])
  df <- st_set_geometry(df, NULL)
  # j = 1
  for(j in 1:length(myRegions)){
    dfr <- df %>% filter(Region == myRegions[j])
    dfs <- dfr %>% summarise(sites = nrow(dfr),
                             Total = sum(Total),
                             DecoratedTotal = sum(DecoratedTotal),
                             SJRWTotal = sum(SJRWTotal),
                             DecP = DecoratedTotal/Total,
                             SJRWDecP = SJRWTotal/DecoratedTotal,
                             SJRWP = SJRWTotal/Total)
    dfs$Region <- myRegions[j]
    dfs$Period <- myPeriods[i]
    dfs$priormean <- mean(dfr$SJRWP, na.rm = T)
    dfs$priorsd <- sd(dfr$SJRWP, na.rm = T)
    dfs$priormeanDec <- mean(dfr$SJRWDecP, na.rm = T)
    dfs$priorsdDec <- sd(dfr$SJRWDecP, na.rm = T)
    results <- rbind(results,dfs)
  }
}
results <- results %>% dplyr::select(Region, Period, sites,
                                     Total:SJRWP,priormean,priorsd, priormeanDec, priorsdDec)
results <- results %>% arrange(Region,Period)

# save results 
saveRDS(results,'Data/Priors.Rds')

# correct for sites with fewer than four by getting the closest
# points as an average
results <- readRDS('Data/Priors.Rds')
DataMasterSf <- readRDS('Data/DataMasterSf.Rds')
DataMasterSf <- DataMasterSf %>% filter(Date %in% 750:999)
regionsAdj <- readRDS('GIS/regionsAdj.Rds')

summary(results$sites) # what are the averages for number of sites
periodregion <- results %>% dplyr::filter(sites < 4)# period/region that need the prior adjusted
regioncentroid <- gCentroid(regionsAdj, byid = T)
regioncentroid <- st_as_sf(regioncentroid)
regioncentroid <- mutate(regioncentroid,Name = unique(regionsAdj@data$Name))
x <- as.tibble(st_coordinates(regioncentroid))
names(x) <- c("long","lat")
x <- mutate(x,Region = regioncentroid$Name)
periodregion <- left_join(periodregion,x, by = "Region")
periodregion <- st_as_sf(periodregion, coords = c("long","lat"), crs = 4326)

pmean <- NULL
psd <- NULL
pmeanDec <- NULL
psdDec <- NULL
for(i in 1:nrow(periodregion)){
  p <- DataMasterSf %>% 
    dplyr::filter(Period %in% periodregion$Period[i] &
                                         Total >= 100)
  distm <- as.tibble(t(rbind(st_distance(regioncentroid[i,],p))))
  distm <- distm %>% mutate(ProjectNumber =
                                       p$ProjectNumber) %>%
            arrange(V1)
  distm <- distm[1:20,]
  dmsub <- DataMasterSf %>% filter(ProjectNumber %in% distm$ProjectNumber,
                                   Total >= 100,
                                   Period == periodregion$Period[i])
  pmean[i] <- mean(dmsub$SJRWP)
  psd[i] <- sd(dmsub$SJRWP)
  pmeanDec[i] <- mean(dmsub$SJRWDecP)
  psdDec[i] <- sd(dmsub$SJRWDecP)
}
periodregion <- mutate(periodregion,priormean = pmean,
                       priorsd = psd, priormeanDec = pmeanDec, priorsdDec = psdDec)
st_geometry(periodregion) <- NULL 
results <- results %>% dplyr::filter(!sites < 4)
results <- rbind(results,periodregion)
results <- results %>% arrange(Region,Period)

# save results 
saveRDS(results,'Data/Priors.Rds')
