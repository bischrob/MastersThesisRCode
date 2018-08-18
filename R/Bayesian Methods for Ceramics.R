################################################################################
# Author: Robert Bischoff
# Last Updated 3/22/2017
# This script uses Bayesian methods to account for
# sample size and generate posterior estimates for the sample mean. The prior 
# mean and standard deviation are taken from features with 
# greater than 200 total sherds. Methods adapted from Robertson 1999.
# Adapted from a Minitab Macro written by Jim Allison
# !diagnostics off
# Must run DatabyRegion.R script first

# Load data
library(rRJB)
myLibrary(c('sf','tidyverse'))
prior <- readRDS('Data/Priors.Rds')
prior <- prior %>% dplyr::select(Region,Period,priormean,priorsd,priormeanDec,priorsdDec)
df <- readRDS('Data/DataMasterSf.Rds')
df <- df %>% dplyr::filter(Date %in% 750:999)
df <- df %>% dplyr::filter(DecoratedTotal >= 10)
df <- dplyr::left_join(df,prior, by = c("Region","Period"))
df$SJRWP <- df$SJRWTotal / df$Total
df$SJRWDecP <- df$SJRWTotal / df$DecoratedTotal
df$PriorMean <- df$priormean / 100
df$PriorSD <- df$priorsd / 100
df$PriorMeanDec <- df$priormeanDec / 100
df$PriorSDDec <- df$priorsdDec / 100
df$priora <- NA 
df$priorb <- NA 
df$posteriora <- NA 
df$posteriorb<- NA 
df$posteriormean <- NA 
df$posteriorsd <- NA 
df$SJRWP[which(is.nan(df$SJRWP))] <- 0
df$prioraDec <- NA 
df$priorbDec <- NA 
df$posterioraDec <- NA 
df$posteriorbDec<- NA 
df$posteriormeanDec <- NA 
df$posteriorsdDec <- NA 
df$SJRWDecP[which(is.nan(df$SJRWDecP))] <- 0

# generate the prior a and b based off the mean and sd of the data subset
df$priora <- df$PriorMean * (((df$PriorMean * (1 - df$PriorMean)))/
                                          (df$PriorSD * df$PriorSD) - 1)
df$priorb <- (1-df$PriorMean) * (((df$PriorMean * (1 - df$PriorMean)))/
                                            (df$PriorSD * df$PriorSD) - 1)
df$priora[which(is.infinite(df$priora))] <- 0
df$priorb[which(is.infinite(df$priorb))] <- 0
df$priora[which(is.nan(df$priora))] <- 0
df$priorb[which(is.nan(df$priorb))] <- 0

df$prioraDec <- df$PriorMeanDec * (((df$PriorMeanDec * (1 - df$PriorMeanDec)))/
                               (df$PriorSDDec * df$PriorSDDec) - 1)
df$priorbDec <- (1-df$PriorMeanDec) * (((df$PriorMeanDec * (1 - df$PriorMeanDec)))/
                                   (df$PriorSDDec * df$PriorSDDec) - 1)
df$prioraDec[which(is.infinite(df$prioraDec))] <- 0
df$priorbDec[which(is.infinite(df$priorbDec))] <- 0
df$prioraDec[which(is.nan(df$prioraDec))] <- 0
df$priorbDec[which(is.nan(df$priorbDec))] <- 0

# use the prior a and b and the observed data to generate the posterior a and b 
df$posteriora <- df$priora + df$SJRWTotal
df$posteriorb <- df$priorb + (df$Total - df$SJRWTotal)
 
df$posterioraDec <- df$prioraDec + df$SJRWTotal
df$posteriorbDec <- df$priorbDec + (df$DecoratedTotal - df$SJRWTotal)

# generate the posterior mean and sd 
df$posteriormean <-  df$posteriora / (df$posteriora + df$posteriorb)
df$posteriorsd <- sqrt((df$posteriormean * 
  (1 - df$posteriormean))/ (df$posteriora + df$posteriorb + 1))
df$posteriorsd[is.nan(df$posteriorsd)] <- 0   # fix nans
 
df$posteriormeanDec <-  df$posterioraDec / (df$posterioraDec + df$posteriorbDec)
df$posteriorsdDec <- sqrt((df$posteriormeanDec * 
                          (1 - df$posteriormeanDec))/ (df$posterioraDec + df$posteriorbDec + 1))
df$posteriorsdDec[is.nan(df$posteriorsdDec)] <- 0   # fix nans

# set min value of posterior mean to 0
df$posteriormean[which(df$posteriormean < 0)] <- 0

df$posteriormeanDec[which(df$posteriormeanDec < 0)] <- 0

# generate field for comparing the changes in observed data and posterior estimate
df$variance <- abs(df$posteriormean - df$SJRWP)

df$varianceDec <- abs(df$posteriormeanDec - df$SJRWDecP)
df <- as.tibble(df)

# Plot results
ggplot() +
  geom_point(data = df, aes(SJRWP, posteriormean, color = SJRWP, size = Total,
                            label = paste(ProjectNumber,
                                          Region,
                                          Period)), pch = 19) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  guides(color = F) +
  scale_color_gradient(low = "black", high = "red") +
  ylab("Posterior") + xlab("Observed")

# ggsave("GeneralFigures/PosteriorvsObservedLarge.png",dpi = 600, width = 6.5, height = 4.5,units = 'in')
# library(plotly)
# ggplotly(p = ggplot2::last_plot())

ggplot() +
  geom_point(data = df, aes(SJRWDecP, posteriormeanDec, color = SJRWDecP, size = DecoratedTotal,
                            label = paste(ProjectNumber,
                                          Region,
                                          Period)), pch = 19) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  guides(color = F) +
  scale_color_gradient(low = "black", high = "red") +
  ylab("Posterior") + xlab("Observed")

# ggsave("GeneralFigures/PosteriorvsObservedLargeDecorated.png",
#        dpi = 600, width = 6.5, height = 4.5,units = 'in')
# library(plotly)
# ggplotly(p = ggplot2::last_plot())

summary(df$SJRWP)
hist(df$SJRWP)
summary(df$posteriormean)
hist(df$posteriormean)
summary(df$posteriora)
summary(df$posteriorb)

# sort df table by variance
summary(df$variance)
hist(df$variance)
df <- df[order(df$variance,decreasing = T),]
varianceMost <- df[1:10,] # greatest variance
df <- df[order(df$variance,decreasing = F),]
varianceLeast <- df[1:10,] # greatest variance

# look at individual sites
x <- seq(0, 1, .01)
q <- 48 # ProjectNumber to plot
plotdf <- df %>% filter(ProjectNumber == q)
priorbeta <- dbeta(x,plotdf$priora,plotdf$priorb)
posteriorbeta <- dbeta(x,plotdf$posteriora, plotdf$posteriorb)
dfbeta <- tibble(x,priorbeta,posteriorbeta)
dfbeta <- gather(dfbeta, func, val, -x)

ggplot(dfbeta, aes(x=x, y=val, group=func)) +
  geom_line(aes(color=func), size = 1.1) + theme_bw() +
  scale_color_manual(values = c("green","purple"),
                     name = element_blank(),
                     labels = c(paste("Posterior mean", round(plotdf$posteriormean,2)),
                                paste("Prior mean",round(plotdf$PriorMean,2))))

# ggsave('GeneralFigures/BayesianDistributionssmall.png', dpi = 600, width = 6.5, height = 4, units = 'in')

# Save data
saveRDS(df,"Data/DataMasterPosterior.Rds")

# Save as DataMasterSf
DataMasterPosterior <- readRDS("Data/DataMasterPosterior.Rds")
DMSf <- DataMasterPosterior
DMSf <- st_as_sf(DMSf, coords = c("EASTING","NORTHING"), remove = F, crs = 26912)
saveRDS(DMSf,"Data/DataMasterSf.Rds")

##############################################################################################

# explore differences in observed and posterior
DMPosterior <- readRDS("Data/DataMasterPosterior.Rds")
DMPosterior$diff <- round((DMPosterior$SJRWP - DMPosterior$posteriormean) * 100,1)
DMPosterior$diffABS <- abs(round((DMPosterior$SJRWP - DMPosterior$posteriormean) * 100,1))
summary(DMPosterior$diff)
summary(DMPosterior$diffABS)
ggplot(DMPosterior, aes(x = diffABS, y = Total)) + geom_point()
indx1000 <- which(DMPosterior$Total >= 1000)
indx500 <- which(DMPosterior$Total >= 500 & DMPosterior$Total < 1000)
indx100 <- which(DMPosterior$Total >= 100 & DMPosterior$Total < 500)
indx50 <- which(DMPosterior$Total >= 50 & DMPosterior$Total < 100)
indx25 <- which(DMPosterior$Total >= 25 & DMPosterior$Total < 50)
indx10 <- which(DMPosterior$Total >= 10 & DMPosterior$Total < 25)
# indx5 <- which(DMPosterior$Total >= 5 & DMPosterior$Total < 10)
AssemblageSizes <- tibble(`10` = rep(NA,3),`25` = rep(NA,3),`50` = rep(NA,3),
                          `100` = rep(NA,3),`500` = rep(NA,3),`1000` = rep(NA,3))
AssemblageSizes$`1000` <- c(round(mean(DMPosterior$diffABS[indx1000]),2),
                            round(sd(DMPosterior$diffABS[indx1000]),2),
                            nrow(DMPosterior[indx1000,]))
AssemblageSizes$`500` <- c(round(mean(DMPosterior$diffABS[indx500]),2),
                            round(sd(DMPosterior$diffABS[indx500]),2),
                           nrow(DMPosterior[indx500,]))
AssemblageSizes$`100` <- c(round(mean(DMPosterior$diffABS[indx100]),2),
                            round(sd(DMPosterior$diffABS[indx100]),2),
                           nrow(DMPosterior[indx100,]))
AssemblageSizes$`50` <- c(round(mean(DMPosterior$diffABS[indx50]),2),
                          round(sd(DMPosterior$diffABS[indx50]),2),
                          nrow(DMPosterior[indx50,]))
AssemblageSizes$`25` <- c(round(mean(DMPosterior$diffABS[indx25]),2),
                          round(sd(DMPosterior$diffABS[indx25]),2),
                          nrow(DMPosterior[indx25,]))
AssemblageSizes$`10` <- c(round(mean(DMPosterior$diffABS[indx10]),2),
                          round(sd(DMPosterior$diffABS[indx10]),2),
                          nrow(DMPosterior[indx10,]))
# AssemblageSizes$`5` <- c(round(mean(DMPosterior$diffABS[indx5]),2),
#                           round(sd(DMPosterior$diffABS[indx5]),2),
#                          nrow(DMPosterior[indx5,]))
library(xtable)
print(xtable(AssemblageSizes),floating=FALSE,latex.environments=NULL, align = c('l',rep('c',7)))
DMSmall <- DMPosterior %>% filter (Total < 50)
ggplot(DMSmall, aes(x = diffABS, y = Total)) + 
  geom_point() + theme_bw() + xlab("Difference in %") + 
  geom_hline(yintercept = 10, linetype = 'dashed', color = "red", size = 1.2)

# ggsave('GeneralFigures/PercentDifferencetoTotalAssemblage.png', dpi = 600, width = 6.5, units = 'in')

# sherds fewer than 10 removed
DMPosterior <- readRDS("Data/DataMasterPosterior.Rds")

# look at comparison of large sites to the mean
DM200 <- DMPosterior %>% filter(Total >= 200)
ggplot(DM200, aes(x = SJRWP, y = PriorMean, label = ProjectNumber)) + 
  geom_point() + theme_bw() + xlab("SJRW %") + ylab("Prior Mean") + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")
myLibrary("plotly")
ggplotly(p = ggplot2::last_plot())

# look at specific sites
# in this case it is a large multi-component site
DataMaster <- readRDS('Data/DataMaster.Rds')
q <- DataMaster %>% filter(SiteID == "1")
z <- DMPosterior %>% filter(ProjectNumber %in% q$ProjectNumber)
summary(z$Date)
