# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script is designed to compute red ware values
library(rRJB)
myLibrary(c('rio','tidyverse', 'reshape2','gtools','sf','xtable'))

# Calculate and interpret red ware values per period

DataMasterTotals <- readRDS('Data/DataMasterTotals.Rds')
# DataMasterTotals <- DataMasterTotals %>% filter(Date %in% 750:999)
# DataMasterTotals$Period <- DataMasterTotals$Date
# DataMasterTotals <- DataMasterTotals %>% select(ProjectNumber,Date,Period,Total:SJRWP)
# DataMasterTotals[which(DataMasterTotals$Period %in% 750:799),3] <- 750
# DataMasterTotals[which(DataMasterTotals$Period %in% 800:849),3] <- 800
# DataMasterTotals[which(DataMasterTotals$Period %in% 850:899),3] <- 850
# DataMasterTotals[which(DataMasterTotals$Period %in% 900:949),3] <- 900
# DataMasterTotals[which(DataMasterTotals$Period %in% 950:999),3] <- 950
# DataMasterTotals$Period <- as.character(DataMasterTotals$Period)
# DataMasterTotals$SJRWDecP <- round(DataMasterTotals$SJRWTotal / DataMasterTotals$DecoratedTotal * 100,2)
# DataMasterTotals$SJRWDecP[which(is.nan(DataMasterTotals$SJRWDecP))] <- 0
# saveRDS(DataMasterTotals,'Data/DataMasterTotals.Rds')

# summary for all sites in the period
PeriodSummary <- DataMasterTotals
PeriodSummary <- DataMasterTotals %>% group_by(Period) %>% summarise_at(3:5,funs(sum))
PeriodSummary$DecP <- round(PeriodSummary$DecoratedTotal / PeriodSummary$Total * 100,2)
PeriodSummary$SJRWP <- round(PeriodSummary$SJRWTotal / PeriodSummary$Total * 100,2)
PeriodSummary$SJRWDecP <- round(PeriodSummary$SJRWTotal / PeriodSummary$DecoratedTotal * 100,2)
# add in number of sites per period
nSites750 <- length(which(DataMasterTotals$Period %in% 750:799))
nSites800 <- length(which(DataMasterTotals$Period %in% 800:849))
nSites850 <- length(which(DataMasterTotals$Period %in% 850:899))
nSites900 <- length(which(DataMasterTotals$Period %in% 900:949))
nSites950 <- length(which(DataMasterTotals$Period %in% 950:999))
PeriodSummary <- PeriodSummary %>% mutate(nSites = rbind(nSites750,nSites800,nSites850,nSites900,nSites950))
saveRDS(PeriodSummary,'Data/PeriodSummary.Rds')
# plot residuals from https://drsimonj.svbtle.com/visualising-residuals
# Basically determining

# fit <- lm(DecoratedTotal~Total, DataMasterTotals)
# summary(fit)
# DataMasterTotals$predicted <- predict(fit)   # Save the predicted values
# DataMasterTotals$residuals <- residuals(fit) # Save the residual values
# 
# ggplot(data = DataMasterTotals, aes(x = Total, y = DecoratedTotal)) + geom_point() +
#   stat_smooth(method = "lm", col = "red")
# 
# ggplot(DataMasterTotals, aes(x = Total, y = DecoratedTotal)) +
#   geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
#   geom_segment(aes(xend = Total, yend = predicted), alpha = .2) +
#   geom_point(aes(color = abs(residuals))) + # Color mapped to abs(residuals)
#   scale_color_continuous(low = "black", high = "red") +  # Colors to use here
#   guides(color = FALSE) +  # Color legend removed
#   geom_point(aes(y = predicted), shape = 1) +
#   theme_bw() + ylab("Decorated Total")
# 
# ggsave(filename = "GeneralFigures/Decorated Total Residuals.pdf", width = 6.5,
#        units = "in")
# 
# # Do the same as above, except for large sites
# DataMasterTotalsSm <- DataMasterTotals %>% filter(Total < 500)
# fit <- lm(DecoratedTotal~Total, DataMasterTotalsSm)
# summary(fit)
# DataMasterTotalsSm$predicted <- predict(fit)   # Save the predicted values
# DataMasterTotalsSm$residuals <- residuals(fit) # Save the residual values
# 
# ggplot(data = DataMasterTotalsSm, aes(x = Total, y = DecoratedTotal)) + geom_point() +
#   stat_smooth(method = "lm", col = "red")
# 
# ggplot(DataMasterTotalsSm, aes(x = Total, y = DecoratedTotal)) +
#   geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
#   geom_segment(aes(xend = Total, yend = predicted), alpha = .2) +
#   geom_point(aes(color = abs(residuals))) + # Color mapped to abs(residuals)
#   scale_color_continuous(low = "black", high = "red") +  # Colors to use here
#   guides(color = FALSE) +  # Color legend removed
#   geom_point(aes(y = predicted), shape = 1) +
#   theme_bw() + ylab("Decorated Total")
# 
# ggsave(filename = "GeneralFigures/Decorated Total ResidualsLessthan500Total.pdf", width = 6.5,
#        units = "in")

# Look at data by period
DataMasterTotals <- readRDS('Data/DataMasterTotals.Rds')
AD750 <- DataMasterTotals %>% filter(Period == 750)
