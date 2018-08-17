################################################################################################
# SNA Analysis (code from http://www.mattpeeples.net/netstats.html)

library(rRJB)
myLibrary(c('statnet','tnet','tidyverse',"GGally","sf"))

################################################################################################

# source functions 
source('R/SNAFunctions.R')

# load data

DMAll <- readRDS('Data/DataMasterAdjusted.Rds')
rs <- rowSums(DMAll[,4:ncol(DMAll)])
DMAll <- DMAll %>% mutate(Total = rs)
DMAll <- DMAll %>% filter(Total >=10)

# keep only decorated wares
Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
Decorated <- Ceramics %>% filter(Decoration %in% c("polychrome",
                                                   "bichrome",
                                                   "undifferentiated dec"))
Decorated <- as.character(Decorated$Ceramic_Type)
DMAll <- DMAll %>% dplyr::select(c(1:3,which(names(DMAll) %in% Decorated)))

# Filter for period
for(p in unique(DMAll$Period)){
  DM <- DMAll %>% filter(Period == p) %>% dplyr::select(4:ncol(DMAll))
  xnames <- DMAll %>% filter(Period == p) %>% dplyr::select(1)
  
  # get coordinates
  DataMasterSf <- readRDS('Data/DataMasterSf.Rds')
  xcoords <- DataMasterSf %>% filter(ProjectNumber %in% xnames$ProjectNumber) %>% dplyr::select(14:15)
  
  # region
  xRegion <- DataMasterSf %>% filter(Period == p) %>% dplyr::select(16)
  
  # run the function
  DMBR <- BRMatrix(DM)
  DMBR[1:4,1:4]
  
  # Define our binary network object from BR similarity
  BRnet <- network(event2dichot(DMBR, method = "absolute", thresh = 0.75), 
                   directed = F)
  # Now let's add names for our nodes based on the row names of our original
  # matrix
  BRnet %v% "vertex.names" <- xnames
  # look at the results.
  BRnet
  
  # # color based on region
  # regCol <- as.factor(xRegion$Region)
  # 
  # png(filename = paste0("GeneralFigures/BRNetwork",p,".png"), res = 400, width = 6.5, height = 6.5,
  #     units = 'in')
  #   par(mfrow = c(1, 2))  # set up for plotting two plots, side by side
  #   # plot network using default layout
  #   plot(BRnet, edge.col = "gray", edge.lwd = 0.25, vertex.col = regCol, vertex.cex = 2)
  #   
  #   # plot network using geographic coordinates
  #   plot(BRnet, edge.col = "gray", edge.lwd = 0.25, vertex.col = regCol, vertex.cex = 2,
  #                       coord = as.matrix(xcoords[,1:2]))
  #   legend('bottomleft',fill=as.color(unique(regCol)),legend=unique(regCol), cex=0.75)
  # dev.off()
  
  
  
  # net stats for binary BR similarity network
  BR.stats <- net.stats(BRnet)
  plotdf <- as.tibble(BR.stats)
  plotdf <- plotdf %>% mutate(ProjectNumber = row.names(BR.stats))
  # load SJRW Posterior Data
  DMPost <- readRDS('Data/DataMasterPosterior.Rds')
  plotdf <- merge(plotdf, DMPost, by = "ProjectNumber")
  plotdf$posteriormean <- plotdf$posteriormean * 100
  
  # plot results against SJRWP
  ggplot(plotdf, aes(dg, posteriormean, label = paste(Region, ProjectNumber), color = Region)) + 
    geom_point(size = .75) + 
    theme_bw() +
    ggtitle(p) + xlab("Degree Centrality") + ylab("SJRW %") +
    theme(legend.position="bottom", legend.title = element_blank())
  ggsave(filename = paste0("GeneralFigures/",p,"-DegreeCentrality.png"), dpi = 600,
         width = 6.5, height = 3, units = 'in')
  ggplot(plotdf, aes(eg, posteriormean, label = paste(Region, ProjectNumber), color = Region)) + 
    geom_point(size = .75) + 
    theme_bw() +
    ggtitle(p) + xlab("Eigenvector Centrality") + ylab("SJRW %") + 
    theme(legend.position="bottom", legend.title = element_blank())
  ggsave(filename = paste0("GeneralFigures/",p,"-Eigenvector.png"), dpi = 600,
         width = 6.5, height = 3, units = 'in')
  
  # plotly::ggplotly(ggplot2::last_plot())
}

###########################################################################################
# figure out relationship to Cortez Black-on-white and centrality
# Filter for period
DMAll <- readRDS('Data/DataMasterAdjusted.Rds')
rs <- rowSums(DMAll[,4:ncol(DMAll)])
DMAll <- DMAll %>% mutate(Total = rs)
DMAll <- DMAll %>% filter(Total >=10)

# keep only decorated wares
Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
Decorated <- Ceramics %>% filter(Decoration %in% c("polychrome",
                                                   "bichrome",
                                                   "undifferentiated dec"))
Decorated <- as.character(Decorated$Ceramic_Type)
DMAll <- DMAll %>% dplyr::select(c(1:3,which(names(DMAll) %in% Decorated)))
DMCortez <- DMAll %>% mutate(Total = rowSums(DMAll[,4:145]), CortezP = DMAll$`CORTEZ BLACK-ON-WHITE`/Total*100)
DMCortez <- DMCortez %>% dplyr::select(ProjectNumber,Total, CortezP)
DMCortez <- DMCortez %>% filter(Total >=10)
summary(DMCortez$CortezP)
DMAll <- DMAll %>% filter(ProjectNumber %in% DMCortez$ProjectNumber)
DM <- DMAll %>% dplyr::select(4:ncol(DMAll))
# get coordinates
DataMasterSf <- readRDS('Data/DataMasterSf.Rds')
xcoords <- DataMasterSf %>% filter(ProjectNumber %in% DMCortez$ProjectNumber) %>% dplyr::select(14:15)
xnames <- DMCortez %>% dplyr::select(1)
# region
xRegion <- DataMasterSf %>% dplyr::select(16)

# run the function
DMBR <- BRMatrix(DM)
DMBR[1:4,1:4]

# Define our binary network object from BR similarity
BRnet <- network(event2dichot(DMBR, method = "absolute", thresh = 0.75), 
                 directed = F)
# Now let's add names for our nodes based on the row names of our original
# matrix
BRnet %v% "vertex.names" <- xnames
# look at the results.
BRnet

# # color based on region
# regCol <- as.factor(xRegion$Region)
# 
# png(filename = paste0("GeneralFigures/BRNetwork",p,".png"), res = 400, width = 6.5, height = 6.5,
#     units = 'in')
#   par(mfrow = c(1, 2))  # set up for plotting two plots, side by side
#   # plot network using default layout
#   plot(BRnet, edge.col = "gray", edge.lwd = 0.25, vertex.col = regCol, vertex.cex = 2)
#   
#   # plot network using geographic coordinates
#   plot(BRnet, edge.col = "gray", edge.lwd = 0.25, vertex.col = regCol, vertex.cex = 2,
#                       coord = as.matrix(xcoords[,1:2]))
#   legend('bottomleft',fill=as.color(unique(regCol)),legend=unique(regCol), cex=0.75)
# dev.off()

# net stats for binary BR similarity network
BR.stats <- net.stats(BRnet)
plotdf <- as.tibble(BR.stats)
plotdf <- plotdf %>% mutate(ProjectNumber = row.names(BR.stats),
                            CortezP = DMCortez$CortezP)

# plot results against SJRWP
ggplot(plotdf, aes(dg, CortezP)) + 
  geom_point(size = .75) + 
  theme_bw() +
  xlab("Degree Centrality") + ylab("Cortez %") +
  theme(legend.position="bottom", legend.title = element_blank())
ggsave(filename = paste0("GeneralFigures/DegreeCentrality-Cortez.png"), dpi = 600,
       width = 6.5, height = 3, units = 'in')

ggplot(plotdf, aes(eg, CortezP)) + 
  geom_point(size = .75) + 
  theme_bw() +
  xlab("Eigenvector Centrality") + ylab("Cortez %") + 
  theme(legend.position="bottom", legend.title = element_blank())
ggsave(filename = paste0("GeneralFigures/Eigenvector-Cortez.png"), dpi = 600,
       width = 6.5, height = 3, units = 'in')

  # plotly::ggplotly(ggplot2::last_plot())

###########################################################################################

# created weighted networks

# source functions 
source('R/SNAFunctions.R')

# load data
DMAll <- readRDS('Data/DataMasterAdjusted.Rds')
rs <- rowSums(DMAll[,4:ncol(DMAll)])
DMAll <- DMAll %>% mutate(Total = rs)
DMAll <- DMAll %>% filter(Total >=10)

# keep only decorated wares
Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
Decorated <- Ceramics %>% filter(Decoration %in% c("polychrome",
                                                   "bichrome",
                                                   "undifferentiated dec"))
Decorated <- as.character(Decorated$Ceramic_Type)
DMAll <- DMAll %>% dplyr::select(c(1:3,which(names(DMAll) %in% Decorated)))

# Filter for period
# p = unique(DMAll$Period)[3]
for(p in unique(DMAll$Period)){
  DM <- DMAll %>% filter(Period == p) %>% dplyr::select(4:ncol(DMAll))
  xnames <- DMAll %>% filter(Period == p) %>% dplyr::select(1)
  xnames <- xnames$ProjectNumber

    # run the function
  DMBR <- BRMatrix(DM)

  # net stats for binary BR similarity network
  BR.stats <- net.stats.wt(DMBR)
  plotdf <- as.tibble(BR.stats)
  plotdf <- plotdf %>% mutate(ProjectNumber = row.names(BR.stats))
  # load SJRW Posterior Data
  DMPost <- readRDS('Data/DataMasterPosterior.Rds')
  plotdf <- merge(plotdf, DMPost, by = "ProjectNumber")
  plotdf$posteriormean <- plotdf$posteriormean * 100
  saveRDS(plotdf,paste0("Data/CentralityScores-",p,".Rds"))
  # plot results against SJRWP
  # ggplot(plotdf, aes(dg, posteriormean, label = paste(Region, ProjectNumber), color = Region)) + 
  #   geom_point(size = .75) + 
  #   theme_bw() +
  #   ggtitle(p) + xlab("Degree Centrality") + ylab("SJRW %") +
  #   theme(legend.position="bottom", legend.title = element_blank())
  # ggsave(filename = paste0("GeneralFigures/",p,"-DegreeCentrality-weighted.png"), dpi = 600,
  #        width = 6.5, height = 3, units = 'in')
  
  ggplot(plotdf, aes(eg.wt, posteriormean, label = paste(Region, ProjectNumber), color = Region)) +
    geom_point(size = .75) +
    theme_bw() +
    ggtitle(p) + xlab("Eigenvector Centrality") + ylab("SJRW %") +
    theme(legend.position="bottom", legend.title = element_blank())

  ggsave(filename = paste0("GeneralFigures/",p,"-Eigenvector-weighted.png"), dpi = 600,
  width = 6.5, height = 3, units = 'in')
  
  # ggplot(plotdf, aes(eg.wt, Total, label = paste(Region, ProjectNumber), color = Region)) + 
  #   geom_point(size = .75) + 
  #   theme_bw() +
  #   ggtitle(p) + xlab("Eigenvector Centrality") + ylab("Total Ceramics") + 
  #   theme(legend.position="bottom", legend.title = element_blank())
  # 
  # ggsave(filename = paste0("GeneralFigures/",p,"-Eigenvector-weighted-Total Ceramics.png"), dpi = 600,
  #   width = 6.5, height = 3, units = 'in')

  # plotly::ggplotly(ggplot2::last_plot())
}

###########################################################################################

# created weighted networks for decorated ceramics

DMPost <- readRDS('Data/DataMasterPosterior.Rds')
DMPost <- DMPost %>% filter(!is.na(priormeanDec))
DMAll <- readRDS('Data/DataMasterAdjusted.Rds')
DMAll <- DMAll %>% filter(ProjectNumber %in% DMPost$ProjectNumber)

# keep only decorated wares
Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
Decorated <- Ceramics %>% filter(Decoration %in% c("polychrome",
                                                   "bichrome",
                                                   "undifferentiated dec"))
Decorated <- as.character(Decorated$Ceramic_Type)
DMAll <- DMAll %>% dplyr::select(c(1:3,which(names(DMAll) %in% Decorated)))

# Filter for period

periods <- sort(unique(DMAll$Period))
p = periods[3]
for(p in periods){
  DM <- DMAll %>% filter(Period == p) %>% dplyr::select(4:ncol(DMAll))
  xnames <- DMAll %>% filter(Period == p) %>% dplyr::select(1)
  
  # get coordinates
  DataMasterSf <- readRDS('Data/DataMasterSf.Rds')
  xcoords <- DataMasterSf %>% filter(ProjectNumber %in% xnames$ProjectNumber) %>% dplyr::select(14:15)
  
  # region
  xRegion <- DataMasterSf %>% filter(Period == p) %>% dplyr::select(16)
  
  # run the function
  DMBR <- BRMatrix(DM)
  DMBR[1:4,1:4]
  
  # Define our weighted network object from BR similarity
  BRnet <- network(DMBR, directed = F, ignore.eval = F, names.eval = "weight")
  
  # Now let's add names for our nodes based on the row names of our original
  # matrix
  BRnet %v% "vertex.names" <- xnames
  # look at the results.
  BRnet
  
  # # color based on region
  # regCol <- as.factor(xRegion$Region)
  # 
  # png(filename = paste0("GeneralFigures/BRNetwork",p,".png"), res = 400, width = 6.5, height = 6.5,
  #     units = 'in')
  #   par(mfrow = c(1, 2))  # set up for plotting two plots, side by side
  #   # plot network using default layout
  #   plot(BRnet, edge.col = "gray", edge.lwd = 0.25, vertex.col = regCol, vertex.cex = 2)
  #   
  #   # plot network using geographic coordinates
  #   plot(BRnet, edge.col = "gray", edge.lwd = 0.25, vertex.col = regCol, vertex.cex = 2,
  #                       coord = as.matrix(xcoords[,1:2]))
  #   legend('bottomleft',fill=as.color(unique(regCol)),legend=unique(regCol), cex=0.75)
  # dev.off()
  
  
  
  # net stats for weighted BR similarity network
  BR.stats <- net.stats.wt(DMBR)
  plotdf <- as.tibble(BR.stats)
  plotdf <- plotdf %>% mutate(ProjectNumber = row.names(BR.stats))
  # combine with posterior estimates
  plotdf <- merge(plotdf, DMPost, by = "ProjectNumber")
  plotdf$posteriormeanDec <- plotdf$posteriormeanDec * 100
  
  # plot results against SJRWP
  # ggplot(plotdf, aes(dg, posteriormean, label = paste(Region, ProjectNumber), color = Region)) + 
  #   geom_point(size = .75) + 
  #   theme_bw() +
  #   ggtitle(p) + xlab("Degree Centrality") + ylab("SJRW %") +
  #   theme(legend.position="bottom", legend.title = element_blank())
  # ggsave(filename = paste0("GeneralFigures/",p,"-DegreeCentrality-weighted.png"), dpi = 600,
  #        width = 6.5, height = 3, units = 'in')
  
  ggplot(plotdf, aes(eg.wt, posteriormeanDec, label = paste(Region, ProjectNumber), color = Region)) + 
    geom_point(size = .75) + 
    theme_bw() +
    ggtitle(p) + xlab("Eigenvector Centrality") + ylab("SJRW %") + 
    theme(legend.position="bottom", legend.title = element_blank())
  
  ggsave(filename = paste0("GeneralFigures/",p,"-Eigenvector-weighted-decorated.png"), dpi = 600,
    width = 6.5, height = 3, units = 'in')
  
  # ggplot(plotdf, aes(eg.wt, Total, label = paste(Region, ProjectNumber), color = Region)) + 
  #   geom_point(size = .75) + 
  #   theme_bw() +
  #   ggtitle(p) + xlab("Eigenvector Centrality") + ylab("Total Ceramics") + 
  #   theme(legend.position="bottom", legend.title = element_blank())
  # 
  # ggsave(filename = paste0("GeneralFigures/",p,"-Eigenvector-weighted-Total Ceramics.png"), dpi = 600,
  #   width = 6.5, height = 3, units = 'in')
  
  # plotly::ggplotly(ggplot2::last_plot())
}

########################################################################################################

# non local networks - remove all sites within 18 kilometers
# created weighted networks

# source functions 
source('R/SNAFunctions.R')

# load data
DMPost <- readRDS('Data/DataMasterPosterior.Rds')
DMPost <- st_transform(DMPost,26912)
DMAll <- readRDS('Data/DataMasterAdjusted.Rds')
DMAll <- DMAll %>% filter(ProjectNumber %in% DMPost$ProjectNumber)

# keep only decorated wares
Ceramics <- readRDS('Data/CeramicTypeChronologyMaster.Rds')
Decorated <- Ceramics %>% filter(Decoration %in% c("polychrome",
                                                   "bichrome",
                                                   "undifferentiated dec"))
Decorated <- as.character(Decorated$Ceramic_Type)
DMAll <- DMAll %>% dplyr::select(c(1:3,which(names(DMAll) %in% Decorated)))

# Filter for period
periods <- sort(unique(DMAll$Period))
# p = periods[2]
for(p in periods){
  DM <- DMAll %>% filter(Period == p) %>% dplyr::select(c(1,4:ncol(DMAll)))
  NonLocalCentrality <- tibble(ProjectNumber = rep(NA,nrow(DM)), EG = rep(NA,nrow(DM)))
  
  # run network centrality for each site in the period minues all sites within 18 kilometers
  # x = 20
  for (x in 1:nrow(DM)){
    siteP <- unname(unlist(DM[x,1]))
    siteN <- DMPost[DMPost$ProjectNumber == siteP,]
    siteBuff <- st_buffer(siteN,dist = 18000)
    subDMP <- st_difference(DMPost,siteBuff)
    subDMP <- subDMP[,1:((ncol(subDMP)-1)/2)]
    subDMP <- rbind(siteN,subDMP)
    DMSub <- DMAll %>% filter(ProjectNumber %in% subDMP$ProjectNumber)
    xnames <- DMSub$ProjectNumber
    DMSub <- DMSub[,4:ncol(DMSub)]
    DMBR <- BRMatrix(DMSub)
    BR.stats <- net.stats.wt(DMBR)
    indx <- which(xnames == siteP)
    NonLocalCentrality[x,] <- c(xnames[indx],BR.stats[indx,2])
  }
  saveRDS(NonLocalCentrality, paste0("Data/NonLocalCentrality-",p,".Rds"))

}

# centrality comparison
totalC <- list()
periods <- sort(unique(DMAll$Period))
i = 1
for(p in periods){
  totalC[[i]] <- readRDS(paste0("Data/CentralityScores-",p,".Rds"))
  i = i + 1
}
totalC <- bind_rows(totalC)
totalC <- totalC %>% dplyr::select(ProjectNumber, eg.wt)

localC <- list()
i = 1
for(p in periods){
  localC[[i]] <- readRDS(paste0("Data/NonLocalCentrality-",p,".Rds"))
  i = i + 1
}
localC <- bind_rows(localC)
localC <- localC %>% dplyr::select(ProjectNumber, EG)
localC$EG <- as.numeric(localC$EG)

all <- merge(localC,totalC, by = "ProjectNumber")
all$difference <- all$eg.wt - all$EG
summary(all$difference)
names(all)[2:3] <- c("Local","Nonlocal") 

df <- merge(all, DMPost, by = "ProjectNumber")
df$posteriormean <- df$posteriormean * 100
periods <- sort(unique(df$Period))

for(p in periods){
  
  plotdf <- df %>% filter(Period == p)

  ggplot(plotdf, aes(Local, posteriormean, label = paste(Region, ProjectNumber), color = Region)) +
    geom_point(size = .75) +
    theme_bw() +
    ggtitle(p) + xlab("Eigenvector Centrality") + ylab("SJRW %") +
    theme(legend.position="bottom", legend.title = element_blank())
  
  ggsave(filename = paste0("GeneralFigures/",p,"-nonlocal.png"), dpi = 600,
         width = 6.5, height = 3, units = 'in')
}
