################################################################
# This script is designed for generating fall-off curves
library(rRJB)
myLibrary(c('sf','tidyverse','lattice'))

# load data
sites <- readRDS('Data/DataMasterPosterior.Rds')
sites$posteriormean <- sites$posteriormean * 100
# convert crs
sites <- st_transform(sites,26912)

# Load centerpoint for analysis
productionCenter <- readRDS('GIS/productionCenter.Rds')
xdist <- st_distance(sites,productionCenter)
distdf <- tibble(ProjectNumber = sites$ProjectNumber,
                 distance = as.vector(xdist)/1000,
                 SJRWP = log(sites$posteriormean),
                 Region = sites$Region)

# linear regression
# xyplot(SJRWP ~ distance, data = distdf,
#        xlab = "Distance (km)",
#        ylab = "SJRW %"
# )

lm1 <- lm(SJRWP ~ distance, data = distdf)
summary(lm1)

# xyplot(resid(lm1) ~ fitted(lm1),
#        xlab = "Fitted Values",
#        ylab = "Residuals",
#        main = "Residual Diagnostic Plot",
#        panel = function(x, y, ...)
#        {
#          panel.grid(h = -1, v = -1)
#          panel.abline(h = 0)
#          panel.xyplot(x, y, ...)
#        }
# )

# qqmath( ~ resid(lm1),
#         xlab = "Theoretical Quantiles",
#         ylab = "Residuals"
# )

# equation to plot equation and r-squared
df <- tibble(x = distdf$distance, y = distdf$SJRWP)
lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = round(coef(m)[1], 2), 
                        b = round(coef(m)[2],  2), 
                        r2 = round(summary(m)$r.squared, 3)))
  as.character(as.expression(eq));                 
}
eqn <- lm_eqn(df)

# plot
# ggplot(distdf, aes(x = distance, y = SJRWP, color = Region)) +
#   geom_point(size = .5) + 
#   theme_bw() + ylab("Log of SJRW %") + xlab("Distance (km)") +
#   geom_smooth(method = lm, color = "red", se = F) + 
#   geom_smooth(method = 'loess', span = .5) +
#   geom_text(x = 180, y = 4.65, label = eqn, parse = T)
# 
# ggsave(filename = 'GeneralFigures/Fall-offCurveAllSites.png', dpi = 600, width = 6.5,
#        height = 3.5, units = 'in')

###########################################################################################

# for each period

library(rRJB)
myLibrary(c('sf','tidyverse','lattice'))

# load data
sites <- readRDS('Data/DataMasterPosterior.Rds')
sites$posteriormean <- sites$posteriormean * 100

# convert crs
sites <- st_transform(sites,26912)

# Load centerpoint for analysis
productionCenter <- readRDS('GIS/productionCenter.Rds')
xdist <- st_distance(sites,productionCenter)
distdf <- tibble(ProjectNumber = sites$ProjectNumber,
                 distance = as.vector(xdist)/1000,
                 SJRWP = log(sites$posteriormean),
                 Region = sites$Region,
                 Period = sites$Period)

# equation to plot equation and r-squared

lm_eqn <- function(df){
  m <- lm(y ~ x, df)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = round(coef(m)[1], digits = 2), 
                        b = round(coef(m)[2], digits = 2), 
                        r2 = round(summary(m)$r.squared, digits = 3)))
  
  return(eq)             
}

distdf
period = unique(sort(distdf$Period))
i = 0
# p = period[3]
for(p in period){
  i = i + 1
  sitessub <- distdf %>% filter(Period == p)
  # xyplot(SJRWP ~ distance, data = distdf,
  #        xlab = "Distance (km)",
  #        ylab = "SJRW %"
  # )
  # lm1 <- lm(SJRWP ~ distance, data = distdf)
  # summary(lm1)
  # xyplot(resid(lm1) ~ fitted(lm1),
  # xlab = "Fitted Values",
  # ylab = "Residuals",
  # main = "Residual Diagnostic Plot",
  # panel = function(x, y, ...)
  # {
  #   panel.grid(h = -1, v = -1)
  #   panel.abline(h = 0)
  #   panel.xyplot(x, y, ...)
  # }
  # )
  # qqmath( ~ resid(lm1),
  # xlab = "Theoretical Quantiles",
  # ylab = "Residuals"
  # )
# equation df
    df <- tibble(x = sitessub$distance, y = sitessub$SJRWP)

  # plot
  g <- ggplot(sitessub, aes(x = distance, y = SJRWP)) +
    geom_point(aes(color = Region),size = .5) + 
    theme_bw() + ylab("Log of San Juan Red Ware %") + xlab("Distance (km)") +
    geom_smooth(method = lm, color = "red", se = F, size = .65) + 
    geom_smooth(method = 'loess', span = .5, size = .65) +
    ggtitle(p) + theme(legend.position = "bottom", legend.title = element_blank()) +
    guides(color = guide_legend(override.aes = list(shape = 16, size = 3.5))) +
    labs(caption = lm_eqn(df))
  g
  ggsave(filename = paste0('GeneralFigures/Fall-offCurve-',p,'.png'), dpi = 600, width = 6.5,
         height = 3.5, units = 'in')
  
}



###########################################################################################

# for each period for decorated

library(rRJB)
myLibrary(c('sf','tidyverse','lattice'))

# load data
sites <- readRDS('Data/DataMasterPosterior.Rds')
sites <- sites %>% filter(!is.na(priormeanDec))
sites$posteriormeanDec <- sites$posteriormeanDec * 100

# convert crs
sites <- st_transform(sites,26912)

# Load centerpoint for analysis
productionCenter <- readRDS('GIS/productionCenter.Rds')
xdist <- st_distance(sites,productionCenter)
distdf <- tibble(ProjectNumber = sites$ProjectNumber,
                 distance = as.vector(xdist)/1000,
                 SJRWP = log(sites$posteriormeanDec),
                 Region = sites$Region)


period = unique(sort(sites$Period))
x <- list(a1 = 50, a2 = 160, a3 = 180, a4 = 180, a5 = 180)
y = list(a1 = 0, a2 = 3.65, a3 = 3.3, a4 = 3.3, a5 = 3.9)
i = 0
p = period[3]
for(p in period){
  i = i + 1
  x1 <- x[[i]]
  y1 <- y[[i]]
  sitessub <- sites %>% filter(Period == p)
  xdist <- st_distance(sitessub,productionCenter)
  distdf <- tibble(ProjectNumber = sitessub$ProjectNumber,
                   distance = as.vector(xdist)/1000,
                   SJRWP = log(sitessub$posteriormeanDec),
                   Region = sitessub$Region)
  # xyplot(SJRWP ~ distance, data = distdf,
  #        xlab = "Distance (km)",
  #        ylab = "SJRW %"
  # )
  lm1 <- lm(SJRWP ~ distance, data = distdf)
  summary(lm1)
  # xyplot(resid(lm1) ~ fitted(lm1),
  # xlab = "Fitted Values",
  # ylab = "Residuals",
  # main = "Residual Diagnostic Plot",
  # panel = function(x, y, ...)
  # {
  #   panel.grid(h = -1, v = -1)
  #   panel.abline(h = 0)
  #   panel.xyplot(x, y, ...)
  # }
  # )
  # qqmath( ~ resid(lm1),
  # xlab = "Theoretical Quantiles",
  # ylab = "Residuals"
  # )
  # equation to plot equation and r-squared
  df <- tibble(x = distdf$distance, y = distdf$SJRWP)
  lm_eqn <- function(df){
    m <- lm(y ~ x, df)
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = round(coef(m)[1], digits = 2), 
                          b = round(coef(m)[2], digits = 2), 
                          r2 = round(summary(m)$r.squared, digits = 3)))
    return(eq)             
  }
  
  
  # plot
  g <- ggplot(distdf, aes(x = distance, y = SJRWP)) +
    geom_point(aes(color = Region),size = .5) + 
    theme_bw() + ylab("Log of SJRW %") + xlab("Distance (km)") +
    geom_smooth(method = lm, color = "red", se = F, size = .65) + 
    geom_smooth(method = 'loess', span = .5, size = .65) +
    ggtitle(p) + theme(legend.position = "bottom", legend.title = element_blank()) +
    guides(color = guide_legend(override.aes = list(shape = 16, size = 3.5))) +
    labs(caption = lm_eqn(df))
  g
  ggsave(filename = paste0('GeneralFigures/Fall-offCurve',p,'-decorated.png'), dpi = 600, width = 6.5,
         height = 3.5, units = 'in')

}

