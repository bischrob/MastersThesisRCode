################################################################
# This script is designed for generating fall-off curves
if(!require(pacman)){install.packages("pacman")}
p_load(sf,tidyverse,lattice, ggthemes, broom, gridExtra)

# load data
sites <- readRDS('Data/DataMasterPosterior.Rds') %>% 
  filter(!ProjectNumber %in% c(59,60), DecoratedTotal >= 10) %>% 
  mutate(posteriormeanDec = posteriormeanDec * 100)
sites <-  st_sf(sites, geometry = sites$geometry, crs = 4326) 
sites <- st_transform(sites,26912)

# Load centerpoint for analysis
# changed this to be decorated
productionCenter <- readRDS('GIS/productionCenter.Rds')
xdist <- st_distance(sites,productionCenter)
sites <- sites %>% mutate(distance = as.vector(xdist)/1000,
                          SJRWLog = log(posteriormeanDec))

# lm
periods <- levels(as.factor(sites$Period))
# p = periods[1]
lms <- NULL
for(p in periods){
  df <- sites %>% filter(Period == p)
  lms <- rbind(lms,summary(lm(distance ~ SJRWLog, data = df))$r.squared)
}
png('SAA2019Figures/Fall-offCurve-RSquared.png', bg = NA, res = 300,
    height = 3, width = 3, units = "in")
p <- tableGrob(tibble(Period = periods, `r-squared` = round(lms,3)), rows = NULL)
grid.arrange(p)
dev.off()

# plot
ggplot(sites, aes(x = distance, y = SJRWLog)) +
  geom_point(aes(color = Region)) +
  theme_bw() + ylab("Log of San Juan Red Ware %") + xlab("Distance (km)") +
  # geom_smooth(method = lm, color = "red", se = F) 
  geom_smooth(method = 'loess', span = .5, se = F) +
  theme_gdocs() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 2)) +
  facet_wrap(~Period, nrow = 3)
# geom_text(x = 180, y = 4.65, label = eqn, parse = T)

ggsave(filename = 'SAA2019Figures/Fall-offCurve-3row.png', dpi = 600, width = 16,
       height = 7, units = 'in')
