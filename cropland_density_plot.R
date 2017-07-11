library(tidyverse)
library(ggthemes)

cropdat <- readRDS("data/full_ag_data.rds")
cropdat1960 <- filter(cropdat, year >= 1960 & year < 1970)

cropdat1960$cropland <- cropdat1960$corn_grain_a + cropdat1960$cotton_a + cropdat1960$hay_a + cropdat1960$wheat_a + cropdat1960$soybean_a
cropdat1960 <- filter(cropdat1960, !is.na(tavg) & !is.na(cropland))
dens.cropland1960 <- density(cropdat1960$tavg, weight = cropdat1960$cropland/sum(cropdat1960$cropland))
plot(dens.cropland1960)

cropdat2000 <- filter(cropdat, year >= 2000 & year < 2010)

cropdat2000$cropland <- cropdat2000$corn_grain_a + cropdat2000$cotton_a + cropdat2000$hay_a + cropdat2000$wheat_a + cropdat2000$soybean_a
cropdat2000 <- filter(cropdat2000, !is.na(tavg) & !is.na(cropland))
dens.cropland2000 <- density(cropdat2000$tavg, weight = cropdat2000$cropland/sum(cropdat2000$cropland))
plot(dens.cropland2000)
lines(dens.cropland1960)

pd <- ggplot(NULL) + geom_line(aes(x = dens.cropland1960$x, y = dens.cropland1960$y, color = "1960's")) + 
  geom_line(aes(x = dens.cropland2000$x, y = dens.cropland2000$y, color = "2000's")) + ylab("Cropland") + xlab("Average Temp") +
  theme_tufte() + ggtitle("Density of Average Temperature Weighted by Cropland")
  
ggsave("figures/cropland_density.png", pd)
