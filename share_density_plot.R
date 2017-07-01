library(tidyverse)
library(ggthemes)
library(cowplot)

library(ggplot2)
library(dplyr)
cropdat1 <- readRDS("data/full_ag_data.rds")
cropdat1 <- filter(cropdat1, year >= 1960 & year < 1970)
cropdat1 <- filter(cropdat1, year >= 2000 & year < 2010)

plots <- list()

# Crop rev
cropdat1$corn_rev <- (cropdat1$corn_grain_p*cropdat1$corn_rprice)/cropdat1$corn_grain_a
cropdat1$cotton_rev <- (cropdat1$cotton_p*cropdat1$cotton_rprice)/cropdat1$cotton_a
cropdat1$hay_rev <- (cropdat1$hay_p*cropdat1$hay_rprice)/cropdat1$hay_a
cropdat1$wheat_rev <- (cropdat1$wheat_p*cropdat1$wheat_rprice)/cropdat1$wheat_a
cropdat1$soybean_rev <- (cropdat1$soybean_p*cropdat1$soybean_rprice)/cropdat1$soybean_a
cropdat1$cropland <- cropdat1$corn_grain_a + cropdat1$cotton_a + cropdat1$hay_a + cropdat1$wheat_a + cropdat1$soybean_a

# Remove inf to na
is.na(cropdat1) <- do.call(cbind, lapply(cropdat1, is.infinite))

corn_dat <- filter(cropdat1, !is.na(corn_rev) & !is.na(tavg))
cotton_dat <- filter(cropdat1, !is.na(cotton_rev) & !is.na(tavg))
hay_dat <- filter(cropdat1, !is.na(hay_rev) & !is.na(tavg))
wheat_dat <- filter(cropdat1, !is.na(wheat_rev) & !is.na(tavg))
soybean_dat <- filter(cropdat1, !is.na(soybean_rev) & !is.na(tavg))
cropland_dat <- filter(cropdat1, !is.na(cropland) & !is.na(cropland))

sum.corn <- sum(corn_dat$corn_rev)
sum.cotton <- sum(cotton_dat$cotton_rev)
sum.hay <- sum(hay_dat$hay_rev)
sum.wheat <- sum(wheat_dat$wheat_rev)
sum.soybean <- sum(soybean_dat$soybean_rev)
sum.all <- sum.corn + sum.cotton + sum.hay + sum.wheat + sum.soybean

dens.corn1 <- density(corn_dat$tavg, weight = corn_dat$corn_rev/sum.all, from = 0, to = 25, n = 30)
dens.cotton1 <- density(cotton_dat$tavg, weight = cotton_dat$cotton_rev/sum.all, from = 0, to = 25, n = 30)
dens.hay1 <- density(hay_dat$tavg, weight = hay_dat$hay_rev/sum.all, from = 0, to = 25, n = 30)
dens.wheat1 <- density(wheat_dat$tavg, weight = wheat_dat$wheat_rev/sum.all, from = 0, to = 25, n = 30)
dens.soybean1 <- density(soybean_dat$tavg, weight = soybean_dat$soybean_rev/sum.all, from = 0, to = 25, n = 30)

dens.wheat1$y <- dens.wheat1$y + dens.cotton1$y
dens.hay1$y <- dens.hay1$y + dens.wheat1$y
dens.soybean1$y <- dens.hay1$y + dens.soybean1$y
dens.corn1$y <- dens.corn1$y + dens.soybean1$y

plot1 <- ggplot(NULL, aes(x = dens.corn1$x, y = dens.corn1$y)) + 
  geom_polygon(aes(x = dens.corn1$x, y = dens.corn1$y, fill = "corn")) +
  geom_polygon(aes(x = dens.soybean1$x, y = dens.soybean1$y, fill = "soybean")) + 
  geom_polygon(aes(x = dens.hay1$x, y = dens.hay1$y, fill = "hay")) + 
  geom_polygon(aes(x = dens.wheat1$x, y = dens.wheat1$y, fill = "wheat")) +
  geom_polygon(aes(x = dens.cotton1$x, y = dens.cotton1$y, fill = "cotton")) + 
  geom_line(aes(x = dens.cropland1$x, y = dens.cropland1$y)) +
  xlab("Average Temp (C)") + ylab("Share of Value per Acre") +  
    annotate("text", y = 0.09, x = 20, label = "1960's", size = 8) +
  scale_fill_discrete(breaks = c("corn", "soybean", "hay", "wheat", "cotton")) 
plot1


#----------------------
# 2000's

cropdat2 <- readRDS("data/full_ag_data.rds")
cropdat2 <- filter(cropdat2, year >= 2000 & year < 2010)

# Crop rev
cropdat2$corn_rev <- (cropdat2$corn_grain_p*cropdat2$corn_rprice)/cropdat2$corn_grain_a
cropdat2$cotton_rev <- (cropdat2$cotton_p*cropdat2$cotton_rprice)/cropdat2$cotton_a
cropdat2$hay_rev <- (cropdat2$hay_p*cropdat2$hay_rprice)/cropdat2$hay_a
cropdat2$wheat_rev <- (cropdat2$wheat_p*cropdat2$wheat_rprice)/cropdat2$wheat_a
cropdat2$soybean_rev <- (cropdat2$soybean_p*cropdat2$soybean_rprice)/cropdat2$soybean_a

# Remove inf to na
is.na(cropdat2) <- do.call(cbind, lapply(cropdat2, is.infinite))

corn_dat <- filter(cropdat2, !is.na(corn_rev) & !is.na(tavg))
cotton_dat <- filter(cropdat2, !is.na(cotton_rev) & !is.na(tavg))
hay_dat <- filter(cropdat2, !is.na(hay_rev) & !is.na(tavg))
wheat_dat <- filter(cropdat2, !is.na(wheat_rev) & !is.na(tavg))
soybean_dat <- filter(cropdat2, !is.na(soybean_rev) & !is.na(tavg))

sum.corn <- sum(corn_dat$corn_rev)
sum.cotton <- sum(cotton_dat$cotton_rev)
sum.hay <- sum(hay_dat$hay_rev)
sum.wheat <- sum(wheat_dat$wheat_rev)
sum.soybean <- sum(soybean_dat$soybean_rev)
sum.all <- sum.corn + sum.cotton + sum.hay + sum.wheat + sum.soybean

dens.corn <- density(corn_dat$tavg, weight = corn_dat$corn_rev/sum.all, from = 0, to = 25, n = 30)
dens.cotton <- density(cotton_dat$tavg, weight = cotton_dat$cotton_rev/sum.all, from = 0, to = 25, n = 30)
dens.hay <- density(hay_dat$tavg, weight = hay_dat$hay_rev/sum.all, from = 0, to = 25, n = 30)
dens.wheat <- density(wheat_dat$tavg, weight = wheat_dat$wheat_rev/sum.all, from = 0, to = 25, n = 30)
dens.soybean <- density(soybean_dat$tavg, weight = soybean_dat$soybean_rev/sum.all, from = 0, to = 25, n = 30)

dens.wheat$y <- dens.wheat$y + dens.cotton$y
dens.hay$y <- dens.hay$y + dens.wheat$y
dens.soybean$y <- dens.hay$y + dens.soybean$y
dens.corn$y <- dens.corn$y + dens.soybean$y

plot2 <- ggplot(NULL, aes(x = dens.corn$x, y = dens.corn$y)) + 
  geom_polygon(aes(x = dens.corn$x, y = dens.corn$y, fill = "corn")) +
  geom_polygon(aes(x = dens.soybean$x, y = dens.soybean$y, fill = "soybean")) + 
  geom_polygon(aes(x = dens.hay$x, y = dens.hay$y, fill = "hay")) + 
  geom_polygon(aes(x = dens.wheat$x, y = dens.wheat$y, fill = "wheat")) +
  geom_polygon(aes(x = dens.cotton$x, y = dens.cotton$y, fill = "cotton")) + 
  xlab("Average Temp (C)") + ylab("Share of Value per Acre") + ylim(0, 0.12) +
  annotate("text", y = 0.09, x = 20, label = "2000's", size = 8) +
  scale_fill_discrete(breaks = c("corn", "soybean", "hay", "wheat", "cotton"))
plot2

plot_grid(plot1, plot2, ncol = 1)

# 
#  plot_grid(p1, p2,ncol = 1)

# dens <- data.frame(x = c(dens.soybean$x, dens.wheat$x, dens.hay$x, dens.cotton$x, dens.corn$x),
#                    y = c(dens.soybean$y, dens.wheat$y, dens.hay$y, dens.cotton$y, dens.corn$y),
#                    crop = rep(c("soybean", "wheat", "hay", "cotton", "corn", each = 30)))
# 
# ggplot(dens, aes(x = x, y = y, fill = crop)) + geom_line(alpha = 0.3) +
#            scale_fill_manual(values = c('red','blue','green', 'yellow', 'pink', 'grey')) 
