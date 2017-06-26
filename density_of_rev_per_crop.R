library(dplyr)
library(ggplot2)
library(ggthemes)
library(hexbin)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)

cropdat$corn_rev <- cropdat$corn_grain_p*cropdat$corn_rprice
cropdat$cotton_rev <- cropdat$cotton_p*cropdat$cotton_rprice
cropdat$hay_rev <- cropdat$hay_p*cropdat$hay_rprice
cropdat$wheat_rev <- cropdat$wheat_p*cropdat$wheat_rprice
cropdat$soybean_rev <- cropdat$soybean_p*cropdat$soybean_rprice

cropdat$total_rev <- cropdat$corn_rev + cropdat$cotton_rev + cropdat$hay_rev + cropdat$wheat_rev + cropdat$soybean_rev
cropdat$total_a <- cropdat$corn_grain_a + cropdat$cotton_a + cropdat$hay_a + cropdat$wheat_a + cropdat$soybean_a

revdat <- cropdat %>% 
  summarise(corn_rev = sum(corn_rev, na.rm = TRUE),
            cotton_rev = sum(cotton_rev, na.rm = TRUE),
            hay_rev = sum(hay_rev, na.rm = TRUE),
            wheat_rev = sum(wheat_rev, na.rm = TRUE),
            soybean_rev = sum(soybean_rev, na.rm = TRUE),
            total_rev = corn_rev + cotton_rev + hay_rev + wheat_rev + soybean_rev)

cornp <- revdat$corn_rev/revdat$total_rev
cottonp <- revdat$cotton_rev/revdat$total_rev
hayp <- revdat$hay_rev/revdat$total_rev
wheatp <- revdat$wheat_rev/revdat$total_rev  
soybeanp <- revdat$soybean_rev/revdat$total_rev  

sum(cornp, cottonp, hayp, wheatp, soybeanp)

# Revenue Density
{
cornd <- select(cropdat, corn_rev, corn_grain_a)
cornd$corn_rev <- remove_outliers(cornd$corn_rev)
cornd <- filter(cornd, !is.na(corn_rev) & !is.na(corn_grain_a))
corn_density <- density(cornd$corn_rev/cornd$corn_grain_a)
corn_density <- data.frame(x = corn_density$x, y = corn_density$y, type = "corn")
corn_density$y <- corn_density$y*cornp
plot(corn_density$x, corn_density$y, type = "l")

cottond <- select(cropdat, cotton_rev, cotton_a)
cottond$cotton_rev <- remove_outliers(cottond$cotton_rev)
cottond$cotton_rev_peracre <- cottond$cotton_rev/cottond$cotton_a
cottond <- filter(cottond, !is.na(cotton_rev) & !is.na(cotton_a))
cottond <- filter(cottond, !is.na(cotton_rev_peracre))
cotton_density <- density(cottond$cotton_rev_peracre)
cotton_density <- data.frame(x = cotton_density$x, y = cotton_density$y, type = "cotton")
cotton_density$y <- cotton_density$y*cottonp
plot(cotton_density$x, cotton_density$y, type = "l")

hayd <- select(cropdat, hay_rev, hay_a)
hayd$hay_rev <- remove_outliers(hayd$hay_rev)
hayd$hay_rev_peracre <- hayd$hay_rev/hayd$hay_a
hayd <- filter(hayd, !is.na(hay_rev) & !is.na(hay_a))
hayd <- filter(hayd, !is.na(hay_rev_peracre))
hay_density <- density(hayd$hay_rev_peracre)
hay_density <- data.frame(x = hay_density$x, y = hay_density$y, type = "hay")
hay_density$y <- hay_density$y*hayp
plot(hay_density$x, hay_density$y, type = "l")

wheatd <- select(cropdat, wheat_rev, wheat_a)
wheatd$wheat_rev <- remove_outliers(wheatd$wheat_rev)
wheatd$wheat_rev_peracre <- wheatd$wheat_rev/wheatd$wheat_a
wheatd <- filter(wheatd, !is.na(wheat_rev) & !is.na(wheat_a))
wheatd <- filter(wheatd, !is.na(wheat_rev_peracre))
wheat_density <- density(wheatd$wheat_rev_peracre)
wheat_density <- data.frame(x = wheat_density$x, y = wheat_density$y, type = "wheat")
wheat_density$y <- wheat_density$y*wheatp
plot(wheat_density$x, wheat_density$y, type = "l")

soybeand <- select(cropdat, soybean_rev, soybean_a)
soybeand$soybean_rev <- remove_outliers(soybeand$soybean_rev)
soybeand$soybean_rev_peracre <- soybeand$soybean_rev/soybeand$soybean_a
soybeand <- filter(soybeand, !is.na(soybean_rev) & !is.na(soybean_a))
soybeand <- filter(soybeand, !is.na(soybean_rev_peracre))
soybean_density <- density(soybeand$soybean_rev_peracre)
soybean_density <- data.frame(x = soybean_density$x, y = soybean_density$y, type = "soybean")
soybean_density$y <- soybean_density$y*soybeanp
plot(soybean_density$x, soybean_density$y, type = "l")

# Density weighted by production*acreage*price ()
dens <- rbind(corn_density, wheat_density, cotton_density, hay_density, soybean_density)
dens <- filter(dens, x <= 500)
ggplot(dens, aes(x, y, color = type)) + geom_line() + xlab("Revenue per Acre") + ggtitle(paste("Densities of Revenue per crop")) + ylab("Density") + theme_tufte()
}

###################################################
# By individual crop change in years
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1960)
cropdat$corn_rev <- cropdat$corn_grain_p*cropdat$corn_rprice
cropdat$cotton_rev <- cropdat$cotton_p*cropdat$cotton_rprice
cropdat$hay_rev <- cropdat$hay_p*cropdat$hay_rprice
cropdat$wheat_rev <- cropdat$wheat_p*cropdat$wheat_rprice
cropdat$soybean_rev <- cropdat$soybean_p*cropdat$soybean_rprice
cropdat$total_rev <- cropdat$corn_rev + cropdat$cotton_rev + cropdat$hay_rev + cropdat$wheat_rev + cropdat$soybean_rev
cropdat$total_a <- cropdat$corn_grain_a + cropdat$cotton_a + cropdat$hay_a + cropdat$wheat_a + cropdat$soybean_a

# revdat <- cropdat %>% 
#   summarise(corn_rev = sum(corn_rev, na.rm = TRUE),
#             cotton_rev = sum(cotton_rev, na.rm = TRUE),
#             hay_rev = sum(hay_rev, na.rm = TRUE),
#             wheat_rev = sum(wheat_rev, na.rm = TRUE),
#             soybean_rev = sum(soybean_rev, na.rm = TRUE),
#             total_rev = corn_rev + cotton_rev + hay_rev + wheat_rev + soybean_rev)
# 
# cornp <- revdat$corn_rev/revdat$total_rev
# cottonp <- revdat$cotton_rev/revdat$total_rev
# hayp <- revdat$hay_rev/revdat$total_rev
# wheatp <- revdat$wheat_rev/revdat$total_rev  
# soybeanp <- revdat$soybean_rev/revdat$total_rev  

dat_60 <- filter(cropdat, year >= 1960 & year < 1970)
dat_60 <- select(dat_60, corn_rev, corn_grain_a, tavg)
dat_60$corn_rev <- remove_outliers(dat_60$corn_rev)
dat_60 <- filter(dat_60, !is.na(corn_rev) & !is.na(corn_grain_a) & !is.na(tavg))
corn_60_density <- density(dat_60$corn_rev/dat_60$corn_grain_a, weights = dat_60$tavg/sum(dat_60$tavg))
corn_60_density <- data.frame(x = corn_60_density$x, y = corn_60_density$y, type = 1960)
#corn_density$y <- corn_density$y*cornp
plot(corn_60_density$x, corn_60_density$y, type = "l")


dat_00 <- filter(cropdat, year >= 2000 & year < 2010)
dat_00 <- select(dat_00, corn_rev, corn_grain_a, tavg)
dat_00$corn_rev <- remove_outliers(dat_00$corn_rev)
dat_00 <- filter(dat_00, !is.na(corn_rev) & !is.na(corn_grain_a) & !is.na(tavg))
corn_00_density <- density(dat_00$corn_rev/dat_00$corn_grain_a, weights = dat_00$tavg/sum(dat_00$tavg))
corn_00_density <- data.frame(x = corn_00_density$x, y = corn_00_density$y, type = 2000)
#corn_density$y <- corn_density$y*cornp
#plot(corn_density$x, corn_density$y, type = "l")

corn_density <- rbind(corn_60_density, corn_00_density)
corn_density$type <- as.factor(corn_density$type)
ggplot(corn_density, aes(x, y, color = type)) + geom_line()

# Histogram changes
hdat <- filter(cropdat, year >= 1960 & year <= 2009)
cropdat <- filter(cropdat, abs(long) <= 100)

# Get tavg weights
hdat <- hdat %>% 
  group_by(year) %>% 
  mutate(w = tavg/mean(tavg, na.rm = TRUE))

hdat <- hdat %>% 
  group_by(year) %>% 
  mutate(w = (total_rev/total_a)/sum(total_rev/total_a, na.rm = TRUE))

hdat$year <- (hdat$year%/%10)*10
hdat$year <- as.factor(hdat$year)
#hdat$type <- as.integer(substr(hdat$year, 3,3))

#hdat <- filter(hdat, type == 6 | type == 0)
#hdat$type <- ifelse(hdat$type == 6, 1960, 2000)
hdat$type <- as.factor(hdat$type)
ggplot(hdat, aes(corn_rev/corn_grain_a, color = type, weight = w)) + geom_freqpoly() + theme_tufte()
ggplot(hdat, aes(cotton_rev/cotton_a, color = type)) + geom_freqpoly() + theme_tufte()
ggplot(hdat, aes(hay_rev/hay_a, color = type)) + geom_freqpoly() + theme_tufte()
ggplot(hdat, aes(wheat_rev/wheat_a, color = type)) + geom_freqpoly() + theme_tufte()
ggplot(hdat, aes(soybean_rev/soybean_a, color = type)) + geom_freqpoly() + theme_tufte()
ggplot(hdat, aes(total_rev/total_a, color = type)) + geom_freqpoly() + theme_tufte()
ggplot(hdat, aes(total_rev/total_a, fill = year)) + geom_histogram(bins = 100) + theme_tufte() + ggtitle("Total Revenue per Acre from 1960-2000") + xlab("Total Revenue/Acre") + ylab(NULL)
ggplot(hdat, aes(total_rev/total_a, fill = year)) + geom_histogram(bins = 100) + theme_tufte() + ggtitle("Total Revenue per Acre from 1960-2010 for U.S. counties east of 100th meridian") + xlab("Total Revenue/Acre") + ylab(NULL)

ggplot(hdat, aes(tavg, color = year, weight = w)) + geom_freqpoly() + theme_tufte() + ggtitle("Total Revenue per Acre from 1960-2010 for U.S. counties east of 100th meridian") + xlab("Total Revenue/Acre") + ylab(NULL)
plot(y = hdat$total_rev/hdat$total_a, x = hdat$tavg)
ggplot(hdat, aes(total_rev/hdat$total_a, x = tavg, color = year)) + geom_point()
ggplot (hdat, aes (x = tavg, y = total_rev/hdat$total_a)) + geom_hex() + geom_vline(aes(xintercept = 16), linetype = "dotted") + scale_x_continuous(breaks = c(12, 14, 16, 18, 20, 22)) + facet_grid (. ~ year) + xlab("Average Temperature") + ylab("Observations")
ggplot (hdat, aes (x = tavg, y = hdat$total_rev/hdat$total_a)) + geom_hex() + geom_vline(aes(xintercept = 16), linetype = "dotted") + scale_x_continuous(breaks = c(12, 14, 16, 18, 20, 22)) + facet_grid (. ~ year) + xlab("Average Temperature") + ylab("Revenue per acre")

ggplot (hdat, aes (x = tmax, y = hdat$total_rev/hdat$total_a)) + geom_hex() + geom_vline(aes(xintercept = 22), linetype = "dotted") + scale_x_continuous(breaks = c(12, 14, 16, 18, 20, 22)) + facet_grid (. ~ year) + xlab("Average Temperature") + ylab("Revenue per acre")
ggplot (hdat, aes (x = dday8C-dday30C, y = hdat$total_rev/hdat$total_a)) + geom_hex() + facet_grid (. ~ year) + xlab("Extreme Degree Days") + ylab("Revenue per acre")
ggplot (hdat, aes (x = dday34C, y = hdat$total_rev/hdat$total_a)) + geom_hex() + facet_grid (. ~ year) + xlab("Extreme Degree Days (> 34)") + ylab("Revenue per acre")

#ggplot (hdat, aes (x = tavg, y = hdat$total_rev)) + geom_hex() + geom_vline(aes(xintercept = 16), linetype = "dotted") + scale_x_continuous(breaks = c(12, 14, 16, 18, 20, 22)) + facet_grid (. ~ year) + xlab("Average Temperature") + ylab("Observations")
