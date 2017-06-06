library(dplyr)
library(ggplot2)

# Location to download full data set full_ag_data.rds (108.2MB) 
# https://mega.nz/#!rBADyAJB!LXcI1A7ktAs4jiW_jCHADmDYVRL41w93TVTk5_VBNEM

# Load data set
cropdat <- readRDS("data/full_ag_data.rds")

# Select all agg data and tavg temp
newcropdat <- cropdat[,c(1:23, 64)]

# Filter years >= 1960
newcropdat <- filter(newcropdat, year >= 1960)
bal <- c(1960, 1970, 1980, 1990, 2000)

# Aggregated data frame after 10-year intervals
aggdat <- data.frame()

# Loops through each 10 year interval
for (i in bal){

# Filter by 10-year intervals
sub_cropdat <- filter(cropdat, year >= i & year <= i+9)

# Aggregate averages to 10 years
ag_cropdat <- sub_cropdat %>% 
  group_by(state, fips) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup()

# Set base year in data set
ag_cropdat$year <- i

# Build weights
ag_cropdat$corn_w1 <- ag_cropdat$corn_grain_p*ag_cropdat$corn_grain_a
ag_cropdat$corn_w2 <- ag_cropdat$corn_grain_p*ag_cropdat$corn_grain_a*ag_cropdat$corn_price
ag_cropdat$cotton_w1 <- ag_cropdat$cotton_p*ag_cropdat$cotton_a
ag_cropdat$cotton_w2 <- ag_cropdat$cotton_p*ag_cropdat$cotton_a*ag_cropdat$cotton_price
ag_cropdat$wheat_w1 <- ag_cropdat$wheat_p*ag_cropdat$wheat_a
ag_cropdat$wheat_w2 <- ag_cropdat$wheat_p*ag_cropdat$wheat_a*ag_cropdat$wheat_price
ag_cropdat$hay_w1 <- ag_cropdat$hay_p*ag_cropdat$hay_a
ag_cropdat$hay_w2 <- ag_cropdat$hay_p*ag_cropdat$hay_a*ag_cropdat$hay_price
ag_cropdat$soybean_w1 <- ag_cropdat$soybean_p*ag_cropdat$soybean_a
ag_cropdat$soybean_w2 <- ag_cropdat$soybean_p*ag_cropdat$soybean_a*ag_cropdat$soybean_price

# Bind together
aggdat <- rbind(aggdat, ag_cropdat)
}

ag_cropdat <- aggdat

# Crop Density tavg weigth = w2 (production*acres*price)
{
cornd <- select(ag_cropdat, tavg, corn_w2)
cornd <- filter(cornd, !is.na(tavg) & !is.na(corn_w2))
corn_density <- density(cornd$tavg, weights = cornd$corn_w2/sum(cornd$corn_w2), na.rm = TRUE, bw = 1)
corn_density <- data.frame(x = corn_density$x, y = corn_density$y, type = "corn")

cottond <- select(ag_cropdat, tavg, cotton_w2)
cottond <- filter(cottond, !is.na(tavg) & !is.na(cotton_w2))
cotton_density <- density(cottond$tavg, weights = cottond$cotton_w2/sum(cottond$cotton_w2), na.rm = TRUE, bw = 1)
cotton_density <- data.frame(x = cotton_density$x, y = cotton_density$y, type = "cotton")

wheatd <- select(ag_cropdat, tavg, wheat_w2)
wheatd <- filter(wheatd, !is.na(tavg) & !is.na(wheat_w2))
wheat_density <- density(wheatd$tavg, na.rm = TRUE, weights = wheatd$wheat_w2/sum(wheatd$wheat_w2), bw = 1)
wheat_density <- data.frame(x = wheat_density$x, y = wheat_density$y, type = "wheat")

hayd <- select(ag_cropdat, tavg, hay_w2)
hayd <- filter(hayd, !is.na(tavg) & !is.na(hay_w2))
hay_density <- density(hayd$tavg, na.rm = TRUE, weights = hayd$hay_w2/sum(hayd$hay_w2), bw = 1)
hay_density <- data.frame(x = hay_density$x, y = hay_density$y, type = "hay")

soybeansd <- select(ag_cropdat, tavg, soybean_w2)
soybeansd <- filter(soybeansd, !is.na(tavg) & !is.na(soybean_w2))
soybeans_density <- density(soybeansd$tavg, na.rm = TRUE, weights = soybeansd$soybean_w2/sum(soybeansd$soybean_w2), bw = 1)
soybeans_density <- data.frame(x = soybeans_density$x, y = soybeans_density$y, type = "soybeans")
}

# Density weighted by production*acreage*price (1960-2000)
dens <- rbind(corn_density, wheat_density, cotton_density, hay_density, soybeans_density)
ggplot(dens, aes(x, y, color = type)) + geom_line() + xlab("Average Annual Temperature") + xlim(0, 30) + ggtitle(paste("County-level Average Temperatures U.S. - \n Weighted by production*acreage*price (1960-2009)")) + ylab("Density")


# Stacked area chart - Proportion of value of activity
dd <- dens
dd$x <- floor(dd$x)
dd <- dd %>% 
  group_by(type, x) %>% 
  summarise(y = mean(y, na.rm = TRUE))

ggplot(dd, aes(x=x,y=y,group=type,fill=type)) + geom_area(position="fill") + 
  ylab("% of Value of Activity") + xlab("Average Temp") + scale_x_continuous(breaks = c(0,5, 10, 15, 20, 25)) +
  ggtitle("Proportion of Value of Activity for Average Temp - U.S. 1960 - 2010") + theme_classic()

