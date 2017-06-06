library(readr)
library(dplyr)
library(tidyr)
library(rms)
library(maps)
library(stringr)
# 
# data(county.fips)
# county.fips$state <- sapply(str_split(county.fips$polyname, ","),'[',1)
# county.fips$county <- sapply(str_split(county.fips$polyname, ","),'[',2)
# county.fips <- select(county.fips, fips, county, state)
# 
# remove_outliers <- function(x, na.rm = TRUE, ...) {
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
#   H <- 1.5 * IQR(x, na.rm = na.rm)
#   y <- x
#   y[x < (qnt[1] - H)] <- NA
#   y[x > (qnt[2] + H)] <- NA
#   y
# }
# 
# # Extract NASS crop data
# extract_d <- function(x){
#   names(x) <- c("year", "state", "county", "data_item", "value")
#   crop <- x
#   crop$state <- tolower(crop$state)
#   crop$county <- tolower(crop$county)
#   crop$row <- 1:nrow(crop)   # unique identifer
#   crop <- spread(crop, data_item, value = value, fill = NA)
#   crop$row <- NULL
#   #crop <- filter(crop, year >= 1900)
#   #crop <- crop[,c(1,2,4,7,10,13,16)]
#   crop <- crop %>% 
#     group_by(state, county, year) %>% 
#     summarise_each(funs(sum(., na.rm=TRUE))) 
#   return(crop)
# }
# 
# #dd <- read_csv("/run/media/john/1TB/MEGA/Projects/Non-Linear-Temperature-Effects-of-the-Dust-Bowl/Data/fips_degree_days_1900-2013.csv")
# #prec <- read_csv("/run/media/john/1TB/Projects/Non-Linear-Temperature-Effects-of-the-Dust-Bowl/Data/fips_precipitation_1900-2013.csv")
# #dd <- left_join(dd, prec, by = c("fips", "year", "month"))
# 
# # Load crop data (balanced years 1927-2007)
# corn <- read_csv("data/corn_1910-2016.csv")
# corn <- extract_d(corn)
# #corn <- left_join(corn, county.fips, by = c("state", "fips"))
# names(corn) <- c("state", "county", "year", "corn_grain_a", "corn_grain_p")
# 
# cotton <- read_csv("data/cotton_1919-2015.csv")
# cotton <- extract_d(cotton)
# names(cotton) <- c("state", "county", "year" , "cotton_a", "cotton_p")
# 
# hay <- read_csv("data/hay_1919-2008.csv")
# hay <- extract_d(hay)
# names(hay) <- c("state", "county", "year" , "hay_a", "hay_p")
# 
# wheat <- read_csv("data/wheat_1909-2007.csv")
# wheat <- extract_d(wheat)
# names(wheat) <- c("state", "county", "year" , "wheat_a", "wheat_p")
# 
# soybeans <- read_csv("data/soybean_1927-2016.csv")
# soybeans <- extract_d(soybeans)
# names(soybeans) <- c("state", "county", "year" , "soybean_a", "soybean_p")
# 
# # Get all combinations of years and states
# newgrid <- expand.grid(county.fips$fips, 1900:2016, stringsAsFactors = FALSE)
# mergdat <- data.frame(county = county.fips$fips, name = county.fips$state)
# statedat <- select(states, state, name)
# statedat$state <- tolower(statedat$state)
# statedat$name <- tolower(statedat$name)
# mergdat <- left_join(mergdat, statedat, by = c("name"))
# mergdat <- select(mergdat, county, state)
# names(newgrid) <- c("county", "year")
# newgrid <- left_join(newgrid, mergdat, by = "county")
# 
# 
# newgrid$county <- as.character(newgrid$county)
# 
# 
# cropdat <- left_join(newgrid, corn, by = c("state", "county", "year"))
# cropdat <- left_join(cropdat, cotton, by = c("state", "county", "year"))
# cropdat <- left_join(cropdat, hay, by = c("state", "county", "year"))
# cropdat <- left_join(cropdat, wheat, by = c("state", "county", "year"))
# cropdat <- left_join(cropdat, soybeans, by = c("state", "county", "year"))
# 
# names(cropdat)[1] <- "fips"
# cropdat$fips <- as.integer(cropdat$fips)
# 
# # Degree Days
# 
# cropdd <- dd %>% 
#   group_by(fips, year) %>% 
#   summarise(dday30C = sum(dday30C),
#             tavg = mean(tavg))
# 
# cropdat <- left_join(cropdat, cropdd, by = c("year", "fips"))
# cropdat <- as.data.frame(cropdat)
# 
# # State Prices
# prices <- read.csv("data/commodity_statelevel_prices.csv", stringsAsFactors = FALSE)
# cropdat <- left_join(cropdat, prices, by = c("state", "year"))
# 
# # Yield
# 
# cropdat$corn_yield <- cropdat$corn_grain_p/cropdat$corn_grain_a
# cropdat$cotton_yield <- cropdat$cotton_p/cropdat$cotton_a
# cropdat$hay_yield <- cropdat$hay_p/cropdat$hay_a
# cropdat$wheat_yield <- cropdat$wheat_p/cropdat$wheat_a
# cropdat$soybeans_yield <- cropdat$soybean_p/cropdat$soybean_a
# 
# # Adjust prices based on production level
# 
# # Corn in price received measured in $/BU (OK)
# # Cotton (upland) in prie received measured in $/LB (Production measured in 480 lb bales)
# cropdat$cotton_adj_price <- cropdat$cotton_price*480
# # Hay price received measured in $/TON (Production measured in tons) (OK)
# # Soybeans price received measured in $/BU (Production measured in BU) (OK)
# # Wheat price received measured in $/BU (Production measured in BU) (OK)
# 
# # Save
# saveRDS(cropdat, "data/cropdat.rds")
# 
# cropdat <- readRDS("data/cropdat.rds")
# 

cropdat <- readRDS("data/full_ag_data.rds")

# Balanced years
bal <- c(1960, 1970, 1980, 1990, 2000)

for (i in bal){

sub_cropdat <- filter(cropdat, year >= i & year <= i+9)

# Average total acreage over 10 years
sub_cropdat <- sub_cropdat %>% 
  group_by(state, fips) %>% 
  mutate(corn_grain_a = sum(corn_grain_a, na.rm = TRUE),
         cotton_a = sum(cotton_a, na.rm = TRUE),
         hay_a = sum(hay_a, na.rm = TRUE),
         wheat_a = sum(wheat_a, na.rm = TRUE),
         soybean_a = sum(soybean_a, na.rm = TRUE))

# Aggregate to 10 years
ag_cropdat <- sub_cropdat %>% 
  group_by(state, fips) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup()

ag_cropdat$year <- NULL

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

# Crop Density tavg weight = w1
cornd <- select(ag_cropdat, tavg, corn_w1, corn_w2)
cornd <- filter(cornd, !is.na(tavg) & !is.na(corn_w1))
corn_density <- density(cornd$tavg, weights = cornd$corn_w1/sum(cornd$corn_w1), na.rm = TRUE, bw = 2)
corn_density <- data.frame(x = corn_density$x, y = corn_density$y, type = "corn")

cottond <- select(ag_cropdat, tavg, cotton_w1, cotton_w2)
cottond <- filter(cottond, !is.na(tavg) & !is.na(cotton_w1))
cotton_density <- density(cottond$tavg, weights = cottond$cotton_w1/sum(cottond$cotton_w1), na.rm = TRUE, bw = 2)
cotton_density <- data.frame(x = cotton_density$x, y = cotton_density$y, type = "cotton")

wheatd <- select(ag_cropdat, tavg, wheat_w1, wheat_w2)
wheatd <- filter(wheatd, !is.na(tavg) & !is.na(wheat_w1))
wheat_density <- density(wheatd$tavg, na.rm = TRUE, weights = wheatd$wheat_w1/sum(wheatd$wheat_w1), bw = 2)
wheat_density <- data.frame(x = wheat_density$x, y = wheat_density$y, type = "wheat")

hayd <- select(ag_cropdat, tavg, hay_w1, hay_w2)
hayd <- filter(hayd, !is.na(tavg) & !is.na(hay_w1))
hay_density <- density(hayd$tavg, na.rm = TRUE, weights = hayd$hay_w1/sum(hayd$hay_w1), bw = 2)
hay_density <- data.frame(x = hay_density$x, y = hay_density$y, type = "hay")

soybeansd <- select(ag_cropdat, tavg, soybean_w1, soybean_w2)
soybeansd <- filter(soybeansd, !is.na(tavg) & !is.na(soybean_w1))
soybeans_density <- density(soybeansd$tavg, na.rm = TRUE, weights = soybeansd$soybean_w1/sum(soybeansd$soybean_w1), bw = 2)
soybeans_density <- data.frame(x = soybeans_density$x, y = soybeans_density$y, type = "soybeans")

density <- rbind(corn_density, wheat_density, cotton_density, hay_density, soybeans_density)
ggplot(density, aes(x, y, color = type)) + geom_line() + xlab("Average Annual Temperature") + xlim(0, 30) + ggtitle(paste("County-level Average Temperatures U.S. - Weighted by production*acreage (", i, "-", i+9,")", sep = "")) + ylab("Density")
filename <- paste("cropdensity_tavg_w1_", i, "-", i+9, ".png", sep = "")
ggsave(filename, path = "figures/")

# Crop Density tavg weigth = w2
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

dens <- rbind(corn_density, wheat_density, cotton_density, hay_density, soybeans_density)
ggplot(dens, aes(x, y, color = type)) + geom_line() + xlab("Average Annual Temperature") + xlim(0, 30) + ggtitle(paste("County-level Average Temperatures U.S. - Weighted by production*acreage*price (", i, "-", i+9,")", sep = "")) + ylab("Density")
filename <- paste("cropdensity_tavg_w2_", i, "-", i+9, ".png", sep = "")
ggsave(filename, path = "figures/")

# Crop Density dday30C weight = w1
cornd <- select(ag_cropdat, dday30C, corn_w1)
cornd <- filter(cornd, !is.na(dday30C) & !is.na(corn_w1))
corn_density <- density(cornd$dday30C, weights = cornd$corn_w1/sum(cornd$corn_w1), na.rm = TRUE, bw = 2)
corn_density <- data.frame(x = corn_density$x, y = corn_density$y, type = "corn")

cottond <- select(ag_cropdat, dday30C, cotton_w1)
cottond <- filter(cottond, !is.na(dday30C) & !is.na(cotton_w1))
cotton_density <- density(cottond$dday30C, weights = cottond$cotton_w1/sum(cottond$cotton_w1), na.rm = TRUE, bw = 2)
cotton_density <- data.frame(x = cotton_density$x, y = cotton_density$y, type = "cotton")

wheatd <- select(ag_cropdat, dday30C, wheat_w1)
wheatd <- filter(wheatd, !is.na(dday30C) & !is.na(wheat_w1))
wheat_density <- density(wheatd$dday30C, na.rm = TRUE, weights = wheatd$wheat_w1/sum(wheatd$wheat_w1), bw = 2)
wheat_density <- data.frame(x = wheat_density$x, y = wheat_density$y, type = "wheat")

hayd <- select(ag_cropdat, dday30C, hay_w1)
hayd <- filter(hayd, !is.na(dday30C) & !is.na(hay_w1))
hay_density <- density(hayd$dday30C, na.rm = TRUE, weights = hayd$hay_w1/sum(hayd$hay_w1), bw = 2)
hay_density <- data.frame(x = hay_density$x, y = hay_density$y, type = "hay")

soybeansd <- select(ag_cropdat, dday30C, soybean_w1)
soybeansd <- filter(soybeansd, !is.na(dday30C) & !is.na(soybean_w1))
soybeans_density <- density(soybeansd$dday30C, na.rm = TRUE, weights = soybeansd$soybean_w1/sum(soybeansd$soybean_w1), bw = 2)
soybeans_density <- data.frame(x = soybeans_density$x, y = soybeans_density$y, type = "soybeans")

density <- rbind(corn_density, wheat_density, cotton_density, hay_density, soybeans_density)
ggplot(density, aes(x, y, color = type)) + geom_line() + xlab("Average Annual Temperature") + ggtitle(paste("County-level Degree Days > 30 U.S. - Weighted by production*acreage (", i, "-", i+9,")", sep = "")) + ylab("Density")
filename <- paste("cropdensity_dday30C_w1_", i, "-", i+9, ".png", sep = "")
ggsave(filename, path = "figures/")

# Crop Density dday30C weigth = w2
cornd <- select(ag_cropdat, dday30C, corn_w2)
cornd <- filter(cornd, !is.na(dday30C) & !is.na(corn_w2))
corn_density <- density(cornd$dday30C, weights = cornd$corn_w2/sum(cornd$corn_w2), na.rm = TRUE, bw = 2)
corn_density <- data.frame(x = corn_density$x, y = corn_density$y, type = "corn")

cottond <- select(ag_cropdat, dday30C, cotton_w2)
cottond <- filter(cottond, !is.na(dday30C) & !is.na(cotton_w2))
cotton_density <- density(cottond$dday30C, weights = cottond$cotton_w2/sum(cottond$cotton_w2), na.rm = TRUE, bw = 2)
cotton_density <- data.frame(x = cotton_density$x, y = cotton_density$y, type = "cotton")

wheatd <- select(ag_cropdat, dday30C, wheat_w2)
wheatd <- filter(wheatd, !is.na(dday30C) & !is.na(wheat_w2))
wheat_density <- density(wheatd$dday30C, na.rm = TRUE, weights = wheatd$wheat_w2/sum(wheatd$wheat_w2), bw = 2)
wheat_density <- data.frame(x = wheat_density$x, y = wheat_density$y, type = "wheat")

hayd <- select(ag_cropdat, dday30C, hay_w2)
hayd <- filter(hayd, !is.na(dday30C) & !is.na(hay_w2))
hay_density <- density(hayd$dday30C, na.rm = TRUE, weights = hayd$hay_w2/sum(hayd$hay_w2), bw = 2)
hay_density <- data.frame(x = hay_density$x, y = hay_density$y, type = "hay")

soybeansd <- select(ag_cropdat, dday30C, soybean_w2)
soybeansd <- filter(soybeansd, !is.na(dday30C) & !is.na(soybean_w2))
soybeans_density <- density(soybeansd$dday30C, na.rm = TRUE, weights = soybeansd$soybean_w2/sum(soybeansd$soybean_w2), bw = 2)
soybeans_density <- data.frame(x = soybeans_density$x, y = soybeans_density$y, type = "soybeans")

density <- rbind(corn_density, wheat_density, cotton_density, hay_density, soybeans_density)
ggplot(density, aes(x, y, color = type)) + geom_line() + xlab("Average Annual Temperature") + xlim(0, 30) + ggtitle(paste("County-level Degree Days > 30 U.S. - Weighted by production*acreage*price (", i, "-", i+9,")", sep = "")) + ylab("Density")
filename <- paste("cropdensity_dday30C_w2_", i, "-", i+9, ".png", sep = "")
ggsave(filename, path = "figures/")
}

# Crop density by acres (no weight)
cornd <- select(ag_cropdat, corn_grain_a)
corn_density <- density(cornd$corn_grain_a, na.rm = TRUE)
corn_density <- data.frame(x = corn_density$x, y = corn_density$y, type = "corn")

wheatd <- select(ag_cropdat, wheat_a)
wheat_density <- density(wheatd$wheat_a, na.rm = TRUE)
wheat_density <- data.frame(x = wheat_density$x, y = wheat_density$y, type = "wheat")

cottond <- select(ag_cropdat, cotton_a)
cotton_density <- density(cottond$cotton_a, na.rm = TRUE)
cotton_density <- data.frame(x = cotton_density$x, y = cotton_density$y, type = "cotton")

hayd <- select(ag_cropdat, hay_a)
hay_density <- density(hayd$hay_a, na.rm = TRUE)
hay_density <- data.frame(x = hay_density$x, y = hay_density$y, type = "hay")

soybeansd <- select(ag_cropdat, soybean_a)
soybeans_density <- density(soybeansd$soybean_a, na.rm = TRUE)
soybeans_density <- data.frame(x = soybeans_density$x, y = soybeans_density$y, type = "soybeans")

density <- rbind(corn_density, wheat_density, cotton_density, hay_density, soybeans_density)
ggplot(density, aes(x, y, color = type)) + geom_line() + xlab("Average Annual Temperature") + ggtitle("County-level Acreage") + ylab("Density")

#--------------------------------------------
# Aggregate of entire sample in decades

cropdat <- readRDS("data/cropdat.rds")
cropdat <- read_csv("data/full_ag_data.csv")

# Balanced years
cropdat <- filter(cropdat, year >= 1960)
# Not enough pricing data for cotton <= 1960
bal <- c(1960, 1970, 1980, 1990, 2000)

aggdat <- data.frame()
for (i in bal){

sub_cropdat <- filter(cropdat, year >= i & year <= i+9)

# Aggregate to 10 years
ag_cropdat <- sub_cropdat %>% 
  group_by(state, fips) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup()
    
ag_cropdat$year <- NULL

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
aggdat <- rbind(aggdat, ag_cropdat)
}

saveRDS(aggdat, "data/commodity_1960-2010_complete.rds")
aggdat <- readRDS("data/commodity_1960-2010_complete.rds")

# Crop Density tavg weight = w1
cornd <- select(aggdat, tavg, corn_w1, corn_w2)
cornd <- filter(cornd, !is.na(tavg) & !is.na(corn_w1))
corn_density <- density(cornd$tavg, weights = cornd$corn_w1/sum(cornd$corn_w1), na.rm = TRUE, bw = 1)
corn_density <- data.frame(x = corn_density$x, y = corn_density$y, type = "corn")

cottond <- select(aggdat, tavg, cotton_w1, cotton_w2)
cottond <- filter(cottond, !is.na(tavg) & !is.na(cotton_w1))
cotton_density <- density(cottond$tavg, weights = cottond$cotton_w1/sum(cottond$cotton_w1), na.rm = TRUE, bw = 1)
cotton_density <- data.frame(x = cotton_density$x, y = cotton_density$y, type = "cotton")

wheatd <- select(aggdat, tavg, wheat_w1, wheat_w2)
wheatd <- filter(wheatd, !is.na(tavg) & !is.na(wheat_w1))
wheat_density <- density(wheatd$tavg, na.rm = TRUE, weights = wheatd$wheat_w1/sum(wheatd$wheat_w1), bw = 1)
wheat_density <- data.frame(x = wheat_density$x, y = wheat_density$y, type = "wheat")

hayd <- select(aggdat, tavg, hay_w1, hay_w2)
hayd <- filter(hayd, !is.na(tavg) & !is.na(hay_w1))
hay_density <- density(hayd$tavg, na.rm = TRUE, weights = hayd$hay_w1/sum(hayd$hay_w1), bw = 1)
hay_density <- data.frame(x = hay_density$x, y = hay_density$y, type = "hay")

soybeansd <- select(aggdat, tavg, soybean_w1, soybean_w2)
soybeansd <- filter(soybeansd, !is.na(tavg) & !is.na(soybean_w1))
soybeans_density <- density(soybeansd$tavg, na.rm = TRUE, weights = soybeansd$soybean_w1/sum(soybeansd$soybean_w1), bw = 1)
soybeans_density <- data.frame(x = soybeans_density$x, y = soybeans_density$y, type = "soybeans")

density <- rbind(corn_density, wheat_density, cotton_density, hay_density, soybeans_density)
ggplot(density, aes(x, y, color = type)) + geom_line() + xlab("Average Annual Temperature") + xlim(0, 30) + ggtitle("County-level Average Temperatures in U.S. 1960-2010 \n (Weighted by production*acreage)") + ylab("Density (Activity)")
filename <- paste("cropdensity_tavg_w1_1960-2010.png")
ggsave(filename, path = "figures/")

# Crop Density tavg weigth = w2
cornd <- select(aggdat, tavg, corn_w2)
cornd <- filter(cornd, !is.na(tavg) & !is.na(corn_w2))
corn_density <- density(cornd$tavg, weights = cornd$corn_w2/sum(cornd$corn_w2), na.rm = TRUE, bw = 1)
corn_density <- data.frame(x = corn_density$x, y = corn_density$y, type = "corn")

cottond <- select(aggdat, tavg, cotton_w2)
cottond <- filter(cottond, !is.na(tavg) & !is.na(cotton_w2))
cotton_density <- density(cottond$tavg, weights = cottond$cotton_w2/sum(cottond$cotton_w2), na.rm = TRUE, bw = 1)
cotton_density <- data.frame(x = cotton_density$x, y = cotton_density$y, type = "cotton")

wheatd <- select(aggdat, tavg, wheat_w2)
wheatd <- filter(wheatd, !is.na(tavg) & !is.na(wheat_w2))
wheat_density <- density(wheatd$tavg, na.rm = TRUE, weights = wheatd$wheat_w2/sum(wheatd$wheat_w2), bw = 1)
wheat_density <- data.frame(x = wheat_density$x, y = wheat_density$y, type = "wheat")

hayd <- select(aggdat, tavg, hay_w2)
hayd <- filter(hayd, !is.na(tavg) & !is.na(hay_w2))
hay_density <- density(hayd$tavg, na.rm = TRUE, weights = hayd$hay_w2/sum(hayd$hay_w2), bw = 1)
hay_density <- data.frame(x = hay_density$x, y = hay_density$y, type = "hay")

soybeansd <- select(aggdat, tavg, soybean_w2)
soybeansd <- filter(soybeansd, !is.na(tavg) & !is.na(soybean_w2))
soybeans_density <- density(soybeansd$tavg, na.rm = TRUE, weights = soybeansd$soybean_w2/sum(soybeansd$soybean_w2), bw = 1)
soybeans_density <- data.frame(x = soybeans_density$x, y = soybeans_density$y, type = "soybeans")

density <- rbind(corn_density, wheat_density, cotton_density, hay_density, soybeans_density)
ggplot(density, aes(x, y, color = type)) + geom_line() + xlab("Average Annual Temperature") + xlim(0, 30) + ggtitle("County-level Average Temperatures in U.S. 1960-2010 \n (Weighted by production*acreage*price)") + ylab("Density (Value of Activity)")
filename <- paste("cropdensity_tavg_w2_1960-2010.png")
ggsave(filename, path = "figures/")

# Crop density by tavg (weighted by production)
cornd <- select(aggdat, tavg, corn_grain_p)
cornd <- filter(cornd, !is.na(tavg) & !is.na(corn_grain_p))
corn_density <- density(cornd$tavg, na.rm = TRUE, weights = cornd$corn_grain_p/sum(cornd$corn_grain_p), bw=1)
corn_density <- data.frame(x = corn_density$x, y = corn_density$y, type = "corn")

wheatd <- select(aggdat, tavg, wheat_p)
wheatd <- filter(wheatd, !is.na(tavg) & !is.na(wheat_p))
wheat_density <- density(wheatd$tavg, na.rm = TRUE, weights = wheatd$wheat_p/sum(wheatd$wheat_p), bw=1)
wheat_density <- data.frame(x = wheat_density$x, y = wheat_density$y, type = "wheat")

cottond <- select(aggdat, cotton_p, tavg)
cottond <- filter(cottond, !is.na(tavg) & !is.na(cotton_p))
cotton_density <- density(cottond$tavg, na.rm = TRUE, weights = cottond$cotton_p/sum(cottond$cotton_p), bw=1)
cotton_density <- data.frame(x = cotton_density$x, y = cotton_density$y, type = "cotton")

hayd <- select(aggdat, hay_p, tavg)
hayd <- filter(hayd, !is.na(tavg) & !is.na(hay_p))
hay_density <- density(hayd$tavg, na.rm = TRUE, weights = hayd$hay_p/sum(hayd$hay_p), bw=1)
hay_density <- data.frame(x = hay_density$x, y = hay_density$y, type = "hay")

soybeansd <- select(aggdat, soybean_p, tavg)
soybeansd <- filter(soybeansd, !is.na(tavg) & !is.na(soybean_p))
soybeans_density <- density(soybeansd$tavg, na.rm = TRUE, weights = soybeansd$soybean_p/sum(soybeansd$soybean_p), bw=1)
soybeans_density <- data.frame(x = soybeans_density$x, y = soybeans_density$y, type = "soybeans")

density <- rbind(corn_density, wheat_density, cotton_density, hay_density, soybeans_density)
ggplot(density, aes(x, y, color = type)) + geom_line() + xlab("Average Annual Temperature") + ggtitle("County-level Average Temperatures in U.S. 1960-2010 \n (weighted by production)") + ylab("Density")
filename <- paste("cropdensity_production_1960-2010.png")
ggsave(filename, path = "figures/")

# Crop density by acres (no weight)
cornd <- select(aggdat, corn_grain_a)
corn_density <- density(cornd$corn_grain_a, na.rm = TRUE)
corn_density <- hist(cornd$corn_grain_a)
corn_density <- data.frame(x = corn_density$x, y = corn_density$y, type = "corn")

wheatd <- select(aggdat, wheat_a)
wheat_density <- density(wheatd$wheat_a, na.rm = TRUE)
wheat_density <- data.frame(x = wheat_density$x, y = wheat_density$y, type = "wheat")

cottond <- select(aggdat, cotton_a)
cotton_density <- density(cottond$cotton_a, na.rm = TRUE)
cotton_density <- data.frame(x = cotton_density$x, y = cotton_density$y, type = "cotton")

hayd <- select(aggdat, hay_a)
hay_density <- density(hayd$hay_a, na.rm = TRUE)
hay_density <- data.frame(x = hay_density$x, y = hay_density$y, type = "hay")

soybeansd <- select(aggdat, soybean_a)
soybeans_density <- density(soybeansd$soybean_a, na.rm = TRUE)
soybeans_density <- data.frame(x = soybeans_density$x, y = soybeans_density$y, type = "soybeans")

density <- rbind(corn_density, wheat_density, cotton_density, hay_density, soybeans_density)
ggplot(density, aes(x, y, color = type)) + geom_line() + xlab("Average Annual Temperature") + xlim(-20000, 80000) + ggtitle("County-level Acreage in U.S. 1960-2010 \n (unweighted)") + ylab("Density (Acreage)")
filename <- paste("cropdensity_acres_1960-2010.png")
ggsave(filename, path = "figures/")

# Histogram acres (unweighted)
histo <- select(ag_cropdat, corn_grain_a, wheat_a, cotton_a, hay_a, soybean_a)
names(histo) <- c("corn", "wheat", "cotton", "hay", "soybean")
histo <- gather(histo)
ggplot(histo, aes(x=value, fill=key)) + geom_histogram(alpha=0.3, position = "identity")

hist1 <- filter(histo, key %in% c("corn", "cotton", "hay"))
ggplot(hist1, aes(x=value, fill=key)) + geom_histogram(alpha=0.3, position = "identity")
hist2 <- filter(histo, key %in% c("hay", "soybean", "wheat"))
ggplot(hist2, aes(x=value, fill=key)) + geom_histogram(alpha=0.3, position = "identity")
