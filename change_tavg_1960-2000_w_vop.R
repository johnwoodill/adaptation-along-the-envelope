library(ggplot2)
library(dplyr)

# Load data set
cropdat <- readRDS("data/full_ag_data.rds")

newdat <- filter(cropdat, year >= 1960)
newdat <- filter(newdat, abs(long) <= 100)
bal <- c(1960, 2000)

newdat$corn_rev <- newdat$corn_grain_p*newdat$corn_rprice
newdat$cotton_rev <- newdat$cotton_p*newdat$cotton_rprice
newdat$hay_rev <- newdat$hay_p*newdat$hay_rprice
newdat$wheat_rev <- newdat$wheat_p*newdat$wheat_rprice
newdat$soybean_rev <- newdat$soybean_p*newdat$soybean_rprice

newdat$total_rev <- rowSums(newdat[,68:72], na.rm = TRUE)

aggdat <- data.frame()

for (i in bal){
  sub_cropdat <- filter(newdat, year >= i & year < i+9)
  ag_cropdat <- sub_cropdat %>% 
    group_by(state, fips) %>% 
    summarise(corn_rev = mean(corn_grain_p*corn_nprice, na.rm = TRUE),
              total_rev = mean(total_rev, na.rm = TRUE),
              tavg = mean(tavg, na.rm = TRUE))
  #ag_cropdat <- filter(ag_cropdat, !is.na(corn_rev) & !is.na(tavg))
  ag_cropdat <- filter(ag_cropdat, !is.na(total_rev) & !is.na(tavg))
  #ag_cropdat$corn_w <- ag_cropdat$corn_rev/sum(ag_cropdat$corn_rev, na.rm = TRUE)
  ag_cropdat$total_w <- ag_cropdat$total_rev/sum(ag_cropdat$total_rev, na.rm = TRUE)
  
  #dens <- density(ag_cropdat$tavg, na.rm = TRUE, weights = ag_cropdat$corn_w)
  dens <- density(ag_cropdat$tavg, na.rm = TRUE, weights = ag_cropdat$total_w)
  #dens <- hist(ag_cropdat$tavg, na.rm = TRUE, weights = ag_cropdat$corn_w)
  #mergedat <- data.frame(x = dens$x, y = dens$y, year = i)
  #mergedat <- data.frame(corn_rev = ag_cropdat$corn_rev, 
                         #corn_w = ag_cropdat$corn_w, tavg = ag_cropdat$tavg, year = i)
  # mergedat <- data.frame(total_rev = ag_cropdat$total_rev, 
  #                        total_w = ag_cropdat$total_w, tavg = ag_cropdat$tavg, year = i)
  mergedat <- data.frame(x = dens$x, y = dens$y, year = paste(i, "'s", sep = ""))
  aggdat <- rbind(aggdat, mergedat)
  }

aggdat$year <- as.factor(aggdat$year)
ggplot(aggdat, aes(x, y, color = year, group = year)) + 
  geom_line()+ annotate("text", x = 5, y = .18, label = "tavg = 9.88C")+ 
  annotate("text", x = 15, y = .18, label = "tavg = 10.41C") + 
  ylab("Value of Activity") + xlab("Average Temp") + 
  ggtitle("Total Crop Change in Average Temperature \n (weighted by Value of Activity)")
#ggplot(aggdat, aes(tavg, weight = corn_w, color = year)) + geom_histogram()
#ggplot(aggdat, aes(tavg, weight = total_w, color = year)) + geom_histogram() 
