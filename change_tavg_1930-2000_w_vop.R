library(ggplot2)
library(dplyr)

# Load data set
setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
cropdat <- readRDS("data/full_ag_data.rds")

newdat <- filter(cropdat, year >= 1930 & year <= 2010)
newdat <- filter(newdat, abs(long) <= 100)
bal <- c(1930, 1950, 1970, 2000)

newdat$total_rrev <- rowSums(newdat[, c("corn_rrev", "cotton_rrev", "hay_rrev", "wheat_rrev", "soybean_rrev")], na.rm = TRUE)
newdat$total_a <- rowSums(newdat[, c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)

aggdat <- data.frame()

for (i in bal){
  sub_cropdat <- filter(newdat, year >= i & year < i+9)
  ag_cropdat <- sub_cropdat %>% 
    group_by(state, fips) %>% 
    summarise(corn_rev = mean(corn_rrev, na.rm = TRUE),
              total_rrev = mean(total_rrev, na.rm = TRUE),
              total_a = mean(total_a, na.rm = TRUE),
              tavg = mean(tavg, na.rm = TRUE))
  ag_cropdat <- filter(ag_cropdat, !is.na(total_rrev) & !is.na(tavg))
  ag_cropdat$rrev_w <- ag_cropdat$total_rrev/sum(ag_cropdat$total_rrev, na.rm = TRUE)
  ag_cropdat$acres_w <- ag_cropdat$total_a/sum(ag_cropdat$total_a, na.rm = TRUE)
  mergedat <- data.frame(tavg = ag_cropdat$tavg, rrev_w = ag_cropdat$rrev_w, acres_w = ag_cropdat$acres_w, year = i)
  aggdat <- rbind(aggdat, mergedat)
}


ggplot(aggdat, aes(tavg, weight = rrev_w, fill = factor(year))) + theme_tufte() +
  geom_histogram(bins = 50) + scale_fill_brewer(palette = "YlGnBu") +
  theme(legend.title = element_blank()) +
  ylab("Value of Activity") + xlab("Average Temp") + 
  ggtitle("Change from Average Temperature from 1930 - 2010 \n (weighted by Value of Activity)") 

ggplot(aggdat, aes(tavg, weight = acres_w, fill = factor(year))) + theme_tufte() +
  geom_histogram(bins = 50) + scale_fill_brewer(palette = "YlGnBu") +
  theme(legend.title = element_blank()) +
  ylab("Value of Activity") + xlab("Average Temp") + 
  ggtitle("Crop Change from Average Temperature from 1930 - 2010 \n (weighted by Total Acres)") 

