library(dplyr)
library(ggplot2)


cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, year >= 1930 & year <= 2010)

# P bar

dat <- cropdat
dat$corn_mprice <- mean(dat$corn_rprice, na.rm = TRUE)
dat$cotton_mprice <- mean(dat$cotton_rprice, na.rm = TRUE)
dat$hay_mprice <- mean(dat$hay_rprice, na.rm = TRUE)
dat$wheat_mprice <- mean(dat$wheat_rprice, na.rm = TRUE)
dat$soybean_mprice <- mean(dat$soybean_rprice, na.rm = TRUE)


dat <- dat %>% 
  group_by(fips) %>% 
  mutate(corn_crev = (corn_yield*corn_mprice + cov(corn_yield, corn_rprice , use = "pairwise.complete.obs")),
         cotton_crev = (cotton_yield*corn_mprice + cov(cotton_yield, cotton_rprice, use = "pairwise.complete.obs")),
         hay_crev = (hay_yield*corn_mprice + cov(hay_yield, hay_rprice, use = "pairwise.complete.obs")),
         wheat_crev = (wheat_yield*corn_mprice + cov(wheat_yield, wheat_rprice, use = "pairwise.complete.obs")),
         soybean_crev = (soybean_yield*corn_mprice + cov(soybean_yield, soybean_rprice, use = "pairwise.complete.obs")))

# Convert inf to NA
dat <- do.call(data.frame,lapply(dat, function(x) replace(x, is.infinite(x),NA)))
length(which(is.infinite(dat$corn_crev )))

mean(dat$corn_rrev, na.rm = TRUE)
mean(dat$corn_crev, na.rm = TRUE)

mean(dat$cotton_rrev, na.rm = TRUE)
mean(dat$cotton_crev, na.rm = TRUE)

mean(dat$hay_rrev, na.rm = TRUE)
mean(dat$hay_crev, na.rm = TRUE)

mean(dat$wheat_rrev, na.rm = TRUE)
mean(dat$wheat_crev, na.rm = TRUE)

mean(dat$soybean_rrev, na.rm = TRUE)
mean(dat$soybean_crev, na.rm = TRUE)




ggplot(data = dat) + geom_smooth(aes(year, corn_rrev), color = "blue") + geom_smooth(aes(year, corn_crev), color = "red")

# Y_it
# dat$total_p <- rowSums(dat[,c(7, 12, 17, 22, 27)], na.rm = TRUE)
# 
# dat <- dat %>% 
#   group_by(year) %>% 
#   mutate(total_price = mean(corn_mprice + cotton_rprice + hay_rprice + wheat_rprice + soybean_rprice, na.rm = TRUE))
# 
# dat <- dat %>% 
#   group_by(year, fips) %>% 
#   mutate(covs = cov(total_p, total_price, use = "pairwise.complete.obs"))
# 
# unique(dat$covs)
# 
# check <- filter(dat, year == "2000" & fips == "51101")
# 
# dat$total_rev <- (dat$total_p*dat$avg_price)
# 
# cov(dat$total_p, dat$total_price, use = "pairwise.complete.obs")
# 
# 
# 
# dat$corn_rev <- dat$corn_grain_p*dat$price + dat$covs  





