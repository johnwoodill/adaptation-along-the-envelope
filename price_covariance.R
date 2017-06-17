cropdat <- readRDS("data/full_ag_data.rds")

# P bar

dat <- cropdat
dat$corn_avg_price <- mean(dat$corn_rprice, na.rm = TRUE)
dat$cotton_avg_price <- mean(dat$cotton_rprice, na.rm = TRUE)
dat$hay_avg_price <- mean(dat$hay_rprice, na.rm = TRUE)
dat$wheat_avg_price <- mean(dat$wheat_rprice, na.rm = TRUE)
dat$soybean_avg_price <- mean(dat$soybean_rprice, na.rm = TRUE)

# Revenue
dat$corn_r <- dat$corn_grain_p*dat$corn_avg_price
dat$cotton_r <- dat$cotton_p*dat$cotton_avg_price
dat$hay_r <- dat$hay_p*dat$hay_avg_price
dat$wheat_r <- dat$wheat_p*dat$wheat_avg_price
dat$soybean_r <- dat$soybean_p*dat$soybean_avg_price

dat <- dat %>% 
  group_by(fips) %>% 
  mutate(corn_rev = corn_r + cov(corn_grain_p, corn_rprice, use = "pairwise.complete.obs"),
         cotton_rev = cotton_r + cov(cotton_p, cotton_rprice, use = "pairwise.complete.obs"),
         hay_rev = hay_r + cov(hay_p, hay_rprice, use = "pairwise.complete.obs"),
         wheat_rev = wheat_r + cov(wheat_p, wheat_rprice, use = "pairwise.complete.obs"),
         soybean_rev = soybean_r + cov(soybean_p, soybean_rprice, use = "pairwise.complete.obs"))

ggplot(data = dat) + geom_smooth(aes(year, corn_rev)) + geom_smooth(aes(year, corn_r), color = "red")

# Y_it
# dat$total_p <- rowSums(dat[,c(7, 12, 17, 22, 27)], na.rm = TRUE)
# 
# dat <- dat %>% 
#   group_by(year) %>% 
#   mutate(total_price = mean(corn_rprice + cotton_rprice + hay_rprice + wheat_rprice + soybean_rprice, na.rm = TRUE))
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




