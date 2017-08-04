cropdat <- readRDS("data/full_ag_data.rds")

# P bar

dat <- cropdat
dat$corn_mprice <- mean(dat$corn_rprice, na.rm = TRUE)
dat$cotton_mprice <- mean(dat$cotton_rprice, na.rm = TRUE)
dat$hay_mprice <- mean(dat$hay_rprice, na.rm = TRUE)
dat$wheat_mprice <- mean(dat$wheat_rprice, na.rm = TRUE)
dat$soybean_mprice <- mean(dat$soybean_rprice, na.rm = TRUE)


dat <- dat %>% 
  group_by(fips) %>% 
  mutate(corn_crev = corn_rrev + cov(corn_mprice, corn_yield , use = "pairwise.complete.obs"),
         cotton_crev = cotton_rrev + cov(cotton_yield, cotton_mprice, use = "pairwise.complete.obs"),
         hay_crev = hay_rrev + cov(hay_yield, hay_mprice, use = "pairwise.complete.obs"),
         wheat_crev = wheat_rrev + cov(wheat_yield, wheat_mprice, use = "pairwise.complete.obs"),
         soybean_crev = soybean_rrev + cov(soybean_yield, soybean_mprice, use = "pairwise.complete.obs"))

cov(dat$corn_mprice, dat$corn_yield, use = "na.or.complete")

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





