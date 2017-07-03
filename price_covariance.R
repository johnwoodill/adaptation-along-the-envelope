cropdat <- readRDS("data/full_ag_data.rds")

# P bar

dat <- cropdat
dat$corn_rprice <- mean(dat$corn_rprice, na.rm = TRUE)
dat$cotton_rprice <- mean(dat$cotton_rprice, na.rm = TRUE)
dat$hay_rprice <- mean(dat$hay_rprice, na.rm = TRUE)
dat$wheat_rprice <- mean(dat$wheat_rprice, na.rm = TRUE)
dat$soybean_rprice <- mean(dat$soybean_rprice, na.rm = TRUE)



# Revenue
# dat$corn_rev <- (dat$corn_grain_p*dat$corn_rprice)/dat$corn_grain_a
# dat$cotton_rev <- (dat$cotton_p*dat$cotton_rprice)/dat$cotton_a
# dat$hay_rev <- (dat$hay_p*dat$hay_rprice)/dat$hay_a
# dat$wheat_rev <- (dat$wheat_p*dat$wheat_rprice)/dat$wheat_a
# dat$soybean_rev <- (dat$soybean_p*dat$soybean_rprice)/dat$soybean_a

dat <- dat %>% 
  group_by(fips) %>% 
  mutate(corn_crev = corn_rrev + cov(corn_yield, corn_rprice, use = "pairwise.complete.obs"),
         cotton_crev = cotton_rrev + cov(cotton_yield, cotton_rprice, use = "pairwise.complete.obs"),
         hay_crev = hay_rrev + cov(hay_yield, hay_rprice, use = "pairwise.complete.obs"),
         wheat_crev = wheat_rrev + cov(wheat_yield, wheat_rprice, use = "pairwise.complete.obs"),
         soybean_crev = soybean_rrev + cov(soybean_yield, soybean_rprice, use = "pairwise.complete.obs"))

cov(dat$corn_yield, dat$corn_rprice, use = "pairwise.complete.obs")

ggplot(data = dat) + geom_smooth(aes(year, corn_rrev), color = "blue") + geom_smooth(aes(year, corn_crev), color = "red")

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

(4 + 5 + 8 + 10 + 3 + 4)/6

(4+5+8)/3
(10+3+4)/3






