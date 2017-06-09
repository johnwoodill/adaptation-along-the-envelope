remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Load data set
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat$corn_rev <- cropdat$corn_grain_p*cropdat$corn_rprice
cropdat$cotton_rev <- cropdat$cotton_p*cropdat$cotton_rprice
cropdat$hay_rev <- cropdat$hay_p*cropdat$hay_rprice
cropdat$wheat_rev <- cropdat$wheat_p*cropdat$wheat_rprice
cropdat$soybean_rev <- cropdat$soybean_p*cropdat$soybean_rprice

cropdat$total_rev <- cropdat$corn_rev + cropdat$cotton_rev + cropdat$hay_rev + cropdat$wheat_rev + cropdat$soybean_rev

#corn_dens <- density(cropdat$corn_rev, na.rm = TRUE)
#corn_dens_y <- corn_dens$y/(sum(cropdat$corn_rev, na.rm = TRUE)/sum(cropdat$total_rev, na.rm = TRUE))
#corn_dens_x <- corn_dens$x
#plot(corn_dens_x, corn_dens_y)

dat <- cropdat %>% 
  group_by(fips) %>% 
  summarise(corn_rev = mean(corn_grain_p*corn_rprice, na.rm = TRUE),
            cotton_rev = mean(cotton_p*cotton_rprice, na.rm = TRUE),
            hay_rev = mean(hay_p*hay_rprice, na.rm = TRUE),
            wheat_rev = mean(wheat_p*wheat_rprice, na.rm = TRUE),
            soybean_rev = mean(soybean_p*soybean_rprice, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE))

dat$corn_rev <- remove_outliers(dat$corn_rev)
dat$cotton_rev <- remove_outliers(dat$cotton_rev)
dat$hay_rev <- remove_outliers(dat$hay_rev)
dat$wheat_rev <- remove_outliers(dat$wheat_rev)
dat$soybean_rev <- remove_outliers(dat$soybean_rev)
dat$total_rev <- rowSums(dat[,2:6], na.rm = TRUE)

corn_share <- sum(dat$corn_rev, na.rm = TRUE)/sum(dat$total_rev, na.rm = TRUE)
cotton_share <- sum(dat$cotton_rev, na.rm = TRUE)/sum(dat$total_rev, na.rm = TRUE)
hay_share <- sum(dat$hay_rev, na.rm = TRUE)/sum(dat$total_rev, na.rm = TRUE)
wheat_share <- sum(dat$wheat_rev, na.rm = TRUE)/sum(dat$total_rev, na.rm = TRUE)
soybean_share <- sum(dat$soybean_rev, na.rm = TRUE)/sum(dat$total_rev, na.rm = TRUE)

corn_d <- density(dat$corn_rev, na.rm = TRUE)
corn_d$y <- corn_d$y*corn_share
corn_dens <- data.frame(x = corn_d$x, y = corn_d$y, crop = "corn")

cotton_d <- density(dat$cotton_rev, na.rm = TRUE)
cotton_d$y <- cotton_d$y*cotton_share
cotton_dens <- data.frame(x = cotton_d$x, y = cotton_d$y, crop = "cotton")

hay_d <- density(dat$hay_rev, na.rm = TRUE)
hay_d$y <- hay_d$y*hay_share
hay_dens <- data.frame(x = hay_d$x, y = hay_d$y, crop = "hay")

wheat_d <- density(dat$wheat_rev, na.rm = TRUE)
wheat_d$y <- wheat_d$y*wheat_share
wheat_dens <- data.frame(x = wheat_d$x, y = wheat_d$y, crop = "wheat")

soybean_d <- density(dat$soybean_rev, na.rm = TRUE)
soybean_d$y <- soybean_d$y*soybean_share
soybean_dens <- data.frame(x = soybean_d$x, y = soybean_d$y, crop = "soybean")

dat_dens <- rbind(corn_dens, cotton_dens, hay_dens, wheat_dens, soybean_dens)

ggplot(dat_dens, aes(x = x, y = y, color = crop)) + geom_line()

# Check changes in corn by decade
years <- c(1960, 1970, 1980, 1990, 2000)
years <- c(1970, 1980)

bdat <- data.frame()
for (i in years){
  newdat <- filter(cropdat, year >= i & year < i + 9)
  dat <- newdat %>% 
    group_by(fips) %>% 
    summarise(corn_rev = mean(corn_grain_p*corn_rprice, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE))
  dat$corn_rev <- remove_outliers(dat$corn_rev)
  dat <- filter(dat, !is.na(corn_rev) & !is.na(tavg))
  dat$corn_w <- dat$tavg/sum(dat$tavg, na.rm = TRUE)
  ddens <- density(dat$corn_rev, na.rm = TRUE, weights = dat$corn_w)
  dens <- data.frame(x = ddens$x, y = ddens$y, year = i)
  bdat <- rbind(bdat, dens)  
}
bdat$year <- as.factor(bdat$year)
ggplot(bdat, aes(x, y, group = year, color = year)) + geom_line()
 