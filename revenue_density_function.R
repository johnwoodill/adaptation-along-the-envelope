library(ggplot2)
library(dplyr)


### NO IDEA WHAT THIS FILE IS FOR
cropdat <- readRDS("data/full_ag_data.rds")

fullcropdensity <- density(cropdat$tavg, na.rm = TRUE, bw = 1)
plot(fullcropdensity)

cornd <- select(cropdat, corn_grain_p, corn_grain_a, corn_price)
cornd <- filter(cornd, !is.na(corn_grain_p) & !is.na(corn_grain_a) & !is.na(corn_price))
plot(density(cornd$corn_grain_p*cornd$corn_price, na.rm = TRUE, weights = cornd$corn_grain_a/sum(cornd$corn_grain_a), bw = 4))

acres <- rbind(cropdat$corn_grain_a, cropdat$hay_a, cropdat$cotton_a, cropdat$wheat_a, cropdat$soybean_a)
plot(density(remove_outliers(acres), na.rm = TRUE))

acresdat <- select(cropdat, year, fips, state, corn_grain_a, cotton_a, hay_a, wheat_a, soybean_a)
acresdat <- acresdat %>% 
  group_by()
acres <- c(rbind(cropdat$corn_grain_a, cropdat$hay_a, cropdat$cotton_a, cropdat$wheat_a, cropdat$soybean_a))
plot(density(remove_outliers(acres), na.rm = TRUE))
plot(histogram(acres))

# Loess
dat <- filter(cropdat, !is.na(y) & !is.na(tavg) & !is.infinite(y))
y <- dat$y[1:20]
x <- dat$tavg[1:20]
mod <- loess(y ~ tavg, data = dat)
predd <- predict(mod)
predd

cropdat$y <- (cropdat$corn_grain_p*cropdat$corn_price)/cropdat$corn_grain_a

loess(y ~ tavg, data = dat)
loess.smooth(y=dat$y, x=dat$tavg, family = "gaussian")
ggplot(dat, aes(y=y, x=tavg)) + geom_smooth()

# gam method
cropdat$corn_y <- (cropdat$corn_grain_p*cropdat$corn_price)/cropdat$corn_grain_a
cropdat$cotton_y <- (cropdat$cotton_p*cropdat$cotton_adj_price)/cropdat$cotton_a
cropdat$wheat_y <- (cropdat$wheat_p*cropdat$wheat_price)/cropdat$wheat_a
cropdat$hay_y <- (cropdat$hay_p*cropdat$hay_price)/cropdat$hay_a
cropdat$soybean_y <- (cropdat$soybean_p*cropdat$soybean_price)/cropdat$soybean_a

ggplot(cropdat) + geom_smooth(aes(tavg, corn_y)) + geom_smooth(aes(tavg, cotton_y))+ geom_smooth(aes(tavg, wheat_y))+ geom_smooth(aes(tavg, hay_y))+ geom_smooth(aes(tavg, soybean_y))
ggplot(cropdat) + geom_smooth(aes(tavg, cotton_y))
ggplot(cropdat) + geom_smooth(aes(tavg, wheat_y))
ggplot(cropdat) + geom_smooth(aes(tavg, hay_y))
ggplot(cropdat) + geom_smooth(aes(tavg, soybean_y))

# Get density for all crops value of activity
den <- c(cropdat$corn_y, cropdat$cotton_y, cropdat$wheat_y, cropdat$hay_y, cropdat$soybean_y)
den <- c(cropdat$corn_grain_a, cropdat$cotton_a, cropdat$wheat_a, cropdat$hay_a, cropdat$soybean_a)
plot(density(den, na.rm = TRUE))
plot(hist(den, na.rm = TRUE))

# Sample data
a <- rnorm(100, mean = 20)
b <- rnorm(100, mean = 30)
d1 <- density(a)
d2 <- density(b)
d3 <- density(c(a,b), 2)

plot(d3)
plot(d1$x, d1$y, type = "l", col = 'red')
lines(d2$x, d2$y, col = 'blue')

plot(d2$x, d2$y, type = "l", col = 'red')

# Stacked bar chart
dd <- dens
dd$x <- floor(dd$x)
dd <- dd %>% 
  group_by(type, x) %>% 
  summarise(y = mean(y, na.rm = TRUE))

ggplot(dd, aes(x=x,y=y,group=type,fill=type)) + geom_area(position="fill") + 
  ylab("% of Value of Activity") + xlab("Average Temp") + scale_x_continuous(breaks = c(0,5, 10, 15, 20, 25)) +
  ggtitle("Proportion of Value of Activity for Average Temp - U.S. 1960 - 2010") + theme_classic()

