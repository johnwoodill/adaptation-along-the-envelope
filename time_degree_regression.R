library(lfe)
library(readr)
library(dplyr)
library(Hmisc)

dat <- readRDS("data/panel_regression_data.rds")
td <- read_csv("data/fips_degree_time_1900-2013.csv")
names(td)[3:48] <- paste0(names(td)[3:48], "C")
td$year <- factor(td$year)
td$fips <- factor(td$fips)
dat <- left_join(dat, td, by = c("year", "fips"))

#dat <- filter(dat, !is.na(corn_yield))

DMat <- rcspline.eval(0:3)
#DMat <- diag(46)
XMat <- as.matrix(dat[, 71:116])
DMat
XMat <- XMat %*% DMat
#newXmat <- as.data.frame(XMat)
dat$zero <- rowSums(dat[,71:81])
dat$ten <- rowSums(dat[, 81:101])
dat$thirty <- rowSums(dat[, 108:116])
dat$fourty <- rowSums(dat[, 111:116])
fit <- felm(corn_yield ~ XMat + prec + prec_sq | fips + state, data = dat)

corn.fit <- felm(ln_corn_rrev ~  ten + thirty + prec + prec_sq | fips + state | 0 | state, data = dat)
summary(corn.fit)
cotton.fit <- felm(ln_cotton_rrev ~ ten + thirty + prec + prec_sq | fips + state | 0 | state, data = dat)
summary(cotton.fit)
hay.fit <- felm(ln_hay_rrev ~  ten + thirty + prec + prec_sq | fips + state | 0 | state, data = dat)
summary(hay.fit)
wheat.fit <- felm(ln_wheat_rrev ~  ten + thirty +  prec + prec_sq | fips + state | 0 | state, data = dat)
summary(wheat.fit)
soybean.fit <- felm(ln_soybean_rrev ~  ten + ten + thirty +  prec + prec_sq | fips + state | 0 | state, data = dat)
summary(soybean.fit)

summary(corn.fit)
corn.coef <- as.numeric(scale(corn.fit$coefficients[c(1,1:2)]))
cotton.coef <- as.numeric(scale(cotton.fit$coefficients[c(1,1:2)]))
hay.coef <- as.numeric(scale(hay.fit$coefficients[c(1,1:2)]))
wheat.coef <- as.numeric(scale(wheat.fit$coefficients[c(1,1:2)]))
soybean.coef <- as.numeric(scale(soybean.fit$coefficients[c(1,1:2)]))

corn.coef <- as.numeric(corn.fit$coefficients[c(1,1:2)])
cotton.coef <- as.numeric(cotton.fit$coefficients[c(1,1:2)])
hay.coef <- as.numeric(hay.fit$coefficients[c(1,1:2)])
wheat.coef <- as.numeric(wheat.fit$coefficients[c(1,1:2)])
soybean.coef <- as.numeric(soybean.fit$coefficients[c(1,1:2)])

pdat <- data.frame(degree = rep(c(0, 10, 30), 5),
                   coef = c(corn.coef, cotton.coef, hay.coef, wheat.coef, soybean.coef),
                   crop = rep(c("corn", "cotton", "hay", "wheat", "soybean"), each = 3))
pdat
ggplot(pdat, aes(x = degree, y = coef, color = crop)) + geom_line()


#pdat <- data.frame(degree = c(0, 10, 30, 40), coef = rep(0, 4))
# 
# pdat$coef[1] <- coef[1]
# pdat$coef[2] <- coef[1]
# pdat$coef[3] <- coef[2]
# pdat$coef[4] <- coef[3]
# #pdat <- fill(pdat, coef, .direction = "down")
# pdat


update.formula(fit, "ln_cotton_rrev" ~ .)

fit$coefficients[1]
DMat <- rcspline.eval(0:45)
XMat <- as.matrix(TemperatureData[,3:48])%*%DMat
fit <- lm(yield~XMat, data=regData)
summary(fit)


