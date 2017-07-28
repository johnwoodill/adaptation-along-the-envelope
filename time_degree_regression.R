library(lfe)
library(readr)
library(dplyr)
library(Hmisc)
library(ggthemes)
library(splines)


##############################################
##############################################
# Segmented Regressions ---------------------------------------------------


p.dat <- readRDS("data/panel_regression_data.rds")
td <- readRDS("data/fips_degree_time_1900-2013.rds")

td$year <- factor(td$year)
td$fips <- factor(td$fips)
p.dat <- left_join(p.dat, td, by = c("year", "fips"))

# 5-C intervals starting at 10C
p.dat$a <- rowSums(p.dat[, 84:88])
p.dat$b <- rowSums(p.dat[, 89:93])
p.dat$c <- rowSums(p.dat[, 94:98])
p.dat$d <- rowSums(p.dat[, 99:103])
p.dat$e <- rowSums(p.dat[, 104:108])
p.dat$f <- rowSums(p.dat[, 109:113])
p.dat$g <- rowSums(p.dat[, 114:119])

# Fit
p.corn.fit <- felm(ln_corn_rrev ~  a + b + c + d + e + f + g + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(p.corn.fit)

p.cotton.fit <- felm(ln_cotton_rrev ~ a + b + c + d + e + f + g + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(p.cotton.fit)

p.hay.fit <- felm(ln_hay_rrev ~ a + b + c + d + e + f + g + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(p.hay.fit)

p.wheat.fit <- felm(ln_wheat_rrev ~ a + b + c + d + e + f + g + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(p.wheat.fit)

p.soybean.fit <- felm(ln_soybean_rrev ~ a + b + c + d + e + f + g +  prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(p.soybean.fit)

p.corn.coef <- as.numeric(p.corn.fit$coefficients[c(1:7)])
p.cotton.coef <- as.numeric(p.cotton.fit$coefficients[c(1:7)])
p.hay.coef <- as.numeric(p.hay.fit$coefficients[c(1:7)])
p.wheat.coef <- as.numeric(p.wheat.fit$coefficients[c(1:7)])
p.soybean.coef <- as.numeric(p.soybean.fit$coefficients[c(1:7)])

seg.pdat <- data.frame(degree = rep(c(10, 15, 20, 25, 30, 35, 40), 5),
                   coef = c(p.corn.coef, p.cotton.coef, p.hay.coef, p.wheat.coef, p.soybean.coef),
                   se = c(p.corn.fit$se[c(1:7)], p.cotton.fit$se[c(1:7)], p.hay.fit$se[c(1:7)], 
                        p.wheat.fit$se[c(1:7)], p.soybean.fit$se[c(1:7)]), 
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 7))
seg.pdat$ymin <- seg.pdat$coef - seg.pdat$se*1.97
seg.pdat$ymax <- seg.pdat$coef + seg.pdat$se*1.97
seg.pdat$reg <- "Panel"

ggplot(seg.pdat, aes(x = degree, y = coef, color = crop)) + geom_line() + 
  geom_line(aes(y = ymin, x = degree, color = crop), linetype = "dashed") + 
  geom_line(aes(y = ymax, x = degree, color = crop), linetype = "dashed")


# Cross-section
cs.dat <- readRDS("data/cross_section_regression_data.rds")
td <- readRDS("data/fips_degree_time_1900-2013.rds")

# Only have cross-sectional data >= 1970 
td <- filter(td, year >= 1970 & year <= 2010)
td$year <- NULL
td <- td %>% 
  group_by(fips) %>% 
  summarise_all(funs(mean))


td$fips <- factor(td$fips)
cs.dat$fips <- factor(cs.dat$fips)
cs.dat <- left_join(cs.dat, td, by = c("fips"))

# 5 C intervals
cs.dat$a <- rowSums(cs.dat[, 55:59])
cs.dat$b <- rowSums(cs.dat[, 60:64])
cs.dat$c <- rowSums(cs.dat[, 65:69])
cs.dat$d <- rowSums(cs.dat[, 70:74])
cs.dat$e <- rowSums(cs.dat[, 75:79])
cs.dat$f <- rowSums(cs.dat[, 80:84])
cs.dat$g <- rowSums(cs.dat[, 85:90])

cs.dat$state <- factor(cs.dat$state)
cs.dat <- filter(cs.dat, state == "mi")
# Fit

cs.corn.fit <- felm(ln_corn_rrev ~  a + b + c + d + e + f + g + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
                  data = cs.dat)
summary(cs.corn.fit)

cs.cotton.fit <- felm(ln_cotton_rrev ~ a + b + c + d + e + f + g + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
                  data = cs.dat)
summary(cs.cotton.fit)

cs.hay.fit <- felm(ln_hay_rrev ~  a + b + c + d + e + f + g + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
                  data = cs.dat)
summary(cs.hay.fit)

cs.wheat.fit <- felm(ln_wheat_rrev ~  a + b + c + d + e + f + g +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, data = cs.dat)
summary(cs.wheat.fit)

cs.soybean.fit <- felm(ln_soybean_rrev ~  a + b + c + d + e + f + g +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, data = cs.dat)
summary(cs.soybean.fit)

# Get coefficients
cs.corn.coef <- as.numeric(cs.corn.fit$coefficients[1:7])
cs.cotton.coef <- as.numeric(cs.cotton.fit$coefficients[1:7])
cs.hay.coef <- as.numeric(cs.hay.fit$coefficients[1:7])
cs.wheat.coef <- as.numeric(cs.wheat.fit$coefficients[1:7])
cs.soybean.coef <- as.numeric(cs.soybean.fit$coefficients[1:7])

# Build data frame for plot
seg.csdat <- data.frame(degree = rep(c(10, 15, 20, 25, 30, 35, 40), 5),
                   coef = c(cs.corn.coef, cs.cotton.coef, cs.hay.coef, cs.wheat.coef, cs.soybean.coef),
                   se = c(cs.corn.fit$se[1:7], cs.cotton.fit$se[1:7], cs.hay.fit$se[1:7], 
                          cs.wheat.fit$se[1:7], cs.soybean.fit$se[1:7]), 
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 7))

seg.csdat$ymin <- seg.csdat$coef - seg.csdat$se*1.97
seg.csdat$ymax <- seg.csdat$coef + seg.csdat$se*1.97
seg.csdat$reg<- "Cross-section"
seg.pcsdat <- rbind(seg.csdat, seg.pdat)

seg.plot <- ggplot(seg.pcsdat) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) + 
  geom_line(aes(x = degree, y = coef, color = reg, group = interaction(crop, reg))) +
  geom_line(aes(y = ymin, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 0.5) + 
  geom_line(aes(y = ymax, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 0.5) + 
  facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Log Revenue") + theme_tufte() + ggtitle("Segemented Regressions") + 
  theme(legend.position="top")
seg.plot 



##############################################
##############################################
# Spline Regressions ------------------------------------------------------

p.dat <- readRDS("data/panel_regression_data.rds")
td <- readRDS("data/fips_degree_time_1900-2013.rds")
td$year <- factor(td$year)
td$fips <- factor(td$fips)
p.dat <- left_join(p.dat, td, by = c("year", "fips"))

# Spline
Tindex = 0:40 + .5
DMat = as.matrix(ns(Tindex, df = 5))
XMat <- as.matrix(p.dat[, 72:112]) %*% DMat

# Fit
p.corn.fit <- felm(ln_corn_rrev ~  XMat + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(p.corn.fit)

p.cotton.fit <- felm(ln_cotton_rrev ~ XMat + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(p.cotton.fit)

p.hay.fit <- felm(ln_hay_rrev ~ XMat + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(p.hay.fit)

p.wheat.fit <- felm(ln_wheat_rrev ~  XMat + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(p.wheat.fit)

p.soybean.fit <- felm(ln_soybean_rrev ~ XMat +  prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(p.soybean.fit)

# Fit coefficients into DMat
p.corn.coef <- DMat %*% matrix(p.corn.fit$coefficients[1:5], ncol = 1)
plot(scale(p.corn.coef))
p.corn.se <- DMat %*% matrix(p.corn.fit$se[1:5], ncol = 1)
plot(scale(p.corn.se))

p.cotton.coef <- DMat %*% matrix(p.cotton.fit$coefficients[1:5], ncol = 1)
p.cotton.se <- DMat %*% matrix(p.cotton.fit$se[1:5], ncol = 1)
plot(p.cotton.coef)

p.hay.coef <- DMat %*% matrix(p.hay.fit$coefficients[1:5], ncol = 1)
p.hay.se <- DMat %*% matrix(p.hay.fit$se[1:5], ncol = 1)
plot(p.hay.coef)

p.wheat.coef <- DMat %*% matrix(p.wheat.fit$coefficients[1:5], ncol = 1)
p.wheat.se <- DMat %*% matrix(p.wheat.fit$se[1:5], ncol = 1)
plot(p.wheat.coef)

p.soybean.coef <- DMat %*% matrix(p.soybean.fit$coefficients[1:5], ncol = 1)
p.soybean.se <- DMat %*% matrix(p.soybean.fit$se[1:5], ncol = 1)
plot(p.soybean.coef)


# Spline data set to plot
spline.pdat <- data.frame(degree = 0:45,
                   coef = c(p.corn.coef, p.cotton.coef, p.hay.coef, p.wheat.coef, p.soybean.coef),
                   se = c(p.corn.se, p.cotton.se, p.hay.se, p.wheat.se, p.soybean.se),
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 46))

spline.pdat$ymin <- spline.pdat$coef - spline.pdat$se*1.97
spline.pdat$ymax <- spline.pdat$coef + spline.pdat$se*1.97
spline.pdat$reg <- "Panel"

ggplot(spline.pdat, aes(x = degree, y = coef, color = crop)) + 
  geom_line() + facet_wrap(~crop) + 
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) +
  geom_line(aes(y = ymin, x = degree), linetype = "dotted") +
  geom_line(aes(y = ymax, x = degree), linetype = "dotted")  + ylab("Log Revenue") + 
  xlab("Temperature (C)") +
  theme_tufte() + theme(legend.position="none")

# Cross-section
cs.dat <- readRDS("data/cross_section_regression_data.rds")
td <- readRDS("data/fips_degree_time_1900-2013.rds")
td <- filter(td, year >= 1970 & year <= 2010)
td$year <- NULL

td <- td %>% 
  group_by(fips) %>% 
  summarise_all(funs(mean))

td$fips <- factor(td$fips)
cs.dat$fips <- factor(cs.dat$fips)
cs.dat <- left_join(cs.dat, td, by = c("fips"))

Tindex = 10:45 + .5
DMat = as.matrix(ns(Tindex, df = 7))
XMat <- as.matrix(cs.dat[, 47:82]) %*% DMat


#cs.dat$fourty <- rowSums(cs.dat[, 101:112])
cs.corn.fit <- felm(ln_corn_rrev ~  XMat + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, exactDOF = TRUE,
                  data = cs.dat)

summary(cs.corn.fit)

cs.cotton.fit <- felm(ln_cotton_rrev ~ XMat + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
                  data = cs.dat)
summary(cs.cotton.fit)

cs.hay.fit <- felm(ln_hay_rrev ~  XMat + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
                  data = cs.dat)
summary(cs.hay.fit)

cs.wheat.fit <- felm(ln_wheat_rrev ~  XMat +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, data = cs.dat)
summary(cs.wheat.fit)

cs.soybean.fit <- felm(ln_soybean_rrev ~  XMat +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, data = cs.dat)
summary(cs.soybean.fit)


# Spline through coefficients
cs.corn.coef <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.corn.fit$coefficients[1:7], n = 7*10)
cs.cotton.coef <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.cotton.fit$coefficients[1:7], n = 7*10)
cs.hay.coef <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.hay.fit$coefficients[1:7], n = 7*10)
cs.wheat.coef <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.wheat.fit$coefficients[1:7], n = 7*10)
cs.soybean.coef <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.soybean.fit$coefficients[1:7], n = 7*10)

# Spline through SE
cs.corn.se <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.corn.fit$se[1:7], n = 7*10)
cs.cotton.se <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.cotton.fit$se[1:7], n = 7*10)
cs.hay.se <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.hay.fit$se[1:7], n = 7*10)
cs.wheat.se <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.wheat.fit$se[1:7], n = 7*10)
cs.soybean.se <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.soybean.fit$se[1:7], n = 7*10)

spline.csdat <- data.frame(degree = c(cs.corn.coef$x, cs.cotton.coef$x, cs.hay.coef$x, cs.wheat.coef$x, cs.soybean.coef$x),
                   coef = c(cs.corn.coef$y, cs.cotton.coef$y, cs.hay.coef$y, cs.wheat.coef$y, cs.soybean.coef$y),
                   se = c(cs.corn.se$y, cs.cotton.se$y, cs.hay.se$y, cs.wheat.se$y, cs.soybean.se$y),
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 70))

spline.csdat$ymin <- spline.csdat$coef - spline.csdat$se*1.97
spline.csdat$ymax <- spline.csdat$coef + spline.csdat$se*1.97
spline.csdat$reg <- "Cross-section"
spline.pcsdat <- rbind(spline.csdat, spline.pdat)

spline.plot <- ggplot(pcsdat) + 
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) + 
  geom_line(aes(x = degree, y = coef, color = reg, group = interaction(crop, reg))) +
  geom_line(aes(y = ymin, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted") + 
  geom_line(aes(y = ymax, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted") +
  facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Log Revenue") + theme_tufte() + theme(legend.position="none")
spline.plot 

library(cowplot)
plot_grid(seg.plot, spline.plot, ncol = 1)

######################################################
######################################################
# Step Regression (3bin) --------------------------------------------------
# p.dat <- readRDS("data/panel_regression_data.rds")
# 
# 
# td <- readRDS("data/fips_degree_time_1900-2013.rds")
# 
# 
# 
# 
# #names(td)[3:48] <- paste0(names(td)[3:48], "C")
# td$year <- factor(td$year)
# td$fips <- factor(td$fips)
# p.dat <- left_join(p.dat, td, by = c("year", "fips"))
# 
# #dat <- filter(dat, !is.na(corn_yield))
# 
# #DMat <- rcspline.eval(0:40, nk = 10)
# #DMat <- diag(41)
# 
# #p.dat[, 111] <- rowSums(p.dat[, 111:116])
# #XMat <- as.matrix(p.dat[, 71:111])
# #XMat <- XMat %*% DMat
# 
# # p.dat$a <- rowSums(p.dat[, 71:73])
# # p.dat$b <- rowSums(p.dat[, 74:76])
# # p.dat$c <- rowSums(p.dat[, 77:79])
# p.dat$a <- rowSums(p.dat[, 81:83])
# p.dat$b <- rowSums(p.dat[, 84:86])
# p.dat$c <- rowSums(p.dat[, 87:89])
# p.dat$d <- rowSums(p.dat[, 90:92])
# p.dat$e <- rowSums(p.dat[, 93:95])
# p.dat$f <- rowSums(p.dat[, 96:98])
# p.dat$g <- rowSums(p.dat[, 99:101])
# p.dat$h <- rowSums(p.dat[, 102:104])
# p.dat$i <- rowSums(p.dat[, 105:107])
# p.dat$j <- rowSums(p.dat[, 108:110])
# p.dat$k <- rowSums(p.dat[, 111:116])
# 
# corn.fit <- felm(ln_corn_rrev ~  a + b + c + d + e + f + g + h + i + j + k + 
#                    prec + prec_sq | fips + year | 0 | state, data = p.dat)
# summary(corn.fit)
# plot(scale(corn.fit$coefficients[1:11]))
# summary(corn.fit)
# 
# cotton.fit <- felm(ln_cotton_rrev ~  a + b + c + d + e + f + g + h + i + j + k + 
#                      prec + prec_sq | fips + year | 0 | 0, data = p.dat)
# summary(cotton.fit)
# plot(cotton.fit$coefficients[1:11])
# 
# hay.fit <- felm(ln_hay_rrev ~  a + b + c + d + e + f + g + h + i + j + k + 
#                   prec + prec_sq | fips + year | 0 | state, data = p.dat)
# summary(hay.fit)
# plot(hay.fit$coefficients[1:11])
# 
# wheat.fit <- felm(ln_wheat_rrev ~   a + b + c + d + e + f + g + h + i + j + k + 
#                     prec + prec_sq | fips + year | 0 | state, data = p.dat)
# summary(wheat.fit)
# plot(wheat.fit$coefficients[1:11])
# 
# soybean.fit <- felm(ln_soybean_rrev ~  a + b + c + d + e + f + g + h + i + j + k + 
#                       prec + prec_sq | fips + year | 0 | state, data = p.dat)
# summary(soybean.fit)
# plot(soybean.fit$coefficients[1:11])
# 
# summary(corn.fit)
# corn.coef <- as.numeric(corn.fit$coefficients[1:11])
# cotton.coef <- as.numeric(cotton.fit$coefficients[1:11])
# hay.coef <- as.numeric(hay.fit$coefficients[1:11])
# wheat.coef <- as.numeric(wheat.fit$coefficients[1:11])
# soybean.coef <- as.numeric(soybean.fit$coefficients[1:11])
# 
# # corn.coef <- as.numeric(corn.fit$coefficients[c(1,1:4)])
# # #corn.fit$se
# # cotton.coef <- as.numeric(cotton.fit$coefficients[c(1,1:4)])
# # hay.coef <- as.numeric(hay.fit$coefficients[c(1,1:4)])
# # wheat.coef <- as.numeric(wheat.fit$coefficients[c(1,1:4)])
# # soybean.coef <- as.numeric(soybean.fit$coefficients[c(1,1:4)])
# 
# pdat <- data.frame(degree = seq(10, 40, by = 3), 
#                    coef = c(corn.coef, cotton.coef, hay.coef, wheat.coef, soybean.coef),
#                    se = c(corn.fit$se[1:11], cotton.fit$se[1:11], hay.fit$se[1:11], 
#                         wheat.fit$se[1:11], soybean.fit$se[1:11]), 
#                    crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 11))
# 
# pdat$ymin <- pdat$coef - pdat$se*1.97
# pdat$ymax <- pdat$coef + pdat$se*1.97
# pdat$reg <- "Panel"
# pdat
# ggplot(pdat, aes(x = degree, y = coef, color = crop)) + 
#   geom_step() + geom_hline(yintercept = 0, linetype = "dotted") + facet_wrap(~crop) + theme_tufte()
# 
# # Cross-section
# cs.dat <- readRDS("data/cross_section_regression_data.rds")
# 
# td <- readRDS("data/fips_degree_time_1900-2013.rds")
# td <- filter(td, year >= 1970 & year <= 2010)
# td$year <- NULL
# td <- td %>% 
#   group_by(fips) %>% 
#   summarise_all(funs(mean))
# 
# td$fips <- factor(td$fips)
# cs.dat$fips <- factor(cs.dat$fips)
# cs.dat <- left_join(cs.dat, td, by = c("fips"))
# 
# 
# cs.dat$a <- rowSums(cs.dat[, 47:49])
# cs.dat$b <- rowSums(cs.dat[, 50:52])
# cs.dat$c <- rowSums(cs.dat[, 53:55])
# cs.dat$d <- rowSums(cs.dat[, 56:58])
# cs.dat$e <- rowSums(cs.dat[, 59:61])
# cs.dat$f <- rowSums(cs.dat[, 62:64])
# cs.dat$g <- rowSums(cs.dat[, 65:67])
# cs.dat$h <- rowSums(cs.dat[, 68:70])
# cs.dat$i <- rowSums(cs.dat[, 71:73])
# cs.dat$j <- rowSums(cs.dat[, 74:76])
# cs.dat$k <- rowSums(cs.dat[, 77:82])
# cs.dat$l <- rowSums(cs.dat[, 80:82])
# 
# 
# corn.fit <- felm(ln_corn_rrev ~  a + b + c + d + e + f + g + h + i + j + k + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
#                   data = cs.dat)
# 
# summary(corn.fit)
# plot(corn.fit$coefficients[1:12])
# 
# cotton.fit <- felm(ln_cotton_rrev ~ a + b + c + d + e + f + g + h + i + j + k  + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
#                   data = cs.dat)
# summary(cotton.fit)
# plot(cotton.fit$coefficients[1:12])
# 
# hay.fit <- felm(ln_hay_rrev ~  a + b + c + d + e + f + g + h + i + j + k  + l + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
#                   data = cs.dat)
# summary(hay.fit)
# plot(hay.fit$coefficients[1:11])
# 
# wheat.fit <- felm(ln_wheat_rrev ~  a + b + c + d + e + f + g + h + i + j + k  + l +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, data = cs.dat)
# summary(wheat.fit)
# 
# soybean.fit <- felm(ln_soybean_rrev ~  a + b + c + d + e + f + g + h + i + j + k  + l +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, data = cs.dat)
# summary(soybean.fit)
# plot(soybean.fit$coefficients[1:11])
# 
# 
# 
# summary(corn.fit)
# corn.coef <- as.numeric(corn.fit$coefficients[1:11])
# cotton.coef <- as.numeric(cotton.fit$coefficients[1:11])
# hay.coef <- as.numeric(hay.fit$coefficients[1:11])
# wheat.coef <- as.numeric(wheat.fit$coefficients[1:11])
# soybean.coef <- as.numeric(soybean.fit$coefficients[1:11])
# 
# # corn.coef <- as.numeric(corn.fit$coefficients[c(2,2:5)])
# # cotton.coef <- as.numeric(cotton.fit$coefficients[c(2,2:5)])
# # hay.coef <- as.numeric(hay.fit$coefficients[c(2,2:5)])
# # wheat.coef <- as.numeric(wheat.fit$coefficients[c(2,2:5)])
# # soybean.coef <- as.numeric(soybean.fit$coefficients[c(2,2:5)])
# 
# csdat <- data.frame(degree = seq(10, 40, by = 3), 
#                    coef = c(corn.coef, cotton.coef, hay.coef, wheat.coef, soybean.coef),
#                    se = c(corn.fit$se[1:11], cotton.fit$se[1:11], hay.fit$se[1:11], 
#                         wheat.fit$se[1:11], soybean.fit$se[1:11]), 
#                    crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 11))
# 
# csdat$ymin <- csdat$coef - csdat$se*1.97
# csdat$ymax <- csdat$coef + csdat$se*1.97
# csdat$reg <- "Cross-section"
# csdat
# pcsdat <- rbind(csdat, pdat)
# 
# step.plot <- ggplot(pcsdat) + 
#   geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) + 
#   geom_step(aes(x = degree, y = coef, color = crop, linetype = reg, group = interaction(crop, reg))) +
#   #geom_step(aes(y = ymin, x = degree, color = crop, linetype = reg, group = interaction(crop, reg)), linetype = "dotted") + 
#   #geom_step(aes(y = ymax, x = degree, color = crop, linetype = reg, group = interaction(crop, reg)), linetype = "dotted") +
#   facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Log Revenue") + theme_tufte() + ggtitle("3-bin Temperature Regression")
# 
# step.plot
# 
# 
# # Merge plots
# library(cowplot)
# plot_grid(step.plot, spline.plot, seg.plot, ncol = 1)
# step.plot
# seg.plot
