library(lfe)
library(readr)
library(dplyr)
library(Hmisc)


##############################################
##############################################
# Segmented Regressions ---------------------------------------------------


p.dat <- readRDS("data/panel_regression_data.rds")


td <- read_csv("data/fips_degree_time_1900-2013.csv")
names(td)[3:48] <- paste0(names(td)[3:48], "C")
td$year <- factor(td$year)
td$fips <- factor(td$fips)
p.dat <- left_join(p.dat, td, by = c("year", "fips"))

#dat <- filter(dat, !is.na(corn_yield))

DMat <- rcspline.eval(0:40, nk = 7)
#DMat <- diag(41)

XMat <- as.matrix(p.dat[, 71:111])
XMat <- XMat %*% DMat
#newXmat <- as.data.frame(XMat)
p.dat$zero <- rowSums(p.dat[,71:80])
p.dat$ten <- rowSums(p.dat[, 81:100])
p.dat$thirty <- rowSums(p.dat[, 101:110])
p.dat$fourty <- rowSums(p.dat[, 111:116])

corn.fit <- felm(ln_corn_rrev ~  zero + ten + thirty + fourty  + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(corn.fit)
plot(corn.fit$coefficients[1:4])
summary(corn.fit)

cotton.fit <- felm(ln_cotton_rrev ~ zero + ten + thirty + fourty + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(cotton.fit)
plot(scale(cotton.fit$coefficients[1:4]))

hay.fit <- felm(ln_hay_rrev ~ zero + ten + thirty + fourty + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(hay.fit)

wheat.fit <- felm(ln_wheat_rrev ~  zero + ten + thirty + fourty + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(wheat.fit)

soybean.fit <- felm(ln_soybean_rrev ~ zero +  ten + thirty + fourty +  prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(soybean.fit)

# summary(corn.fit)
# corn.coef <- as.numeric(scale(corn.fit$coefficients[c(1,1:4)]))
# cotton.coef <- as.numeric(scale(cotton.fit$coefficients[c(1,1:4)]))
# hay.coef <- as.numeric(scale(hay.fit$coefficients[c(1,1:4)]))
# wheat.coef <- as.numeric(scale(wheat.fit$coefficients[c(1,1:4)]))
# soybean.coef <- as.numeric(scale(soybean.fit$coefficients[c(1,1:4)]))

corn.coef <- as.numeric(corn.fit$coefficients[c(1,1:4)])
#corn.fit$se
cotton.coef <- as.numeric(cotton.fit$coefficients[c(1,1:4)])
hay.coef <- as.numeric(hay.fit$coefficients[c(1,1:4)])
wheat.coef <- as.numeric(wheat.fit$coefficients[c(1,1:4)])
soybean.coef <- as.numeric(soybean.fit$coefficients[c(1,1:4)])

pdat <- data.frame(degree = rep(c(0, 10, 30, 40, 45), 5),
                   coef = c(corn.coef, cotton.coef, hay.coef, wheat.coef, soybean.coef),
                   se = c(corn.fit$se[c(1,1:4)], cotton.fit$se[c(1,1:4)], hay.fit$se[c(1,1:4)], 
                        wheat.fit$se[c(1,1:4)], soybean.fit$se[c(1,1:4)]), 
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 5))
pdat$ymin <- pdat$coef - pdat$se*1.97
pdat$ymax <- pdat$coef + pdat$se*1.97
pdat$reg <- "Panel"
pdat
ggplot(pdat, aes(x = degree, y = coef, color = crop)) + geom_line()

cs.dat <- readRDS("data/cross_section_regression_data.rds")


td <- read_csv("data/fips_degree_time_1900-2013.csv")
names(td)[3:48] <- paste0(names(td)[3:48], "C")
td$year <- NULL
td <- td %>% 
  group_by(fips) %>% 
  summarise_all(funs(mean))


td$fips <- factor(td$fips)
cs.dat$fips <- factor(cs.dat$fips)
cs.dat <- left_join(cs.dat, td, by = c("fips"))

cs.dat$zero <- rowSums(cs.dat[,38:47])
cs.dat$ten <- rowSums(cs.dat[, 48:67])
cs.dat$thirty <- rowSums(cs.dat[, 68:77])
cs.dat$fourty <- rowSums(cs.dat[, 78:83])

#cs.dat$fourty <- rowSums(cs.dat[, 101:112])

corn.fit <- felm(ln_corn_rrev ~  zero + ten + thirty + fourty + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
                  data = cs.dat)

summary(corn.fit)
plot(corn.fit$coefficients[1:4])
cotton.fit <- felm(ln_cotton_rrev ~ zero + ten + thirty + fourty + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
                  data = cs.dat)
summary(cotton.fit)
plot(cotton.fit$coefficients[1:4])

hay.fit <- felm(ln_hay_rrev ~  zero + ten + thirty + fourty + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
                  data = cs.dat)
summary(hay.fit)
plot(hay.fit$coefficients[1:4])
wheat.fit <- felm(ln_wheat_rrev ~  zero + ten + thirty + fourty +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, data = cs.dat)
summary(wheat.fit)
soybean.fit <- felm(ln_soybean_rrev ~  zero + ten + thirty + fourty +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, data = cs.dat)
summary(soybean.fit)
plot(soybean.fit$coefficients[2:5])


# 
# summary(corn.fit)
# corn.coef <- as.numeric(scale(corn.fit$coefficients[c(2,2:5)]))
# cotton.coef <- as.numeric(scale(cotton.fit$coefficients[c(2,2:5)]))
# hay.coef <- as.numeric(scale(hay.fit$coefficients[c(2,2:5)]))
# wheat.coef <- as.numeric(scale(wheat.fit$coefficients[c(2,2:5)]))
# soybean.coef <- as.numeric(scale(soybean.fit$coefficients[c(2,2:5)]))

corn.coef <- as.numeric(corn.fit$coefficients[c(2,2:5)])
cotton.coef <- as.numeric(cotton.fit$coefficients[c(2,2:5)])
hay.coef <- as.numeric(hay.fit$coefficients[c(2,2:5)])
wheat.coef <- as.numeric(wheat.fit$coefficients[c(2,2:5)])
soybean.coef <- as.numeric(soybean.fit$coefficients[c(2,2:5)])

csdat <- data.frame(degree = rep(c(0, 10, 30, 40, 45), 5),
                   coef = c(corn.coef, cotton.coef, hay.coef, wheat.coef, soybean.coef),
                   se = c(corn.fit$se[c(2,2:5)], cotton.fit$se[c(2,2:5)], hay.fit$se[c(2,2:5)], 
                          wheat.fit$se[c(2,2:5)], soybean.fit$se[c(2,2:5)]), 
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 5))
csdat$ymin <- csdat$coef - csdat$se*1.97
csdat$ymax <- csdat$coef + csdat$se*1.97
csdat$reg <- "Cross-section"
csdat
seg.pcsdat <- rbind(csdat, pdat)

seg.plot <- ggplot(seg.pcsdat) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) + 
  geom_line(aes(x = degree, y = coef, color = crop, linetype = reg, group = interaction(crop, reg))) +
  geom_line(aes(y = ymin, x = degree, color = crop, linetype = reg, group = interaction(crop, reg)), linetype = "dotted") + 
  geom_line(aes(y = ymax, x = degree, color = crop, linetype = reg, group = interaction(crop, reg)), linetype = "dotted") +
  facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Log Revenue") + theme_tufte() + ggtitle("Segemented Regressions")
seg.plot 



##############################################
##############################################
# Spline Regressions ------------------------------------------------------

p.dat <- readRDS("data/panel_regression_data.rds")


td <- read_csv("data/fips_degree_time_1900-2013.csv")
names(td)[3:48] <- paste0(names(td)[3:48], "C")
td$year <- factor(td$year)
td$fips <- factor(td$fips)
p.dat <- left_join(p.dat, td, by = c("year", "fips"))

#dat <- filter(dat, !is.na(corn_yield))

DMat <- rcspline.eval(0:40, nk = 8)
#DMat <- diag(41)

p.dat[, 111] <- rowSums(p.dat[, 111:116])
XMat <- as.matrix(p.dat[, 71:111])
XMat <- XMat %*% DMat

corn.fit <- felm(ln_corn_rrev ~  XMat  + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(corn.fit)
plot(corn.fit$coefficients[1:8])
summary(corn.fit)

cotton.fit <- felm(ln_cotton_rrev ~ XMat + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(cotton.fit)
plot(scale(cotton.fit$coefficients[1:8]))

hay.fit <- felm(ln_hay_rrev ~ XMat + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(hay.fit)

wheat.fit <- felm(ln_wheat_rrev ~  XMat + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(wheat.fit)

soybean.fit <- felm(ln_soybean_rrev ~ XMat +  prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(soybean.fit)

corn.coef <- spline(x = c(4, 6, 12, 17, 23, 28, 34, 36), y = corn.fit$coefficients[1:8], n = 8*10)
cotton.coef <- spline(x = c(4, 6, 12, 17, 23, 28, 34, 36), y = cotton.fit$coefficients[1:8], n = 8*10)
hay.coef <- spline(x = c(4, 6, 12, 17, 23, 28, 34, 36), y = hay.fit$coefficients[1:8], n = 8*10)
wheat.coef <- spline(x = c(4, 6, 12, 17, 23, 28, 34, 36), y = wheat.fit$coefficients[1:8], n = 8*10)
soybean.coef <- spline(x = c(4, 6, 12, 17, 23, 28, 34, 36), y = soybean.fit$coefficients[1:8], n = 8*10)

# corn.coef <- as.numeric(corn.fit$coefficients[c(1,1:4)])
# #corn.fit$se
# cotton.coef <- as.numeric(cotton.fit$coefficients[c(1,1:4)])
# hay.coef <- as.numeric(hay.fit$coefficients[c(1,1:4)])
# wheat.coef <- as.numeric(wheat.fit$coefficients[c(1,1:4)])
# soybean.coef <- as.numeric(soybean.fit$coefficients[c(1,1:4)])

pdat <- data.frame(degree = c(corn.coef$x, cotton.coef$x, hay.coef$x, wheat.coef$x, soybean.coef$x),
                   coef = c(corn.coef$y, cotton.coef$y, hay.coef$y, wheat.coef$y, soybean.coef$y),
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 80))

# pdat$ymin <- pdat$coef - pdat$se*1.97
# pdat$ymax <- pdat$coef + pdat$se*1.97
pdat$reg <- "Panel"
pdat
ggplot(pdat, aes(x = degree, y = coef, color = crop)) + geom_line() + facet_wrap(~crop)

# Cross-section
cs.dat <- readRDS("data/cross_section_regression_data.rds")

td <- read_csv("data/fips_degree_time_1900-2013.csv")
names(td)[3:48] <- paste0(names(td)[3:48], "C")
td$year <- NULL
td <- td %>% 
  group_by(fips) %>% 
  summarise_all(funs(mean))

td$fips <- factor(td$fips)
cs.dat$fips <- factor(cs.dat$fips)
cs.dat <- left_join(cs.dat, td, by = c("fips"))

DMat <- rcspline.eval(0:40, nk = 8)

cs.dat[, 78] <- rowSums(cs.dat[, 78:83])
XMat <- as.matrix(cs.dat[, 38:78])
XMat <- XMat %*% DMat


#cs.dat$fourty <- rowSums(cs.dat[, 101:112])

corn.fit <- felm(ln_corn_rrev ~  XMat + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
                  data = cs.dat)

summary(corn.fit)
plot(corn.fit$coefficients[1:6])
cotton.fit <- felm(ln_cotton_rrev ~ XMat + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
                  data = cs.dat)
summary(cotton.fit)
plot(cotton.fit$coefficients[1:6])

hay.fit <- felm(ln_hay_rrev ~  XMat + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
                  data = cs.dat)
summary(hay.fit)
plot(hay.fit$coefficients[1:6])

wheat.fit <- felm(ln_wheat_rrev ~  XMat +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, data = cs.dat)
summary(wheat.fit)
soybean.fit <- felm(ln_soybean_rrev ~  XMat +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, data = cs.dat)
summary(soybean.fit)
plot(soybean.fit$coefficients[1:6])



corn.coef <- spline(x = c(4, 6, 12, 17, 23, 28, 34, 36), y = corn.fit$coefficients[1:8], n = 8*10)
cotton.coef <- spline(x = c(4, 6, 12, 17, 23, 28, 34, 36), y = cotton.fit$coefficients[1:8], n = 8*10)
hay.coef <- spline(x = c(4, 6, 12, 17, 23, 28, 34, 36), y = hay.fit$coefficients[1:8], n = 8*10)
wheat.coef <- spline(x = c(4, 6, 12, 17, 23, 28, 34, 36), y = wheat.fit$coefficients[1:8], n = 8*10)
soybean.coef <- spline(x = c(4, 6, 12, 17, 23, 28, 34, 36), y = soybean.fit$coefficients[1:8], n = 8*10)

# corn.coef <- as.numeric(corn.fit$coefficients[c(2,2:5)])
# cotton.coef <- as.numeric(cotton.fit$coefficients[c(2,2:5)])
# hay.coef <- as.numeric(hay.fit$coefficients[c(2,2:5)])
# wheat.coef <- as.numeric(wheat.fit$coefficients[c(2,2:5)])
# soybean.coef <- as.numeric(soybean.fit$coefficients[c(2,2:5)])

csdat <- data.frame(degree = c(corn.coef$x, cotton.coef$x, hay.coef$x, wheat.coef$x, soybean.coef$x),
                   coef = c(corn.coef$y, cotton.coef$y, hay.coef$y, wheat.coef$y, soybean.coef$y),
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 80))

csdat$ymin <- csdat$coef - csdat$se*1.97
csdat$ymax <- csdat$coef + csdat$se*1.97
csdat$reg <- "Cross-section"
csdat
pcsdat <- rbind(csdat, pdat)

spline.plot <- ggplot(pcsdat) + 
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) + 
  geom_line(aes(x = degree, y = coef, color = crop, linetype = reg, group = interaction(crop, reg))) +
  #geom_line(aes(y = ymin, x = degree, color = crop, linetype = reg, group = interaction(crop, reg)), linetype = "dotted") + 
  #geom_line(aes(y = ymax, x = degree, color = crop, linetype = reg, group = interaction(crop, reg)), linetype = "dotted") +
  facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Log Revenue") + theme_tufte()
spline.plot 


######################################################
######################################################
# Step Regression (3bin) --------------------------------------------------
p.dat <- readRDS("data/panel_regression_data.rds")


td <- read_csv("data/fips_degree_time_1900-2013.csv")




names(td)[3:48] <- paste0(names(td)[3:48], "C")
td$year <- factor(td$year)
td$fips <- factor(td$fips)
p.dat <- left_join(p.dat, td, by = c("year", "fips"))

#dat <- filter(dat, !is.na(corn_yield))

#DMat <- rcspline.eval(0:40, nk = 10)
#DMat <- diag(41)

#p.dat[, 111] <- rowSums(p.dat[, 111:116])
#XMat <- as.matrix(p.dat[, 71:111])
#XMat <- XMat %*% DMat

p.dat$a <- rowSums(p.dat[, 71:73])
p.dat$b <- rowSums(p.dat[, 74:76])
p.dat$c <- rowSums(p.dat[, 77:79])
p.dat$d <- rowSums(p.dat[, 80:82])
p.dat$e <- rowSums(p.dat[, 83:85])
p.dat$f <- rowSums(p.dat[, 86:88])
p.dat$g <- rowSums(p.dat[, 89:91])
p.dat$h <- rowSums(p.dat[, 92:94])
p.dat$i <- rowSums(p.dat[, 95:97])
p.dat$j <- rowSums(p.dat[, 98:100])
p.dat$k <- rowSums(p.dat[, 101:103])
p.dat$l <- rowSums(p.dat[, 104:106])
p.dat$m <- rowSums(p.dat[, 107:109])
p.dat$n <- rowSums(p.dat[, 110:116])

corn.fit <- felm(ln_corn_rrev ~  a + b + c + d + e + f + g + h + i + j + k + l + m + n  + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(corn.fit)
plot(scale(corn.fit$coefficients[1:14]))
summary(corn.fit)

cotton.fit <- felm(ln_cotton_rrev ~ a + b + c + d + e + f + g + h + i + j + k + l + m + n + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(cotton.fit)
plot(cotton.fit$coefficients[1:14])

hay.fit <- felm(ln_hay_rrev ~ a + b + c + d + e + f + g + h + i + j + k + l + m + n + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(hay.fit)
plot(hay.fit$coefficients[1:14])

wheat.fit <- felm(ln_wheat_rrev ~  a + b + c + d + e + f + g + h + i + j + k + l + m + n + prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(wheat.fit)
plot(hay.fit$coefficients[1:14])

soybean.fit <- felm(ln_soybean_rrev ~ a + b + c + d + e + f + g + h + i + j + k + l + m + n +  prec + prec_sq | fips + year | 0 | state, data = p.dat)
summary(soybean.fit)
plot(hay.fit$coefficients[1:14])

summary(corn.fit)
corn.coef <- as.numeric(corn.fit$coefficients[1:14])
cotton.coef <- as.numeric(cotton.fit$coefficients[1:14])
hay.coef <- as.numeric(hay.fit$coefficients[1:14])
wheat.coef <- as.numeric(wheat.fit$coefficients[1:14])
soybean.coef <- as.numeric(soybean.fit$coefficients[1:14])

# corn.coef <- as.numeric(corn.fit$coefficients[c(1,1:4)])
# #corn.fit$se
# cotton.coef <- as.numeric(cotton.fit$coefficients[c(1,1:4)])
# hay.coef <- as.numeric(hay.fit$coefficients[c(1,1:4)])
# wheat.coef <- as.numeric(wheat.fit$coefficients[c(1,1:4)])
# soybean.coef <- as.numeric(soybean.fit$coefficients[c(1,1:4)])

pdat <- data.frame(degree = seq(0, 40, by = 3), 
                   coef = c(corn.coef, cotton.coef, hay.coef, wheat.coef, soybean.coef),
                   se = c(corn.fit$se[1:14], cotton.fit$se[1:14], hay.fit$se[1:14], 
                        wheat.fit$se[1:14], soybean.fit$se[1:14]), 
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 14))

pdat$ymin <- pdat$coef - pdat$se*1.97
pdat$ymax <- pdat$coef + pdat$se*1.97
pdat$reg <- "Panel"
pdat
ggplot(pdat, aes(x = degree, y = coef, color = crop)) + 
  geom_step() + geom_hline(yintercept = 0, linetype = "dotted") + facet_wrap(~crop) + theme_tufte()

cs.dat <- readRDS("data/cross_section_regression_data.rds")

td <- read_csv("data/fips_degree_time_1900-2013.csv")
names(td)[3:48] <- paste0(names(td)[3:48], "C")

td$year <- NULL
td <- td %>% 
  group_by(fips) %>% 
  summarise_all(funs(mean))

td$fips <- factor(td$fips)
cs.dat$fips <- factor(cs.dat$fips)
cs.dat <- left_join(cs.dat, td, by = c("fips"))

cs.dat$a <- rowSums(cs.dat[, 38:40])
cs.dat$b <- rowSums(cs.dat[, 41:43])
cs.dat$c <- rowSums(cs.dat[, 44:46])
cs.dat$d <- rowSums(cs.dat[, 47:49])
cs.dat$e <- rowSums(cs.dat[, 50:52])
cs.dat$f <- rowSums(cs.dat[, 53:55])
cs.dat$g <- rowSums(cs.dat[, 56:57])
cs.dat$h <- rowSums(cs.dat[, 59:61])
cs.dat$i <- rowSums(cs.dat[, 62:64])
cs.dat$j <- rowSums(cs.dat[, 65:67])
cs.dat$k <- rowSums(cs.dat[, 68:70])
cs.dat$l <- rowSums(cs.dat[, 71:73])
cs.dat$m <- rowSums(cs.dat[, 74:76])
cs.dat$n <- rowSums(cs.dat[, 77:83])


corn.fit <- felm(ln_corn_rrev ~  a + b + c + d + e + f + g + h + i + j + k + l + m + n + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | 0 | 0 | state,
                  data = cs.dat)

summary(corn.fit)
plot(corn.fit$coefficients[2:15])

cotton.fit <- felm(ln_cotton_rrev ~ a + b + c + d + e + f + g + h + i + j + k + l + m + n + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | 0 | 0 | state,
                  data = cs.dat)
summary(cotton.fit)
plot(cotton.fit$coefficients[2:15])

hay.fit <- felm(ln_hay_rrev ~  a + b + c + d + e + f + g + h + i + j + k + l + m + n + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | 0 | 0 | state,
                  data = cs.dat)
summary(hay.fit)
plot(hay.fit$coefficients[2:15])

wheat.fit <- felm(ln_wheat_rrev ~  a + b + c + d + e + f + g + h + i + j + k + l + m + n +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | 0 | 0 | state, data = cs.dat)
summary(wheat.fit)

soybean.fit <- felm(ln_soybean_rrev ~  a + b + c + d + e + f + g + h + i + j + k + l + m + n +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
                   waterCapacity + percentClay + minPermeability + kFactor + bestSoil | 0 | 0 | state, data = cs.dat)
summary(soybean.fit)
plot(soybean.fit$coefficients[2:5])



summary(corn.fit)
corn.coef <- as.numeric(corn.fit$coefficients[2:15])
cotton.coef <- as.numeric(cotton.fit$coefficients[2:15])
hay.coef <- as.numeric(hay.fit$coefficients[2:15])
wheat.coef <- as.numeric(wheat.fit$coefficients[2:15])
soybean.coef <- as.numeric(soybean.fit$coefficients[2:15])

# corn.coef <- as.numeric(corn.fit$coefficients[c(2,2:5)])
# cotton.coef <- as.numeric(cotton.fit$coefficients[c(2,2:5)])
# hay.coef <- as.numeric(hay.fit$coefficients[c(2,2:5)])
# wheat.coef <- as.numeric(wheat.fit$coefficients[c(2,2:5)])
# soybean.coef <- as.numeric(soybean.fit$coefficients[c(2,2:5)])

csdat <- data.frame(degree = seq(0, 40, by = 3), 
                   coef = c(corn.coef, cotton.coef, hay.coef, wheat.coef, soybean.coef),
                   se = c(corn.fit$se[1:14], cotton.fit$se[1:14], hay.fit$se[1:14], 
                        wheat.fit$se[1:14], soybean.fit$se[1:14]), 
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 14))

csdat$ymin <- csdat$coef - csdat$se*1.97
csdat$ymax <- csdat$coef + csdat$se*1.97
csdat$reg <- "Cross-section"
csdat
pcsdat <- rbind(csdat, pdat)

step.plot <- ggplot(pcsdat) + 
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) + 
  geom_step(aes(x = degree, y = coef, color = crop, linetype = reg, group = interaction(crop, reg))) +
  #geom_step(aes(y = ymin, x = degree, color = crop, linetype = reg, group = interaction(crop, reg)), linetype = "dotted") + 
  #geom_step(aes(y = ymax, x = degree, color = crop, linetype = reg, group = interaction(crop, reg)), linetype = "dotted") +
  facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Log Revenue") + theme_tufte() + ggtitle("3-bin Temperature Regression")

step.plot


# Merge plots
library(cowplot)
plot_grid(step.plot, spline.plot, seg.plot, ncol = 1)
step.plot
seg.plot
