library(lfe)
library(readr)
library(dplyr)
library(Hmisc)
library(ggthemes)
library(splines)


##############################################
##############################################
# Segmented Regressions ---------------------------------------------------

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

# Cross-section Revenue Per Acre
cs.dd.ln_corn_rrev <- readRDS("models/cs.dd.ln_corn_rrev")
cs.dd.ln_cotton_rrev <- readRDS("models/cs.dd.ln_cotton_rrev")
cs.dd.ln_hay_rrev <- readRDS("models/cs.dd.ln_hay_rrev")
cs.dd.ln_wheat_rrev <- readRDS("models/cs.dd.ln_wheat_rrev")
cs.dd.ln_soybean_rrev <- readRDS("models/cs.dd.ln_soybean_rrev")

# Panel Revenue Per Acre
p.dd.ln_corn_rrev <- readRDS("models/p.dd.ln_corn_rrev")
p.dd.ln_cotton_rrev <- readRDS("models/p.dd.ln_cotton_rrev")
p.dd.ln_hay_rrev <- readRDS("models/p.dd.ln_hay_rrev")
p.dd.ln_wheat_rrev <- readRDS("models/p.dd.ln_wheat_rrev")
p.dd.ln_soybean_rrev <- readRDS("models/p.dd.ln_soybean_rrev")

# Diff Revenue Per Acre
diff.dd.ln_corn_rrev <- readRDS("models/diff.dd.ln_corn_rrev")
diff.dd.ln_cotton_rrev <- readRDS("models/diff.dd.ln_cotton_rrev")
diff.dd.ln_hay_rrev <- readRDS("models/diff.dd.ln_hay_rrev")
diff.dd.ln_wheat_rrev <- readRDS("models/diff.dd.ln_wheat_rrev")
diff.dd.ln_soybean_rrev <- readRDS("models/diff.dd.ln_soybean_rrev")

cs.corn.coef <- as.numeric(cs.dd.ln_corn_rrev$coefficients[c(1, 1:3, 3)])
cs.cotton.coef <- as.numeric(cs.dd.ln_cotton_rrev$coefficients[c(1, 1:3, 3)])
cs.hay.coef <- as.numeric(cs.dd.ln_hay_rrev$coefficients[c(1, 1:3, 3)])
cs.wheat.coef <- as.numeric(cs.dd.ln_wheat_rrev$coefficients[c(1, 1:3, 3)])
cs.soybean.coef <- as.numeric(cs.dd.ln_soybean_rrev$coefficients[c(1, 1:3, 3)])

p.corn.coef <- as.numeric(p.dd.ln_corn_rrev$coefficients[c(1, 1:3, 3)])
p.cotton.coef <- as.numeric(p.dd.ln_cotton_rrev$coefficients[c(1, 1:3, 3)])
p.hay.coef <- as.numeric(p.dd.ln_hay_rrev$coefficients[c(1, 1:3, 3)])
p.wheat.coef <- as.numeric(p.dd.ln_wheat_rrev$coefficients[c(1, 1:3, 3)])
p.soybean.coef <- as.numeric(p.dd.ln_soybean_rrev$coefficients[c(1, 1:3, 3)])

diff.corn.coef <- as.numeric(diff.dd.ln_corn_rrev$coefficients[c(1, 1:3, 3)])
diff.cotton.coef <- as.numeric(diff.dd.ln_cotton_rrev$coefficients[c(1, 1:3, 3)])
diff.hay.coef <- as.numeric(diff.dd.ln_hay_rrev$coefficients[c(1, 1:3, 3)])
diff.wheat.coef <- as.numeric(diff.dd.ln_wheat_rrev$coefficients[c(1, 1:3, 3)])
diff.soybean.coef <- as.numeric(diff.dd.ln_soybean_rrev$coefficients[c(1, 1:3, 3)])

cs.corn.se <- as.numeric(cs.dd.ln_corn_rrev$se[c(1, 1:3, 3)])
cs.cotton.se <- as.numeric(cs.dd.ln_cotton_rrev$se[c(1, 1:3, 3)])
cs.hay.se <- as.numeric(cs.dd.ln_hay_rrev$se[c(1, 1:3, 3)])
cs.wheat.se <- as.numeric(cs.dd.ln_wheat_rrev$se[c(1, 1:3, 3)])
cs.soybean.se <- as.numeric(cs.dd.ln_soybean_rrev$se[c(1, 1:3, 3)])

p.corn.se <- as.numeric(p.dd.ln_corn_rrev$se[c(1, 1:3, 3)])
p.cotton.se <- as.numeric(p.dd.ln_cotton_rrev$se[c(1, 1:3, 3)])
p.hay.se <- as.numeric(p.dd.ln_hay_rrev$se[c(1, 1:3, 3)])
p.wheat.se <- as.numeric(p.dd.ln_wheat_rrev$se[c(1, 1:3, 3)])
p.soybean.se <- as.numeric(p.dd.ln_soybean_rrev$se[c(1, 1:3, 3)])

diff.corn.se <- as.numeric(diff.dd.ln_corn_rrev$se[c(1, 1:3, 3)])
diff.cotton.se <- as.numeric(diff.dd.ln_cotton_rrev$se[c(1, 1:3, 3)])
diff.hay.se <- as.numeric(diff.dd.ln_hay_rrev$se[c(1, 1:3, 3)])
diff.wheat.se <- as.numeric(diff.dd.ln_wheat_rrev$se[c(1, 1:3, 3)])
diff.soybean.se <- as.numeric(diff.dd.ln_soybean_rrev$se[c(1, 1:3, 3)])

seg.dat <- data.frame(degree = rep(c(0, 10, 30, 35, 40), 15, each = 1),
                    coef = c(cs.corn.coef, cs.cotton.coef, cs.hay.coef, cs.wheat.coef, cs.soybean.coef,
                               p.corn.coef, p.cotton.coef, p.hay.coef, p.wheat.coef, p.soybean.coef,
                               diff.corn.coef, diff.cotton.coef, diff.hay.coef, diff.wheat.coef, diff.soybean.coef),
                      se = c(cs.corn.se, cs.cotton.se, cs.hay.se, cs.wheat.se, cs.soybean.se,
                               p.corn.se, p.cotton.se, p.hay.se, p.wheat.se, p.soybean.se,
                               diff.corn.se, diff.cotton.se, diff.hay.se, diff.wheat.se, diff.soybean.se),
                      crop = rep(c("corn", "cotton", "hay", "wheat", "soybean"), 3, each = 5),
                      reg = rep(c("cross-section", "panel", "diff"), 1, each = 25))


seg.dat$ymin <- seg.dat$coef - 1.96*seg.dat$se
seg.dat$ymax <- seg.dat$coef + 1.96*seg.dat$se


ggplot(seg.dat) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) + 
  geom_line(aes(x = degree, y = coef, color = reg, group = interaction(crop, reg))) +
  geom_line(aes(y = ymin, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 1) + 
  geom_line(aes(y = ymax, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 1) + 
  facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Log Revenue") + ggtitle("Log Revenue Segemented Regressions") + 
   theme_tufte() + theme(legend.position="top")

# Proportion Seg Regression
library(lfe)
library(readr)
library(dplyr)
library(Hmisc)
library(ggthemes)
library(splines)


##############################################
##############################################
# Segmented Regressions ---------------------------------------------------

source("cross_section_regression_data.R")
source("panel_regression_data.R")
source("cross_section_regression.R")
source("panel_regression.R")
source("diff_regression.R")

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

# Cross-section Revenue Per Acre
cs.dd.p_corn_share <- readRDS("models/cs.dd.p_corn_share")
cs.dd.p_cotton_share <- readRDS("models/cs.dd.p_cotton_share")
cs.dd.p_hay_share <- readRDS("models/cs.dd.p_hay_share")
cs.dd.p_wheat_share <- readRDS("models/cs.dd.p_wheat_share")
cs.dd.p_soybean_share <- readRDS("models/cs.dd.p_soybean_share")

# Panel Revenue Per Acre
p.dd.p_corn_share <- readRDS("models/p.dd.p_corn_share")
p.dd.p_cotton_share <- readRDS("models/p.dd.p_cotton_share")
p.dd.p_hay_share <- readRDS("models/p.dd.p_hay_share")
p.dd.p_wheat_share <- readRDS("models/p.dd.p_wheat_share")
p.dd.p_soybean_share <- readRDS("models/p.dd.p_soybean_share")

# Diff Revenue Per Acre
diff.dd.p_corn_share <- readRDS("models/diff.dd.p_corn_share")
diff.dd.p_cotton_share <- readRDS("models/diff.dd.p_cotton_share")
diff.dd.p_hay_share <- readRDS("models/diff.dd.p_hay_share")
diff.dd.p_wheat_share <- readRDS("models/diff.dd.p_wheat_share")
diff.dd.p_soybean_share <- readRDS("models/diff.dd.p_soybean_share")

cs.corn.coef <- as.numeric(cs.dd.p_corn_share$coefficients[c(1, 1:3, 3)])
cs.cotton.coef <- as.numeric(cs.dd.p_cotton_share$coefficients[c(1, 1:3, 3)])
cs.hay.coef <- as.numeric(cs.dd.p_hay_share$coefficients[c(1, 1:3, 3)])
cs.wheat.coef <- as.numeric(cs.dd.p_wheat_share$coefficients[c(1, 1:3, 3)])
cs.soybean.coef <- as.numeric(cs.dd.p_soybean_share$coefficients[c(1, 1:3, 3)])

p.corn.coef <- as.numeric(p.dd.p_corn_share$coefficients[c(1, 1:3, 3)])
p.cotton.coef <- as.numeric(p.dd.p_cotton_share$coefficients[c(1, 1:3, 3)])
p.hay.coef <- as.numeric(p.dd.p_hay_share$coefficients[c(1, 1:3, 3)])
p.wheat.coef <- as.numeric(p.dd.p_wheat_share$coefficients[c(1, 1:3, 3)])
p.soybean.coef <- as.numeric(p.dd.p_soybean_share$coefficients[c(1, 1:3, 3)])

diff.corn.coef <- as.numeric(diff.dd.p_corn_share$coefficients[c(1, 1:3, 3)])
diff.cotton.coef <- as.numeric(diff.dd.p_cotton_share$coefficients[c(1, 1:3, 3)])
diff.hay.coef <- as.numeric(diff.dd.p_hay_share$coefficients[c(1, 1:3, 3)])
diff.wheat.coef <- as.numeric(diff.dd.p_wheat_share$coefficients[c(1, 1:3, 3)])
diff.soybean.coef <- as.numeric(diff.dd.p_soybean_share$coefficients[c(1, 1:3, 3)])

cs.corn.se <- as.numeric(cs.dd.p_corn_share$se[c(1, 1:3, 3)])
cs.cotton.se <- as.numeric(cs.dd.p_cotton_share$se[c(1, 1:3, 3)])
cs.hay.se <- as.numeric(cs.dd.p_hay_share$se[c(1, 1:3, 3)])
cs.wheat.se <- as.numeric(cs.dd.p_wheat_share$se[c(1, 1:3, 3)])
cs.soybean.se <- as.numeric(cs.dd.p_soybean_share$se[c(1, 1:3, 3)])

p.corn.se <- as.numeric(p.dd.p_corn_share$se[c(1, 1:3, 3)])
p.cotton.se <- as.numeric(p.dd.p_cotton_share$se[c(1, 1:3, 3)])
p.hay.se <- as.numeric(p.dd.p_hay_share$se[c(1, 1:3, 3)])
p.wheat.se <- as.numeric(p.dd.p_wheat_share$se[c(1, 1:3, 3)])
p.soybean.se <- as.numeric(p.dd.p_soybean_share$se[c(1, 1:3, 3)])

diff.corn.se <- as.numeric(diff.dd.p_corn_share$se[c(1, 1:3, 3)])
diff.cotton.se <- as.numeric(diff.dd.p_cotton_share$se[c(1, 1:3, 3)])
diff.hay.se <- as.numeric(diff.dd.p_hay_share$se[c(1, 1:3, 3)])
diff.wheat.se <- as.numeric(diff.dd.p_wheat_share$se[c(1, 1:3, 3)])
diff.soybean.se <- as.numeric(diff.dd.p_soybean_share$se[c(1, 1:3, 3)])

seg.dat <- data.frame(degree = rep(c(0, 10, 30, 35, 40), 15, each = 1),
                    coef = c(cs.corn.coef, cs.cotton.coef, cs.hay.coef, cs.wheat.coef, cs.soybean.coef,
                               p.corn.coef, p.cotton.coef, p.hay.coef, p.wheat.coef, p.soybean.coef,
                               diff.corn.coef, diff.cotton.coef, diff.hay.coef, diff.wheat.coef, diff.soybean.coef),
                      se = c(cs.corn.se, cs.cotton.se, cs.hay.se, cs.wheat.se, cs.soybean.se,
                               p.corn.se, p.cotton.se, p.hay.se, p.wheat.se, p.soybean.se,
                               diff.corn.se, diff.cotton.se, diff.hay.se, diff.wheat.se, diff.soybean.se),
                      crop = rep(c("corn", "cotton", "hay", "wheat", "soybean"), 3, each = 5),
                      reg = rep(c("cross-section", "panel", "diff"), 1, each = 25))


seg.dat$ymin <- seg.dat$coef - 1.96*seg.dat$se
seg.dat$ymax <- seg.dat$coef + 1.96*seg.dat$se


ggplot(seg.dat) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) + 
  geom_line(aes(x = degree, y = coef, color = reg, group = interaction(crop, reg))) +
  geom_line(aes(y = ymin, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 1) + 
  geom_line(aes(y = ymax, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 1) + 
  facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Proportion of Crop Acres") + ggtitle("Proportion of Crop Acres Segemented Regressions") + 
  theme_tufte() + theme(legend.position="top")


##############################################
##############################################
# Spline Regressions ------------------------------------------------------

p.dat <- readRDS("data/panel_regression_data.rds")
td <- readRDS("data/fips_degree_time_1900-2013.rds")
td$year <- factor(td$year)
td$fips <- factor(td$fips)
p.dat <- left_join(p.dat, td, by = c("year", "fips"))

p.dat[, 118] <- rowSums(p.dat[, 118:123])
# Spline
Tindex = 0:40 + .5
DMat = as.matrix(ns(Tindex, df = 7))
XMat <- as.matrix(p.dat[, 78:118]) %*% DMat

p.corndat <- filter(p.dat, !is.na(ln_corn_rrev))
corn.XMat <- as.matrix(p.corndat[, 78:118]) %*% DMat

p.cottondat <- filter(p.dat, !is.na(ln_cotton_rrev))
cotton.XMat <- as.matrix(p.cottondat[, 78:118]) %*% DMat

p.haydat <- filter(p.dat, !is.na(ln_hay_rrev))
hay.XMat <- as.matrix(p.haydat[, 78:118]) %*% DMat

p.wheatdat <- filter(p.dat, !is.na(ln_wheat_rrev))
wheat.XMat <- as.matrix(p.wheatdat[, 78:118]) %*% DMat

p.soybeandat <- filter(p.dat, !is.na(ln_soybean_rrev))
soybean.XMat <- as.matrix(p.soybeandat[, 78:118]) %*% DMat

# Fit
p.corn.fit <- felm(ln_corn_rrev ~  corn.XMat + prec + prec_sq | fips + year | 0 | state, 
                   data = p.corndat, weights = p.corndat$corn_grain_a)
summary(p.corn.fit)

p.cotton.fit <- felm(ln_cotton_rrev ~ cotton.XMat + prec + prec_sq | fips + year | 0 | state, 
                     data = p.cottondat, weights = p.cottondat$cotton_a)
summary(p.cotton.fit)

p.hay.fit <- felm(ln_hay_rrev ~ hay.XMat + prec + prec_sq | fips + year | 0 | state, 
                  data = p.haydat, weights = p.haydat$hay_a)
summary(p.hay.fit)

p.wheat.fit <- felm(ln_wheat_rrev ~  wheat.XMat + prec + prec_sq | fips + year | 0 | state, 
                    data = p.wheatdat, weights = p.wheatdat$wheat_a)
summary(p.wheat.fit)

p.soybean.fit <- felm(ln_soybean_rrev ~ soybean.XMat +  prec + prec_sq | fips + year | 0 | state, 
                      data = p.soybeandat, weights = p.soybeandat$soybean_a)
summary(p.soybean.fit)

# Fit coefficients into DMat
p.corn.coef <- DMat %*% matrix(p.corn.fit$coefficients[1:7], ncol = 1)
p.corn.se <- DMat %*% matrix(p.corn.fit$se[1:7], ncol = 1)

p.cotton.coef <- DMat %*% matrix(p.cotton.fit$coefficients[1:7], ncol = 1)
p.cotton.se <- DMat %*% matrix(p.cotton.fit$se[1:7], ncol = 1)
plot(p.cotton.coef)

p.hay.coef <- DMat %*% matrix(p.hay.fit$coefficients[1:7], ncol = 1)
p.hay.se <- DMat %*% matrix(p.hay.fit$se[1:7], ncol = 1)
plot(p.hay.coef)

p.wheat.coef <- DMat %*% matrix(p.wheat.fit$coefficients[1:7], ncol = 1)
p.wheat.se <- DMat %*% matrix(p.wheat.fit$se[1:7], ncol = 1)
plot(p.wheat.coef)

p.soybean.coef <- DMat %*% matrix(p.soybean.fit$coefficients[1:7], ncol = 1)
p.soybean.se <- DMat %*% matrix(p.soybean.fit$se[1:7], ncol = 1)
plot(p.soybean.coef)


# Spline data set to plot
spline.pdat <- data.frame(degree = 0:40,
                   coef = c(p.corn.coef, p.cotton.coef, p.hay.coef, p.wheat.coef, p.soybean.coef),
                   se = c(p.corn.se, p.cotton.se, p.hay.se, p.wheat.se, p.soybean.se),
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 41))

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

##
##
# Cross-section
cs.dat <- readRDS("data/cross_section_regression_data.rds")
td <- readRDS("data/fips_degree_time_1900-2013.rds")
td <- filter(td, year >= 1950 & year <= 2010)
td$year <- NULL

td <- td %>% 
  group_by(fips) %>% 
  summarise_all(funs(mean))

td$fips <- factor(td$fips)
cs.dat$fips <- factor(cs.dat$fips)
cs.dat <- left_join(cs.dat, td, by = c("fips"))

#cs.dat[, 84] <- rowSums(cs.dat[, 84:89])

# Spline
Tindex = 0:40 + .5
DMat = as.matrix(ns(Tindex, df = 7))
XMat <- as.matrix(cs.dat[, 44:84]) %*% DMat

cs.corndat <- filter(cs.dat, !is.na(ln_corn_rrev))
corn.XMat <- as.matrix(cs.corndat[, 44:84]) %*% DMat

cs.cottondat <- filter(cs.dat, !is.na(ln_cotton_rrev))
cotton.XMat <- as.matrix(cs.cottondat[44:84]) %*% DMat

cs.haydat <- filter(cs.dat, !is.na(ln_hay_rrev))
hay.XMat <- as.matrix(cs.haydat[, 44:84]) %*% DMat

cs.wheatdat <- filter(cs.dat, !is.na(ln_wheat_rrev))
wheat.XMat <- as.matrix(cs.wheatdat[, 44:84]) %*% DMat

cs.soybeandat <- filter(cs.dat, !is.na(ln_soybean_rrev))
soybean.XMat <- as.matrix(cs.soybeandat[, 44:84]) %*% DMat

# Fit
cs.corn.fit <- felm(ln_corn_rrev ~  corn.XMat + prec + prec_sq | state | 0 | state, 
                   data = cs.corndat, weights = cs.corndat$corn_grain_a)
summary(cs.corn.fit)

cs.cotton.fit <- felm(ln_cotton_rrev ~ cotton.XMat + prec + prec_sq | state | 0 | state, 
                     data = cs.cottondat, weights = cs.cottondat$cotton_a)
summary(cs.cotton.fit)

cs.hay.fit <- felm(ln_hay_rrev ~ hay.XMat + prec + prec_sq | state | 0 | state, 
                  data = cs.haydat, weights = cs.haydat$hay_a)
summary(cs.hay.fit)

cs.wheat.fit <- felm(ln_wheat_rrev ~  wheat.XMat + prec + prec_sq | state | 0 | state, 
                    data = cs.wheatdat, weights = cs.wheatdat$wheat_a)
summary(cs.wheat.fit)

cs.soybean.fit <- felm(ln_soybean_rrev ~ soybean.XMat +  prec + prec_sq | state | 0 | state, 
                      data = cs.soybeandat, weights = cs.soybeandat$soybean_a)
summary(cs.soybean.fit)

# Fit coefficients into DMat
cs.corn.coef <- DMat %*% matrix(cs.corn.fit$coefficients[1:7], ncol = 1)
cs.corn.se <- DMat %*% matrix(cs.corn.fit$se[1:7], ncol = 1)
plot(cs.corn.coef)

cs.cotton.coef <- DMat %*% matrix(cs.cotton.fit$coefficients[1:7], ncol = 1)
cs.cotton.se <- DMat %*% matrix(cs.cotton.fit$se[1:7], ncol = 1)
plot(cs.cotton.coef)

cs.hay.coef <- DMat %*% matrix(cs.hay.fit$coefficients[1:7], ncol = 1)
cs.hay.se <- DMat %*% matrix(cs.hay.fit$se[1:7], ncol = 1)
plot(cs.hay.coef)

cs.wheat.coef <- DMat %*% matrix(cs.wheat.fit$coefficients[1:7], ncol = 1)
cs.wheat.se <- DMat %*% matrix(cs.wheat.fit$se[1:7], ncol = 1)
plot(cs.wheat.coef)

cs.soybean.coef <- DMat %*% matrix(cs.soybean.fit$coefficients[1:7], ncol = 1)
cs.soybean.se <- DMat %*% matrix(cs.soybean.fit$se[1:7], ncol = 1)
plot(cs.soybean.coef)


# Spline data set to plot
spline.csdat <- data.frame(degree = 0:40,
                   coef = c(cs.corn.coef, cs.cotton.coef, cs.hay.coef, cs.wheat.coef, cs.soybean.coef),
                   se = c(cs.corn.se, cs.cotton.se, cs.hay.se, cs.wheat.se, cs.soybean.se),
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 41))

spline.csdat$ymin <- spline.csdat$coef - spline.csdat$se*1.97
spline.csdat$ymax <- spline.csdat$coef + spline.csdat$se*1.97
spline.csdat$reg <- "Panel"

ggplot(spline.csdat, aes(x = degree, y = coef, color = crop)) + 
  geom_line() + facet_wrap(~crop) + 
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) +
  geom_line(aes(y = ymin, x = degree), linetype = "dotted") +
  geom_line(aes(y = ymax, x = degree), linetype = "dotted")  + ylab("Log Revenue") + 
  xlab("Temperature (C)") +
  theme_tufte() + theme(legend.position="none")

# Difference
diff.dat <- readRDS("data/diff_regression_data.rds")
td <- readRDS("data/fips_degree_time_1900-2013.rds")
td <- filter(td, year >= 1930 & year <= 2010)
td$year <- (td$year %/% 10)  * 10

td <- td %>% 
  group_by(fips, year) %>% 
  summarise_all(funs(mean))

td$fips <- factor(td$fips)
diff.dat$fips <- factor(diff.dat$fips)
diff.dat <- left_join(diff.dat, td, by = c("fips", "year"))

#diff.dat[, 69] <- rowSums(diff.dat[, 69:74])

# Spline
Tindex = 0:35 + .5
DMat = as.matrix(ns(Tindex, df = 7))
XMat <- as.matrix(diff.dat[, 29:64]) %*% DMat

diff.corndat <- filter(diff.dat, !is.na(ln_corn_rrev))
corn.XMat <- as.matrix(diff.corndat[, 29:64]) %*% DMat

diff.cottondat <- filter(diff.dat, !is.na(ln_cotton_rrev))
cotton.XMat <- as.matrix(diff.cottondat[, 29:64]) %*% DMat

diff.haydat <- filter(diff.dat, !is.na(ln_hay_rrev))
hay.XMat <- as.matrix(diff.haydat[, 29:64]) %*% DMat

diff.wheatdat <- filter(diff.dat, !is.na(ln_wheat_rrev))
wheat.XMat <- as.matrix(diff.wheatdat[, 29:64]) %*% DMat

diff.soybeandat <- filter(diff.dat, !is.na(ln_soybean_rrev))
soybean.XMat <- as.matrix(diff.soybeandat[, 29:64]) %*% DMat

# Fit
diff.corn.fit <- felm(ln_corn_rrev ~  corn.XMat + prec + prec_sq | state | 0 | state, 
                   data = diff.corndat, weights = diff.corndat$corn_grain_a)
summary(diff.corn.fit)

diff.cotton.fit <- felm(ln_cotton_rrev ~ cotton.XMat + prec + prec_sq | state | 0 | state, 
                     data = diff.cottondat, weights = diff.cottondat$cotton_a)
summary(diff.cotton.fit)

diff.hay.fit <- felm(ln_hay_rrev ~ hay.XMat + prec + prec_sq | state | 0 | state, 
                  data = diff.haydat, weights = diff.haydat$hay_a)
summary(diff.hay.fit)

diff.wheat.fit <- felm(ln_wheat_rrev ~  wheat.XMat + prec + prec_sq | state | 0 | state, 
                    data = diff.wheatdat, weights = diff.wheatdat$wheat_a)
summary(diff.wheat.fit)

diff.soybean.fit <- felm(ln_soybean_rrev ~ soybean.XMat +  prec + prec_sq | state | 0 | state, 
                      data = diff.soybeandat, weights = diff.soybeandat$soybean_a)
summary(diff.soybean.fit)

# Fit coefficients into DMat
diff.corn.coef <- DMat %*% matrix(diff.corn.fit$coefficients[1:7], ncol = 1)
diff.corn.se <- DMat %*% matrix(diff.corn.fit$se[1:7], ncol = 1)
plot(diff.corn.coef)

diff.cotton.coef <- DMat %*% matrix(diff.cotton.fit$coefficients[1:7], ncol = 1)
diff.cotton.se <- DMat %*% matrix(diff.cotton.fit$se[1:7], ncol = 1)
plot(diff.cotton.coef)

diff.hay.coef <- DMat %*% matrix(diff.hay.fit$coefficients[1:7], ncol = 1)
diff.hay.se <- DMat %*% matrix(diff.hay.fit$se[1:7], ncol = 1)
plot(diff.hay.coef)

diff.wheat.coef <- DMat %*% matrix(diff.wheat.fit$coefficients[1:7], ncol = 1)
diff.wheat.se <- DMat %*% matrix(diff.wheat.fit$se[1:7], ncol = 1)
plot(diff.wheat.coef)

diff.soybean.coef <- DMat %*% matrix(diff.soybean.fit$coefficients[1:7], ncol = 1)
diff.soybean.se <- DMat %*% matrix(diff.soybean.fit$se[1:7], ncol = 1)
plot(diff.soybean.coef)


# Spline data set to plot
spline.diffdat <- data.frame(degree = 0:35,
                   coef = c(diff.corn.coef, diff.cotton.coef, diff.hay.coef, diff.wheat.coef, diff.soybean.coef),
                   se = c(diff.corn.se, diff.cotton.se, diff.hay.se, diff.wheat.se, diff.soybean.se),
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 36))

spline.diffdat$ymin <- spline.diffdat$coef - spline.diffdat$se*1.97
spline.diffdat$ymax <- spline.diffdat$coef + spline.diffdat$se*1.97
spline.diffdat$reg <- "Diff"

ggplot(spline.diffdat, aes(x = degree, y = coef, color = crop)) + 
  geom_line() + facet_wrap(~crop) + 
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) +
  geom_line(aes(y = ymin, x = degree), linetype = "dotted") +
  geom_line(aes(y = ymax, x = degree), linetype = "dotted")  + ylab("Log Revenue") + 
  xlab("Temperature (C)") +
  theme_tufte() + theme(legend.position="none")




spline.plot <- ggplot(pcsdat) + 
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) + 
  geom_line(aes(x = degree, y = coef, color = reg, group = interaction(crop, reg))) +
  geom_line(aes(y = ymin, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted") + 
  geom_line(aes(y = ymax, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted") +
  facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Log Revenue") + theme_tufte() + theme(legend.position="none")
spline.plot 



##############################################
##############################################
# Step Regressions ---------------------------------------------------
# p.dat <- readRDS("data/panel_regression_data.rds")
# td <- readRDS("data/fips_degree_time_1900-2013.rds")
# 
# td$year <- factor(td$year)
# td$fips <- factor(td$fips)
# p.dat <- left_join(p.dat, td, by = c("year", "fips"))
# 
# # 5-C intervals starting at 10C
# p.dat$a <- rowSums(p.dat[, 84:88])
# p.dat$b <- rowSums(p.dat[, 89:93])
# p.dat$c <- rowSums(p.dat[, 94:98])
# p.dat$d <- rowSums(p.dat[, 99:103])
# p.dat$e <- rowSums(p.dat[, 104:108])
# p.dat$f <- rowSums(p.dat[, 109:113])
# p.dat$g <- rowSums(p.dat[, 114:119])
# 
# # Fit
# p.corn.fit <- felm(ln_corn_rrev ~  a + b + c + d + e + f + g + prec + prec_sq | fips + year | 0 | state, data = p.dat)
# summary(p.corn.fit)
# 
# p.cotton.fit <- felm(ln_cotton_rrev ~ a + b + c + d + e + f + g + prec + prec_sq | fips + year | 0 | state, data = p.dat)
# summary(p.cotton.fit)
# 
# p.hay.fit <- felm(ln_hay_rrev ~ a + b + c + d + e + f + g + prec + prec_sq | fips + year | 0 | state, data = p.dat)
# summary(p.hay.fit)
# 
# p.wheat.fit <- felm(ln_wheat_rrev ~ a + b + c + d + e + f + g + prec + prec_sq | fips + year | 0 | state, data = p.dat)
# summary(p.wheat.fit)
# 
# p.soybean.fit <- felm(ln_soybean_rrev ~ a + b + c + d + e + f + g +  prec + prec_sq | fips + year | 0 | state, data = p.dat)
# summary(p.soybean.fit)
# 
# p.corn.coef <- as.numeric(p.corn.fit$coefficients[c(1:7)])
# p.cotton.coef <- as.numeric(p.cotton.fit$coefficients[c(1:7)])
# p.hay.coef <- as.numeric(p.hay.fit$coefficients[c(1:7)])
# p.wheat.coef <- as.numeric(p.wheat.fit$coefficients[c(1:7)])
# p.soybean.coef <- as.numeric(p.soybean.fit$coefficients[c(1:7)])
# 
# seg.pdat <- data.frame(degree = rep(c(10, 15, 20, 25, 30, 35, 40), 5),
#                    coef = c(p.corn.coef, p.cotton.coef, p.hay.coef, p.wheat.coef, p.soybean.coef),
#                    se = c(p.corn.fit$se[c(1:7)], p.cotton.fit$se[c(1:7)], p.hay.fit$se[c(1:7)], 
#                         p.wheat.fit$se[c(1:7)], p.soybean.fit$se[c(1:7)]), 
#                    crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 7))
# seg.pdat$ymin <- seg.pdat$coef - seg.pdat$se*1.97
# seg.pdat$ymax <- seg.pdat$coef + seg.pdat$se*1.97
# seg.pdat$reg <- "Panel"
# 
# ggplot(seg.pdat, aes(x = degree, y = coef, color = crop)) + geom_line() + 
#   geom_line(aes(y = ymin, x = degree, color = crop), linetype = "dashed") + 
#   geom_line(aes(y = ymax, x = degree, color = crop), linetype = "dashed")
# 
# 
# # Cross-section
# cs.dat <- readRDS("data/cross_section_regression_data.rds")
# td <- readRDS("data/fips_degree_time_1900-2013.rds")
# 
# # Only have cross-sectional data >= 1970 
# td <- filter(td, year >= 1970 & year <= 2010)
# td$year <- NULL
# td <- td %>% 
#   group_by(fips) %>% 
#   summarise_all(funs(mean))
# 
# 
# td$fips <- factor(td$fips)
# cs.dat$fips <- factor(cs.dat$fips)
# cs.dat <- left_join(cs.dat, td, by = c("fips"))
# 
# # 5 C intervals
# cs.dat$a <- rowSums(cs.dat[, 55:59])
# cs.dat$b <- rowSums(cs.dat[, 60:64])
# cs.dat$c <- rowSums(cs.dat[, 65:69])
# cs.dat$d <- rowSums(cs.dat[, 70:74])
# cs.dat$e <- rowSums(cs.dat[, 75:79])
# cs.dat$f <- rowSums(cs.dat[, 80:84])
# cs.dat$g <- rowSums(cs.dat[, 85:90])
# 
# cs.dat$state <- factor(cs.dat$state)
# cs.dat <- filter(cs.dat, state == "mi")
# # Fit
# 
# cs.corn.fit <- felm(ln_corn_rrev ~  a + b + c + d + e + f + g + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
#                   data = cs.dat)
# summary(cs.corn.fit)
# 
# cs.cotton.fit <- felm(ln_cotton_rrev ~ a + b + c + d + e + f + g + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
#                   data = cs.dat)
# summary(cs.cotton.fit)
# 
# cs.hay.fit <- felm(ln_hay_rrev ~  a + b + c + d + e + f + g + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
#                   data = cs.dat)
# summary(cs.hay.fit)
# 
# cs.wheat.fit <- felm(ln_wheat_rrev ~  a + b + c + d + e + f + g +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, data = cs.dat)
# summary(cs.wheat.fit)
# 
# cs.soybean.fit <- felm(ln_soybean_rrev ~  a + b + c + d + e + f + g +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, data = cs.dat)
# summary(cs.soybean.fit)
# 
# # Get coefficients
# cs.corn.coef <- as.numeric(cs.corn.fit$coefficients[1:7])
# cs.cotton.coef <- as.numeric(cs.cotton.fit$coefficients[1:7])
# cs.hay.coef <- as.numeric(cs.hay.fit$coefficients[1:7])
# cs.wheat.coef <- as.numeric(cs.wheat.fit$coefficients[1:7])
# cs.soybean.coef <- as.numeric(cs.soybean.fit$coefficients[1:7])
# 
# # Build data frame for plot
# seg.csdat <- data.frame(degree = rep(c(10, 15, 20, 25, 30, 35, 40), 5),
#                    coef = c(cs.corn.coef, cs.cotton.coef, cs.hay.coef, cs.wheat.coef, cs.soybean.coef),
#                    se = c(cs.corn.fit$se[1:7], cs.cotton.fit$se[1:7], cs.hay.fit$se[1:7], 
#                           cs.wheat.fit$se[1:7], cs.soybean.fit$se[1:7]), 
#                    crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 7))
# 
# seg.csdat$ymin <- seg.csdat$coef - seg.csdat$se*1.97
# seg.csdat$ymax <- seg.csdat$coef + seg.csdat$se*1.97
# seg.csdat$reg<- "Cross-section"
# seg.pcsdat <- rbind(seg.csdat, seg.pdat)
# 
# seg.plot <- ggplot(seg.pcsdat) + 
#   geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) + 
#   geom_line(aes(x = degree, y = coef, color = reg, group = interaction(crop, reg))) +
#   geom_line(aes(y = ymin, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 0.5) + 
#   geom_line(aes(y = ymax, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 0.5) + 
#   facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Log Revenue") + theme_tufte() + ggtitle("Segemented Regressions") + 
#   theme(legend.position="top")
# seg.plot 
# 
# 

##############################################
##############################################
# Spline Regressions ------------------------------------------------------
# 
# p.dat <- readRDS("data/panel_regression_data.rds")
# td <- readRDS("data/fips_degree_time_1900-2013.rds")
# td$year <- factor(td$year)
# td$fips <- factor(td$fips)
# p.dat <- left_join(p.dat, td, by = c("year", "fips"))
# 
# # Spline
# Tindex = 0:40 + .5
# DMat = as.matrix(ns(Tindex, df = 5))
# XMat <- as.matrix(p.dat[, 72:112]) %*% DMat
# 
# # Fit
# p.corn.fit <- felm(ln_corn_rrev ~  XMat + prec + prec_sq | fips + year | 0 | state, data = p.dat)
# summary(p.corn.fit)
# 
# p.cotton.fit <- felm(ln_cotton_rrev ~ XMat + prec + prec_sq | fips + year | 0 | state, data = p.dat)
# summary(p.cotton.fit)
# 
# p.hay.fit <- felm(ln_hay_rrev ~ XMat + prec + prec_sq | fips + year | 0 | state, data = p.dat)
# summary(p.hay.fit)
# 
# p.wheat.fit <- felm(ln_wheat_rrev ~  XMat + prec + prec_sq | fips + year | 0 | state, data = p.dat)
# summary(p.wheat.fit)
# 
# p.soybean.fit <- felm(ln_soybean_rrev ~ XMat +  prec + prec_sq | fips + year | 0 | state, data = p.dat)
# summary(p.soybean.fit)
# 
# # Fit coefficients into DMat
# p.corn.coef <- DMat %*% matrix(p.corn.fit$coefficients[1:5], ncol = 1)
# plot(scale(p.corn.coef))
# p.corn.se <- DMat %*% matrix(p.corn.fit$se[1:5], ncol = 1)
# plot(scale(p.corn.se))
# 
# p.cotton.coef <- DMat %*% matrix(p.cotton.fit$coefficients[1:5], ncol = 1)
# p.cotton.se <- DMat %*% matrix(p.cotton.fit$se[1:5], ncol = 1)
# plot(p.cotton.coef)
# 
# p.hay.coef <- DMat %*% matrix(p.hay.fit$coefficients[1:5], ncol = 1)
# p.hay.se <- DMat %*% matrix(p.hay.fit$se[1:5], ncol = 1)
# plot(p.hay.coef)
# 
# p.wheat.coef <- DMat %*% matrix(p.wheat.fit$coefficients[1:5], ncol = 1)
# p.wheat.se <- DMat %*% matrix(p.wheat.fit$se[1:5], ncol = 1)
# plot(p.wheat.coef)
# 
# p.soybean.coef <- DMat %*% matrix(p.soybean.fit$coefficients[1:5], ncol = 1)
# p.soybean.se <- DMat %*% matrix(p.soybean.fit$se[1:5], ncol = 1)
# plot(p.soybean.coef)
# 
# 
# # Spline data set to plot
# spline.pdat <- data.frame(degree = 0:45,
#                    coef = c(p.corn.coef, p.cotton.coef, p.hay.coef, p.wheat.coef, p.soybean.coef),
#                    se = c(p.corn.se, p.cotton.se, p.hay.se, p.wheat.se, p.soybean.se),
#                    crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 46))
# 
# spline.pdat$ymin <- spline.pdat$coef - spline.pdat$se*1.97
# spline.pdat$ymax <- spline.pdat$coef + spline.pdat$se*1.97
# spline.pdat$reg <- "Panel"
# 
# ggplot(spline.pdat, aes(x = degree, y = coef, color = crop)) + 
#   geom_line() + facet_wrap(~crop) + 
#   geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) +
#   geom_line(aes(y = ymin, x = degree), linetype = "dotted") +
#   geom_line(aes(y = ymax, x = degree), linetype = "dotted")  + ylab("Log Revenue") + 
#   xlab("Temperature (C)") +
#   theme_tufte() + theme(legend.position="none")
# 
# # Cross-section
# cs.dat <- readRDS("data/cross_section_regression_data.rds")
# td <- readRDS("data/fips_degree_time_1900-2013.rds")
# td <- filter(td, year >= 1970 & year <= 2010)
# td$year <- NULL
# 
# td <- td %>% 
#   group_by(fips) %>% 
#   summarise_all(funs(mean))
# 
# td$fips <- factor(td$fips)
# cs.dat$fips <- factor(cs.dat$fips)
# cs.dat <- left_join(cs.dat, td, by = c("fips"))
# 
# Tindex = 10:45 + .5
# DMat = as.matrix(ns(Tindex, df = 7))
# XMat <- as.matrix(cs.dat[, 47:82]) %*% DMat
# 
# 
# #cs.dat$fourty <- rowSums(cs.dat[, 101:112])
# cs.corn.fit <- felm(ln_corn_rrev ~  XMat + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, exactDOF = TRUE,
#                   data = cs.dat)
# 
# summary(cs.corn.fit)
# 
# cs.cotton.fit <- felm(ln_cotton_rrev ~ XMat + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
#                   data = cs.dat)
# summary(cs.cotton.fit)
# 
# cs.hay.fit <- felm(ln_hay_rrev ~  XMat + prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
#                   data = cs.dat)
# summary(cs.hay.fit)
# 
# cs.wheat.fit <- felm(ln_wheat_rrev ~  XMat +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, data = cs.dat)
# summary(cs.wheat.fit)
# 
# cs.soybean.fit <- felm(ln_soybean_rrev ~  XMat +  prec + prec_sq + lat + ipc + pop_dens + pop_dens_sq + 
#                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, data = cs.dat)
# summary(cs.soybean.fit)
# 
# 
# # Spline through coefficients
# cs.corn.coef <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.corn.fit$coefficients[1:7], n = 7*10)
# cs.cotton.coef <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.cotton.fit$coefficients[1:7], n = 7*10)
# cs.hay.coef <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.hay.fit$coefficients[1:7], n = 7*10)
# cs.wheat.coef <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.wheat.fit$coefficients[1:7], n = 7*10)
# cs.soybean.coef <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.soybean.fit$coefficients[1:7], n = 7*10)
# 
# # Spline through SE
# cs.corn.se <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.corn.fit$se[1:7], n = 7*10)
# cs.cotton.se <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.cotton.fit$se[1:7], n = 7*10)
# cs.hay.se <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.hay.fit$se[1:7], n = 7*10)
# cs.wheat.se <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.wheat.fit$se[1:7], n = 7*10)
# cs.soybean.se <- spline(x = c(10, 15.5, 20.5, 25.5, 30.5, 35.5, 40.5), y = cs.soybean.fit$se[1:7], n = 7*10)
# 
# spline.csdat <- data.frame(degree = c(cs.corn.coef$x, cs.cotton.coef$x, cs.hay.coef$x, cs.wheat.coef$x, cs.soybean.coef$x),
#                    coef = c(cs.corn.coef$y, cs.cotton.coef$y, cs.hay.coef$y, cs.wheat.coef$y, cs.soybean.coef$y),
#                    se = c(cs.corn.se$y, cs.cotton.se$y, cs.hay.se$y, cs.wheat.se$y, cs.soybean.se$y),
#                    crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 70))
# 
# spline.csdat$ymin <- spline.csdat$coef - spline.csdat$se*1.97
# spline.csdat$ymax <- spline.csdat$coef + spline.csdat$se*1.97
# spline.csdat$reg <- "Cross-section"
# spline.pcsdat <- rbind(spline.csdat, spline.pdat)
# 
# spline.plot <- ggplot(pcsdat) + 
#   geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) + 
#   geom_line(aes(x = degree, y = coef, color = reg, group = interaction(crop, reg))) +
#   geom_line(aes(y = ymin, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted") + 
#   geom_line(aes(y = ymax, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted") +
#   facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Log Revenue") + theme_tufte() + theme(legend.position="none")
# spline.plot 
# 
# library(cowplot)
# plot_grid(seg.plot, spline.plot, ncol = 1)

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
