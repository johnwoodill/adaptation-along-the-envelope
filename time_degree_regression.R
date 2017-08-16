library(lfe)
library(readr)
library(dplyr)
library(Hmisc)
library(ggthemes)
library(splines)
library(cowplot)

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

revseg.dat <- data.frame(degree = rep(c(0, 10, 30, 35, 40), 15, each = 1),
                    coef = c(cs.corn.coef, cs.cotton.coef, cs.hay.coef, cs.wheat.coef, cs.soybean.coef,
                               p.corn.coef, p.cotton.coef, p.hay.coef, p.wheat.coef, p.soybean.coef,
                               diff.corn.coef, diff.cotton.coef, diff.hay.coef, diff.wheat.coef, diff.soybean.coef),
                      se = c(cs.corn.se, cs.cotton.se, cs.hay.se, cs.wheat.se, cs.soybean.se,
                               p.corn.se, p.cotton.se, p.hay.se, p.wheat.se, p.soybean.se,
                               diff.corn.se, diff.cotton.se, diff.hay.se, diff.wheat.se, diff.soybean.se),
                      crop = rep(c("corn", "cotton", "hay", "wheat", "soybean"), 3, each = 5),
                      reg = rep(c("cross-section", "panel", "diff"), 1, each = 25))


revseg.dat$ymin <- revseg.dat$coef - 1.96*revseg.dat$se
revseg.dat$ymax <- revseg.dat$coef + 1.96*revseg.dat$se


rev.seg <- ggplot(revseg.dat) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) + 
  geom_line(aes(x = degree, y = coef, color = reg, group = interaction(crop, reg))) +
  geom_line(aes(y = ymin, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 1) + 
  geom_line(aes(y = ymax, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 1) + 
  facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Log Revenue") + ggtitle("Log Revenue Segmented Regressions") + 
   theme_tufte() + theme(legend.position="top")
rev.seg

##############################################
##############################################
# Segmented Regressions ---------------------------------------------------

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

cs.corn.coef <- as.numeric(cs.dd.p_corn_share$coefficients[c(2, 2:4, 4)])
cs.cotton.coef <- as.numeric(cs.dd.p_cotton_share$coefficients[c(2, 2:4, 4)])
cs.hay.coef <- as.numeric(cs.dd.p_hay_share$coefficients[c(2, 2:4, 4)])
cs.wheat.coef <- as.numeric(cs.dd.p_wheat_share$coefficients[c(2, 2:4, 4)])
cs.soybean.coef <- as.numeric(cs.dd.p_soybean_share$coefficients[c(2, 2:4, 4)])

p.corn.coef <- as.numeric(p.dd.p_corn_share$coefficients[c(2, 2:4, 4)])
p.cotton.coef <- as.numeric(p.dd.p_cotton_share$coefficients[c(2, 2:4, 4)])
p.hay.coef <- as.numeric(p.dd.p_hay_share$coefficients[c(2, 2:4, 4)])
p.wheat.coef <- as.numeric(p.dd.p_wheat_share$coefficients[c(2, 2:4, 4)])
p.soybean.coef <- as.numeric(p.dd.p_soybean_share$coefficients[c(2, 2:4, 4)])

diff.corn.coef <- as.numeric(diff.dd.p_corn_share$coefficients[c(2, 2:4, 4)])
diff.cotton.coef <- as.numeric(diff.dd.p_cotton_share$coefficients[c(2, 2:4, 4)])
diff.hay.coef <- as.numeric(diff.dd.p_hay_share$coefficients[c(2, 2:4, 4)])
diff.wheat.coef <- as.numeric(diff.dd.p_wheat_share$coefficients[c(2, 2:4, 4)])
diff.soybean.coef <- as.numeric(diff.dd.p_soybean_share$coefficients[c(2, 2:4, 4)])

cs.corn.se <- as.numeric(summary(cs.dd.p_corn_share)[[7]][2:4, 2])
cs.cotton.se <- as.numeric(summary(cs.dd.p_cotton_share)[[7]][2:4,2])
cs.hay.se <- as.numeric(summary(cs.dd.p_hay_share)[[7]][2:4,2])
cs.wheat.se <- as.numeric(summary(cs.dd.p_wheat_share)[[7]][2:4,2])
cs.soybean.se <- as.numeric(summary(cs.dd.p_soybean_share)[[7]][2:4,2])

p.corn.se <- as.numeric(summary(p.dd.p_corn_share)[[7]][2:4,2])
p.cotton.se <- as.numeric(summary(p.dd.p_cotton_share)[[8]][2:4,2])
p.hay.se <- as.numeric(summary(p.dd.p_hay_share)[[7]][2:4,2])
p.wheat.se <- as.numeric(summary(p.dd.p_wheat_share)[[7]][2:4,2])
p.soybean.se <- as.numeric(summary(p.dd.p_soybean_share)[[7]][2:4,2])

diff.corn.se <- as.numeric(summary(diff.dd.p_corn_share)[[7]][2:4,2])
diff.cotton.se <- as.numeric(summary(diff.dd.p_cotton_share)[[8]][2:4,2])
diff.hay.se <- as.numeric(summary(diff.dd.p_hay_share)[[7]][2:4,2])
diff.wheat.se <- as.numeric(summary(diff.dd.p_wheat_share)[[7]][2:4,2])
diff.soybean.se <- as.numeric(summary(diff.dd.p_soybean_share)[[7]][2:4,2])

pseg.dat <- data.frame(degree = rep(c(0, 10, 30, 35, 40), 15, each = 1),
                    coef = c(cs.corn.coef, cs.cotton.coef, cs.hay.coef, cs.wheat.coef, cs.soybean.coef,
                               p.corn.coef, p.cotton.coef, p.hay.coef, p.wheat.coef, p.soybean.coef,
                               diff.corn.coef, diff.cotton.coef, diff.hay.coef, diff.wheat.coef, diff.soybean.coef),
                      se = c(cs.corn.se[1], cs.corn.se, cs.corn.se[3], 
                             cs.cotton.se[1], cs.cotton.se, cs.cotton.se[3],
                             cs.hay.se[1], cs.hay.se, cs.hay.se[3],
                             cs.wheat.se[1], cs.wheat.se, cs.wheat.se[3],
                             cs.soybean.se[1], cs.soybean.se, cs.soybean.se[3],
                             
                             p.corn.se[1], p.corn.se, p.corn.se[3], 
                             p.cotton.se[1], p.cotton.se, p.cotton.se[3],
                             p.hay.se[1], p.hay.se, p.hay.se[3],
                             p.wheat.se[1], p.wheat.se, p.wheat.se[3],
                             p.soybean.se[1], p.soybean.se, p.soybean.se[3],
                             
                             diff.corn.se[1], diff.corn.se, diff.corn.se[3], 
                             diff.cotton.se[1], diff.cotton.se, diff.cotton.se[3],
                             diff.hay.se[1], diff.hay.se, diff.hay.se[3],
                             diff.wheat.se[1], diff.wheat.se, diff.wheat.se[3],
                             diff.soybean.se[1], diff.soybean.se, diff.soybean.se[3]
                             ),
                      crop = rep(c("corn", "cotton", "hay", "wheat", "soybean"), 3, each = 5),
                      reg = rep(c("cross-section", "panel", "diff"), 1, each = 25))


pseg.dat$ymin <- pseg.dat$coef - 1.96*pseg.dat$se
pseg.dat$ymax <- pseg.dat$coef + 1.96*pseg.dat$se


p.seg <- ggplot(pseg.dat) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) + 
  geom_line(aes(x = degree, y = coef, color = reg, group = interaction(crop, reg))) +
  geom_line(aes(y = ymin, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 1) + 
  geom_line(aes(y = ymax, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 1) + 
  facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Proportion of Crop Acres") + ggtitle("Proportion of Crop Acres Segmented Regressions") + 
  theme_tufte() + theme(legend.position="top")
p.seg
plot_grid(rev.seg, p.seg, ncol = 1)

##############################################
##############################################
# Spline Regressions Log Rev------------------------------------------------------
{

Tindex = 0:40 + .5
DMat = as.matrix(ns(Tindex, df = 5))
td0 <- which(colnames(cs.dat) == "td_0C")
td40 <- td0 + 40
XMat <- as.matrix(cs.dat[, td0:td40]) %*% DMat

cs.corndat <- filter(cs.dat, !is.na(ln_corn_rrev))
corn.XMat <- as.matrix(cs.corndat[, td0:td40]) %*% DMat

cs.cottondat <- filter(cs.dat, !is.na(ln_cotton_rrev))
cotton.XMat <- as.matrix(cs.cottondat[, td0:td40]) %*% DMat

cs.haydat <- filter(cs.dat, !is.na(ln_hay_rrev))
hay.XMat <- as.matrix(cs.haydat[, td0:td40]) %*% DMat

cs.wheatdat <- filter(cs.dat, !is.na(ln_wheat_rrev))
wheat.XMat <- as.matrix(cs.wheatdat[, td0:td40]) %*% DMat

cs.soybeandat <- filter(cs.dat, !is.na(ln_soybean_rrev))
soybean.XMat <- as.matrix(cs.soybeandat[, td0:td40]) %*% DMat

# Fit
cs.corn.fit <- felm(p_corn_share ~  corn.XMat + prec + prec_sq | state| 0 | state, 
                   data = cs.corndat, weights = cs.corndat$total_w)
summary(cs.corn.fit)

cs.cotton.fit <- felm(p_cotton_share ~ cotton.XMat + prec + prec_sq | state | 0 | state, 
                     data = cs.cottondat, weights = cs.cottondat$total_w)
summary(cs.cotton.fit)

cs.hay.fit <- felm(p_hay_share ~ hay.XMat + prec + prec_sq | state | 0 | state, 
                  data = cs.haydat, weights = cs.haydat$total_w)
summary(cs.hay.fit)

cs.wheat.fit <- felm(p_wheat_share ~  wheat.XMat + prec + prec_sq | state | 0 | state, 
                    data = cs.wheatdat, weights = cs.wheatdat$total_w)
summary(cs.wheat.fit)

cs.soybean.fit <- felm(p_soybean_share ~ soybean.XMat +  prec + prec_sq | state | 0 | state, 
                      data = cs.soybeandat, weights = cs.soybeandat$total_w)
summary(cs.soybean.fit)

# Fit coefficients into DMat
cs.corn.coef <- DMat %*% matrix(cs.corn.fit$coefficients[1:5], ncol = 1)
cs.corn.se <- DMat %*% matrix(cs.corn.fit$se[1:5], ncol = 1)
plot(cs.corn.coef)

cs.cotton.coef <- DMat %*% matrix(cs.cotton.fit$coefficients[1:5], ncol = 1)
cs.cotton.se <- DMat %*% matrix(cs.cotton.fit$se[1:5], ncol = 1)
plot(cs.cotton.coef)

cs.hay.coef <- DMat %*% matrix(cs.hay.fit$coefficients[1:5], ncol = 1)
cs.hay.se <- DMat %*% matrix(cs.hay.fit$se[1:5], ncol = 1)
plot(cs.hay.coef)

cs.wheat.coef <- DMat %*% matrix(cs.wheat.fit$coefficients[1:5], ncol = 1)
cs.wheat.se <- DMat %*% matrix(cs.wheat.fit$se[1:5], ncol = 1)
plot(cs.wheat.coef)

cs.soybean.coef <- DMat %*% matrix(cs.soybean.fit$coefficients[1:5], ncol = 1)
cs.soybean.se <- DMat %*% matrix(cs.soybean.fit$se[1:5], ncol = 1)
plot(cs.soybean.coef)


# Spline data set to plot
spline.csdat <- data.frame(degree = rep(0:40, 5),
                   coef = c(cs.corn.coef, cs.cotton.coef, cs.hay.coef, cs.wheat.coef, cs.soybean.coef),
                   se = c(cs.corn.se, cs.cotton.se, cs.hay.se, cs.wheat.se, cs.soybean.se),
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 41))

spline.csdat$ymin <- spline.csdat$coef - spline.csdat$se*1.97
spline.csdat$ymax <- spline.csdat$coef + spline.csdat$se*1.97
spline.csdat$reg <- "Cross-section"

ggplot(spline.csdat, aes(x = degree, y = coef, color = crop)) + 
  geom_line() + facet_wrap(~crop) + 
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) +
  geom_line(aes(y = ymin, x = degree), linetype = "dotted") +
  geom_line(aes(y = ymax, x = degree), linetype = "dotted")  + ylab("Log Revenue") + 
  xlab("Temperature (C)") +
  theme_tufte() + theme(legend.position="none")


# Log Revenue
# Panel
p.dat <- readRDS("data/panel_regression_data.rds")
td <- readRDS("data/fips_degree_time_1900-2013.rds")
p.dat <- left_join(p.dat, td, by = c("year", "fips"))

#p.dat[, 122] <- rowSums(p.dat[, 122:127])
# Spline
Tindex = 0:40 + .5
DMat = as.matrix(ns(Tindex, df = 5))
td0 <- which(colnames(p.dat) == "td_0C")
td40 <- td0 + 40
XMat <- as.matrix(p.dat[, td0:td40]) %*% DMat

p.corndat <- filter(p.dat, !is.na(ln_corn_rrev))
corn.XMat <- as.matrix(p.corndat[, td0:td40]) %*% DMat

p.cottondat <- filter(p.dat, !is.na(ln_cotton_rrev))
cotton.XMat <- as.matrix(p.cottondat[, td0:td40]) %*% DMat

p.haydat <- filter(p.dat, !is.na(ln_hay_rrev))
hay.XMat <- as.matrix(p.haydat[, td0:td40]) %*% DMat

p.wheatdat <- filter(p.dat, !is.na(ln_wheat_rrev))
wheat.XMat <- as.matrix(p.wheatdat[, td0:td40]) %*% DMat

p.soybeandat <- filter(p.dat, !is.na(ln_soybean_rrev))
soybean.XMat <- as.matrix(p.soybeandat[, td0:td40]) %*% DMat

# Fit
p.corn.fit <- felm(ln_corn_rrev ~  corn.XMat + prec + prec_sq | fips + year | 0 | state, 
                   data = p.corndat, weights = p.corndat$corn_w)
summary(p.corn.fit)

p.cotton.fit <- felm(ln_cotton_rrev ~ cotton.XMat + prec + prec_sq | fips + year | 0 | state, 
                     data = p.cottondat, weights = p.cottondat$cotton_w)
summary(p.cotton.fit)

p.hay.fit <- felm(ln_hay_rrev ~ hay.XMat + prec + prec_sq | fips + year | 0 | state, 
                  data = p.haydat, weights = p.haydat$hay_w)
summary(p.hay.fit)

p.wheat.fit <- felm(ln_wheat_rrev ~  wheat.XMat + prec + prec_sq | fips + year | 0 | state, 
                    data = p.wheatdat, weights = p.wheatdat$wheat_w)
summary(p.wheat.fit)

p.soybean.fit <- felm(ln_soybean_rrev ~ soybean.XMat +  prec + prec_sq | fips + year | 0 | state, 
                      data = p.soybeandat, weights = p.soybeandat$soybean_a)
summary(p.soybean.fit)

# Fit coefficients into DMat
p.corn.coef <- DMat %*% matrix(p.corn.fit$coefficients[1:5], ncol = 1)
p.corn.se <- DMat %*% matrix(p.corn.fit$se[1:5], ncol = 1)
plot(p.corn.coef)

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
spline.pdat <- data.frame(degree = rep(0:40, 5),
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
td <- filter(td, year >= 1930 & year <= 2010)
td$year <- NULL

td <- td %>% 
  group_by(fips) %>% 
  summarise_all(funs(mean))

cs.dat <- left_join(cs.dat, td, by = c("fips"))

cs.dat[, 76] <- rowSums(cs.dat[, 76:81], na.rm = TRUE)

# Spline
Tindex = 0:40 + .5
DMat = as.matrix(ns(Tindex, df = 5))
td0 <- which(colnames(cs.dat) == "td_0C")
td40 <- td0 + 40
XMat <- as.matrix(cs.dat[, td0:td40]) %*% DMat

cs.corndat <- filter(cs.dat, !is.na(ln_corn_rrev))
corn.XMat <- as.matrix(cs.corndat[, td0:td40]) %*% DMat

cs.cottondat <- filter(cs.dat, !is.na(ln_cotton_rrev))
cotton.XMat <- as.matrix(cs.cottondat[, td0:td40]) %*% DMat

cs.haydat <- filter(cs.dat, !is.na(ln_hay_rrev))
hay.XMat <- as.matrix(cs.haydat[, td0:td40]) %*% DMat

cs.wheatdat <- filter(cs.dat, !is.na(ln_wheat_rrev))
wheat.XMat <- as.matrix(cs.wheatdat[, td0:td40]) %*% DMat

cs.soybeandat <- filter(cs.dat, !is.na(ln_soybean_rrev))
soybean.XMat <- as.matrix(cs.soybeandat[, td0:td40]) %*% DMat

# Fit
cs.corn.fit <- felm(ln_corn_rrev ~  corn.XMat + prec + prec_sq | state | 0 | state, 
                   data = cs.corndat, weights = cs.corndat$corn_w)
summary(cs.corn.fit)

cs.cotton.fit <- felm(ln_cotton_rrev ~ cotton.XMat + prec + prec_sq | state | 0 | state, 
                     data = cs.cottondat, weights = cs.cottondat$cotton_w)
summary(cs.cotton.fit)

cs.hay.fit <- felm(ln_hay_rrev ~ hay.XMat + prec + prec_sq | state | 0 | state, 
                  data = cs.haydat, weights = cs.haydat$hay_w)
summary(cs.hay.fit)

cs.wheat.fit <- felm(ln_wheat_rrev ~  wheat.XMat + prec + prec_sq | state | 0 | state, 
                    data = cs.wheatdat, weights = cs.wheatdat$wheat_w)
summary(cs.wheat.fit)

cs.soybean.fit <- felm(ln_soybean_rrev ~ soybean.XMat +  prec + prec_sq | state | 0 | state, 
                      data = cs.soybeandat, weights = cs.soybeandat$soybean_w)
summary(cs.soybean.fit)

# Fit coefficients into DMat
cs.corn.coef <- DMat %*% matrix(cs.corn.fit$coefficients[1:5], ncol = 1)
cs.corn.se <- DMat %*% matrix(cs.corn.fit$se[1:5], ncol = 1)
plot(cs.corn.coef)

cs.cotton.coef <- DMat %*% matrix(cs.cotton.fit$coefficients[1:5], ncol = 1)
cs.cotton.se <- DMat %*% matrix(cs.cotton.fit$se[1:5], ncol = 1)
plot(cs.cotton.coef)

cs.hay.coef <- DMat %*% matrix(cs.hay.fit$coefficients[1:5], ncol = 1)
cs.hay.se <- DMat %*% matrix(cs.hay.fit$se[1:5], ncol = 1)
plot(cs.hay.coef)

cs.wheat.coef <- DMat %*% matrix(cs.wheat.fit$coefficients[1:5], ncol = 1)
cs.wheat.se <- DMat %*% matrix(cs.wheat.fit$se[1:5], ncol = 1)
plot(cs.wheat.coef)

cs.soybean.coef <- DMat %*% matrix(cs.soybean.fit$coefficients[1:5], ncol = 1)
cs.soybean.se <- DMat %*% matrix(cs.soybean.fit$se[1:5], ncol = 1)
plot(cs.soybean.coef)


# Spline data set to plot
spline.pdat <- data.frame(degree = rep(0:40, 5),
                   coef = c(cs.corn.coef, cs.cotton.coef, cs.hay.coef, cs.wheat.coef, cs.soybean.coef),
                   se = c(cs.corn.se, cs.cotton.se, cs.hay.se, cs.wheat.se, cs.soybean.se),
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 41))

spline.pdat$ymin <- spline.pdat$coef - spline.pdat$se*1.97
spline.pdat$ymax <- spline.pdat$coef + spline.pdat$se*1.97
spline.pdat$reg <- "Cross-section"

ggplot(spline.pdat, aes(x = degree, y = coef, color = crop)) + 
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

diff.dat[, 40] <- rowSums(diff.dat[, 40:80], na.rm = TRUE)

# Spline
Tindex = 0:40 + .5
DMat = as.matrix(ns(Tindex, df = 5))
td0 <- which(colnames(diff.dat) == "td_0C")
td40 <- td0 + 40
XMat <- as.matrix(diff.dat[, td0:td40]) %*% DMat

diff.corndat <- filter(diff.dat, !is.na(ln_corn_rrev))
corn.XMat <- as.matrix(diff.corndat[, td0:td40]) %*% DMat

diff.cottondat <- filter(diff.dat, !is.na(ln_cotton_rrev))
cotton.XMat <- as.matrix(diff.cottondat[, td0:td40]) %*% DMat

diff.haydat <- filter(diff.dat, !is.na(ln_hay_rrev))
hay.XMat <- as.matrix(diff.haydat[, td0:td40]) %*% DMat

diff.wheatdat <- filter(diff.dat, !is.na(ln_wheat_rrev))
wheat.XMat <- as.matrix(diff.wheatdat[, td0:td40]) %*% DMat

diff.soybeandat <- filter(diff.dat, !is.na(ln_soybean_rrev))
soybean.XMat <- as.matrix(diff.soybeandat[, td0:td40]) %*% DMat

# Fit
diff.corn.fit <- felm(ln_corn_rrev ~  corn.XMat + prec + prec_sq | fips + state | 0 | state, 
                   data = diff.corndat, weights = diff.corndat$corn_w)
summary(diff.corn.fit)

diff.cotton.fit <- felm(ln_cotton_rrev ~ cotton.XMat + prec + prec_sq |  fips + state | 0 | state, 
                     data = diff.cottondat, weights = diff.cottondat$cotton_w)
summary(diff.cotton.fit)

diff.hay.fit <- felm(ln_hay_rrev ~ hay.XMat + prec + prec_sq |  fips + state | 0 | state, 
                  data = diff.haydat, weights = diff.haydat$hay_w)
summary(diff.hay.fit)

diff.wheat.fit <- felm(ln_wheat_rrev ~  wheat.XMat + prec + prec_sq |  fips + state | 0 | state, 
                    data = diff.wheatdat, weights = diff.wheatdat$wheat_w)
summary(diff.wheat.fit)

diff.soybean.fit <- felm(ln_soybean_rrev ~ soybean.XMat +  prec + prec_sq |  fips + state | 0 | state, 
                      data = diff.soybeandat, weights = diff.soybeandat$soybean_w)
summary(diff.soybean.fit)

# Fit coefficients into DMat
diff.corn.coef <- DMat %*% matrix(diff.corn.fit$coefficients[1:5], ncol = 1)
diff.corn.se <- DMat %*% matrix(diff.corn.fit$se[1:5], ncol = 1)
plot(diff.corn.coef)

diff.cotton.coef <- DMat %*% matrix(diff.cotton.fit$coefficients[1:5], ncol = 1)
diff.cotton.se <- DMat %*% matrix(diff.cotton.fit$se[1:5], ncol = 1)
plot(diff.cotton.coef)

diff.hay.coef <- DMat %*% matrix(diff.hay.fit$coefficients[1:5], ncol = 1)
diff.hay.se <- DMat %*% matrix(diff.hay.fit$se[1:5], ncol = 1)
plot(diff.hay.coef)

diff.wheat.coef <- DMat %*% matrix(diff.wheat.fit$coefficients[1:5], ncol = 1)
diff.wheat.se <- DMat %*% matrix(diff.wheat.fit$se[1:5], ncol = 1)
plot(diff.wheat.coef)

diff.soybean.coef <- DMat %*% matrix(diff.soybean.fit$coefficients[1:5], ncol = 1)
diff.soybean.se <- DMat %*% matrix(diff.soybean.fit$se[1:5], ncol = 1)
plot(diff.soybean.coef)


# Spline data set to plot
spline.pdat <- data.frame(degree = rep(0:40, 5),
                   coef = c(diff.corn.coef, diff.cotton.coef, diff.hay.coef, diff.wheat.coef, diff.soybean.coef),
                   se = c(diff.corn.se, diff.cotton.se, diff.hay.se, diff.wheat.se, diff.soybean.se),
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 41))

spline.pdat$ymin <- spline.pdat$coef - spline.pdat$se*1.97
spline.pdat$ymax <- spline.pdat$coef + spline.pdat$se*1.97
spline.pdat$reg <- "Diff"

ggplot(spline.pdat, aes(x = degree, y = coef, color = crop)) + 
  geom_line() + facet_wrap(~crop) + 
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) +
  geom_line(aes(y = ymin, x = degree), linetype = "dotted") +
  geom_line(aes(y = ymax, x = degree), linetype = "dotted")  + ylab("Log Revenue") + 
  xlab("Temperature (C)") +
  theme_tufte() + theme(legend.position="none")

revspline.dat <- data.frame(degree = rep(c(0:40), 15),
                    coef = c(cs.corn.coef, cs.cotton.coef, cs.hay.coef, cs.wheat.coef, cs.soybean.coef,
                               p.corn.coef, p.cotton.coef, p.hay.coef, p.wheat.coef, p.soybean.coef,
                               diff.corn.coef, diff.cotton.coef, diff.hay.coef, diff.wheat.coef, diff.soybean.coef),
                      se = c(cs.corn.se, cs.cotton.se, cs.hay.se, cs.wheat.se, cs.soybean.se,
                               p.corn.se, p.cotton.se, p.hay.se, p.wheat.se, p.soybean.se,
                               diff.corn.se, diff.cotton.se, diff.hay.se, diff.wheat.se, diff.soybean.se),
                      crop = rep(c("corn", "cotton", "hay", "wheat", "soybean"), 3, each = 41),
                      reg = rep(c("cross-section", "panel", "diff"), 3, each = 205))


revspline.dat$ymin <- revspline.dat$coef - 1.96*revspline.dat$se
revspline.dat$ymax <- revspline.dat$coef + 1.96*revspline.dat$se


rev.spline <- ggplot(revspline.dat) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) + 
  geom_line(aes(x = degree, y = coef, color = reg, group = interaction(crop, reg))) +
  geom_line(aes(y = ymin, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 1) + 
  geom_line(aes(y = ymax, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 1) + 
  facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Log Revenue") + ggtitle("Log Revenue Spline Regressions") + 
   theme_tufte() + theme(legend.position="top")
rev.spline
}


# Spline Regressions Share ------------------------------------------------------

{
# Panel
p.dat <- readRDS("data/panel_regression_data.rds")
td <- readRDS("data/fips_degree_time_1900-2013.rds")
p.dat <- left_join(p.dat, td, by = c("year", "fips"))

#p.dat[, 122] <- rowSums(p.dat[, 122:127])
# Spline
Tindex = 0:40 + .5
DMat = as.matrix(ns(Tindex, df = 5))
td0 <- which(colnames(p.dat) == "td_0C")
td40 <- td0 + 40
XMat <- as.matrix(p.dat[, td0:td40]) %*% DMat

p.corndat <- filter(p.dat, !is.na(ln_corn_rrev))
corn.XMat <- as.matrix(p.corndat[, td0:td40]) %*% DMat

p.cottondat <- filter(p.dat, !is.na(ln_cotton_rrev))
cotton.XMat <- as.matrix(p.cottondat[, td0:td40]) %*% DMat

p.haydat <- filter(p.dat, !is.na(ln_hay_rrev))
hay.XMat <- as.matrix(p.haydat[, td0:td40]) %*% DMat

p.wheatdat <- filter(p.dat, !is.na(ln_wheat_rrev))
wheat.XMat <- as.matrix(p.wheatdat[, td0:td40]) %*% DMat

p.soybeandat <- filter(p.dat, !is.na(ln_soybean_rrev))
soybean.XMat <- as.matrix(p.soybeandat[, td0:td40]) %*% DMat

# Fit
p.corn.fit <- felm(p_corn_share ~  corn.XMat + prec + prec_sq | fips + year | 0 | state, 
                   data = p.corndat, weights = p.corndat$total_w)
summary(p.corn.fit)

p.cotton.fit <- felm(p_cotton_share ~ cotton.XMat + prec + prec_sq | fips + year | 0 | state, 
                     data = p.cottondat, weights = p.cottondat$total_w)
summary(p.cotton.fit)

p.hay.fit <- felm(p_hay_share ~ hay.XMat + prec + prec_sq | fips + year | 0 | state, 
                  data = p.haydat, weights = p.haydat$total_w)
summary(p.hay.fit)

p.wheat.fit <- felm(p_wheat_share ~  wheat.XMat + prec + prec_sq | fips + year | 0 | state, 
                    data = p.wheatdat, weights = p.wheatdat$total_w)
summary(p.wheat.fit)

p.soybean.fit <- felm(p_soybean_share ~ soybean.XMat +  prec + prec_sq | fips + year | 0 | state, 
                      data = p.soybeandat, weights = p.soybeandat$total_a)
summary(p.soybean.fit)

# Fit coefficients into DMat
p.corn.coef <- DMat %*% matrix(p.corn.fit$coefficients[1:5], ncol = 1)
p.corn.se <- DMat %*% matrix(p.corn.fit$se[1:5], ncol = 1)
plot(p.corn.coef)

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
spline.pdat <- data.frame(degree = rep(0:40, 5),
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
td <- filter(td, year >= 1930 & year <= 2010)
td$year <- NULL

td <- td %>% 
  group_by(fips) %>% 
  summarise_all(funs(mean))

cs.dat <- left_join(cs.dat, td, by = c("fips"))

cs.dat[, 76] <- rowSums(cs.dat[, 76:81], na.rm = TRUE)
Tindex = 0:40 + .5
DMat = as.matrix(ns(Tindex, df = 5))
td0 <- which(colnames(cs.dat) == "td_0C")
td40 <- td0 + 40
XMat <- as.matrix(cs.dat[, td0:td40]) %*% DMat

cs.corndat <- filter(cs.dat, !is.na(ln_corn_rrev))
corn.XMat <- as.matrix(cs.corndat[, td0:td40]) %*% DMat

cs.cottondat <- filter(cs.dat, !is.na(ln_cotton_rrev))
cotton.XMat <- as.matrix(cs.cottondat[, td0:td40]) %*% DMat

cs.haydat <- filter(cs.dat, !is.na(ln_hay_rrev))
hay.XMat <- as.matrix(cs.haydat[, td0:td40]) %*% DMat

cs.wheatdat <- filter(cs.dat, !is.na(ln_wheat_rrev))
wheat.XMat <- as.matrix(cs.wheatdat[, td0:td40]) %*% DMat

cs.soybeandat <- filter(cs.dat, !is.na(ln_soybean_rrev))
soybean.XMat <- as.matrix(cs.soybeandat[, td0:td40]) %*% DMat

# Fit
cs.corn.fit <- felm(p_corn_share ~  corn.XMat + prec + prec_sq | state| 0 | state, 
                   data = cs.corndat, weights = cs.corndat$total_w)
summary(cs.corn.fit)

cs.cotton.fit <- felm(p_cotton_share ~ cotton.XMat + prec + prec_sq | state | 0 | state, 
                     data = cs.cottondat, weights = cs.cottondat$total_w)
summary(cs.cotton.fit)

cs.hay.fit <- felm(p_hay_share ~ hay.XMat + prec + prec_sq | state | 0 | state, 
                  data = cs.haydat, weights = cs.haydat$total_w)
summary(cs.hay.fit)

cs.wheat.fit <- felm(p_wheat_share ~  wheat.XMat + prec + prec_sq | state | 0 | state, 
                    data = cs.wheatdat, weights = cs.wheatdat$total_w)
summary(cs.wheat.fit)

cs.soybean.fit <- felm(p_soybean_share ~ soybean.XMat +  prec + prec_sq | state | 0 | state, 
                      data = cs.soybeandat, weights = cs.soybeandat$total_w)
summary(cs.soybean.fit)

# Fit coefficients into DMat
cs.corn.coef <- DMat %*% matrix(cs.corn.fit$coefficients[1:5], ncol = 1)
cs.corn.se <- DMat %*% matrix(cs.corn.fit$se[1:5], ncol = 1)
plot(cs.corn.coef)

cs.cotton.coef <- DMat %*% matrix(cs.cotton.fit$coefficients[1:5], ncol = 1)
cs.cotton.se <- DMat %*% matrix(cs.cotton.fit$se[1:5], ncol = 1)
plot(cs.cotton.coef)

cs.hay.coef <- DMat %*% matrix(cs.hay.fit$coefficients[1:5], ncol = 1)
cs.hay.se <- DMat %*% matrix(cs.hay.fit$se[1:5], ncol = 1)
plot(cs.hay.coef)

cs.wheat.coef <- DMat %*% matrix(cs.wheat.fit$coefficients[1:5], ncol = 1)
cs.wheat.se <- DMat %*% matrix(cs.wheat.fit$se[1:5], ncol = 1)
plot(cs.wheat.coef)

cs.soybean.coef <- DMat %*% matrix(cs.soybean.fit$coefficients[1:5], ncol = 1)
cs.soybean.se <- DMat %*% matrix(cs.soybean.fit$se[1:5], ncol = 1)
plot(cs.soybean.coef)


# Spline data set to plot
spline.csdat <- data.frame(degree = rep(0:40, 5),
                   coef = c(cs.corn.coef, cs.cotton.coef, cs.hay.coef, cs.wheat.coef, cs.soybean.coef),
                   se = c(cs.corn.se, cs.cotton.se, cs.hay.se, cs.wheat.se, cs.soybean.se),
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 41))

spline.csdat$ymin <- spline.csdat$coef - spline.csdat$se*1.97
spline.csdat$ymax <- spline.csdat$coef + spline.csdat$se*1.97
spline.csdat$reg <- "Cross-section"

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

diff.dat[, 80] <- rowSums(diff.dat[, 80:85], na.rm = TRUE)

Tindex = 0:40 + .5
DMat = as.matrix(ns(Tindex, df = 5))
td0 <- which(colnames(diff.dat) == "td_0C")
td40 <- td0 + 40
XMat <- as.matrix(diff.dat[, td0:td40]) %*% DMat

diff.corndat <- filter(diff.dat, !is.na(ln_corn_rrev))
corn.XMat <- as.matrix(diff.corndat[, td0:td40]) %*% DMat

diff.cottondat <- filter(diff.dat, !is.na(ln_cotton_rrev))
cotton.XMat <- as.matrix(diff.cottondat[, td0:td40]) %*% DMat

diff.haydat <- filter(diff.dat, !is.na(ln_hay_rrev))
hay.XMat <- as.matrix(diff.haydat[, td0:td40]) %*% DMat

diff.wheatdat <- filter(diff.dat, !is.na(ln_wheat_rrev))
wheat.XMat <- as.matrix(diff.wheatdat[, td0:td40]) %*% DMat

diff.soybeandat <- filter(diff.dat, !is.na(ln_soybean_rrev))
soybean.XMat <- as.matrix(diff.soybeandat[, td0:td40]) %*% DMat

# Fit
diff.corn.fit <- felm(p_corn_share ~  corn.XMat + prec + prec_sq | state| 0 | state, 
                   data = diff.corndat, weights = diff.corndat$total_w)
summary(diff.corn.fit)

diff.cotton.fit <- felm(p_cotton_share ~ cotton.XMat + prec + prec_sq | state | 0 | state, 
                     data = diff.cottondat, weights = diff.cottondat$total_w)
summary(diff.cotton.fit)

diff.hay.fit <- felm(p_hay_share ~ hay.XMat + prec + prec_sq | state | 0 | state, 
                  data = diff.haydat, weights = diff.haydat$total_w)
summary(diff.hay.fit)

diff.wheat.fit <- felm(p_wheat_share ~  wheat.XMat + prec + prec_sq | state | 0 | state, 
                    data = diff.wheatdat, weights = diff.wheatdat$total_w)
summary(diff.wheat.fit)

diff.soybean.fit <- felm(p_soybean_share ~ soybean.XMat +  prec + prec_sq | state | 0 | state, 
                      data = diff.soybeandat, weights = diff.soybeandat$total_w)
summary(diff.soybean.fit)

# Fit coefficients into DMat
diff.corn.coef <- DMat %*% matrix(diff.corn.fit$coefficients[1:5], ncol = 1)
diff.corn.se <- DMat %*% matrix(diff.corn.fit$se[1:5], ncol = 1)
plot(diff.corn.coef)

diff.cotton.coef <- DMat %*% matrix(diff.cotton.fit$coefficients[1:5], ncol = 1)
diff.cotton.se <- DMat %*% matrix(diff.cotton.fit$se[1:5], ncol = 1)
plot(diff.cotton.coef)

diff.hay.coef <- DMat %*% matrix(diff.hay.fit$coefficients[1:5], ncol = 1)
diff.hay.se <- DMat %*% matrix(diff.hay.fit$se[1:5], ncol = 1)
plot(diff.hay.coef)

diff.wheat.coef <- DMat %*% matrix(diff.wheat.fit$coefficients[1:5], ncol = 1)
diff.wheat.se <- DMat %*% matrix(diff.wheat.fit$se[1:5], ncol = 1)
plot(diff.wheat.coef)

diff.soybean.coef <- DMat %*% matrix(diff.soybean.fit$coefficients[1:5], ncol = 1)
diff.soybean.se <- DMat %*% matrix(diff.soybean.fit$se[1:5], ncol = 1)
plot(diff.soybean.coef)


# Spline data set to plot
spline.diffdat <- data.frame(degree = rep(0:40, 5),
                   coef = c(diff.corn.coef, diff.cotton.coef, diff.hay.coef, diff.wheat.coef, diff.soybean.coef),
                   se = c(diff.corn.se, diff.cotton.se, diff.hay.se, diff.wheat.se, diff.soybean.se),
                   crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"), each = 41))

spline.diffdat$ymin <- spline.diffdat$coef - spline.diffdat$se*1.97
spline.diffdat$ymax <- spline.diffdat$coef + spline.diffdat$se*1.97
spline.diffdat$reg <- "Difference"

ggplot(spline.diffdat, aes(x = degree, y = coef, color = crop)) + 
  geom_line() + facet_wrap(~crop) + 
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.3) +
  geom_line(aes(y = ymin, x = degree), linetype = "dotted") +
  geom_line(aes(y = ymax, x = degree), linetype = "dotted")  + ylab("Log Revenue") + 
  xlab("Temperature (C)") +
  theme_tufte() + theme(legend.position="none")

sharespline.dat <- data.frame(degree = rep(c(0:40), 15),
                    coef = c(cs.corn.coef, cs.cotton.coef, cs.hay.coef, cs.wheat.coef, cs.soybean.coef,
                               p.corn.coef, p.cotton.coef, p.hay.coef, p.wheat.coef, p.soybean.coef,
                               diff.corn.coef, diff.cotton.coef, diff.hay.coef, diff.wheat.coef, diff.soybean.coef),
                      se = c(cs.corn.se, cs.cotton.se, cs.hay.se, cs.wheat.se, cs.soybean.se,
                               p.corn.se, p.cotton.se, p.hay.se, p.wheat.se, p.soybean.se,
                               diff.corn.se, diff.cotton.se, diff.hay.se, diff.wheat.se, diff.soybean.se),
                      crop = rep(c("corn", "cotton", "hay", "wheat", "soybean"), 3, each = 41),
                      reg = rep(c("cross-section", "panel", "diff"), 3, each = 205))


sharespline.dat$ymin <- sharespline.dat$coef - 1.96*sharespline.dat$se
sharespline.dat$ymax <- sharespline.dat$coef + 1.96*sharespline.dat$se


share.spline <- ggplot(sharespline.dat) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) + 
  geom_line(aes(x = degree, y = coef, color = reg, group = interaction(crop, reg))) +
  geom_line(aes(y = ymin, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 1) + 
  geom_line(aes(y = ymax, x = degree, color = reg, group = interaction(crop, reg)), linetype = "dotted", alpha = 1) + 
  facet_wrap(~crop) + xlab("Temperature (Celsius)") + ylab("Log Revenue") + ggtitle("Share Spline Regressions") + 
   theme_tufte() + theme(legend.position="top")
share.spline
