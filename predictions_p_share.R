library(tidyverse)
library(lfe)
library(AER)
library(cowplot)

source("predictFelm.R")

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

tobit.ey <- function(mu, sigma){
  p0 <- pnorm(mu/sigma)
  lambda <- function(x) dnorm(x)/pnorm(x)
  ey0 <- mu + sigma * lambda(mu/sigma)
  ey <- p0 * ey0
  return(ey)
}

###################
# # Baseline Degree Day Data
cs.dat <- readRDS("data/baseline_cross_section_regression_data.rds")
p.dat <- readRDS("data/baseline_panel_regression_data.rds")
diff.dat <- readRDS("data/baseline_diff_regression_data.rds")

# New Degree Day Data
cs.1C <- readRDS("data/degree_day_changes/cross_section_regression_data_1C")
cs.2C <- readRDS("data/degree_day_changes/cross_section_regression_data_2C")
cs.3C <- readRDS("data/degree_day_changes/cross_section_regression_data_3C")
cs.4C <- readRDS("data/degree_day_changes/cross_section_regression_data_4C")
cs.5C <- readRDS("data/degree_day_changes/cross_section_regression_data_5C")

p.1C <- readRDS("data/degree_day_changes/panel_regression_data_1C")
p.2C <- readRDS("data/degree_day_changes/panel_regression_data_2C")
p.3C <- readRDS("data/degree_day_changes/panel_regression_data_3C")
p.4C <- readRDS("data/degree_day_changes/panel_regression_data_4C")
p.5C <- readRDS("data/degree_day_changes/panel_regression_data_5C")

diff.1C <- readRDS("data/degree_day_changes/diff_regression_data_1C")
diff.2C <- readRDS("data/degree_day_changes/diff_regression_data_2C")
diff.3C <- readRDS("data/degree_day_changes/diff_regression_data_3C")
diff.4C <- readRDS("data/degree_day_changes/diff_regression_data_4C")
diff.5C <- readRDS("data/degree_day_changes/diff_regression_data_5C")

#############################
# Get models
cs.p_corn_share <- readRDS("models/cs.dd.p_corn_share")
p.p_corn_share <- readRDS("models/p.dd.p_corn_share")
diff.p_corn_share <- readRDS("models/diff.dd.p_corn_share")

cs.p_cotton_share <- readRDS("models/cs.dd.p_cotton_share")
p.p_cotton_share <- readRDS("models/p.dd.p_cotton_share")
diff.p_cotton_share <- readRDS("models/diff.dd.p_cotton_share")

cs.p_hay_share <- readRDS("models/cs.dd.p_hay_share")
p.p_hay_share <- readRDS("models/p.dd.p_hay_share")
diff.p_hay_share <- readRDS("models/diff.dd.p_hay_share")

cs.p_wheat_share <- readRDS("models/cs.dd.p_wheat_share")
p.p_wheat_share <- readRDS("models/p.dd.p_wheat_share")
diff.p_wheat_share <- readRDS("models/diff.dd.p_wheat_share")

cs.p_soybean_share <- readRDS("models/cs.dd.p_soybean_share")
p.p_soybean_share <- readRDS("models/p.dd.p_soybean_share")
diff.p_soybean_share <- readRDS("models/diff.dd.p_soybean_share")


########################
# Cross section data
cs.p_corn_share_0C <- filter(cs.dat, !is.na(ln_corn_rrev))
cs.p_corn_share_1C <- filter(cs.1C, !is.na(ln_corn_rrev))
cs.p_corn_share_2C <- filter(cs.2C, !is.na(ln_corn_rrev))
cs.p_corn_share_3C <- filter(cs.3C, !is.na(ln_corn_rrev))
cs.p_corn_share_4C <- filter(cs.4C, !is.na(ln_corn_rrev))
cs.p_corn_share_5C <- filter(cs.5C, !is.na(ln_corn_rrev))

cs.p_cotton_share_0C <- filter(cs.dat, !is.na(ln_cotton_rrev))
cs.p_cotton_share_1C <- filter(cs.1C, !is.na(ln_cotton_rrev))
cs.p_cotton_share_2C <- filter(cs.2C, !is.na(ln_cotton_rrev))
cs.p_cotton_share_3C <- filter(cs.3C, !is.na(ln_cotton_rrev))
cs.p_cotton_share_4C <- filter(cs.4C, !is.na(ln_cotton_rrev))
cs.p_cotton_share_5C <- filter(cs.5C, !is.na(ln_cotton_rrev))

cs.p_hay_share_0C <- filter(cs.dat, !is.na(ln_hay_rrev))
cs.p_hay_share_1C <- filter(cs.dat, !is.na(ln_hay_rrev))
cs.p_hay_share_2C <- filter(cs.2C, !is.na(ln_hay_rrev))
cs.p_hay_share_3C <- filter(cs.3C, !is.na(ln_hay_rrev))
cs.p_hay_share_4C <- filter(cs.4C, !is.na(ln_hay_rrev))
cs.p_hay_share_5C <- filter(cs.5C, !is.na(ln_hay_rrev))

cs.p_wheat_share_0C <- filter(cs.dat, !is.na(ln_wheat_rrev))
cs.p_wheat_share_1C <- filter(cs.1C, !is.na(ln_wheat_rrev))
cs.p_wheat_share_2C <- filter(cs.2C, !is.na(ln_wheat_rrev))
cs.p_wheat_share_3C <- filter(cs.3C, !is.na(ln_wheat_rrev))
cs.p_wheat_share_4C <- filter(cs.4C, !is.na(ln_wheat_rrev))
cs.p_wheat_share_5C <- filter(cs.5C, !is.na(ln_wheat_rrev))

cs.p_soybean_share_0C <- filter(cs.dat, !is.na(ln_soybean_rrev))
cs.p_soybean_share_1C <- filter(cs.1C, !is.na(ln_soybean_rrev))
cs.p_soybean_share_2C <- filter(cs.2C, !is.na(ln_soybean_rrev))
cs.p_soybean_share_3C <- filter(cs.3C, !is.na(ln_soybean_rrev))
cs.p_soybean_share_4C <- filter(cs.4C, !is.na(ln_soybean_rrev))
cs.p_soybean_share_5C <- filter(cs.5C, !is.na(ln_soybean_rrev))

# Panel data
p.p_corn_share_0C <- filter(p.dat, !is.na(ln_corn_rrev))
p.p_corn_share_1C <- filter(p.1C, !is.na(ln_corn_rrev))
p.p_corn_share_2C <- filter(p.2C, !is.na(ln_corn_rrev))
p.p_corn_share_3C <- filter(p.3C, !is.na(ln_corn_rrev))
p.p_corn_share_4C <- filter(p.4C, !is.na(ln_corn_rrev))
p.p_corn_share_5C <- filter(p.5C, !is.na(ln_corn_rrev))

p.p_cotton_share_0C <- filter(p.dat, !is.na(ln_cotton_rrev))
p.p_cotton_share_1C <- filter(p.1C, !is.na(ln_cotton_rrev))
p.p_cotton_share_2C <- filter(p.2C, !is.na(ln_cotton_rrev))
p.p_cotton_share_3C <- filter(p.3C, !is.na(ln_cotton_rrev))
p.p_cotton_share_4C <- filter(p.4C, !is.na(ln_cotton_rrev))
p.p_cotton_share_5C <- filter(p.5C, !is.na(ln_cotton_rrev))

p.p_hay_share_0C <- filter(p.dat, !is.na(ln_hay_rrev))
p.p_hay_share_1C <- filter(p.1C, !is.na(ln_hay_rrev))
p.p_hay_share_2C <- filter(p.2C, !is.na(ln_hay_rrev))
p.p_hay_share_3C <- filter(p.3C, !is.na(ln_hay_rrev))
p.p_hay_share_4C <- filter(p.4C, !is.na(ln_hay_rrev))
p.p_hay_share_5C <- filter(p.5C, !is.na(ln_hay_rrev))

p.p_wheat_share_0C <- filter(p.dat, !is.na(ln_wheat_rrev))
p.p_wheat_share_1C <- filter(p.1C, !is.na(ln_wheat_rrev))
p.p_wheat_share_2C <- filter(p.2C, !is.na(ln_wheat_rrev))
p.p_wheat_share_3C <- filter(p.3C, !is.na(ln_wheat_rrev))
p.p_wheat_share_4C <- filter(p.4C, !is.na(ln_wheat_rrev))
p.p_wheat_share_5C <- filter(p.5C, !is.na(ln_wheat_rrev))

p.p_soybean_share_0C <- filter(p.dat, !is.na(ln_soybean_rrev))
p.p_soybean_share_1C <- filter(p.1C, !is.na(ln_soybean_rrev))
p.p_soybean_share_2C <- filter(p.2C, !is.na(ln_soybean_rrev))
p.p_soybean_share_3C <- filter(p.3C, !is.na(ln_soybean_rrev))
p.p_soybean_share_4C <- filter(p.4C, !is.na(ln_soybean_rrev))
p.p_soybean_share_5C <- filter(p.5C, !is.na(ln_soybean_rrev))

# Diff data
diff.p_corn_share_0C <- filter(diff.dat, !is.na(ln_corn_rrev))
diff.p_corn_share_1C <- filter(diff.1C, !is.na(ln_corn_rrev))
diff.p_corn_share_2C <- filter(diff.2C, !is.na(ln_corn_rrev))
diff.p_corn_share_3C <- filter(diff.3C, !is.na(ln_corn_rrev))
diff.p_corn_share_4C <- filter(diff.4C, !is.na(ln_corn_rrev))
diff.p_corn_share_5C <- filter(diff.5C, !is.na(ln_corn_rrev))

diff.p_cotton_share_0C <- filter(diff.dat, !is.na(ln_cotton_rrev))
diff.p_cotton_share_1C <- filter(diff.1C, !is.na(ln_cotton_rrev))
diff.p_cotton_share_2C <- filter(diff.2C, !is.na(ln_cotton_rrev))
diff.p_cotton_share_3C <- filter(diff.3C, !is.na(ln_cotton_rrev))
diff.p_cotton_share_4C <- filter(diff.4C, !is.na(ln_cotton_rrev))
diff.p_cotton_share_5C <- filter(diff.5C, !is.na(ln_cotton_rrev))

diff.p_hay_share_0C <- filter(diff.dat, !is.na(ln_hay_rrev))
diff.p_hay_share_1C <- filter(diff.1C, !is.na(ln_hay_rrev))
diff.p_hay_share_2C <- filter(diff.2C, !is.na(ln_hay_rrev))
diff.p_hay_share_3C <- filter(diff.3C, !is.na(ln_hay_rrev))
diff.p_hay_share_4C <- filter(diff.4C, !is.na(ln_hay_rrev))
diff.p_hay_share_5C <- filter(diff.5C, !is.na(ln_hay_rrev))

diff.p_wheat_share_0C <- filter(diff.dat, !is.na(ln_wheat_rrev))
diff.p_wheat_share_1C <- filter(diff.1C, !is.na(ln_wheat_rrev))
diff.p_wheat_share_2C <- filter(diff.2C, !is.na(ln_wheat_rrev))
diff.p_wheat_share_3C <- filter(diff.3C, !is.na(ln_wheat_rrev))
diff.p_wheat_share_4C <- filter(diff.4C, !is.na(ln_wheat_rrev))
diff.p_wheat_share_5C <- filter(diff.5C, !is.na(ln_wheat_rrev))

diff.p_soybean_share_0C <- filter(diff.dat, !is.na(ln_soybean_rrev))
diff.p_soybean_share_1C <- filter(diff.1C, !is.na(ln_soybean_rrev))
diff.p_soybean_share_2C <- filter(diff.2C, !is.na(ln_soybean_rrev))
diff.p_soybean_share_3C <- filter(diff.3C, !is.na(ln_soybean_rrev))
diff.p_soybean_share_4C <- filter(diff.4C, !is.na(ln_soybean_rrev))
diff.p_soybean_share_5C <- filter(diff.5C, !is.na(ln_soybean_rrev))

###########################
# Predictions

# Corn

cs1C.pred_p_corn_share <- predict(cs.p_corn_share, newdata = cs.p_corn_share_1C)
cs2C.pred_p_corn_share <- predict(cs.p_corn_share, newdata = cs.p_corn_share_2C)
cs3C.pred_p_corn_share <- predict(cs.p_corn_share, newdata = cs.p_corn_share_3C)
cs4C.pred_p_corn_share <- predict(cs.p_corn_share, newdata = cs.p_corn_share_4C)
cs5C.pred_p_corn_share <- predict(cs.p_corn_share, newdata = cs.p_corn_share_5C)

p1C.pred_p_corn_share <- predict(p.p_corn_share, newdata = p.p_corn_share_1C)
p2C.pred_p_corn_share <- predict(p.p_corn_share, newdata = p.p_corn_share_2C)
p3C.pred_p_corn_share <- predict(p.p_corn_share, newdata = p.p_corn_share_3C)
p4C.pred_p_corn_share <- predict(p.p_corn_share, newdata = p.p_corn_share_4C)
p5C.pred_p_corn_share <- predict(p.p_corn_share, newdata = p.p_corn_share_5C)

diff1C.pred_p_corn_share <- predict(diff.p_corn_share, newdata = diff.p_corn_share_1C)
diff2C.pred_p_corn_share <- predict(diff.p_corn_share, newdata = diff.p_corn_share_2C)
diff3C.pred_p_corn_share <- predict(diff.p_corn_share, newdata = diff.p_corn_share_3C)
diff4C.pred_p_corn_share <- predict(diff.p_corn_share, newdata = diff.p_corn_share_4C)
diff5C.pred_p_corn_share <- predict(diff.p_corn_share, newdata = diff.p_corn_share_5C)


# cs.rev <- data.frame(rev = c(predict(cs.p_corn_share), cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))
# 
# p.rev <- data.frame(rev = c(predict(p.p_corn_share), p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))
# 
# diff.rev <- data.frame(rev = c(predict(diff.p_corn_share), diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))
# 
# 
# ggplot(cs.rev, aes(rev, fill = change)) + geom_density(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(filter(p.rev, rev < 50), aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(diff.rev, aes(rev, fill = change)) + geom_density() + scale_fill_brewer(palette = "OrRd")

#cs.corn.rev0 <- sum((predict(cs.p_corn_share, newdata = cs.p_corn_share_0C)))

cs.corn.rev0 <- sum( tobit.ey(predict(cs.p_corn_share), cs.p_corn_share$scale), na.rm = TRUE)
cs.corn.rev1 <- (sum( tobit.ey(cs1C.pred_p_corn_share, cs.p_corn_share$scale), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev2 <- (sum( tobit.ey(cs2C.pred_p_corn_share, cs.p_corn_share$scale), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev3 <- (sum( tobit.ey(cs3C.pred_p_corn_share, cs.p_corn_share$scale), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev4 <- (sum( tobit.ey(cs4C.pred_p_corn_share, cs.p_corn_share$scale), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev5 <- (sum( tobit.ey(cs5C.pred_p_corn_share, cs.p_corn_share$scale), na.rm = TRUE)/cs.corn.rev0 - 1)*100

p.corn.rev0 <- sum( tobit.ey(predict(p.p_corn_share), p.p_corn_share$scale))
p.corn.rev1 <- (sum( tobit.ey(p1C.pred_p_corn_share, p.p_corn_share$scale))/p.corn.rev0 - 1)*100
p.corn.rev2 <- (sum( tobit.ey(p2C.pred_p_corn_share, p.p_corn_share$scale))/p.corn.rev0 - 1)*100
p.corn.rev3 <- (sum( tobit.ey(p3C.pred_p_corn_share, p.p_corn_share$scale))/p.corn.rev0 - 1)*100
p.corn.rev4 <- (sum( tobit.ey(p4C.pred_p_corn_share, p.p_corn_share$scale))/p.corn.rev0 - 1)*100
p.corn.rev5 <- (sum( tobit.ey(p5C.pred_p_corn_share, p.p_corn_share$scale))/p.corn.rev0 - 1)*100

diff.corn.rev0 <- sum( tobit.ey(predict(diff.p_corn_share), diff.p_corn_share$scale))
diff.corn.rev1 <- (sum( tobit.ey(diff1C.pred_p_corn_share, diff.p_corn_share$scale))/diff.corn.rev0 - 1)*100
diff.corn.rev2 <- (sum( tobit.ey(diff2C.pred_p_corn_share, diff.p_corn_share$scale))/diff.corn.rev0 - 1)*100
diff.corn.rev3 <- (sum( tobit.ey(diff3C.pred_p_corn_share, diff.p_corn_share$scale))/diff.corn.rev0 - 1)*100
diff.corn.rev4 <- (sum( tobit.ey(diff4C.pred_p_corn_share, diff.p_corn_share$scale))/diff.corn.rev0 - 1)*100
diff.corn.rev5 <- (sum( tobit.ey(diff5C.pred_p_corn_share, diff.p_corn_share$scale))/diff.corn.rev0 - 1)*100

corn.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.corn.rev1, cs.corn.rev2, cs.corn.rev3, cs.corn.rev4, cs.corn.rev5,
                                   p.corn.rev1, p.corn.rev2, p.corn.rev3, p.corn.rev4, p.corn.rev5,
                                   diff.corn.rev1, diff.corn.rev2, diff.corn.rev3, diff.corn.rev4, diff.corn.rev5),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 5),
                           crop = "Corn")

ggplot(corn.plotdat, aes(temp, rev, color = reg)) + geom_line()


# Cotton
cs1C.pred_p_cotton_share <- predict(cs.p_cotton_share, newdata = cs.p_cotton_share_1C)
cs2C.pred_p_cotton_share <- predict(cs.p_cotton_share, newdata = cs.p_cotton_share_2C)
cs3C.pred_p_cotton_share <- predict(cs.p_cotton_share, newdata = cs.p_cotton_share_3C)
cs4C.pred_p_cotton_share <- predict(cs.p_cotton_share, newdata = cs.p_cotton_share_4C)
cs5C.pred_p_cotton_share <- predict(cs.p_cotton_share, newdata = cs.p_cotton_share_5C)

p1C.pred_p_cotton_share <- predict(p.p_cotton_share, newdata = p.p_cotton_share_1C)
p2C.pred_p_cotton_share <- predict(p.p_cotton_share, newdata = p.p_cotton_share_2C)
p3C.pred_p_cotton_share <- predict(p.p_cotton_share, newdata = p.p_cotton_share_3C)
p4C.pred_p_cotton_share <- predict(p.p_cotton_share, newdata = p.p_cotton_share_4C)
p5C.pred_p_cotton_share <- predict(p.p_cotton_share, newdata = p.p_cotton_share_5C)

diff1C.pred_p_cotton_share <- predict(diff.p_cotton_share, newdata = diff.p_cotton_share_1C)
diff2C.pred_p_cotton_share <- predict(diff.p_cotton_share, newdata = diff.p_cotton_share_2C)
diff3C.pred_p_cotton_share <- predict(diff.p_cotton_share, newdata = diff.p_cotton_share_3C)
diff4C.pred_p_cotton_share <- predict(diff.p_cotton_share, newdata = diff.p_cotton_share_4C)
diff5C.pred_p_cotton_share <- predict(diff.p_cotton_share, newdata = diff.p_cotton_share_5C)


# cs.rev <- data.frame(rev = c(predict(cs.p_cotton_share), cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))
# 
# p.rev <- data.frame(rev = c(predict(p.p_cotton_share), p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))
# 
# diff.rev <- data.frame(rev = c(predict(diff.p_cotton_share), diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))
# 
# 
# ggplot(cs.rev, aes(rev, fill = change)) + geom_density(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(filter(p.rev, rev < 50), aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(diff.rev, aes(rev, fill = change)) + geom_density() + scale_fill_brewer(palette = "OrRd")

cs.cotton.rev0 <- sum( tobit.ey(predict(cs.p_cotton_share), cs.p_cotton_share$scale))
cs.cotton.rev1 <- (sum( tobit.ey(cs1C.pred_p_cotton_share, cs.p_cotton_share$scale))/cs.cotton.rev0 - 1)*100
cs.cotton.rev2 <- (sum( tobit.ey(cs2C.pred_p_cotton_share, cs.p_cotton_share$scale))/cs.cotton.rev0 - 1)*100
cs.cotton.rev3 <- (sum( tobit.ey(cs3C.pred_p_cotton_share, cs.p_cotton_share$scale))/cs.cotton.rev0 - 1)*100
cs.cotton.rev4 <- (sum( tobit.ey(cs4C.pred_p_cotton_share, cs.p_cotton_share$scale))/cs.cotton.rev0 - 1)*100
cs.cotton.rev5 <- (sum( tobit.ey(cs5C.pred_p_cotton_share, cs.p_cotton_share$scale))/cs.cotton.rev0 - 1)*100

p.cotton.rev0 <- sum( tobit.ey(predict(p.p_cotton_share), p.p_cotton_share$scale))
p.cotton.rev1 <- (sum( tobit.ey(p1C.pred_p_cotton_share, p.p_cotton_share$scale))/p.cotton.rev0 - 1)*100
p.cotton.rev2 <- (sum( tobit.ey(p2C.pred_p_cotton_share, p.p_cotton_share$scale))/p.cotton.rev0 - 1)*100
p.cotton.rev3 <- (sum( tobit.ey(p3C.pred_p_cotton_share, p.p_cotton_share$scale))/p.cotton.rev0 - 1)*100
p.cotton.rev4 <- (sum( tobit.ey(p4C.pred_p_cotton_share, p.p_cotton_share$scale))/p.cotton.rev0 - 1)*100
p.cotton.rev5 <- (sum( tobit.ey(p5C.pred_p_cotton_share, p.p_cotton_share$scale))/p.cotton.rev0 - 1)*100

diff.cotton.rev0 <- sum( tobit.ey(predict(diff.p_cotton_share), diff.p_cotton_share$scale))
diff.cotton.rev1 <- (sum( tobit.ey(diff1C.pred_p_cotton_share, diff.p_cotton_share$scale))/diff.cotton.rev0 - 1)*100
diff.cotton.rev2 <- (sum( tobit.ey(diff2C.pred_p_cotton_share, diff.p_cotton_share$scale))/diff.cotton.rev0 - 1)*100
diff.cotton.rev3 <- (sum( tobit.ey(diff3C.pred_p_cotton_share, diff.p_cotton_share$scale))/diff.cotton.rev0 - 1)*100
diff.cotton.rev4 <- (sum( tobit.ey(diff4C.pred_p_cotton_share, diff.p_cotton_share$scale))/diff.cotton.rev0 - 1)*100
diff.cotton.rev5 <- (sum( tobit.ey(diff5C.pred_p_cotton_share, diff.p_cotton_share$scale))/diff.cotton.rev0 - 1)*100

cotton.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.cotton.rev1, cs.cotton.rev2, cs.cotton.rev3, cs.cotton.rev4, cs.cotton.rev5,
                                   p.cotton.rev1, p.cotton.rev2, p.cotton.rev3, p.cotton.rev4, p.cotton.rev5,
                                   diff.cotton.rev1, diff.cotton.rev2, diff.cotton.rev3, diff.cotton.rev4, diff.cotton.rev5),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 5),
                           crop = "Cotton")

ggplot(cotton.plotdat, aes(temp, rev, color = reg)) + geom_line()


# Hay

cs1C.pred_p_hay_share <- predict(cs.p_hay_share, newdata = cs.p_hay_share_1C)
cs2C.pred_p_hay_share <- predict(cs.p_hay_share, newdata = cs.p_hay_share_2C)
cs3C.pred_p_hay_share <- predict(cs.p_hay_share, newdata = cs.p_hay_share_3C)
cs4C.pred_p_hay_share <- predict(cs.p_hay_share, newdata = cs.p_hay_share_4C)
cs5C.pred_p_hay_share <- predict(cs.p_hay_share, newdata = cs.p_hay_share_5C)

p1C.pred_p_hay_share <- predict(p.p_hay_share, newdata = p.p_hay_share_1C)
p2C.pred_p_hay_share <- predict(p.p_hay_share, newdata = p.p_hay_share_2C)
p3C.pred_p_hay_share <- predict(p.p_hay_share, newdata = p.p_hay_share_3C)
p4C.pred_p_hay_share <- predict(p.p_hay_share, newdata = p.p_hay_share_4C)
p5C.pred_p_hay_share <- predict(p.p_hay_share, newdata = p.p_hay_share_5C)

diff1C.pred_p_hay_share <- predict(diff.p_hay_share, newdata = diff.p_hay_share_1C)
diff2C.pred_p_hay_share <- predict(diff.p_hay_share, newdata = diff.p_hay_share_2C)
diff3C.pred_p_hay_share <- predict(diff.p_hay_share, newdata = diff.p_hay_share_3C)
diff4C.pred_p_hay_share <- predict(diff.p_hay_share, newdata = diff.p_hay_share_4C)
diff5C.pred_p_hay_share <- predict(diff.p_hay_share, newdata = diff.p_hay_share_5C)


# cs.rev <- data.frame(rev = c(predict(cs.p_hay_share), cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))
# 
# p.rev <- data.frame(rev = c(predict(p.p_hay_share), p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))
# 
# diff.rev <- data.frame(rev = c(predict(diff.p_hay_share), diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))
# 
# 
# ggplot(cs.rev, aes(rev, fill = change)) + geom_density(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(filter(p.rev, rev < 50), aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(diff.rev, aes(rev, fill = change)) + geom_density() + scale_fill_brewer(palette = "OrRd")

cs.hay.rev0 <- sum( tobit.ey(predict(cs.p_hay_share), cs.p_hay_share$scale))
cs.hay.rev1 <- (sum( tobit.ey(cs1C.pred_p_hay_share, cs.p_hay_share$scale))/cs.hay.rev0 - 1)*100
cs.hay.rev2 <- (sum( tobit.ey(cs2C.pred_p_hay_share, cs.p_hay_share$scale))/cs.hay.rev0 - 1)*100
cs.hay.rev3 <- (sum( tobit.ey(cs3C.pred_p_hay_share, cs.p_hay_share$scale))/cs.hay.rev0 - 1)*100
cs.hay.rev4 <- (sum( tobit.ey(cs4C.pred_p_hay_share, cs.p_hay_share$scale))/cs.hay.rev0 - 1)*100
cs.hay.rev5 <- (sum( tobit.ey(cs5C.pred_p_hay_share, cs.p_hay_share$scale))/cs.hay.rev0 - 1)*100

p.hay.rev0 <- sum( tobit.ey(predict(p.p_hay_share), p.p_hay_share$scale))
p.hay.rev1 <- (sum( tobit.ey(p1C.pred_p_hay_share, p.p_hay_share$scale))/p.hay.rev0 - 1)*100
p.hay.rev2 <- (sum( tobit.ey(p2C.pred_p_hay_share, p.p_hay_share$scale))/p.hay.rev0 - 1)*100
p.hay.rev3 <- (sum( tobit.ey(p3C.pred_p_hay_share, p.p_hay_share$scale))/p.hay.rev0 - 1)*100
p.hay.rev4 <- (sum( tobit.ey(p4C.pred_p_hay_share, p.p_hay_share$scale))/p.hay.rev0 - 1)*100
p.hay.rev5 <- (sum( tobit.ey(p5C.pred_p_hay_share, p.p_hay_share$scale))/p.hay.rev0 - 1)*100

diff.hay.rev0 <- sum( tobit.ey(predict(diff.p_hay_share), diff.p_hay_share$scale))
diff.hay.rev1 <- (sum( tobit.ey(diff1C.pred_p_hay_share, diff.p_hay_share$scale))/diff.hay.rev0 - 1)*100
diff.hay.rev2 <- (sum( tobit.ey(diff2C.pred_p_hay_share, diff.p_hay_share$scale))/diff.hay.rev0 - 1)*100
diff.hay.rev3 <- (sum( tobit.ey(diff3C.pred_p_hay_share, diff.p_hay_share$scale))/diff.hay.rev0 - 1)*100
diff.hay.rev4 <- (sum( tobit.ey(diff4C.pred_p_hay_share, diff.p_hay_share$scale))/diff.hay.rev0 - 1)*100
diff.hay.rev5 <- (sum( tobit.ey(diff5C.pred_p_hay_share, diff.p_hay_share$scale))/diff.hay.rev0 - 1)*100

hay.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.hay.rev1, cs.hay.rev2, cs.hay.rev3, cs.hay.rev4, cs.hay.rev5,
                                   p.hay.rev1, p.hay.rev2, p.hay.rev3, p.hay.rev4, p.hay.rev5,
                                   diff.hay.rev1, diff.hay.rev2, diff.hay.rev3, diff.hay.rev4, diff.hay.rev5),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 5),
                           crop = "Hay")

ggplot(hay.plotdat, aes(temp, rev, color = reg)) + geom_line()


# Wheat


cs1C.pred_p_wheat_share <- predict(cs.p_wheat_share, newdata = cs.p_wheat_share_1C)
cs2C.pred_p_wheat_share <- predict(cs.p_wheat_share, newdata = cs.p_wheat_share_2C)
cs3C.pred_p_wheat_share <- predict(cs.p_wheat_share, newdata = cs.p_wheat_share_3C)
cs4C.pred_p_wheat_share <- predict(cs.p_wheat_share, newdata = cs.p_wheat_share_4C)
cs5C.pred_p_wheat_share <- predict(cs.p_wheat_share, newdata = cs.p_wheat_share_5C)

p1C.pred_p_wheat_share <- predict(p.p_wheat_share, newdata = p.p_wheat_share_1C)
p2C.pred_p_wheat_share <- predict(p.p_wheat_share, newdata = p.p_wheat_share_2C)
p3C.pred_p_wheat_share <- predict(p.p_wheat_share, newdata = p.p_wheat_share_3C)
p4C.pred_p_wheat_share <- predict(p.p_wheat_share, newdata = p.p_wheat_share_4C)
p5C.pred_p_wheat_share <- predict(p.p_wheat_share, newdata = p.p_wheat_share_5C)

diff1C.pred_p_wheat_share <- predict(diff.p_wheat_share, newdata = diff.p_wheat_share_1C)
diff2C.pred_p_wheat_share <- predict(diff.p_wheat_share, newdata = diff.p_wheat_share_2C)
diff3C.pred_p_wheat_share <- predict(diff.p_wheat_share, newdata = diff.p_wheat_share_3C)
diff4C.pred_p_wheat_share <- predict(diff.p_wheat_share, newdata = diff.p_wheat_share_4C)
diff5C.pred_p_wheat_share <- predict(diff.p_wheat_share, newdata = diff.p_wheat_share_5C)


cs.wheat.rev0 <- sum( tobit.ey(predict(cs.p_wheat_share), cs.p_wheat_share$scale))
cs.wheat.rev1 <- (sum( tobit.ey(cs1C.pred_p_wheat_share, cs.p_wheat_share$scale))/cs.wheat.rev0 - 1)*100
cs.wheat.rev2 <- (sum( tobit.ey(cs2C.pred_p_wheat_share, cs.p_wheat_share$scale))/cs.wheat.rev0 - 1)*100
cs.wheat.rev3 <- (sum( tobit.ey(cs3C.pred_p_wheat_share, cs.p_wheat_share$scale))/cs.wheat.rev0 - 1)*100
cs.wheat.rev4 <- (sum( tobit.ey(cs4C.pred_p_wheat_share, cs.p_wheat_share$scale))/cs.wheat.rev0 - 1)*100
cs.wheat.rev5 <- (sum( tobit.ey(cs5C.pred_p_wheat_share, cs.p_wheat_share$scale))/cs.wheat.rev0 - 1)*100

p.wheat.rev0 <- sum( tobit.ey(predict(p.p_wheat_share), p.p_wheat_share$scale))
p.wheat.rev1 <- (sum( tobit.ey(p1C.pred_p_wheat_share, p.p_wheat_share$scale))/p.wheat.rev0 - 1)*100
p.wheat.rev2 <- (sum( tobit.ey(p2C.pred_p_wheat_share, p.p_wheat_share$scale))/p.wheat.rev0 - 1)*100
p.wheat.rev3 <- (sum( tobit.ey(p3C.pred_p_wheat_share, p.p_wheat_share$scale))/p.wheat.rev0 - 1)*100
p.wheat.rev4 <- (sum( tobit.ey(p4C.pred_p_wheat_share, p.p_wheat_share$scale))/p.wheat.rev0 - 1)*100
p.wheat.rev5 <- (sum( tobit.ey(p5C.pred_p_wheat_share, p.p_wheat_share$scale))/p.wheat.rev0 - 1)*100

diff.wheat.rev0 <- sum( tobit.ey(predict(diff.p_wheat_share), diff.p_wheat_share$scale))
diff.wheat.rev1 <- (sum( tobit.ey(diff1C.pred_p_wheat_share, diff.p_wheat_share$scale))/diff.wheat.rev0 - 1)*100
diff.wheat.rev2 <- (sum( tobit.ey(diff2C.pred_p_wheat_share, diff.p_wheat_share$scale))/diff.wheat.rev0 - 1)*100
diff.wheat.rev3 <- (sum( tobit.ey(diff3C.pred_p_wheat_share, diff.p_wheat_share$scale))/diff.wheat.rev0 - 1)*100
diff.wheat.rev4 <- (sum( tobit.ey(diff4C.pred_p_wheat_share, diff.p_wheat_share$scale))/diff.wheat.rev0 - 1)*100
diff.wheat.rev5 <- (sum( tobit.ey(diff5C.pred_p_wheat_share, diff.p_wheat_share$scale))/diff.wheat.rev0 - 1)*100

wheat.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.wheat.rev1, cs.wheat.rev2, cs.wheat.rev3, cs.wheat.rev4, cs.wheat.rev5,
                                   p.wheat.rev1, p.wheat.rev2, p.wheat.rev3, p.wheat.rev4, p.wheat.rev5,
                                   diff.wheat.rev1, diff.wheat.rev2, diff.wheat.rev3, diff.wheat.rev4, diff.wheat.rev5),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 5),
                           crop = "Wheat")

ggplot(wheat.plotdat, aes(temp, rev, color = reg)) + geom_line()



# Soybean

cs1C.pred_p_soybean_share <- predict(cs.p_soybean_share, newdata = cs.p_soybean_share_1C)
cs2C.pred_p_soybean_share <- predict(cs.p_soybean_share, newdata = cs.p_soybean_share_2C)
cs3C.pred_p_soybean_share <- predict(cs.p_soybean_share, newdata = cs.p_soybean_share_3C)
cs4C.pred_p_soybean_share <- predict(cs.p_soybean_share, newdata = cs.p_soybean_share_4C)
cs5C.pred_p_soybean_share <- predict(cs.p_soybean_share, newdata = cs.p_soybean_share_5C)

p1C.pred_p_soybean_share <- predict(p.p_soybean_share, newdata = p.p_soybean_share_1C)
p2C.pred_p_soybean_share <- predict(p.p_soybean_share, newdata = p.p_soybean_share_2C)
p3C.pred_p_soybean_share <- predict(p.p_soybean_share, newdata = p.p_soybean_share_3C)
p4C.pred_p_soybean_share <- predict(p.p_soybean_share, newdata = p.p_soybean_share_4C)
p5C.pred_p_soybean_share <- predict(p.p_soybean_share, newdata = p.p_soybean_share_5C)

diff1C.pred_p_soybean_share <- predict(diff.p_soybean_share, newdata = diff.p_soybean_share_1C)
diff2C.pred_p_soybean_share <- predict(diff.p_soybean_share, newdata = diff.p_soybean_share_2C)
diff3C.pred_p_soybean_share <- predict(diff.p_soybean_share, newdata = diff.p_soybean_share_3C)
diff4C.pred_p_soybean_share <- predict(diff.p_soybean_share, newdata = diff.p_soybean_share_4C)
diff5C.pred_p_soybean_share <- predict(diff.p_soybean_share, newdata = diff.p_soybean_share_5C)


# cs.rev <- data.frame(rev = c(predict(cs.p_soybean_share), cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))
# 
# p.rev <- data.frame(rev = c(predict(p.p_soybean_share), p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))
# 
# diff.rev <- data.frame(rev = c(predict(diff.p_soybean_share), diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))
# 
# 
# ggplot(cs.rev, aes(rev, fill = change)) + geom_density(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(filter(p.rev, rev < 50), aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(diff.rev, aes(rev, fill = change)) + geom_density() + scale_fill_brewer(palette = "OrRd")

cs.soybean.rev0 <- sum( tobit.ey(predict(cs.p_soybean_share), cs.p_soybean_share$scale))
cs.soybean.rev1 <- (sum( tobit.ey(cs1C.pred_p_soybean_share, cs.p_soybean_share$scale))/cs.soybean.rev0 - 1)*100
cs.soybean.rev2 <- (sum( tobit.ey(cs2C.pred_p_soybean_share, cs.p_soybean_share$scale))/cs.soybean.rev0 - 1)*100
cs.soybean.rev3 <- (sum( tobit.ey(cs3C.pred_p_soybean_share, cs.p_soybean_share$scale))/cs.soybean.rev0 - 1)*100
cs.soybean.rev4 <- (sum( tobit.ey(cs4C.pred_p_soybean_share, cs.p_soybean_share$scale), na.rm = TRUE)/cs.soybean.rev0 - 1)*100
cs.soybean.rev5 <- (sum( tobit.ey(cs5C.pred_p_soybean_share, cs.p_soybean_share$scale), na.rm = TRUE)/cs.soybean.rev0 - 1)*100

p.soybean.rev0 <- sum( tobit.ey(predict(p.p_soybean_share), p.p_soybean_share$scale))
p.soybean.rev1 <- (sum( tobit.ey(p1C.pred_p_soybean_share, p.p_soybean_share$scale))/p.soybean.rev0 - 1)*100
p.soybean.rev2 <- (sum( tobit.ey(p2C.pred_p_soybean_share, p.p_soybean_share$scale))/p.soybean.rev0 - 1)*100
p.soybean.rev3 <- (sum( tobit.ey(p3C.pred_p_soybean_share, p.p_soybean_share$scale))/p.soybean.rev0 - 1)*100
p.soybean.rev4 <- (sum( tobit.ey(p4C.pred_p_soybean_share, p.p_soybean_share$scale))/p.soybean.rev0 - 1)*100
p.soybean.rev5 <- (sum( tobit.ey(p5C.pred_p_soybean_share, p.p_soybean_share$scale))/p.soybean.rev0 - 1)*100

diff.soybean.rev0 <- sum( tobit.ey(predict(diff.p_soybean_share), diff.p_soybean_share$scale))
diff.soybean.rev1 <- (sum( tobit.ey(diff1C.pred_p_soybean_share, diff.p_soybean_share$scale))/diff.soybean.rev0 - 1)*100
diff.soybean.rev2 <- (sum( tobit.ey(diff2C.pred_p_soybean_share, diff.p_soybean_share$scale))/diff.soybean.rev0 - 1)*100
diff.soybean.rev3 <- (sum( tobit.ey(diff3C.pred_p_soybean_share, diff.p_soybean_share$scale))/diff.soybean.rev0 - 1)*100
diff.soybean.rev4 <- (sum( tobit.ey(diff4C.pred_p_soybean_share, diff.p_soybean_share$scale))/diff.soybean.rev0 - 1)*100
diff.soybean.rev5 <- (sum( tobit.ey(diff5C.pred_p_soybean_share, diff.p_soybean_share$scale))/diff.soybean.rev0 - 1)*100

soybean.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.soybean.rev1, cs.soybean.rev2, cs.soybean.rev3, cs.soybean.rev4, cs.soybean.rev5,
                                   p.soybean.rev1, p.soybean.rev2, p.soybean.rev3, p.soybean.rev4, p.soybean.rev5,
                                   diff.soybean.rev1, diff.soybean.rev2, diff.soybean.rev3, diff.soybean.rev4, diff.soybean.rev5),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 5),
                           crop = "Soybean")

ggplot(soybean.plotdat, aes(temp, rev, color = reg)) + geom_line()


# Merge data
plotdat <- rbind(corn.plotdat, cotton.plotdat, hay.plotdat, wheat.plotdat, soybean.plotdat)

sharep1 <- ggplot(plotdat, aes(temp, rev, color = reg)) + geom_line() + ylab("Impact (% Change) ") + xlab("Change in Temperature (C)") + facet_wrap(~crop)
sharep1

# Just Corn and Soybean
sharep1 <- ggplot(filter(plotdat, crop %in% c("Corn", "Soybean", "Hay")), aes(temp, rev, color = reg)) + 
  geom_line() + ylab("Impact (% Change) ") + xlab("Change in Temperature (C)") + facet_wrap(~crop) +
  theme_tufte() + geom_hline(yintercept = 0, linetype = "dashed", color = "grey") 
sharep1

sharep2 <- ggplot(filter(plotdat, crop %in% c("Wheat", "Cotton")), aes(temp, rev, color = reg)) + 
  geom_line() + ylab("Impact (% Change) ") + xlab("Change in Temperature (C)") + facet_wrap(~crop) +
  theme_tufte() + geom_hline(yintercept = 0, linetype = "dashed", color = "grey") 
sharep2

plot_grid(sharep1, sharep2, ncol = 1)



