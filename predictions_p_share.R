library(tidyverse)
library(lfe)
library(AER)
library(cowplot)
library(ggthemes)

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
cs.dat <- readRDS("data/cross_section_regression_data.rds")
p.dat <- readRDS("data/panel_regression_data.rds")
diff.dat <- readRDS("data/diff_regression_data.rds")

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

cs0C.pred_p_corn_share <- predict(cs.p_corn_share, newdata = cs.p_corn_share_0C)
cs1C.pred_p_corn_share <- predict(cs.p_corn_share, newdata = cs.p_corn_share_1C)
cs2C.pred_p_corn_share <- predict(cs.p_corn_share, newdata = cs.p_corn_share_2C)
cs3C.pred_p_corn_share <- predict(cs.p_corn_share, newdata = cs.p_corn_share_3C)
cs4C.pred_p_corn_share <- predict(cs.p_corn_share, newdata = cs.p_corn_share_4C)
cs5C.pred_p_corn_share <- predict(cs.p_corn_share, newdata = cs.p_corn_share_5C)

p0C.pred_p_corn_share <- predict(p.p_corn_share, newdata = p.p_corn_share_0C)
p1C.pred_p_corn_share <- predict(p.p_corn_share, newdata = p.p_corn_share_1C)
p2C.pred_p_corn_share <- predict(p.p_corn_share, newdata = p.p_corn_share_2C)
p3C.pred_p_corn_share <- predict(p.p_corn_share, newdata = p.p_corn_share_3C)
p4C.pred_p_corn_share <- predict(p.p_corn_share, newdata = p.p_corn_share_4C)
p5C.pred_p_corn_share <- predict(p.p_corn_share, newdata = p.p_corn_share_5C)

diff0C.pred_p_corn_share <- predict(diff.p_corn_share, newdata = diff.p_corn_share_0C)
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

cs.corn.share0 <- sum( tobit.ey(cs0C.pred_p_corn_share, cs.p_corn_share$scale)*cs.p_corn_share_0C$total_a, na.rm = TRUE)
cs.corn.share1 <- (sum( tobit.ey(cs1C.pred_p_corn_share, cs.p_corn_share$scale)*cs.p_corn_share_0C$total_a, na.rm = TRUE)/cs.corn.share0 - 1)*100
cs.corn.share2 <- (sum( tobit.ey(cs2C.pred_p_corn_share, cs.p_corn_share$scale)*cs.p_corn_share_0C$total_a, na.rm = TRUE)/cs.corn.share0 - 1)*100
cs.corn.share3 <- (sum( tobit.ey(cs3C.pred_p_corn_share, cs.p_corn_share$scale)*cs.p_corn_share_0C$total_a, na.rm = TRUE)/cs.corn.share0 - 1)*100
cs.corn.share4 <- (sum( tobit.ey(cs4C.pred_p_corn_share, cs.p_corn_share$scale)*cs.p_corn_share_0C$total_a, na.rm = TRUE)/cs.corn.share0 - 1)*100
cs.corn.share5 <- (sum( tobit.ey(cs5C.pred_p_corn_share, cs.p_corn_share$scale)*cs.p_corn_share_0C$total_a, na.rm = TRUE)/cs.corn.share0 - 1)*100

p.corn.share0 <- sum( tobit.ey(p0C.pred_p_corn_share, p.p_corn_share$scale)*p.p_corn_share_0C$total_a)
p.corn.share1 <- (sum( tobit.ey(p1C.pred_p_corn_share, p.p_corn_share$scale)*p.p_corn_share_0C$total_a)/p.corn.share0 - 1)*100
p.corn.share2 <- (sum( tobit.ey(p2C.pred_p_corn_share, p.p_corn_share$scale)*p.p_corn_share_0C$total_a)/p.corn.share0 - 1)*100
p.corn.share3 <- (sum( tobit.ey(p3C.pred_p_corn_share, p.p_corn_share$scale)*p.p_corn_share_0C$total_a)/p.corn.share0 - 1)*100
p.corn.share4 <- (sum( tobit.ey(p4C.pred_p_corn_share, p.p_corn_share$scale)*p.p_corn_share_0C$total_a)/p.corn.share0 - 1)*100
p.corn.share5 <- (sum( tobit.ey(p5C.pred_p_corn_share, p.p_corn_share$scale)*p.p_corn_share_0C$total_a)/p.corn.share0 - 1)*100

diff.corn.share0 <- sum( tobit.ey(diff0C.pred_p_corn_share, diff.p_corn_share$scale)*diff.p_corn_share_0C$total_a)
diff.corn.share1 <- (sum( tobit.ey(diff1C.pred_p_corn_share, diff.p_corn_share$scale)*diff.p_corn_share_0C$total_a)/diff.corn.share0 - 1)*100
diff.corn.share2 <- (sum( tobit.ey(diff2C.pred_p_corn_share, diff.p_corn_share$scale)*diff.p_corn_share_0C$total_a)/diff.corn.share0 - 1)*100
diff.corn.share3 <- (sum( tobit.ey(diff3C.pred_p_corn_share, diff.p_corn_share$scale)*diff.p_corn_share_0C$total_a)/diff.corn.share0 - 1)*100
diff.corn.share4 <- (sum( tobit.ey(diff4C.pred_p_corn_share, diff.p_corn_share$scale)*diff.p_corn_share_0C$total_a)/diff.corn.share0 - 1)*100
diff.corn.share5 <- (sum( tobit.ey(diff5C.pred_p_corn_share, diff.p_corn_share$scale)*diff.p_corn_share_0C$total_a)/diff.corn.share0 - 1)*100

# Average predicted acreage
cs.corn.share0_avg <- mean(tobit.ey(predict(cs.p_corn_share), cs.p_corn_share$scale)*cs.p_corn_share_0C$total_a)
cs.corn.share1_avg <- mean(tobit.ey(cs1C.pred_p_corn_share, cs.p_corn_share$scale)*cs.p_corn_share_0C$total_a)
cs.corn.share2_avg <- mean(tobit.ey(cs2C.pred_p_corn_share, cs.p_corn_share$scale)*cs.p_corn_share_0C$total_a)
cs.corn.share3_avg <- mean(tobit.ey(cs3C.pred_p_corn_share, cs.p_corn_share$scale)*cs.p_corn_share_0C$total_a)
cs.corn.share4_avg <- mean(tobit.ey(cs4C.pred_p_corn_share, cs.p_corn_share$scale)*cs.p_corn_share_0C$total_a)
cs.corn.share5_avg <- mean(tobit.ey(cs5C.pred_p_corn_share, cs.p_corn_share$scale)*cs.p_corn_share_0C$total_a)

p.corn.share0_avg <- mean(tobit.ey(predict(p.p_corn_share), p.p_corn_share$scale)*p.p_corn_share_0C$total_a)
p.corn.share1_avg <- mean(tobit.ey(p1C.pred_p_corn_share, p.p_corn_share$scale)*p.p_corn_share_0C$total_a)
p.corn.share2_avg <- mean(tobit.ey(p2C.pred_p_corn_share, p.p_corn_share$scale)*p.p_corn_share_0C$total_a)
p.corn.share3_avg <- mean(tobit.ey(p3C.pred_p_corn_share, p.p_corn_share$scale)*p.p_corn_share_0C$total_a)
p.corn.share4_avg <- mean(tobit.ey(p4C.pred_p_corn_share, p.p_corn_share$scale)*p.p_corn_share_0C$total_a)
p.corn.share5_avg <- mean(tobit.ey(p5C.pred_p_corn_share, p.p_corn_share$scale)*p.p_corn_share_0C$total_a)

diff.corn.share0_avg <- mean(tobit.ey(predict(diff.p_corn_share), diff.p_corn_share$scale)*diff.p_corn_share_0C$total_a)
diff.corn.share1_avg <- mean(tobit.ey(diff1C.pred_p_corn_share, diff.p_corn_share$scale)*diff.p_corn_share_0C$total_a)
diff.corn.share2_avg <- mean(tobit.ey(diff2C.pred_p_corn_share, diff.p_corn_share$scale)*diff.p_corn_share_0C$total_a)
diff.corn.share3_avg <- mean(tobit.ey(diff3C.pred_p_corn_share, diff.p_corn_share$scale)*diff.p_corn_share_0C$total_a)
diff.corn.share4_avg <- mean(tobit.ey(diff4C.pred_p_corn_share, diff.p_corn_share$scale)*diff.p_corn_share_0C$total_a)
diff.corn.share5_avg <- mean(tobit.ey(diff5C.pred_p_corn_share, diff.p_corn_share$scale)*diff.p_corn_share_0C$total_a)


corn.plotdat <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           share = c(0,cs.corn.share1, cs.corn.share2, cs.corn.share3, cs.corn.share4, cs.corn.share5,
                                   0,p.corn.share1, p.corn.share2, p.corn.share3, p.corn.share4, p.corn.share5,
                                   0,diff.corn.share1, diff.corn.share2, diff.corn.share3, diff.corn.share4, diff.corn.share5),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "Corn")

ggplot(corn.plotdat, aes(temp, share, color = reg)) + geom_line()

corn.pred_acre <- data.frame(temp = rep(c(0, 1, 2, 3, 4, 5), 3),
                             acreage = c(cs.corn.share0_avg, cs.corn.share1_avg, cs.corn.share2_avg, cs.corn.share3_avg, cs.corn.share4_avg, cs.corn.share5_avg,
                             p.corn.share0_avg, p.corn.share1_avg, p.corn.share2_avg, p.corn.share3_avg, p.corn.share4_avg, p.corn.share5_avg,
                             diff.corn.share0_avg, diff.corn.share1_avg, diff.corn.share2_avg, diff.corn.share3_avg, diff.corn.share4_avg, diff.corn.share5_avg),
                             reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                             crop = "corn")

ggplot(corn.pred_acre, aes(temp, acreage, color = reg)) + geom_line()

# Predictions

# Cotton

cs0C.pred_p_cotton_share <- predict(cs.p_cotton_share, newdata = cs.p_cotton_share_0C)
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


# cs.share <- data.frame(share = c(predict(cs.p_cotton_share), cs.share1C, cs.share2C, cs.share3C, cs.share4C, cs.share5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.share1C)))
# 
# p.share <- data.frame(share = c(predict(p.p_cotton_share), p.share1C, p.share2C, p.share3C, p.share4C, p.share5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(p.share1C)))
# 
# diff.share <- data.frame(share = c(predict(diff.p_cotton_share), diff.share1C, diff.share2C, diff.share3C, diff.share4C, diff.share5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(diff.share1C)))
# 
# 
# ggplot(cs.share, aes(share, fill = change)) + geom_density(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(filter(p.share, share < 50), aes(share, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(diff.share, aes(share, fill = change)) + geom_density() + scale_fill_brewer(palette = "OrRd")

#cs.cotton.share0 <- sum((predict(cs.p_cotton_share, newdata = cs.p_cotton_share_0C)))

cs.cotton.share0 <- sum( tobit.ey(predict(cs.p_cotton_share), cs.p_cotton_share$scale)*cs.p_cotton_share_0C$total_a, na.rm = TRUE)
cs.cotton.share1 <- (sum( tobit.ey(cs1C.pred_p_cotton_share, cs.p_cotton_share$scale)*cs.p_cotton_share_0C$total_a, na.rm = TRUE)/cs.cotton.share0 - 1)*100
cs.cotton.share2 <- (sum( tobit.ey(cs2C.pred_p_cotton_share, cs.p_cotton_share$scale)*cs.p_cotton_share_0C$total_a, na.rm = TRUE)/cs.cotton.share0 - 1)*100
cs.cotton.share3 <- (sum( tobit.ey(cs3C.pred_p_cotton_share, cs.p_cotton_share$scale)*cs.p_cotton_share_0C$total_a, na.rm = TRUE)/cs.cotton.share0 - 1)*100
cs.cotton.share4 <- (sum( tobit.ey(cs4C.pred_p_cotton_share, cs.p_cotton_share$scale)*cs.p_cotton_share_0C$total_a, na.rm = TRUE)/cs.cotton.share0 - 1)*100
cs.cotton.share5 <- (sum( tobit.ey(cs5C.pred_p_cotton_share, cs.p_cotton_share$scale)*cs.p_cotton_share_0C$total_a, na.rm = TRUE)/cs.cotton.share0 - 1)*100

p.cotton.share0 <- sum( tobit.ey(predict(p.p_cotton_share), p.p_cotton_share$scale)*p.p_cotton_share_0C$total_a)
p.cotton.share1 <- (sum( tobit.ey(p1C.pred_p_cotton_share, p.p_cotton_share$scale)*p.p_cotton_share_0C$total_a)/p.cotton.share0 - 1)*100
p.cotton.share2 <- (sum( tobit.ey(p2C.pred_p_cotton_share, p.p_cotton_share$scale)*p.p_cotton_share_0C$total_a)/p.cotton.share0 - 1)*100
p.cotton.share3 <- (sum( tobit.ey(p3C.pred_p_cotton_share, p.p_cotton_share$scale)*p.p_cotton_share_0C$total_a)/p.cotton.share0 - 1)*100
p.cotton.share4 <- (sum( tobit.ey(p4C.pred_p_cotton_share, p.p_cotton_share$scale)*p.p_cotton_share_0C$total_a)/p.cotton.share0 - 1)*100
p.cotton.share5 <- (sum( tobit.ey(p5C.pred_p_cotton_share, p.p_cotton_share$scale)*p.p_cotton_share_0C$total_a)/p.cotton.share0 - 1)*100

diff.cotton.share0 <- sum( tobit.ey(predict(diff.p_cotton_share), diff.p_cotton_share$scale)*diff.p_cotton_share_0C$total_a)
diff.cotton.share1 <- (sum( tobit.ey(diff1C.pred_p_cotton_share, diff.p_cotton_share$scale)*diff.p_cotton_share_0C$total_a)/diff.cotton.share0 - 1)*100
diff.cotton.share2 <- (sum( tobit.ey(diff2C.pred_p_cotton_share, diff.p_cotton_share$scale)*diff.p_cotton_share_0C$total_a)/diff.cotton.share0 - 1)*100
diff.cotton.share3 <- (sum( tobit.ey(diff3C.pred_p_cotton_share, diff.p_cotton_share$scale)*diff.p_cotton_share_0C$total_a)/diff.cotton.share0 - 1)*100
diff.cotton.share4 <- (sum( tobit.ey(diff4C.pred_p_cotton_share, diff.p_cotton_share$scale)*diff.p_cotton_share_0C$total_a)/diff.cotton.share0 - 1)*100
diff.cotton.share5 <- (sum( tobit.ey(diff5C.pred_p_cotton_share, diff.p_cotton_share$scale)*diff.p_cotton_share_0C$total_a)/diff.cotton.share0 - 1)*100

# Average predicted acreage
cs.cotton.share0_avg <- mean(tobit.ey(predict(cs.p_cotton_share), cs.p_cotton_share$scale)*cs.p_cotton_share_0C$total_a)
cs.cotton.share1_avg <- mean(tobit.ey(cs1C.pred_p_cotton_share, cs.p_cotton_share$scale)*cs.p_cotton_share_0C$total_a)
cs.cotton.share2_avg <- mean(tobit.ey(cs2C.pred_p_cotton_share, cs.p_cotton_share$scale)*cs.p_cotton_share_0C$total_a)
cs.cotton.share3_avg <- mean(tobit.ey(cs3C.pred_p_cotton_share, cs.p_cotton_share$scale)*cs.p_cotton_share_0C$total_a)
cs.cotton.share4_avg <- mean(tobit.ey(cs4C.pred_p_cotton_share, cs.p_cotton_share$scale)*cs.p_cotton_share_0C$total_a)
cs.cotton.share5_avg <- mean(tobit.ey(cs5C.pred_p_cotton_share, cs.p_cotton_share$scale)*cs.p_cotton_share_0C$total_a)

p.cotton.share0_avg <- mean(tobit.ey(predict(p.p_cotton_share), p.p_cotton_share$scale)*p.p_cotton_share_0C$total_a)
p.cotton.share1_avg <- mean(tobit.ey(p1C.pred_p_cotton_share, p.p_cotton_share$scale)*p.p_cotton_share_0C$total_a)
p.cotton.share2_avg <- mean(tobit.ey(p2C.pred_p_cotton_share, p.p_cotton_share$scale)*p.p_cotton_share_0C$total_a)
p.cotton.share3_avg <- mean(tobit.ey(p3C.pred_p_cotton_share, p.p_cotton_share$scale)*p.p_cotton_share_0C$total_a)
p.cotton.share4_avg <- mean(tobit.ey(p4C.pred_p_cotton_share, p.p_cotton_share$scale)*p.p_cotton_share_0C$total_a)
p.cotton.share5_avg <- mean(tobit.ey(p5C.pred_p_cotton_share, p.p_cotton_share$scale)*p.p_cotton_share_0C$total_a)

diff.cotton.share0_avg <- mean(tobit.ey(predict(diff.p_cotton_share), diff.p_cotton_share$scale)*diff.p_cotton_share_0C$total_a)
diff.cotton.share1_avg <- mean(tobit.ey(diff1C.pred_p_cotton_share, diff.p_cotton_share$scale)*diff.p_cotton_share_0C$total_a)
diff.cotton.share2_avg <- mean(tobit.ey(diff2C.pred_p_cotton_share, diff.p_cotton_share$scale)*diff.p_cotton_share_0C$total_a)
diff.cotton.share3_avg <- mean(tobit.ey(diff3C.pred_p_cotton_share, diff.p_cotton_share$scale)*diff.p_cotton_share_0C$total_a)
diff.cotton.share4_avg <- mean(tobit.ey(diff4C.pred_p_cotton_share, diff.p_cotton_share$scale)*diff.p_cotton_share_0C$total_a)
diff.cotton.share5_avg <- mean(tobit.ey(diff5C.pred_p_cotton_share, diff.p_cotton_share$scale)*diff.p_cotton_share_0C$total_a)

cotton.pred_acre <- data.frame(temp = rep(c(0, 1, 2, 3, 4, 5), 3),
                             acreage = c(cs.cotton.share0_avg, cs.cotton.share1_avg, cs.cotton.share2_avg, cs.cotton.share3_avg, cs.cotton.share4_avg, cs.cotton.share5_avg,
                             p.cotton.share0_avg, p.cotton.share1_avg, p.cotton.share2_avg, p.cotton.share3_avg, p.cotton.share4_avg, p.cotton.share5_avg,
                             diff.cotton.share0_avg, diff.cotton.share1_avg, diff.cotton.share2_avg, diff.cotton.share3_avg, diff.cotton.share4_avg, diff.cotton.share5_avg),
                             reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                             crop = "cotton")

ggplot(cotton.pred_acre, aes(temp, acreage, color = reg)) + geom_line()


cotton.plotdat <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           share = c(0,cs.cotton.share1, cs.cotton.share2, cs.cotton.share3, cs.cotton.share4, cs.cotton.share5,
                                   0,p.cotton.share1, p.cotton.share2, p.cotton.share3, p.cotton.share4, p.cotton.share5,
                                   0,diff.cotton.share1, diff.cotton.share2, diff.cotton.share3, diff.cotton.share4, diff.cotton.share5),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "cotton")

ggplot(cotton.plotdat, aes(temp, share, color = reg)) + geom_line()



# Hay

cs0C.pred_p_hay_share <- predict(cs.p_hay_share, newdata = cs.p_hay_share_0C)
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


# cs.share <- data.frame(share = c(predict(cs.p_hay_share), cs.share1C, cs.share2C, cs.share3C, cs.share4C, cs.share5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.share1C)))
# 
# p.share <- data.frame(share = c(predict(p.p_hay_share), p.share1C, p.share2C, p.share3C, p.share4C, p.share5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(p.share1C)))
# 
# diff.share <- data.frame(share = c(predict(diff.p_hay_share), diff.share1C, diff.share2C, diff.share3C, diff.share4C, diff.share5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(diff.share1C)))
# 
# 
# ggplot(cs.share, aes(share, fill = change)) + geom_density(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(filter(p.share, share < 50), aes(share, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(diff.share, aes(share, fill = change)) + geom_density() + scale_fill_brewer(palette = "OrRd")

#cs.hay.share0 <- sum((predict(cs.p_hay_share, newdata = cs.p_hay_share_0C)))

cs.hay.share0 <- sum( tobit.ey(predict(cs.p_hay_share), cs.p_hay_share$scale)*cs.p_hay_share_0C$total_a, na.rm = TRUE)
cs.hay.share1 <- (sum( tobit.ey(cs1C.pred_p_hay_share, cs.p_hay_share$scale)*cs.p_hay_share_0C$total_a, na.rm = TRUE)/cs.hay.share0 - 1)*100
cs.hay.share2 <- (sum( tobit.ey(cs2C.pred_p_hay_share, cs.p_hay_share$scale)*cs.p_hay_share_0C$total_a, na.rm = TRUE)/cs.hay.share0 - 1)*100
cs.hay.share3 <- (sum( tobit.ey(cs3C.pred_p_hay_share, cs.p_hay_share$scale)*cs.p_hay_share_0C$total_a, na.rm = TRUE)/cs.hay.share0 - 1)*100
cs.hay.share4 <- (sum( tobit.ey(cs4C.pred_p_hay_share, cs.p_hay_share$scale)*cs.p_hay_share_0C$total_a, na.rm = TRUE)/cs.hay.share0 - 1)*100
cs.hay.share5 <- (sum( tobit.ey(cs5C.pred_p_hay_share, cs.p_hay_share$scale)*cs.p_hay_share_0C$total_a, na.rm = TRUE)/cs.hay.share0 - 1)*100

p.hay.share0 <- sum( tobit.ey(predict(p.p_hay_share), p.p_hay_share$scale)*p.p_hay_share_0C$total_a)
p.hay.share1 <- (sum( tobit.ey(p1C.pred_p_hay_share, p.p_hay_share$scale)*p.p_hay_share_0C$total_a)/p.hay.share0 - 1)*100
p.hay.share2 <- (sum( tobit.ey(p2C.pred_p_hay_share, p.p_hay_share$scale)*p.p_hay_share_0C$total_a)/p.hay.share0 - 1)*100
p.hay.share3 <- (sum( tobit.ey(p3C.pred_p_hay_share, p.p_hay_share$scale)*p.p_hay_share_0C$total_a)/p.hay.share0 - 1)*100
p.hay.share4 <- (sum( tobit.ey(p4C.pred_p_hay_share, p.p_hay_share$scale)*p.p_hay_share_0C$total_a)/p.hay.share0 - 1)*100
p.hay.share5 <- (sum( tobit.ey(p5C.pred_p_hay_share, p.p_hay_share$scale)*p.p_hay_share_0C$total_a)/p.hay.share0 - 1)*100

diff.hay.share0 <- sum( tobit.ey(predict(diff.p_hay_share), diff.p_hay_share$scale)*diff.p_hay_share_0C$total_a)
diff.hay.share1 <- (sum( tobit.ey(diff1C.pred_p_hay_share, diff.p_hay_share$scale)*diff.p_hay_share_0C$total_a)/diff.hay.share0 - 1)*100
diff.hay.share2 <- (sum( tobit.ey(diff2C.pred_p_hay_share, diff.p_hay_share$scale)*diff.p_hay_share_0C$total_a)/diff.hay.share0 - 1)*100
diff.hay.share3 <- (sum( tobit.ey(diff3C.pred_p_hay_share, diff.p_hay_share$scale)*diff.p_hay_share_0C$total_a)/diff.hay.share0 - 1)*100
diff.hay.share4 <- (sum( tobit.ey(diff4C.pred_p_hay_share, diff.p_hay_share$scale)*diff.p_hay_share_0C$total_a)/diff.hay.share0 - 1)*100
diff.hay.share5 <- (sum( tobit.ey(diff5C.pred_p_hay_share, diff.p_hay_share$scale)*diff.p_hay_share_0C$total_a)/diff.hay.share0 - 1)*100

# Average predicted acreage
cs.hay.share0_avg <- mean(tobit.ey(predict(cs.p_hay_share), cs.p_hay_share$scale)*cs.p_hay_share_0C$total_a)
cs.hay.share1_avg <- mean(tobit.ey(cs1C.pred_p_hay_share, cs.p_hay_share$scale)*cs.p_hay_share_0C$total_a)
cs.hay.share2_avg <- mean(tobit.ey(cs2C.pred_p_hay_share, cs.p_hay_share$scale)*cs.p_hay_share_0C$total_a)
cs.hay.share3_avg <- mean(tobit.ey(cs3C.pred_p_hay_share, cs.p_hay_share$scale)*cs.p_hay_share_0C$total_a)
cs.hay.share4_avg <- mean(tobit.ey(cs4C.pred_p_hay_share, cs.p_hay_share$scale)*cs.p_hay_share_0C$total_a)
cs.hay.share5_avg <- mean(tobit.ey(cs5C.pred_p_hay_share, cs.p_hay_share$scale)*cs.p_hay_share_0C$total_a)

p.hay.share0_avg <- mean(tobit.ey(predict(p.p_hay_share), p.p_hay_share$scale)*p.p_hay_share_0C$total_a)
p.hay.share1_avg <- mean(tobit.ey(p1C.pred_p_hay_share, p.p_hay_share$scale)*p.p_hay_share_0C$total_a)
p.hay.share2_avg <- mean(tobit.ey(p2C.pred_p_hay_share, p.p_hay_share$scale)*p.p_hay_share_0C$total_a)
p.hay.share3_avg <- mean(tobit.ey(p3C.pred_p_hay_share, p.p_hay_share$scale)*p.p_hay_share_0C$total_a)
p.hay.share4_avg <- mean(tobit.ey(p4C.pred_p_hay_share, p.p_hay_share$scale)*p.p_hay_share_0C$total_a)
p.hay.share5_avg <- mean(tobit.ey(p5C.pred_p_hay_share, p.p_hay_share$scale)*p.p_hay_share_0C$total_a)

diff.hay.share0_avg <- mean(tobit.ey(predict(diff.p_hay_share), diff.p_hay_share$scale)*diff.p_hay_share_0C$total_a)
diff.hay.share1_avg <- mean(tobit.ey(diff1C.pred_p_hay_share, diff.p_hay_share$scale)*diff.p_hay_share_0C$total_a)
diff.hay.share2_avg <- mean(tobit.ey(diff2C.pred_p_hay_share, diff.p_hay_share$scale)*diff.p_hay_share_0C$total_a)
diff.hay.share3_avg <- mean(tobit.ey(diff3C.pred_p_hay_share, diff.p_hay_share$scale)*diff.p_hay_share_0C$total_a)
diff.hay.share4_avg <- mean(tobit.ey(diff4C.pred_p_hay_share, diff.p_hay_share$scale)*diff.p_hay_share_0C$total_a)
diff.hay.share5_avg <- mean(tobit.ey(diff5C.pred_p_hay_share, diff.p_hay_share$scale)*diff.p_hay_share_0C$total_a)

hay.pred_acre <- data.frame(temp = rep(c(0, 1, 2, 3, 4, 5), 3),
                             acreage = c(cs.hay.share0_avg, cs.hay.share1_avg, cs.hay.share2_avg, cs.hay.share3_avg, cs.hay.share4_avg, cs.hay.share5_avg,
                             p.hay.share0_avg, p.hay.share1_avg, p.hay.share2_avg, p.hay.share3_avg, p.hay.share4_avg, p.hay.share5_avg,
                             diff.hay.share0_avg, diff.hay.share1_avg, diff.hay.share2_avg, diff.hay.share3_avg, diff.hay.share4_avg, diff.hay.share5_avg),
                             reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                            crop = "hay")

ggplot(hay.pred_acre, aes(temp, acreage, color = reg)) + geom_line()

hay.plotdat <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           share = c(0,cs.hay.share1, cs.hay.share2, cs.hay.share3, cs.hay.share4, cs.hay.share5,
                                   0,p.hay.share1, p.hay.share2, p.hay.share3, p.hay.share4, p.hay.share5,
                                   0,diff.hay.share1, diff.hay.share2, diff.hay.share3, diff.hay.share4, diff.hay.share5),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "hay")

ggplot(hay.plotdat, aes(temp, share, color = reg)) + geom_line()


# Wheat

cs0C.pred_p_wheat_share <- predict(cs.p_wheat_share, newdata = cs.p_wheat_share_0C)
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


# cs.share <- data.frame(share = c(predict(cs.p_wheat_share), cs.share1C, cs.share2C, cs.share3C, cs.share4C, cs.share5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.share1C)))
# 
# p.share <- data.frame(share = c(predict(p.p_wheat_share), p.share1C, p.share2C, p.share3C, p.share4C, p.share5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(p.share1C)))
# 
# diff.share <- data.frame(share = c(predict(diff.p_wheat_share), diff.share1C, diff.share2C, diff.share3C, diff.share4C, diff.share5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(diff.share1C)))
# 
# 
# ggplot(cs.share, aes(share, fill = change)) + geom_density(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(filter(p.share, share < 50), aes(share, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(diff.share, aes(share, fill = change)) + geom_density() + scale_fill_brewer(palette = "OrRd")

#cs.wheat.share0 <- sum((predict(cs.p_wheat_share, newdata = cs.p_wheat_share_0C)))

cs.wheat.share0 <- sum( tobit.ey(predict(cs.p_wheat_share), cs.p_wheat_share$scale)*cs.p_wheat_share_0C$total_a, na.rm = TRUE)
cs.wheat.share1 <- (sum( tobit.ey(cs1C.pred_p_wheat_share, cs.p_wheat_share$scale)*cs.p_wheat_share_0C$total_a, na.rm = TRUE)/cs.wheat.share0 - 1)*100
cs.wheat.share2 <- (sum( tobit.ey(cs2C.pred_p_wheat_share, cs.p_wheat_share$scale)*cs.p_wheat_share_0C$total_a, na.rm = TRUE)/cs.wheat.share0 - 1)*100
cs.wheat.share3 <- (sum( tobit.ey(cs3C.pred_p_wheat_share, cs.p_wheat_share$scale)*cs.p_wheat_share_0C$total_a, na.rm = TRUE)/cs.wheat.share0 - 1)*100
cs.wheat.share4 <- (sum( tobit.ey(cs4C.pred_p_wheat_share, cs.p_wheat_share$scale)*cs.p_wheat_share_0C$total_a, na.rm = TRUE)/cs.wheat.share0 - 1)*100
cs.wheat.share5 <- (sum( tobit.ey(cs5C.pred_p_wheat_share, cs.p_wheat_share$scale)*cs.p_wheat_share_0C$total_a, na.rm = TRUE)/cs.wheat.share0 - 1)*100

p.wheat.share0 <- sum( tobit.ey(predict(p.p_wheat_share), p.p_wheat_share$scale)*p.p_wheat_share_0C$total_a)
p.wheat.share1 <- (sum( tobit.ey(p1C.pred_p_wheat_share, p.p_wheat_share$scale)*p.p_wheat_share_0C$total_a)/p.wheat.share0 - 1)*100
p.wheat.share2 <- (sum( tobit.ey(p2C.pred_p_wheat_share, p.p_wheat_share$scale)*p.p_wheat_share_0C$total_a)/p.wheat.share0 - 1)*100
p.wheat.share3 <- (sum( tobit.ey(p3C.pred_p_wheat_share, p.p_wheat_share$scale)*p.p_wheat_share_0C$total_a)/p.wheat.share0 - 1)*100
p.wheat.share4 <- (sum( tobit.ey(p4C.pred_p_wheat_share, p.p_wheat_share$scale)*p.p_wheat_share_0C$total_a)/p.wheat.share0 - 1)*100
p.wheat.share5 <- (sum( tobit.ey(p5C.pred_p_wheat_share, p.p_wheat_share$scale)*p.p_wheat_share_0C$total_a)/p.wheat.share0 - 1)*100

diff.wheat.share0 <- sum( tobit.ey(predict(diff.p_wheat_share), diff.p_wheat_share$scale)*diff.p_wheat_share_0C$total_a)
diff.wheat.share1 <- (sum( tobit.ey(diff1C.pred_p_wheat_share, diff.p_wheat_share$scale)*diff.p_wheat_share_0C$total_a)/diff.wheat.share0 - 1)*100
diff.wheat.share2 <- (sum( tobit.ey(diff2C.pred_p_wheat_share, diff.p_wheat_share$scale)*diff.p_wheat_share_0C$total_a)/diff.wheat.share0 - 1)*100
diff.wheat.share3 <- (sum( tobit.ey(diff3C.pred_p_wheat_share, diff.p_wheat_share$scale)*diff.p_wheat_share_0C$total_a)/diff.wheat.share0 - 1)*100
diff.wheat.share4 <- (sum( tobit.ey(diff4C.pred_p_wheat_share, diff.p_wheat_share$scale)*diff.p_wheat_share_0C$total_a)/diff.wheat.share0 - 1)*100
diff.wheat.share5 <- (sum( tobit.ey(diff5C.pred_p_wheat_share, diff.p_wheat_share$scale)*diff.p_wheat_share_0C$total_a)/diff.wheat.share0 - 1)*100

# Average predicted acreage
cs.wheat.share0_avg <- mean(tobit.ey(predict(cs.p_wheat_share), cs.p_wheat_share$scale)*cs.p_wheat_share_0C$total_a)
cs.wheat.share1_avg <- mean(tobit.ey(cs1C.pred_p_wheat_share, cs.p_wheat_share$scale)*cs.p_wheat_share_0C$total_a)
cs.wheat.share2_avg <- mean(tobit.ey(cs2C.pred_p_wheat_share, cs.p_wheat_share$scale)*cs.p_wheat_share_0C$total_a)
cs.wheat.share3_avg <- mean(tobit.ey(cs3C.pred_p_wheat_share, cs.p_wheat_share$scale)*cs.p_wheat_share_0C$total_a)
cs.wheat.share4_avg <- mean(tobit.ey(cs4C.pred_p_wheat_share, cs.p_wheat_share$scale)*cs.p_wheat_share_0C$total_a)
cs.wheat.share5_avg <- mean(tobit.ey(cs5C.pred_p_wheat_share, cs.p_wheat_share$scale)*cs.p_wheat_share_0C$total_a)

p.wheat.share0_avg <- mean(tobit.ey(predict(p.p_wheat_share), p.p_wheat_share$scale)*p.p_wheat_share_0C$total_a)
p.wheat.share1_avg <- mean(tobit.ey(p1C.pred_p_wheat_share, p.p_wheat_share$scale)*p.p_wheat_share_0C$total_a)
p.wheat.share2_avg <- mean(tobit.ey(p2C.pred_p_wheat_share, p.p_wheat_share$scale)*p.p_wheat_share_0C$total_a)
p.wheat.share3_avg <- mean(tobit.ey(p3C.pred_p_wheat_share, p.p_wheat_share$scale)*p.p_wheat_share_0C$total_a)
p.wheat.share4_avg <- mean(tobit.ey(p4C.pred_p_wheat_share, p.p_wheat_share$scale)*p.p_wheat_share_0C$total_a)
p.wheat.share5_avg <- mean(tobit.ey(p5C.pred_p_wheat_share, p.p_wheat_share$scale)*p.p_wheat_share_0C$total_a)

diff.wheat.share0_avg <- mean(tobit.ey(predict(diff.p_wheat_share), diff.p_wheat_share$scale)*diff.p_wheat_share_0C$total_a)
diff.wheat.share1_avg <- mean(tobit.ey(diff1C.pred_p_wheat_share, diff.p_wheat_share$scale)*diff.p_wheat_share_0C$total_a)
diff.wheat.share2_avg <- mean(tobit.ey(diff2C.pred_p_wheat_share, diff.p_wheat_share$scale)*diff.p_wheat_share_0C$total_a)
diff.wheat.share3_avg <- mean(tobit.ey(diff3C.pred_p_wheat_share, diff.p_wheat_share$scale)*diff.p_wheat_share_0C$total_a)
diff.wheat.share4_avg <- mean(tobit.ey(diff4C.pred_p_wheat_share, diff.p_wheat_share$scale)*diff.p_wheat_share_0C$total_a)
diff.wheat.share5_avg <- mean(tobit.ey(diff5C.pred_p_wheat_share, diff.p_wheat_share$scale)*diff.p_wheat_share_0C$total_a)

wheat.pred_acre <- data.frame(temp = rep(c(0, 1, 2, 3, 4, 5), 3),
                             acreage = c(cs.wheat.share0_avg, cs.wheat.share1_avg, cs.wheat.share2_avg, cs.wheat.share3_avg, cs.wheat.share4_avg, cs.wheat.share5_avg,
                             p.wheat.share0_avg, p.wheat.share1_avg, p.wheat.share2_avg, p.wheat.share3_avg, p.wheat.share4_avg, p.wheat.share5_avg,
                             diff.wheat.share0_avg, diff.wheat.share1_avg, diff.wheat.share2_avg, diff.wheat.share3_avg, diff.wheat.share4_avg, diff.wheat.share5_avg),
                             reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                             crop = "wheat")

ggplot(wheat.pred_acre, aes(temp, acreage, color = reg)) + geom_line()

wheat.plotdat <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           share = c(0,cs.wheat.share1, cs.wheat.share2, cs.wheat.share3, cs.wheat.share4, cs.wheat.share5,
                                   0,p.wheat.share1, p.wheat.share2, p.wheat.share3, p.wheat.share4, p.wheat.share5,
                                   0,diff.wheat.share1, diff.wheat.share2, diff.wheat.share3, diff.wheat.share4, diff.wheat.share5),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "wheat")

ggplot(wheat.plotdat, aes(temp, share, color = reg)) + geom_line()





# Soybean

cs0C.pred_p_soybean_share <- predict(cs.p_soybean_share, newdata = cs.p_soybean_share_0C)
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


# cs.share <- data.frame(share = c(predict(cs.p_soybean_share), cs.share1C, cs.share2C, cs.share3C, cs.share4C, cs.share5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.share1C)))
# 
# p.share <- data.frame(share = c(predict(p.p_soybean_share), p.share1C, p.share2C, p.share3C, p.share4C, p.share5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(p.share1C)))
# 
# diff.share <- data.frame(share = c(predict(diff.p_soybean_share), diff.share1C, diff.share2C, diff.share3C, diff.share4C, diff.share5C),
#                      change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(diff.share1C)))
# 
# 
# ggplot(cs.share, aes(share, fill = change)) + geom_density(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(filter(p.share, share < 50), aes(share, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
# ggplot(diff.share, aes(share, fill = change)) + geom_density() + scale_fill_brewer(palette = "OrRd")

#cs.soybean.share0 <- sum((predict(cs.p_soybean_share, newdata = cs.p_soybean_share_0C)))

cs.soybean.share0 <- sum( tobit.ey(predict(cs.p_soybean_share), cs.p_soybean_share$scale)*cs.p_soybean_share_0C$total_a, na.rm = TRUE)
cs.soybean.share1 <- (sum( tobit.ey(cs1C.pred_p_soybean_share, cs.p_soybean_share$scale)*cs.p_soybean_share_0C$total_a, na.rm = TRUE)/cs.soybean.share0 - 1)*100
cs.soybean.share2 <- (sum( tobit.ey(cs2C.pred_p_soybean_share, cs.p_soybean_share$scale)*cs.p_soybean_share_0C$total_a, na.rm = TRUE)/cs.soybean.share0 - 1)*100
cs.soybean.share3 <- (sum( tobit.ey(cs3C.pred_p_soybean_share, cs.p_soybean_share$scale)*cs.p_soybean_share_0C$total_a, na.rm = TRUE)/cs.soybean.share0 - 1)*100
cs.soybean.share4 <- (sum( tobit.ey(cs4C.pred_p_soybean_share, cs.p_soybean_share$scale)*cs.p_soybean_share_0C$total_a, na.rm = TRUE)/cs.soybean.share0 - 1)*100
cs.soybean.share5 <- (sum( tobit.ey(cs5C.pred_p_soybean_share, cs.p_soybean_share$scale)*cs.p_soybean_share_0C$total_a, na.rm = TRUE)/cs.soybean.share0 - 1)*100

p.soybean.share0 <- sum( tobit.ey(predict(p.p_soybean_share), p.p_soybean_share$scale)*p.p_soybean_share_0C$total_a)
p.soybean.share1 <- (sum( tobit.ey(p1C.pred_p_soybean_share, p.p_soybean_share$scale)*p.p_soybean_share_0C$total_a)/p.soybean.share0 - 1)*100
p.soybean.share2 <- (sum( tobit.ey(p2C.pred_p_soybean_share, p.p_soybean_share$scale)*p.p_soybean_share_0C$total_a)/p.soybean.share0 - 1)*100
p.soybean.share3 <- (sum( tobit.ey(p3C.pred_p_soybean_share, p.p_soybean_share$scale)*p.p_soybean_share_0C$total_a)/p.soybean.share0 - 1)*100
p.soybean.share4 <- (sum( tobit.ey(p4C.pred_p_soybean_share, p.p_soybean_share$scale)*p.p_soybean_share_0C$total_a)/p.soybean.share0 - 1)*100
p.soybean.share5 <- (sum( tobit.ey(p5C.pred_p_soybean_share, p.p_soybean_share$scale)*p.p_soybean_share_0C$total_a)/p.soybean.share0 - 1)*100

diff.soybean.share0 <- sum( tobit.ey(predict(diff.p_soybean_share), diff.p_soybean_share$scale)*diff.p_soybean_share_0C$total_a)
diff.soybean.share1 <- (sum( tobit.ey(diff1C.pred_p_soybean_share, diff.p_soybean_share$scale)*diff.p_soybean_share_0C$total_a)/diff.soybean.share0 - 1)*100
diff.soybean.share2 <- (sum( tobit.ey(diff2C.pred_p_soybean_share, diff.p_soybean_share$scale)*diff.p_soybean_share_0C$total_a)/diff.soybean.share0 - 1)*100
diff.soybean.share3 <- (sum( tobit.ey(diff3C.pred_p_soybean_share, diff.p_soybean_share$scale)*diff.p_soybean_share_0C$total_a)/diff.soybean.share0 - 1)*100
diff.soybean.share4 <- (sum( tobit.ey(diff4C.pred_p_soybean_share, diff.p_soybean_share$scale)*diff.p_soybean_share_0C$total_a)/diff.soybean.share0 - 1)*100
diff.soybean.share5 <- (sum( tobit.ey(diff5C.pred_p_soybean_share, diff.p_soybean_share$scale)*diff.p_soybean_share_0C$total_a)/diff.soybean.share0 - 1)*100

# Average predicted acreage
cs.soybean.share0_avg <- mean(tobit.ey(predict(cs.p_soybean_share), cs.p_soybean_share$scale)*cs.p_soybean_share_0C$total_a)
cs.soybean.share1_avg <- mean(tobit.ey(cs1C.pred_p_soybean_share, cs.p_soybean_share$scale)*cs.p_soybean_share_0C$total_a)
cs.soybean.share2_avg <- mean(tobit.ey(cs2C.pred_p_soybean_share, cs.p_soybean_share$scale)*cs.p_soybean_share_0C$total_a)
cs.soybean.share3_avg <- mean(tobit.ey(cs3C.pred_p_soybean_share, cs.p_soybean_share$scale)*cs.p_soybean_share_0C$total_a)
cs.soybean.share4_avg <- mean(tobit.ey(cs4C.pred_p_soybean_share, cs.p_soybean_share$scale)*cs.p_soybean_share_0C$total_a)
cs.soybean.share5_avg <- mean(tobit.ey(cs5C.pred_p_soybean_share, cs.p_soybean_share$scale)*cs.p_soybean_share_0C$total_a)

p.soybean.share0_avg <- mean(tobit.ey(predict(p.p_soybean_share), p.p_soybean_share$scale)*p.p_soybean_share_0C$total_a)
p.soybean.share1_avg <- mean(tobit.ey(p1C.pred_p_soybean_share, p.p_soybean_share$scale)*p.p_soybean_share_0C$total_a)
p.soybean.share2_avg <- mean(tobit.ey(p2C.pred_p_soybean_share, p.p_soybean_share$scale)*p.p_soybean_share_0C$total_a)
p.soybean.share3_avg <- mean(tobit.ey(p3C.pred_p_soybean_share, p.p_soybean_share$scale)*p.p_soybean_share_0C$total_a)
p.soybean.share4_avg <- mean(tobit.ey(p4C.pred_p_soybean_share, p.p_soybean_share$scale)*p.p_soybean_share_0C$total_a)
p.soybean.share5_avg <- mean(tobit.ey(p5C.pred_p_soybean_share, p.p_soybean_share$scale)*p.p_soybean_share_0C$total_a)

diff.soybean.share0_avg <- mean(tobit.ey(predict(diff.p_soybean_share), diff.p_soybean_share$scale)*diff.p_soybean_share_0C$total_a)
diff.soybean.share1_avg <- mean(tobit.ey(diff1C.pred_p_soybean_share, diff.p_soybean_share$scale)*diff.p_soybean_share_0C$total_a)
diff.soybean.share2_avg <- mean(tobit.ey(diff2C.pred_p_soybean_share, diff.p_soybean_share$scale)*diff.p_soybean_share_0C$total_a)
diff.soybean.share3_avg <- mean(tobit.ey(diff3C.pred_p_soybean_share, diff.p_soybean_share$scale)*diff.p_soybean_share_0C$total_a)
diff.soybean.share4_avg <- mean(tobit.ey(diff4C.pred_p_soybean_share, diff.p_soybean_share$scale)*diff.p_soybean_share_0C$total_a)
diff.soybean.share5_avg <- mean(tobit.ey(diff5C.pred_p_soybean_share, diff.p_soybean_share$scale)*diff.p_soybean_share_0C$total_a)

soybean.pred_acre <- data.frame(temp = rep(c(0, 1, 2, 3, 4, 5), 3),
                             acreage = c(cs.soybean.share0_avg, cs.soybean.share1_avg, cs.soybean.share2_avg, cs.soybean.share3_avg, cs.soybean.share4_avg, cs.soybean.share5_avg,
                             p.soybean.share0_avg, p.soybean.share1_avg, p.soybean.share2_avg, p.soybean.share3_avg, p.soybean.share4_avg, p.soybean.share5_avg,
                             diff.soybean.share0_avg, diff.soybean.share1_avg, diff.soybean.share2_avg, diff.soybean.share3_avg, diff.soybean.share4_avg, diff.soybean.share5_avg),
                             reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                             crop = "soybean")

ggplot(soybean.pred_acre, aes(temp, acreage, color = reg)) + geom_line()

soybean.plotdat <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           share = c(0,cs.soybean.share1, cs.soybean.share2, cs.soybean.share3, cs.soybean.share4, cs.soybean.share5,
                                   0,p.soybean.share1, p.soybean.share2, p.soybean.share3, p.soybean.share4, p.soybean.share5,
                                   0,diff.soybean.share1, diff.soybean.share2, diff.soybean.share3, diff.soybean.share4, diff.soybean.share5),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "soybean")

ggplot(soybean.plotdat, aes(temp, share, color = reg)) + geom_line()





# Merge data

pa <- rbind(corn.pred_acre, cotton.pred_acre, hay.pred_acre, wheat.pred_acre, soybean.pred_acre)
saveRDS(pa, "data/avg_predicted_acreage.rds")
pa$acreage <- round(pa$acreage, 2)

pa.plot <- ggplot(pa, aes(temp, acreage, color = reg)) + 
  geom_line(alpha=0.3, size=0.7) + ylab("Crop Acreage Impact (% Change) ") + 
  xlab(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
  facet_wrap(~crop) + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
  theme_tufte(base_size = 14) +
  theme(panel.grid = element_blank(),legend.position = "top",
        legend.title = element_blank()) +
  guides(colour=guide_legend(override.aes=list(alpha = 1, size=1))) +
  theme(legend.position = "top",
        legend.title = element_blank()) 
  #geom_text_repel(aes(temp, acreage, label = acreage), show.legend  = FALSE) 
pa.plot




plotdat <- rbind(corn.plotdat, cotton.plotdat, hay.plotdat, wheat.plotdat, soybean.plotdat)
plotdat$share <- round(plotdat$share)

p1 <- ggplot(plotdat, aes(temp, share, color = reg)) + 
  geom_line(alpha=0.3, size=0.7) + ylab("Crop Acreage Impact (% Change) ") + 
  xlab(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
  facet_wrap(~crop) + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
  theme_tufte(base_size = 14) +
  theme(panel.grid = element_blank(),legend.position = "top",
        legend.title = element_blank()) +
  guides(colour=guide_legend(override.aes=list(alpha = 1, size=1))) +
  theme(legend.position = "top",
        legend.title = element_blank()) + 
  geom_text_repel(aes(temp, share, label = share), show.legend  = FALSE) 
p1

p2 <- ggplot(filter(plotdat, crop %in% c("Wheat", "Cotton")), aes(temp, share, color = reg)) + 
  geom_line(alpha=0.3, size=0.7) + ylab("Crop Acreage Impact (% Change) ") + 
  xlab(NULL) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
  facet_wrap(~crop) + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("+1", "+2", "+3", "+4", "+5")) +
  theme_tufte(base_size = 14) +
  theme(panel.grid = element_blank(),legend.position = "top",
        legend.title = element_blank()) +
  guides(colour=guide_legend(override.aes=list(alpha = 1, size=1))) +
  theme(legend.position = "none",
        legend.title = element_blank()) + 
  geom_text_repel(aes(temp, share, label = rev), show.legend  = FALSE) + xlab("Change in Temperature (C)")  
p2

plot_grid(p1, p2, ncol = 1)
