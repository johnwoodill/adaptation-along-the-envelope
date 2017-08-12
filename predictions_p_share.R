library(tidyverse)
library(lfe)

source("predictFelm.R")

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

###################
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
cs.p_corn_share_1C <- filter(cs.1C, !is.na(ln_corn_rrev))
cs.p_corn_share_2C <- filter(cs.2C, !is.na(ln_corn_rrev))
cs.p_corn_share_3C <- filter(cs.3C, !is.na(ln_corn_rrev))
cs.p_corn_share_4C <- filter(cs.4C, !is.na(ln_corn_rrev))
cs.p_corn_share_5C <- filter(cs.5C, !is.na(ln_corn_rrev))

cs.p_cotton_share_1C <- filter(cs.1C, !is.na(ln_cotton_rrev))
cs.p_cotton_share_2C <- filter(cs.2C, !is.na(ln_cotton_rrev))
cs.p_cotton_share_3C <- filter(cs.3C, !is.na(ln_cotton_rrev))
cs.p_cotton_share_4C <- filter(cs.4C, !is.na(ln_cotton_rrev))
cs.p_cotton_share_5C <- filter(cs.5C, !is.na(ln_cotton_rrev))

cs.p_hay_share_1C <- filter(cs.1C, !is.na(ln_hay_rrev))
cs.p_hay_share_2C <- filter(cs.2C, !is.na(ln_hay_rrev))
cs.p_hay_share_3C <- filter(cs.3C, !is.na(ln_hay_rrev))
cs.p_hay_share_4C <- filter(cs.4C, !is.na(ln_hay_rrev))
cs.p_hay_share_5C <- filter(cs.5C, !is.na(ln_hay_rrev))

cs.p_wheat_share1C <- filter(cs.1C, !is.na(ln_wheat_rrev))
cs.p_wheat_share2C <- filter(cs.2C, !is.na(ln_wheat_rrev))
cs.p_wheat_share3C <- filter(cs.3C, !is.na(ln_wheat_rrev))
cs.p_wheat_share4C <- filter(cs.4C, !is.na(ln_wheat_rrev))
cs.p_wheat_share5C <- filter(cs.5C, !is.na(ln_wheat_rrev))

cs.p_soybean_share1C <- filter(cs.1C, !is.na(ln_soybean_rrev))
cs.p_soybean_share2C <- filter(cs.2C, !is.na(ln_soybean_rrev))
cs.p_soybean_share3C <- filter(cs.3C, !is.na(ln_soybean_rrev))
cs.p_soybean_share4C <- filter(cs.4C, !is.na(ln_soybean_rrev))
cs.p_soybean_share5C <- filter(cs.5C, !is.na(ln_soybean_rrev))

# Panel data
p.p_corn_share_1C <- filter(p.1C, !is.na(ln_corn_rrev))
p.p_corn_share_2C <- filter(p.2C, !is.na(ln_corn_rrev))
p.p_corn_share_3C <- filter(p.3C, !is.na(ln_corn_rrev))
p.p_corn_share_4C <- filter(p.4C, !is.na(ln_corn_rrev))
p.p_corn_share_5C <- filter(p.5C, !is.na(ln_corn_rrev))

p.p_cotton_share_1C <- filter(p.1C, !is.na(ln_cotton_rrev))
p.p_cotton_share_2C <- filter(p.2C, !is.na(ln_cotton_rrev))
p.p_cotton_share_3C <- filter(p.3C, !is.na(ln_cotton_rrev))
p.p_cotton_share_4C <- filter(p.4C, !is.na(ln_cotton_rrev))
p.p_cotton_share_5C <- filter(p.5C, !is.na(ln_cotton_rrev))

p.p_hay_share_1C <- filter(p.1C, !is.na(ln_hay_rrev))
p.p_hay_share_2C <- filter(p.2C, !is.na(ln_hay_rrev))
p.p_hay_share_3C <- filter(p.3C, !is.na(ln_hay_rrev))
p.p_hay_share_4C <- filter(p.4C, !is.na(ln_hay_rrev))
p.p_hay_share_5C <- filter(p.5C, !is.na(ln_hay_rrev))

p.p_wheat_share1C <- filter(p.1C, !is.na(ln_wheat_rrev))
p.p_wheat_share2C <- filter(p.2C, !is.na(ln_wheat_rrev))
p.p_wheat_share3C <- filter(p.3C, !is.na(ln_wheat_rrev))
p.p_wheat_share4C <- filter(p.4C, !is.na(ln_wheat_rrev))
p.p_wheat_share5C <- filter(p.5C, !is.na(ln_wheat_rrev))

p.p_soybean_share1C <- filter(p.1C, !is.na(ln_soybean_rrev))
p.p_soybean_share2C <- filter(p.2C, !is.na(ln_soybean_rrev))
p.p_soybean_share3C <- filter(p.3C, !is.na(ln_soybean_rrev))
p.p_soybean_share4C <- filter(p.4C, !is.na(ln_soybean_rrev))
p.p_soybean_share5C <- filter(p.5C, !is.na(ln_soybean_rrev))

# Diff data
diff.p_corn_share_1C <- filter(diff.1C, !is.na(ln_corn_rrev))
diff.p_corn_share_2C <- filter(diff.2C, !is.na(ln_corn_rrev))
diff.p_corn_share_3C <- filter(diff.3C, !is.na(ln_corn_rrev))
diff.p_corn_share_4C <- filter(diff.4C, !is.na(ln_corn_rrev))
diff.p_corn_share_5C <- filter(diff.5C, !is.na(ln_corn_rrev))

diff.p_cotton_share_1C <- filter(diff.1C, !is.na(ln_cotton_rrev))
diff.p_cotton_share_2C <- filter(diff.2C, !is.na(ln_cotton_rrev))
diff.p_cotton_share_3C <- filter(diff.3C, !is.na(ln_cotton_rrev))
diff.p_cotton_share_4C <- filter(diff.4C, !is.na(ln_cotton_rrev))
diff.p_cotton_share_5C <- filter(diff.5C, !is.na(ln_cotton_rrev))

diff.p_hay_share_1C <- filter(diff.1C, !is.na(ln_hay_rrev))
diff.p_hay_share_2C <- filter(diff.2C, !is.na(ln_hay_rrev))
diff.p_hay_share_3C <- filter(diff.3C, !is.na(ln_hay_rrev))
diff.p_hay_share_4C <- filter(diff.4C, !is.na(ln_hay_rrev))
diff.p_hay_share_5C <- filter(diff.5C, !is.na(ln_hay_rrev))

diff.p_wheat_share1C <- filter(diff.1C, !is.na(ln_wheat_rrev))
diff.p_wheat_share2C <- filter(diff.2C, !is.na(ln_wheat_rrev))
diff.p_wheat_share3C <- filter(diff.3C, !is.na(ln_wheat_rrev))
diff.p_wheat_share4C <- filter(diff.4C, !is.na(ln_wheat_rrev))
diff.p_wheat_share5C <- filter(diff.5C, !is.na(ln_wheat_rrev))

diff.p_soybean_share1C <- filter(diff.1C, !is.na(ln_soybean_rrev))
diff.p_soybean_share2C <- filter(diff.2C, !is.na(ln_soybean_rrev))
diff.p_soybean_share3C <- filter(diff.3C, !is.na(ln_soybean_rrev))
diff.p_soybean_share4C <- filter(diff.4C, !is.na(ln_soybean_rrev))
diff.p_soybean_share5C <- filter(diff.5C, !is.na(ln_soybean_rrev))


###########################
# Predictions

# Corn

#pred <- predict(cs.p_corn_share, cs.p_corn_share_1C, se.fit = TRUE)

{
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

diff1C.pred_p_corn_share <- predict(diff.p_corn_share, newdata = diff.p_corn_share_1C, se.fit = TRUE)
diff2C.pred_p_corn_share <- predict(diff.p_corn_share, newdata = diff.p_corn_share_2C, se.fit = TRUE)
diff3C.pred_p_corn_share <- predict(diff.p_corn_share, newdata = diff.p_corn_share_3C, se.fit = TRUE)
diff4C.pred_p_corn_share <- predict(diff.p_corn_share, newdata = diff.p_corn_share_4C, se.fit = TRUE)
diff5C.pred_p_corn_share <- predict(diff.p_corn_share, newdata = diff.p_corn_share_5C, se.fit = TRUE)


cs.rev1C <- cs1C.pred_p_corn_share
cs.rev2C <- cs2C.pred_p_corn_share
cs.rev3C <- cs3C.pred_p_corn_share
cs.rev4C <- cs4C.pred_p_corn_share
cs.rev5C <- cs5C.pred_p_corn_share

p.rev1C <- p1C.pred_p_corn_share
p.rev2C <- p2C.pred_p_corn_share
p.rev3C <- p3C.pred_p_corn_share
p.rev4C <- p4C.pred_p_corn_share
p.rev5C <- p5C.pred_p_corn_share

diff.rev1C <- diff1C.pred_p_corn_share$fit
diff.rev2C <- diff2C.pred_p_corn_share$fit
diff.rev3C <- diff3C.pred_p_corn_share$fit
diff.rev4C <- diff4C.pred_p_corn_share$fit
diff.rev5C <- diff5C.pred_p_corn_share$fit

cs.rev <- data.frame(rev = c(predict(cs.p_corn_share), cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
                     change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))

p.rev <- data.frame(rev = c(predict(p.p_corn_share), p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
                     change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))

diff.rev <- data.frame(rev = c(predict(diff.p_corn_share), diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
                     change = rep(c("base", "1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))


ggplot(cs.rev, aes(rev, fill = change)) + geom_density() + scale_fill_brewer(palette = "OrRd")
ggplot(filter(p.rev, rev < 50), aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
ggplot(diff.rev, aes(rev, fill = change)) + geom_density() + scale_fill_brewer(palette = "OrRd")


cs.corn.rev0 <- sum( predict(cs.p_corn_share))
cs.corn.rev1 <- (sum( cs.rev1C, na.rm = TRUE)/abs(cs.corn.rev0) - 1)*100
cs.corn.rev2 <- (sum( cs.rev2C, na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev3 <- (sum( cs.rev3C, na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev4 <- (sum( cs.rev4C, na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev5 <- (sum( cs.rev5C, na.rm = TRUE)/cs.corn.rev0 - 1)*100

p.corn.rev0 <- sum( predict(p.p_corn_share))
p.corn.rev1 <- (sum( p.rev1C, na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev2 <- (sum( p.rev2C, na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev3 <- (sum( p.rev3C, na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev4 <- (sum( p.rev4C, na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev5 <- (sum( p.rev5C, na.rm = TRUE)/p.corn.rev0 - 1)*100

diff.corn.rev0 <- sum( predict(diff.p_corn_share))
diff.corn.rev1 <- (sum( diff.rev1C, na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev2 <- (sum( diff.rev2C, na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev3 <- (sum( diff.rev3C, na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev4 <- (sum( diff.rev4C, na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev5 <- (sum( diff.rev5C, na.rm = TRUE)/diff.corn.rev0 - 1)*100

corn.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.corn.rev1, cs.corn.rev2, cs.corn.rev3, cs.corn.rev4, cs.corn.rev5,
                                   p.corn.rev1, p.corn.rev2, p.corn.rev3, p.corn.rev4, p.corn.rev5,
                                   diff.corn.rev1, diff.corn.rev2, diff.corn.rev3, diff.corn.rev4, diff.corn.rev5),
                           reg = rep(c("cross-section", "panel", "diff"), each = 5),
                           crop = "corn")

ggplot(corn.plotdat, aes(temp, rev, color = reg)) + geom_line()
}


# Merge data
plotdat <- rbind(corn.plotdat, cotton.plotdat, hay.plotdat, wheat.plotdat, soybean.plotdat)

ggplot(plotdat, aes(temp, rev, color = reg)) + geom_line() + ylab("Impact (% Change) ") + xlab("Change in Temperature (C)") + facet_wrap(~crop)


