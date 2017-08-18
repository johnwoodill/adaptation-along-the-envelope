library(tidyverse)
library(lfe)

source("predictFelm.R")

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

###################
# Baseline data
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
cs.ln_corn_rrev <- readRDS("models/cs.dd.ln_corn_rrev")
p.ln_corn_rrev <- readRDS("models/p.dd.ln_corn_rrev")
diff.ln_corn_rrev <- readRDS("models/diff.dd.ln_corn_rrev")

cs.ln_wheat_rrev <- readRDS("models/cs.dd.ln_wheat_rrev")
p.ln_wheat_rrev <- readRDS("models/p.dd.ln_wheat_rrev")
diff.ln_wheat_rrev <- readRDS("models/diff.dd.ln_wheat_rrev")

cs.ln_hay_rrev <- readRDS("models/cs.dd.ln_hay_rrev")
p.ln_hay_rrev <- readRDS("models/p.dd.ln_hay_rrev")
diff.ln_hay_rrev <- readRDS("models/diff.dd.ln_hay_rrev")

cs.ln_wheat_rrev <- readRDS("models/cs.dd.ln_wheat_rrev")
p.ln_wheat_rrev <- readRDS("models/p.dd.ln_wheat_rrev")
diff.ln_wheat_rrev <- readRDS("models/diff.dd.ln_wheat_rrev")

cs.ln_soybean_rrev <- readRDS("models/cs.dd.ln_soybean_rrev")
p.ln_soybean_rrev <- readRDS("models/p.dd.ln_soybean_rrev")
diff.ln_soybean_rrev <- readRDS("models/diff.dd.ln_soybean_rrev")

########################
# Cross section data

cs.ln_corn_rrev_0C <- filter(cs.dat, !is.na(ln_corn_rrev))
cs.ln_corn_rrev_1C <- filter(cs.1C, !is.na(ln_corn_rrev))
cs.ln_corn_rrev_2C <- filter(cs.2C, !is.na(ln_corn_rrev))
cs.ln_corn_rrev_3C <- filter(cs.3C, !is.na(ln_corn_rrev))
cs.ln_corn_rrev_4C <- filter(cs.4C, !is.na(ln_corn_rrev))
cs.ln_corn_rrev_5C <- filter(cs.5C, !is.na(ln_corn_rrev))

cs.ln_cotton_rrev_0C <- filter(cs.dat, !is.na(ln_cotton_rrev))
cs.ln_cotton_rrev_1C <- filter(cs.1C, !is.na(ln_cotton_rrev))
cs.ln_cotton_rrev_2C <- filter(cs.2C, !is.na(ln_cotton_rrev))
cs.ln_cotton_rrev_3C <- filter(cs.3C, !is.na(ln_cotton_rrev))
cs.ln_cotton_rrev_4C <- filter(cs.4C, !is.na(ln_cotton_rrev))
cs.ln_cotton_rrev_5C <- filter(cs.5C, !is.na(ln_cotton_rrev))

cs.ln_hay_rrev_0C <- filter(cs.dat, !is.na(ln_hay_rrev))
cs.ln_hay_rrev_1C <- filter(cs.1C, !is.na(ln_hay_rrev))
cs.ln_hay_rrev_2C <- filter(cs.2C, !is.na(ln_hay_rrev))
cs.ln_hay_rrev_3C <- filter(cs.3C, !is.na(ln_hay_rrev))
cs.ln_hay_rrev_4C <- filter(cs.4C, !is.na(ln_hay_rrev))
cs.ln_hay_rrev_5C <- filter(cs.5C, !is.na(ln_hay_rrev))

cs.ln_wheat_rrev_0C <- filter(cs.dat, !is.na(ln_wheat_rrev))
cs.ln_wheat_rrev_1C <- filter(cs.1C, !is.na(ln_wheat_rrev))
cs.ln_wheat_rrev_2C <- filter(cs.2C, !is.na(ln_wheat_rrev))
cs.ln_wheat_rrev_3C <- filter(cs.3C, !is.na(ln_wheat_rrev))
cs.ln_wheat_rrev_4C <- filter(cs.4C, !is.na(ln_wheat_rrev))
cs.ln_wheat_rrev_5C <- filter(cs.5C, !is.na(ln_wheat_rrev))

cs.ln_soybean_rrev_0C <- filter(cs.dat, !is.na(ln_soybean_rrev))
cs.ln_soybean_rrev_1C <- filter(cs.1C, !is.na(ln_soybean_rrev))
cs.ln_soybean_rrev_2C <- filter(cs.2C, !is.na(ln_soybean_rrev))
cs.ln_soybean_rrev_3C <- filter(cs.3C, !is.na(ln_soybean_rrev))
cs.ln_soybean_rrev_4C <- filter(cs.4C, !is.na(ln_soybean_rrev))
cs.ln_soybean_rrev_5C <- filter(cs.5C, !is.na(ln_soybean_rrev))

# Panel data

p.ln_corn_rrev_0C <- filter(p.dat, !is.na(ln_corn_rrev))
p.ln_corn_rrev_1C <- filter(p.1C, !is.na(ln_corn_rrev))
p.ln_corn_rrev_2C <- filter(p.2C, !is.na(ln_corn_rrev))
p.ln_corn_rrev_3C <- filter(p.3C, !is.na(ln_corn_rrev))
p.ln_corn_rrev_4C <- filter(p.4C, !is.na(ln_corn_rrev))
p.ln_corn_rrev_5C <- filter(p.5C, !is.na(ln_corn_rrev))

p.ln_cotton_rrev_0C <- filter(p.dat, !is.na(ln_cotton_rrev))
p.ln_cotton_rrev_1C <- filter(p.1C, !is.na(ln_cotton_rrev))
p.ln_cotton_rrev_2C <- filter(p.2C, !is.na(ln_cotton_rrev))
p.ln_cotton_rrev_3C <- filter(p.3C, !is.na(ln_cotton_rrev))
p.ln_cotton_rrev_4C <- filter(p.4C, !is.na(ln_cotton_rrev))
p.ln_cotton_rrev_5C <- filter(p.5C, !is.na(ln_cotton_rrev))

p.ln_hay_rrev_0C <- filter(p.dat, !is.na(ln_hay_rrev))
p.ln_hay_rrev_1C <- filter(p.1C, !is.na(ln_hay_rrev))
p.ln_hay_rrev_2C <- filter(p.2C, !is.na(ln_hay_rrev))
p.ln_hay_rrev_3C <- filter(p.3C, !is.na(ln_hay_rrev))
p.ln_hay_rrev_4C <- filter(p.4C, !is.na(ln_hay_rrev))
p.ln_hay_rrev_5C <- filter(p.5C, !is.na(ln_hay_rrev))

p.ln_wheat_rrev_0C <- filter(p.dat, !is.na(ln_wheat_rrev))
p.ln_wheat_rrev_1C <- filter(p.1C, !is.na(ln_wheat_rrev))
p.ln_wheat_rrev_2C <- filter(p.2C, !is.na(ln_wheat_rrev))
p.ln_wheat_rrev_3C <- filter(p.3C, !is.na(ln_wheat_rrev))
p.ln_wheat_rrev_4C <- filter(p.4C, !is.na(ln_wheat_rrev))
p.ln_wheat_rrev_5C <- filter(p.5C, !is.na(ln_wheat_rrev))

p.ln_soybean_rrev_0C <- filter(p.dat, !is.na(ln_soybean_rrev))
p.ln_soybean_rrev_1C <- filter(p.1C, !is.na(ln_soybean_rrev))
p.ln_soybean_rrev_2C <- filter(p.2C, !is.na(ln_soybean_rrev))
p.ln_soybean_rrev_3C <- filter(p.3C, !is.na(ln_soybean_rrev))
p.ln_soybean_rrev_4C <- filter(p.4C, !is.na(ln_soybean_rrev))
p.ln_soybean_rrev_5C <- filter(p.5C, !is.na(ln_soybean_rrev))

# Diff data
diff.ln_corn_rrev_0C <- filter(diff.dat, !is.na(ln_corn_rrev))
diff.ln_corn_rrev_1C <- filter(diff.1C, !is.na(ln_corn_rrev))
diff.ln_corn_rrev_2C <- filter(diff.2C, !is.na(ln_corn_rrev))
diff.ln_corn_rrev_3C <- filter(diff.3C, !is.na(ln_corn_rrev))
diff.ln_corn_rrev_4C <- filter(diff.4C, !is.na(ln_corn_rrev))
diff.ln_corn_rrev_5C <- filter(diff.5C, !is.na(ln_corn_rrev))

diff.ln_cotton_rrev_0C <- filter(diff.dat, !is.na(ln_cotton_rrev))
diff.ln_cotton_rrev_1C <- filter(diff.1C, !is.na(ln_cotton_rrev))
diff.ln_cotton_rrev_2C <- filter(diff.2C, !is.na(ln_cotton_rrev))
diff.ln_cotton_rrev_3C <- filter(diff.3C, !is.na(ln_cotton_rrev))
diff.ln_cotton_rrev_4C <- filter(diff.4C, !is.na(ln_cotton_rrev))
diff.ln_cotton_rrev_5C <- filter(diff.5C, !is.na(ln_cotton_rrev))

diff.ln_hay_rrev_0C <- filter(diff.dat, !is.na(ln_hay_rrev))
diff.ln_hay_rrev_1C <- filter(diff.1C, !is.na(ln_hay_rrev))
diff.ln_hay_rrev_2C <- filter(diff.2C, !is.na(ln_hay_rrev))
diff.ln_hay_rrev_3C <- filter(diff.3C, !is.na(ln_hay_rrev))
diff.ln_hay_rrev_4C <- filter(diff.4C, !is.na(ln_hay_rrev))
diff.ln_hay_rrev_5C <- filter(diff.5C, !is.na(ln_hay_rrev))

diff.ln_wheat_rrev_0C <- filter(diff.dat, !is.na(ln_wheat_rrev))
diff.ln_wheat_rrev_1C <- filter(diff.1C, !is.na(ln_wheat_rrev))
diff.ln_wheat_rrev_2C <- filter(diff.2C, !is.na(ln_wheat_rrev))
diff.ln_wheat_rrev_3C <- filter(diff.3C, !is.na(ln_wheat_rrev))
diff.ln_wheat_rrev_4C <- filter(diff.4C, !is.na(ln_wheat_rrev))
diff.ln_wheat_rrev_5C <- filter(diff.5C, !is.na(ln_wheat_rrev))

diff.ln_soybean_rrev_0C <- filter(diff.dat, !is.na(ln_soybean_rrev))
diff.ln_soybean_rrev_1C <- filter(diff.1C, !is.na(ln_soybean_rrev))
diff.ln_soybean_rrev_2C <- filter(diff.2C, !is.na(ln_soybean_rrev))
diff.ln_soybean_rrev_3C <- filter(diff.3C, !is.na(ln_soybean_rrev))
diff.ln_soybean_rrev_4C <- filter(diff.4C, !is.na(ln_soybean_rrev))
diff.ln_soybean_rrev_5C <- filter(diff.5C, !is.na(ln_soybean_rrev))


###########################
# Predictions

# Corn

cs0C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_0C)
cs0C.pred_ln_corn_rrev_se <- cc.pred.se(cs.ln_corn_rrev$X - cs.ln_corn_rrev_0C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_corn_rrev$clustervcv, cs.ln_corn_rrev$weights^2)

cs1C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_1C)
cs1C.pred_ln_corn_rrev_se <- cc.pred.se(cs.ln_corn_rrev$X - cs.ln_corn_rrev_1C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
           cs.ln_corn_rrev$clustervcv, cs.ln_corn_rrev$weights^2)
cs2C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_2C)
cs2C.pred_ln_corn_rrev_se <- cc.pred.se(cs.ln_corn_rrev$X - cs.ln_corn_rrev_2C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
           cs.ln_corn_rrev$clustervcv, cs.ln_corn_rrev$weights^2)
cs3C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_3C)
cs3C.pred_ln_corn_rrev_se <- cc.pred.se(cs.ln_corn_rrev$X - cs.ln_corn_rrev_3C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
           cs.ln_corn_rrev$clustervcv, cs.ln_corn_rrev$weights^2)
cs4C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_4C)
cs4C.pred_ln_corn_rrev_se <- cc.pred.se(cs.ln_corn_rrev$X - cs.ln_corn_rrev_4C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
           cs.ln_corn_rrev$clustervcv, cs.ln_corn_rrev$weights^2)
cs5C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_5C)
cs5C.pred_ln_corn_rrev_se <- cc.pred.se(cs.ln_corn_rrev$X - cs.ln_corn_rrev_5C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
           cs.ln_corn_rrev$clustervcv, cs.ln_corn_rrev$weights^2)

p0C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_0C)
p0C.pred_ln_corn_rrev_se <- cc.pred.se(p.ln_corn_rrev$X - p.ln_corn_rrev_0C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                        p.ln_corn_rrev$clustervcv, p.ln_corn_rrev$weights^2)

p1C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_1C)
p1C.pred_ln_corn_rrev_se <- cc.pred.se(p.ln_corn_rrev$X - p.ln_corn_rrev_1C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                        p.ln_corn_rrev$clustervcv, p.ln_corn_rrev$weights^2)
p2C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_2C)
p2C.pred_ln_corn_rrev_se <- cc.pred.se(p.ln_corn_rrev$X - p.ln_corn_rrev_2C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                        p.ln_corn_rrev$clustervcv, p.ln_corn_rrev$weights^2)
p3C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_3C)
p3C.pred_ln_corn_rrev_se <- cc.pred.se(p.ln_corn_rrev$X - p.ln_corn_rrev_3C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                        p.ln_corn_rrev$clustervcv, p.ln_corn_rrev$weights^2)
p4C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_4C)
p4C.pred_ln_corn_rrev_se <- cc.pred.se(p.ln_corn_rrev$X - p.ln_corn_rrev_4C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                        p.ln_corn_rrev$clustervcv, p.ln_corn_rrev$weights^2)
p5C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_5C)
p5C.pred_ln_corn_rrev_se <- cc.pred.se(p.ln_corn_rrev$X - p.ln_corn_rrev_5C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                        p.ln_corn_rrev$clustervcv, p.ln_corn_rrev$weights^2)

diff0C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_0C)
diff1C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_1C)
diff1C.pred_ln_corn_rrev_se <- cc.pred.se(diff.ln_corn_rrev$X - diff.ln_corn_rrev_1C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           diff.ln_corn_rrev$clustervcv, diff.ln_corn_rrev$weights^2)
diff2C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_2C)
diff2C.pred_ln_corn_rrev_se <- cc.pred.se(diff.ln_corn_rrev$X - diff.ln_corn_rrev_2C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           diff.ln_corn_rrev$clustervcv, diff.ln_corn_rrev$weights^2)
diff3C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_3C)
diff3C.pred_ln_corn_rrev_se <- cc.pred.se(diff.ln_corn_rrev$X - diff.ln_corn_rrev_3C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           diff.ln_corn_rrev$clustervcv, diff.ln_corn_rrev$weights^2)
diff4C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_4C)
diff4C.pred_ln_corn_rrev_se <- cc.pred.se(diff.ln_corn_rrev$X - diff.ln_corn_rrev_4C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           diff.ln_corn_rrev$clustervcv, diff.ln_corn_rrev$weights^2)
diff5C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_5C)
diff5C.pred_ln_corn_rrev_se <- cc.pred.se(diff.ln_corn_rrev$X - diff.ln_corn_rrev_5C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           diff.ln_corn_rrev$clustervcv, diff.ln_corn_rrev$weights^2)

mean(residuals(cs.ln_corn_rrev))
mean(residuals(cs0C.pred_ln_corn_rrev))


cs.rev0C <- exp(cs0C.pred_ln_corn_rrev$fit + residuals(cs.ln_corn_rrev))
cs.rev1C <- exp(cs1C.pred_ln_corn_rrev$fit + residuals(cs.ln_corn_rrev))
cs.rev2C <- exp(cs2C.pred_ln_corn_rrev$fit + residuals(cs.ln_corn_rrev))
cs.rev3C <- exp(cs3C.pred_ln_corn_rrev$fit + residuals(cs.ln_corn_rrev))
cs.rev4C <- exp(cs4C.pred_ln_corn_rrev$fit + residuals(cs.ln_corn_rrev))
cs.rev5C <- exp(cs5C.pred_ln_corn_rrev$fit + residuals(cs.ln_corn_rrev))

p.rev0C <- exp(p0C.pred_ln_corn_rrev$fit + residuals(p.ln_corn_rrev) + p0C.pred_ln_corn_rrev$effect)
p.rev1C <- exp(p1C.pred_ln_corn_rrev$fit + residuals(p.ln_corn_rrev) + p1C.pred_ln_corn_rrev$effect)
p.rev2C <- exp(p2C.pred_ln_corn_rrev$fit + residuals(p.ln_corn_rrev) + p2C.pred_ln_corn_rrev$effect)
p.rev3C <- exp(p3C.pred_ln_corn_rrev$fit + residuals(p.ln_corn_rrev) + p3C.pred_ln_corn_rrev$effect)
p.rev4C <- exp(p4C.pred_ln_corn_rrev$fit + residuals(p.ln_corn_rrev) + p4C.pred_ln_corn_rrev$effect)
p.rev5C <- exp(p5C.pred_ln_corn_rrev$fit + residuals(p.ln_corn_rrev) + p5C.pred_ln_corn_rrev$effect)


mean(residuals(diff.ln_corn_rrev))
diff.rev0C <- exp(diff0C.pred_ln_corn_rrev$fit + residuals(diff.ln_corn_rrev) + diff0C.pred_ln_corn_rrev$effect)
diff.rev1C <- exp(diff1C.pred_ln_corn_rrev$fit + residuals(diff.ln_corn_rrev) + diff1C.pred_ln_corn_rrev$effect)
diff.rev2C <- exp(diff2C.pred_ln_corn_rrev$fit + residuals(diff.ln_corn_rrev) + diff2C.pred_ln_corn_rrev$effect)
diff.rev3C <- exp(diff3C.pred_ln_corn_rrev$fit + residuals(diff.ln_corn_rrev) + diff3C.pred_ln_corn_rrev$effect)
diff.rev4C <- exp(diff4C.pred_ln_corn_rrev$fit + residuals(diff.ln_corn_rrev) + diff4C.pred_ln_corn_rrev$effect)
diff.rev5C <- exp(diff5C.pred_ln_corn_rrev$fit + residuals(diff.ln_corn_rrev) + diff5C.pred_ln_corn_rrev$effect)

cs.rev <- data.frame(rev = c(cs.rev0C, cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
                     change = rep(c("Base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))

p.rev <- data.frame(rev = c(p.rev0C, p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
                     change = rep(c("Base", "1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))

diff.rev <- data.frame(rev = c(diff.rev0C, diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
                     change = rep(c("Base", "1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))


ggplot(cs.rev, aes(rev, fill = change)) + geom_density() + scale_fill_brewer(palette = "OrRd")
ggplot(filter(p.rev, rev < 20), aes(rev, fill = change)) + geom_histogram(bins = 50) + scale_fill_brewer(palette = "OrRd")
ggplot(diff.rev, aes(rev, fill = change)) + geom_histogram(bins = 50) + scale_fill_brewer(palette = "OrRd")



cs.corn.rev0 <- (sum( exp(cs0C.pred_ln_corn_rrev$fit + cs0C.pred_ln_corn_rrev$res)))
cs.corn.rev1 <- (sum( exp(cs1C.pred_ln_corn_rrev$fit + cs1C.pred_ln_corn_rrev$res  ))/cs.corn.rev0 - 1)*100
cs.corn.rev1.min <- (sum( exp(cs1C.pred_ln_corn_rrev$fit - c(cs1C.pred_ln_corn_rrev_se*1.96) + cs1C.pred_ln_corn_rrev$res ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev1.max <- (sum( exp(cs1C.pred_ln_corn_rrev$fit + c(cs1C.pred_ln_corn_rrev_se*1.96) + cs1C.pred_ln_corn_rrev$res  ), na.rm = TRUE)/cs.corn.rev0 - 1)*100

cs.corn.rev2 <- (sum( exp(cs2C.pred_ln_corn_rrev$fit + cs2C.pred_ln_corn_rrev$res  ))/cs.corn.rev0 - 1)*100
cs.corn.rev2.min <- (sum( exp(cs2C.pred_ln_corn_rrev$fit - c(cs2C.pred_ln_corn_rrev_se*1.96) + cs2C.pred_ln_corn_rrev$res  ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev2.max <- (sum( exp(cs2C.pred_ln_corn_rrev$fit + c(cs2C.pred_ln_corn_rrev_se*1.96) + cs2C.pred_ln_corn_rrev$res  ), na.rm = TRUE)/cs.corn.rev0 - 1)*100

cs.corn.rev3 <- (sum( exp(cs3C.pred_ln_corn_rrev$fit + cs3C.pred_ln_corn_rrev$res  ))/cs.corn.rev0 - 1)*100
cs.corn.rev3.min <- (sum( exp(cs3C.pred_ln_corn_rrev$fit - c(cs3C.pred_ln_corn_rrev_se*1.96) + cs3C.pred_ln_corn_rrev$res  ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev3.max <- (sum( exp(cs3C.pred_ln_corn_rrev$fit + c(cs3C.pred_ln_corn_rrev_se*1.96) + cs3C.pred_ln_corn_rrev$res  ), na.rm = TRUE)/cs.corn.rev0 - 1)*100

cs.corn.rev4 <- (sum( exp(cs4C.pred_ln_corn_rrev$fit + cs4C.pred_ln_corn_rrev$res  ))/cs.corn.rev0 - 1)*100
cs.corn.rev4.min <- (sum( exp(cs4C.pred_ln_corn_rrev$fit - c(cs4C.pred_ln_corn_rrev_se*1.96) + cs4C.pred_ln_corn_rrev$res  ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev4.max <- (sum( exp(cs4C.pred_ln_corn_rrev$fit + c(cs4C.pred_ln_corn_rrev_se*1.96) + cs4C.pred_ln_corn_rrev$res  ), na.rm = TRUE)/cs.corn.rev0 - 1)*100

cs.corn.rev5 <- (sum( exp(cs5C.pred_ln_corn_rrev$fit + cs5C.pred_ln_corn_rrev$res  ))/cs.corn.rev0 - 1)*100
cs.corn.rev5.min <- (sum( exp(cs5C.pred_ln_corn_rrev$fit - c(cs5C.pred_ln_corn_rrev_se*1.96) + cs5C.pred_ln_corn_rrev$res  ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev5.max <- (sum( exp(cs5C.pred_ln_corn_rrev$fit + c(cs5C.pred_ln_corn_rrev_se*1.96) + cs5C.pred_ln_corn_rrev$res  ), na.rm = TRUE)/cs.corn.rev0 - 1)*100


p.corn.rev0 <- (sum( exp(p0C.pred_ln_corn_rrev$fit + p0C.pred_ln_corn_rrev$res + p0C.pred_ln_corn_rrev$effect)))
p.corn.rev1 <- (sum( exp(p1C.pred_ln_corn_rrev$fit + p1C.pred_ln_corn_rrev$res + p1C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev1.min <- (sum( exp(p1C.pred_ln_corn_rrev$fit - c(p1C.pred_ln_corn_rrev_se*1.96) + p1C.pred_ln_corn_rrev$res + p1C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev1.max <- (sum( exp(p1C.pred_ln_corn_rrev$fit + c(p1C.pred_ln_corn_rrev_se*1.96) + p1C.pred_ln_corn_rrev$res + p1C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100

p.corn.rev2 <- (sum( exp(p2C.pred_ln_corn_rrev$fit + p2C.pred_ln_corn_rrev$res + p2C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev2.min <- (sum( exp(p2C.pred_ln_corn_rrev$fit - c(p2C.pred_ln_corn_rrev_se*1.96) + p2C.pred_ln_corn_rrev$res + p2C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev2.max <- (sum( exp(p2C.pred_ln_corn_rrev$fit + c(p2C.pred_ln_corn_rrev_se*1.96) + p2C.pred_ln_corn_rrev$res + p2C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100

p.corn.rev3 <- (sum( exp(p3C.pred_ln_corn_rrev$fit + p3C.pred_ln_corn_rrev$res + p3C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev3.min <- (sum( exp(p3C.pred_ln_corn_rrev$fit - c(p3C.pred_ln_corn_rrev_se*1.96) + p3C.pred_ln_corn_rrev$res + p3C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev3.max <- (sum( exp(p3C.pred_ln_corn_rrev$fit + c(p3C.pred_ln_corn_rrev_se*1.96) + p3C.pred_ln_corn_rrev$res + p3C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100

p.corn.rev4 <- (sum( exp(p4C.pred_ln_corn_rrev$fit + p4C.pred_ln_corn_rrev$res + p4C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev4.min <- (sum( exp(p4C.pred_ln_corn_rrev$fit - c(p4C.pred_ln_corn_rrev_se*1.96) + p4C.pred_ln_corn_rrev$res + p4C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev4.max <- (sum( exp(p4C.pred_ln_corn_rrev$fit + c(p4C.pred_ln_corn_rrev_se*1.96) + p4C.pred_ln_corn_rrev$res + p4C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100

p.corn.rev5 <- (sum( exp(p5C.pred_ln_corn_rrev$fit + p5C.pred_ln_corn_rrev$res + p5C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev5.min <- (sum( exp(p5C.pred_ln_corn_rrev$fit - c(p5C.pred_ln_corn_rrev_se*1.96) + p5C.pred_ln_corn_rrev$res + p5C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev5.max <- (sum( exp(p5C.pred_ln_corn_rrev$fit + c(p5C.pred_ln_corn_rrev_se*1.96) + p5C.pred_ln_corn_rrev$res + p5C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100

diff.corn.rev0 <- (sum( exp(diff0C.pred_ln_corn_rrev$fit + diff0C.pred_ln_corn_rrev$res + diff1C.pred_ln_corn_rrev$effect )))
diff.corn.rev1 <- (sum( exp(diff1C.pred_ln_corn_rrev$fit + diff1C.pred_ln_corn_rrev$res + diff1C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev1.min <- (sum( exp(diff1C.pred_ln_corn_rrev$fit - c(diff1C.pred_ln_corn_rrev_se*1.96) + diff1C.pred_ln_corn_rrev$res + diff1C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev1.max <- (sum( exp(diff1C.pred_ln_corn_rrev$fit + c(diff1C.pred_ln_corn_rrev_se*1.96) + diff1C.pred_ln_corn_rrev$res + diff1C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100

diff.corn.rev2 <- (sum( exp(diff2C.pred_ln_corn_rrev$fit + diff2C.pred_ln_corn_rrev$res + diff2C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev2.min <- (sum( exp(diff2C.pred_ln_corn_rrev$fit - c(diff2C.pred_ln_corn_rrev_se*1.96) + diff2C.pred_ln_corn_rrev$res + diff2C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev2.max <- (sum( exp(diff2C.pred_ln_corn_rrev$fit + c(diff2C.pred_ln_corn_rrev_se*1.96) + diff2C.pred_ln_corn_rrev$res + diff2C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100

diff.corn.rev3 <- (sum( exp(diff3C.pred_ln_corn_rrev$fit + diff3C.pred_ln_corn_rrev$res + diff3C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev3.min <- (sum( exp(diff3C.pred_ln_corn_rrev$fit - c(diff3C.pred_ln_corn_rrev_se*1.96) + diff3C.pred_ln_corn_rrev$res + diff3C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev3.max <- (sum( exp(diff3C.pred_ln_corn_rrev$fit + c(diff3C.pred_ln_corn_rrev_se*1.96) + diff3C.pred_ln_corn_rrev$res + diff3C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100

diff.corn.rev4 <- (sum( exp(diff4C.pred_ln_corn_rrev$fit + diff4C.pred_ln_corn_rrev$res + diff4C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev4.min <- (sum( exp(diff4C.pred_ln_corn_rrev$fit - c(diff4C.pred_ln_corn_rrev_se*1.96) + diff4C.pred_ln_corn_rrev$res + diff4C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev4.max <- (sum( exp(diff4C.pred_ln_corn_rrev$fit + c(diff4C.pred_ln_corn_rrev_se*1.96) + diff4C.pred_ln_corn_rrev$res + diff4C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100

diff.corn.rev5 <- (sum( exp(diff5C.pred_ln_corn_rrev$fit + diff5C.pred_ln_corn_rrev$res + diff5C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev5.min <- (sum( exp(diff5C.pred_ln_corn_rrev$fit - c(diff5C.pred_ln_corn_rrev_se*1.96) + diff5C.pred_ln_corn_rrev$res + diff5C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev5.max <- (sum( exp(diff5C.pred_ln_corn_rrev$fit + c(diff5C.pred_ln_corn_rrev_se*1.96) + diff5C.pred_ln_corn_rrev$res + diff5C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100

corn.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.corn.rev1, cs.corn.rev2, cs.corn.rev3, cs.corn.rev4, cs.corn.rev5,
                                   p.corn.rev1, p.corn.rev2, p.corn.rev3, p.corn.rev4, p.corn.rev5,
                                   diff.corn.rev1, diff.corn.rev2, diff.corn.rev3, diff.corn.rev4, diff.corn.rev5),
                           min = c(cs.corn.rev1.min, cs.corn.rev2.min, cs.corn.rev3.min, cs.corn.rev4.min, cs.corn.rev5.min,
                                   p.corn.rev1.min, p.corn.rev2.min, p.corn.rev3.min, p.corn.rev4.min, p.corn.rev5.min,
                                   diff.corn.rev1.min, diff.corn.rev2.min, diff.corn.rev3.min, diff.corn.rev4.min, diff.corn.rev5.min),
                           max = c(cs.corn.rev1.max, cs.corn.rev2.max, cs.corn.rev3.max, cs.corn.rev4.max, cs.corn.rev5.max,
                                   p.corn.rev1.max, p.corn.rev2.max, p.corn.rev3.max, p.corn.rev4.max, p.corn.rev5.max,
                                   diff.corn.rev1.max, diff.corn.rev2.max, diff.corn.rev3.max, diff.corn.rev4.max, diff.corn.rev5.max),
                           reg = rep(c("cross-section", "panel", "diff"), each = 5),
                           crop = "corn")

ggplot(corn.plotdat, aes(temp, rev, group = reg)) + 
  #geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5", alpha = 0.5) + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")

# cotton

cs0C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_0C)
cs0C.pred_ln_cotton_rrev_se <- cc.pred.se(cs.ln_cotton_rrev$X - cs.ln_cotton_rrev_0C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_cotton_rrev$clustervcv, cs.ln_cotton_rrev$weights^2)

cs1C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_1C)
cs1C.pred_ln_cotton_rrev_se <- cc.pred.se(cs.ln_cotton_rrev$X - cs.ln_cotton_rrev_1C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_cotton_rrev$clustervcv, cs.ln_cotton_rrev$weights^2)
cs2C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_2C)
cs2C.pred_ln_cotton_rrev_se <- cc.pred.se(cs.ln_cotton_rrev$X - cs.ln_cotton_rrev_2C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_cotton_rrev$clustervcv, cs.ln_cotton_rrev$weights^2)
cs3C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_3C)
cs3C.pred_ln_cotton_rrev_se <- cc.pred.se(cs.ln_cotton_rrev$X - cs.ln_cotton_rrev_3C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_cotton_rrev$clustervcv, cs.ln_cotton_rrev$weights^2)
cs4C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_4C)
cs4C.pred_ln_cotton_rrev_se <- cc.pred.se(cs.ln_cotton_rrev$X - cs.ln_cotton_rrev_4C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_cotton_rrev$clustervcv, cs.ln_cotton_rrev$weights^2)
cs5C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_5C)
cs5C.pred_ln_cotton_rrev_se <- cc.pred.se(cs.ln_cotton_rrev$X - cs.ln_cotton_rrev_5C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_cotton_rrev$clustervcv, cs.ln_cotton_rrev$weights^2)

p0C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_0C)
p0C.pred_ln_cotton_rrev_se <- cc.pred.se(p.ln_cotton_rrev$X - p.ln_cotton_rrev_0C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_cotton_rrev$clustervcv, p.ln_cotton_rrev$weights^2)

p1C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_1C)
p1C.pred_ln_cotton_rrev_se <- cc.pred.se(p.ln_cotton_rrev$X - p.ln_cotton_rrev_1C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_cotton_rrev$clustervcv, p.ln_cotton_rrev$weights^2)
p2C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_2C)
p2C.pred_ln_cotton_rrev_se <- cc.pred.se(p.ln_cotton_rrev$X - p.ln_cotton_rrev_2C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_cotton_rrev$clustervcv, p.ln_cotton_rrev$weights^2)
p3C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_3C)
p3C.pred_ln_cotton_rrev_se <- cc.pred.se(p.ln_cotton_rrev$X - p.ln_cotton_rrev_3C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_cotton_rrev$clustervcv, p.ln_cotton_rrev$weights^2)
p4C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_4C)
p4C.pred_ln_cotton_rrev_se <- cc.pred.se(p.ln_cotton_rrev$X - p.ln_cotton_rrev_4C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_cotton_rrev$clustervcv, p.ln_cotton_rrev$weights^2)
p5C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_5C)
p5C.pred_ln_cotton_rrev_se <- cc.pred.se(p.ln_cotton_rrev$X - p.ln_cotton_rrev_5C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_cotton_rrev$clustervcv, p.ln_cotton_rrev$weights^2)

diff0C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_0C)
diff1C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_1C)
diff1C.pred_ln_cotton_rrev_se <- cc.pred.se(diff.ln_cotton_rrev$X - diff.ln_cotton_rrev_1C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_cotton_rrev$clustervcv, diff.ln_cotton_rrev$weights^2)
diff2C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_2C)
diff2C.pred_ln_cotton_rrev_se <- cc.pred.se(diff.ln_cotton_rrev$X - diff.ln_cotton_rrev_2C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_cotton_rrev$clustervcv, diff.ln_cotton_rrev$weights^2)
diff3C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_3C)
diff3C.pred_ln_cotton_rrev_se <- cc.pred.se(diff.ln_cotton_rrev$X - diff.ln_cotton_rrev_3C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_cotton_rrev$clustervcv, diff.ln_cotton_rrev$weights^2)
diff4C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_4C)
diff4C.pred_ln_cotton_rrev_se <- cc.pred.se(diff.ln_cotton_rrev$X - diff.ln_cotton_rrev_4C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_cotton_rrev$clustervcv, diff.ln_cotton_rrev$weights^2)
diff5C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_5C)
diff5C.pred_ln_cotton_rrev_se <- cc.pred.se(diff.ln_cotton_rrev$X - diff.ln_cotton_rrev_5C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_cotton_rrev$clustervcv, diff.ln_cotton_rrev$weights^2)


cs.rev1C <- exp(cs1C.pred_ln_cotton_rrev$fit)
cs.rev2C <- exp(cs2C.pred_ln_cotton_rrev$fit + cs2C.pred_ln_cotton_rrev$res )
cs.rev3C <- exp(cs3C.pred_ln_cotton_rrev$fit + cs3C.pred_ln_cotton_rrev$res )
cs.rev4C <- exp(cs4C.pred_ln_cotton_rrev$fit + cs4C.pred_ln_cotton_rrev$res )
cs.rev5C <- exp(cs5C.pred_ln_cotton_rrev$fit + cs5C.pred_ln_cotton_rrev$res )

p.rev1C <- exp(p1C.pred_ln_cotton_rrev$fit + p1C.pred_ln_cotton_rrev$res + p1C.pred_ln_cotton_rrev$effect)
p.rev2C <- exp(p2C.pred_ln_cotton_rrev$fit + p2C.pred_ln_cotton_rrev$res + p2C.pred_ln_cotton_rrev$effect)
p.rev3C <- exp(p3C.pred_ln_cotton_rrev$fit + p3C.pred_ln_cotton_rrev$res + p3C.pred_ln_cotton_rrev$effect)
p.rev4C <- exp(p4C.pred_ln_cotton_rrev$fit + p4C.pred_ln_cotton_rrev$res + p4C.pred_ln_cotton_rrev$effect)
p.rev5C <- exp(p5C.pred_ln_cotton_rrev$fit + p5C.pred_ln_cotton_rrev$res + p5C.pred_ln_cotton_rrev$effect)

diff.rev1C <- exp(diff1C.pred_ln_cotton_rrev$fit + diff1C.pred_ln_cotton_rrev$res + diff1C.pred_ln_cotton_rrev$effect)
diff.rev2C <- exp(diff2C.pred_ln_cotton_rrev$fit + diff2C.pred_ln_cotton_rrev$res + diff2C.pred_ln_cotton_rrev$effect)
diff.rev3C <- exp(diff3C.pred_ln_cotton_rrev$fit + diff3C.pred_ln_cotton_rrev$res + diff3C.pred_ln_cotton_rrev$effect)
diff.rev4C <- exp(diff4C.pred_ln_cotton_rrev$fit + diff4C.pred_ln_cotton_rrev$res + diff4C.pred_ln_cotton_rrev$effect)
diff.rev5C <- exp(diff5C.pred_ln_cotton_rrev$fit + diff5C.pred_ln_cotton_rrev$res + diff5C.pred_ln_cotton_rrev$effect)

cs.rev <- data.frame(rev = c(exp(cs.ln_cotton_rrev), cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
                     change = rep(c("Base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))

p.rev <- data.frame(rev = c(p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
                    change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))

diff.rev <- data.frame(rev = c(diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
                       change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))


ggplot(cs.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")
ggplot(filter(p.rev, rev < 50), aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
ggplot(diff.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")



cs.cotton.rev0 <- (sum( exp(cs0C.pred_ln_cotton_rrev$fit + cs0C.pred_ln_cotton_rrev$res)))
cs.cotton.rev1 <- (sum( exp(cs1C.pred_ln_cotton_rrev$fit + cs1C.pred_ln_cotton_rrev$res  ))/cs.cotton.rev0 - 1)*100
cs.cotton.rev1.min <- (sum( exp(cs1C.pred_ln_cotton_rrev$fit - c(cs1C.pred_ln_cotton_rrev_se*1.96) + cs1C.pred_ln_cotton_rrev$res ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev1.max <- (sum( exp(cs1C.pred_ln_cotton_rrev$fit + c(cs1C.pred_ln_cotton_rrev_se*1.96) + cs1C.pred_ln_cotton_rrev$res  ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100

cs.cotton.rev2 <- (sum( exp(cs2C.pred_ln_cotton_rrev$fit + cs2C.pred_ln_cotton_rrev$res  ))/cs.cotton.rev0 - 1)*100
cs.cotton.rev2.min <- (sum( exp(cs2C.pred_ln_cotton_rrev$fit - c(cs2C.pred_ln_cotton_rrev_se*1.96) + cs2C.pred_ln_cotton_rrev$res  ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev2.max <- (sum( exp(cs2C.pred_ln_cotton_rrev$fit + c(cs2C.pred_ln_cotton_rrev_se*1.96) + cs2C.pred_ln_cotton_rrev$res  ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100

cs.cotton.rev3 <- (sum( exp(cs3C.pred_ln_cotton_rrev$fit + cs3C.pred_ln_cotton_rrev$res  ))/cs.cotton.rev0 - 1)*100
cs.cotton.rev3.min <- (sum( exp(cs3C.pred_ln_cotton_rrev$fit - c(cs3C.pred_ln_cotton_rrev_se*1.96) + cs3C.pred_ln_cotton_rrev$res  ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev3.max <- (sum( exp(cs3C.pred_ln_cotton_rrev$fit + c(cs3C.pred_ln_cotton_rrev_se*1.96) + cs3C.pred_ln_cotton_rrev$res  ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100

cs.cotton.rev4 <- (sum( exp(cs4C.pred_ln_cotton_rrev$fit + cs4C.pred_ln_cotton_rrev$res  ))/cs.cotton.rev0 - 1)*100
cs.cotton.rev4.min <- (sum( exp(cs4C.pred_ln_cotton_rrev$fit - c(cs4C.pred_ln_cotton_rrev_se*1.96) + cs4C.pred_ln_cotton_rrev$res  ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev4.max <- (sum( exp(cs4C.pred_ln_cotton_rrev$fit + c(cs4C.pred_ln_cotton_rrev_se*1.96) + cs4C.pred_ln_cotton_rrev$res  ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100

cs.cotton.rev5 <- (sum( exp(cs5C.pred_ln_cotton_rrev$fit + cs5C.pred_ln_cotton_rrev$res  ))/cs.cotton.rev0 - 1)*100
cs.cotton.rev5.min <- (sum( exp(cs5C.pred_ln_cotton_rrev$fit - c(cs5C.pred_ln_cotton_rrev_se*1.96) + cs5C.pred_ln_cotton_rrev$res  ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev5.max <- (sum( exp(cs5C.pred_ln_cotton_rrev$fit + c(cs5C.pred_ln_cotton_rrev_se*1.96) + cs5C.pred_ln_cotton_rrev$res  ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100


p.cotton.rev0 <- (sum( exp(p0C.pred_ln_cotton_rrev$fit + p0C.pred_ln_cotton_rrev$res + p0C.pred_ln_cotton_rrev$effect)))
p.cotton.rev1 <- (sum( exp(p1C.pred_ln_cotton_rrev$fit + p1C.pred_ln_cotton_rrev$res + p1C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100
p.cotton.rev1.min <- (sum( exp(p1C.pred_ln_cotton_rrev$fit - c(p1C.pred_ln_cotton_rrev_se*1.96) + p1C.pred_ln_cotton_rrev$res + p1C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100
p.cotton.rev1.max <- (sum( exp(p1C.pred_ln_cotton_rrev$fit + c(p1C.pred_ln_cotton_rrev_se*1.96) + p1C.pred_ln_cotton_rrev$res + p1C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100

p.cotton.rev2 <- (sum( exp(p2C.pred_ln_cotton_rrev$fit + p2C.pred_ln_cotton_rrev$res + p2C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100
p.cotton.rev2.min <- (sum( exp(p2C.pred_ln_cotton_rrev$fit - c(p2C.pred_ln_cotton_rrev_se*1.96) + p2C.pred_ln_cotton_rrev$res + p2C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100
p.cotton.rev2.max <- (sum( exp(p2C.pred_ln_cotton_rrev$fit + c(p2C.pred_ln_cotton_rrev_se*1.96) + p2C.pred_ln_cotton_rrev$res + p2C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100

p.cotton.rev3 <- (sum( exp(p3C.pred_ln_cotton_rrev$fit + p3C.pred_ln_cotton_rrev$res + p3C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100
p.cotton.rev3.min <- (sum( exp(p3C.pred_ln_cotton_rrev$fit - c(p3C.pred_ln_cotton_rrev_se*1.96) + p3C.pred_ln_cotton_rrev$res + p3C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100
p.cotton.rev3.max <- (sum( exp(p3C.pred_ln_cotton_rrev$fit + c(p3C.pred_ln_cotton_rrev_se*1.96) + p3C.pred_ln_cotton_rrev$res + p3C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100

p.cotton.rev4 <- (sum( exp(p4C.pred_ln_cotton_rrev$fit + p4C.pred_ln_cotton_rrev$res + p4C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100
p.cotton.rev4.min <- (sum( exp(p4C.pred_ln_cotton_rrev$fit - c(p4C.pred_ln_cotton_rrev_se*1.96) + p4C.pred_ln_cotton_rrev$res + p4C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100
p.cotton.rev4.max <- (sum( exp(p4C.pred_ln_cotton_rrev$fit + c(p4C.pred_ln_cotton_rrev_se*1.96) + p4C.pred_ln_cotton_rrev$res + p4C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100

p.cotton.rev5 <- (sum( exp(p5C.pred_ln_cotton_rrev$fit + p5C.pred_ln_cotton_rrev$res + p5C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100
p.cotton.rev5.min <- (sum( exp(p5C.pred_ln_cotton_rrev$fit - c(p5C.pred_ln_cotton_rrev_se*1.96) + p5C.pred_ln_cotton_rrev$res + p5C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100
p.cotton.rev5.max <- (sum( exp(p5C.pred_ln_cotton_rrev$fit + c(p5C.pred_ln_cotton_rrev_se*1.96) + p5C.pred_ln_cotton_rrev$res + p5C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100

diff.cotton.rev0 <- (sum( exp(diff0C.pred_ln_cotton_rrev$fit + diff0C.pred_ln_cotton_rrev$res + diff1C.pred_ln_cotton_rrev$effect )))
diff.cotton.rev1 <- (sum( exp(diff1C.pred_ln_cotton_rrev$fit + diff1C.pred_ln_cotton_rrev$res + diff1C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100
diff.cotton.rev1.min <- (sum( exp(diff1C.pred_ln_cotton_rrev$fit - c(diff1C.pred_ln_cotton_rrev_se*1.96) + diff1C.pred_ln_cotton_rrev$res + diff1C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100
diff.cotton.rev1.max <- (sum( exp(diff1C.pred_ln_cotton_rrev$fit + c(diff1C.pred_ln_cotton_rrev_se*1.96) + diff1C.pred_ln_cotton_rrev$res + diff1C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100

diff.cotton.rev2 <- (sum( exp(diff2C.pred_ln_cotton_rrev$fit + diff2C.pred_ln_cotton_rrev$res + diff2C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100
diff.cotton.rev2.min <- (sum( exp(diff2C.pred_ln_cotton_rrev$fit - c(diff2C.pred_ln_cotton_rrev_se*1.96) + diff2C.pred_ln_cotton_rrev$res + diff2C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100
diff.cotton.rev2.max <- (sum( exp(diff2C.pred_ln_cotton_rrev$fit + c(diff2C.pred_ln_cotton_rrev_se*1.96) + diff2C.pred_ln_cotton_rrev$res + diff2C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100

diff.cotton.rev3 <- (sum( exp(diff3C.pred_ln_cotton_rrev$fit + diff3C.pred_ln_cotton_rrev$res + diff3C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100
diff.cotton.rev3.min <- (sum( exp(diff3C.pred_ln_cotton_rrev$fit - c(diff3C.pred_ln_cotton_rrev_se*1.96) + diff3C.pred_ln_cotton_rrev$res + diff3C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100
diff.cotton.rev3.max <- (sum( exp(diff3C.pred_ln_cotton_rrev$fit + c(diff3C.pred_ln_cotton_rrev_se*1.96) + diff3C.pred_ln_cotton_rrev$res + diff3C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100

diff.cotton.rev4 <- (sum( exp(diff4C.pred_ln_cotton_rrev$fit + diff4C.pred_ln_cotton_rrev$res + diff4C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100
diff.cotton.rev4.min <- (sum( exp(diff4C.pred_ln_cotton_rrev$fit - c(diff4C.pred_ln_cotton_rrev_se*1.96) + diff4C.pred_ln_cotton_rrev$res + diff4C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100
diff.cotton.rev4.max <- (sum( exp(diff4C.pred_ln_cotton_rrev$fit + c(diff4C.pred_ln_cotton_rrev_se*1.96) + diff4C.pred_ln_cotton_rrev$res + diff4C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100

diff.cotton.rev5 <- (sum( exp(diff5C.pred_ln_cotton_rrev$fit + diff5C.pred_ln_cotton_rrev$res + diff5C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100
diff.cotton.rev5.min <- (sum( exp(diff5C.pred_ln_cotton_rrev$fit - c(diff5C.pred_ln_cotton_rrev_se*1.96) + diff5C.pred_ln_cotton_rrev$res + diff5C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100
diff.cotton.rev5.max <- (sum( exp(diff5C.pred_ln_cotton_rrev$fit + c(diff5C.pred_ln_cotton_rrev_se*1.96) + diff5C.pred_ln_cotton_rrev$res + diff5C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100

cotton.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.cotton.rev1, cs.cotton.rev2, cs.cotton.rev3, cs.cotton.rev4, cs.cotton.rev5,
                                   p.cotton.rev1, p.cotton.rev2, p.cotton.rev3, p.cotton.rev4, p.cotton.rev5,
                                   diff.cotton.rev1, diff.cotton.rev2, diff.cotton.rev3, diff.cotton.rev4, diff.cotton.rev5),
                           min = c(cs.cotton.rev1.min, cs.cotton.rev2.min, cs.cotton.rev3.min, cs.cotton.rev4.min, cs.cotton.rev5.min,
                                   p.cotton.rev1.min, p.cotton.rev2.min, p.cotton.rev3.min, p.cotton.rev4.min, p.cotton.rev5.min,
                                   diff.cotton.rev1.min, diff.cotton.rev2.min, diff.cotton.rev3.min, diff.cotton.rev4.min, diff.cotton.rev5.min),
                           max = c(cs.cotton.rev1.max, cs.cotton.rev2.max, cs.cotton.rev3.max, cs.cotton.rev4.max, cs.cotton.rev5.max,
                                   p.cotton.rev1.max, p.cotton.rev2.max, p.cotton.rev3.max, p.cotton.rev4.max, p.cotton.rev5.max,
                                   diff.cotton.rev1.max, diff.cotton.rev2.max, diff.cotton.rev3.max, diff.cotton.rev4.max, diff.cotton.rev5.max),
                           reg = rep(c("cross-section", "panel", "diff"), each = 5),
                           crop = "cotton")

ggplot(cotton.plotdat, aes(temp, rev, group = reg)) + 
  #geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5", alpha = 0.5) + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")

# hay

cs0C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_0C)
cs0C.pred_ln_hay_rrev_se <- cc.pred.se(cs.ln_hay_rrev$X - cs.ln_hay_rrev_0C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_hay_rrev$clustervcv, cs.ln_hay_rrev$weights^2)

cs1C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_1C)
cs1C.pred_ln_hay_rrev_se <- cc.pred.se(cs.ln_hay_rrev$X - cs.ln_hay_rrev_1C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_hay_rrev$clustervcv, cs.ln_hay_rrev$weights^2)
cs2C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_2C)
cs2C.pred_ln_hay_rrev_se <- cc.pred.se(cs.ln_hay_rrev$X - cs.ln_hay_rrev_2C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_hay_rrev$clustervcv, cs.ln_hay_rrev$weights^2)
cs3C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_3C)
cs3C.pred_ln_hay_rrev_se <- cc.pred.se(cs.ln_hay_rrev$X - cs.ln_hay_rrev_3C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_hay_rrev$clustervcv, cs.ln_hay_rrev$weights^2)
cs4C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_4C)
cs4C.pred_ln_hay_rrev_se <- cc.pred.se(cs.ln_hay_rrev$X - cs.ln_hay_rrev_4C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_hay_rrev$clustervcv, cs.ln_hay_rrev$weights^2)
cs5C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_5C)
cs5C.pred_ln_hay_rrev_se <- cc.pred.se(cs.ln_hay_rrev$X - cs.ln_hay_rrev_5C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_hay_rrev$clustervcv, cs.ln_hay_rrev$weights^2)

p0C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_0C)
p0C.pred_ln_hay_rrev_se <- cc.pred.se(p.ln_hay_rrev$X - p.ln_hay_rrev_0C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_hay_rrev$clustervcv, p.ln_hay_rrev$weights^2)

p1C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_1C)
p1C.pred_ln_hay_rrev_se <- cc.pred.se(p.ln_hay_rrev$X - p.ln_hay_rrev_1C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_hay_rrev$clustervcv, p.ln_hay_rrev$weights^2)
p2C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_2C)
p2C.pred_ln_hay_rrev_se <- cc.pred.se(p.ln_hay_rrev$X - p.ln_hay_rrev_2C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_hay_rrev$clustervcv, p.ln_hay_rrev$weights^2)
p3C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_3C)
p3C.pred_ln_hay_rrev_se <- cc.pred.se(p.ln_hay_rrev$X - p.ln_hay_rrev_3C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_hay_rrev$clustervcv, p.ln_hay_rrev$weights^2)
p4C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_4C)
p4C.pred_ln_hay_rrev_se <- cc.pred.se(p.ln_hay_rrev$X - p.ln_hay_rrev_4C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_hay_rrev$clustervcv, p.ln_hay_rrev$weights^2)
p5C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_5C)
p5C.pred_ln_hay_rrev_se <- cc.pred.se(p.ln_hay_rrev$X - p.ln_hay_rrev_5C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_hay_rrev$clustervcv, p.ln_hay_rrev$weights^2)

diff0C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_0C)
diff1C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_1C)
diff1C.pred_ln_hay_rrev_se <- cc.pred.se(diff.ln_hay_rrev$X - diff.ln_hay_rrev_1C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_hay_rrev$clustervcv, diff.ln_hay_rrev$weights^2)
diff2C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_2C)
diff2C.pred_ln_hay_rrev_se <- cc.pred.se(diff.ln_hay_rrev$X - diff.ln_hay_rrev_2C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_hay_rrev$clustervcv, diff.ln_hay_rrev$weights^2)
diff3C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_3C)
diff3C.pred_ln_hay_rrev_se <- cc.pred.se(diff.ln_hay_rrev$X - diff.ln_hay_rrev_3C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_hay_rrev$clustervcv, diff.ln_hay_rrev$weights^2)
diff4C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_4C)
diff4C.pred_ln_hay_rrev_se <- cc.pred.se(diff.ln_hay_rrev$X - diff.ln_hay_rrev_4C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_hay_rrev$clustervcv, diff.ln_hay_rrev$weights^2)
diff5C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_5C)
diff5C.pred_ln_hay_rrev_se <- cc.pred.se(diff.ln_hay_rrev$X - diff.ln_hay_rrev_5C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_hay_rrev$clustervcv, diff.ln_hay_rrev$weights^2)


cs.rev1C <- exp(cs1C.pred_ln_hay_rrev$fit)
cs.rev2C <- exp(cs2C.pred_ln_hay_rrev$fit + cs2C.pred_ln_hay_rrev$res )
cs.rev3C <- exp(cs3C.pred_ln_hay_rrev$fit + cs3C.pred_ln_hay_rrev$res )
cs.rev4C <- exp(cs4C.pred_ln_hay_rrev$fit + cs4C.pred_ln_hay_rrev$res )
cs.rev5C <- exp(cs5C.pred_ln_hay_rrev$fit + cs5C.pred_ln_hay_rrev$res )

p.rev1C <- exp(p1C.pred_ln_hay_rrev$fit + p1C.pred_ln_hay_rrev$res + p1C.pred_ln_hay_rrev$effect)
p.rev2C <- exp(p2C.pred_ln_hay_rrev$fit + p2C.pred_ln_hay_rrev$res + p2C.pred_ln_hay_rrev$effect)
p.rev3C <- exp(p3C.pred_ln_hay_rrev$fit + p3C.pred_ln_hay_rrev$res + p3C.pred_ln_hay_rrev$effect)
p.rev4C <- exp(p4C.pred_ln_hay_rrev$fit + p4C.pred_ln_hay_rrev$res + p4C.pred_ln_hay_rrev$effect)
p.rev5C <- exp(p5C.pred_ln_hay_rrev$fit + p5C.pred_ln_hay_rrev$res + p5C.pred_ln_hay_rrev$effect)

diff.rev1C <- exp(diff1C.pred_ln_hay_rrev$fit + diff1C.pred_ln_hay_rrev$res + diff1C.pred_ln_hay_rrev$effect)
diff.rev2C <- exp(diff2C.pred_ln_hay_rrev$fit + diff2C.pred_ln_hay_rrev$res + diff2C.pred_ln_hay_rrev$effect)
diff.rev3C <- exp(diff3C.pred_ln_hay_rrev$fit + diff3C.pred_ln_hay_rrev$res + diff3C.pred_ln_hay_rrev$effect)
diff.rev4C <- exp(diff4C.pred_ln_hay_rrev$fit + diff4C.pred_ln_hay_rrev$res + diff4C.pred_ln_hay_rrev$effect)
diff.rev5C <- exp(diff5C.pred_ln_hay_rrev$fit + diff5C.pred_ln_hay_rrev$res + diff5C.pred_ln_hay_rrev$effect)

cs.rev <- data.frame(rev = c(exp(cs.ln_hay_rrev), cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
                     change = rep(c("Base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))

p.rev <- data.frame(rev = c(p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
                    change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))

diff.rev <- data.frame(rev = c(diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
                       change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))


ggplot(cs.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")
ggplot(filter(p.rev, rev < 50), aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
ggplot(diff.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")



cs.hay.rev0 <- (sum( exp(cs0C.pred_ln_hay_rrev$fit + cs0C.pred_ln_hay_rrev$res)))
cs.hay.rev1 <- (sum( exp(cs1C.pred_ln_hay_rrev$fit + cs1C.pred_ln_hay_rrev$res  ))/cs.hay.rev0 - 1)*100
cs.hay.rev1.min <- (sum( exp(cs1C.pred_ln_hay_rrev$fit - c(cs1C.pred_ln_hay_rrev_se*1.96) + cs1C.pred_ln_hay_rrev$res ), na.rm = TRUE)/cs.hay.rev0 - 1)*100
cs.hay.rev1.max <- (sum( exp(cs1C.pred_ln_hay_rrev$fit + c(cs1C.pred_ln_hay_rrev_se*1.96) + cs1C.pred_ln_hay_rrev$res  ), na.rm = TRUE)/cs.hay.rev0 - 1)*100

cs.hay.rev2 <- (sum( exp(cs2C.pred_ln_hay_rrev$fit + cs2C.pred_ln_hay_rrev$res  ))/cs.hay.rev0 - 1)*100
cs.hay.rev2.min <- (sum( exp(cs2C.pred_ln_hay_rrev$fit - c(cs2C.pred_ln_hay_rrev_se*1.96) + cs2C.pred_ln_hay_rrev$res  ), na.rm = TRUE)/cs.hay.rev0 - 1)*100
cs.hay.rev2.max <- (sum( exp(cs2C.pred_ln_hay_rrev$fit + c(cs2C.pred_ln_hay_rrev_se*1.96) + cs2C.pred_ln_hay_rrev$res  ), na.rm = TRUE)/cs.hay.rev0 - 1)*100

cs.hay.rev3 <- (sum( exp(cs3C.pred_ln_hay_rrev$fit + cs3C.pred_ln_hay_rrev$res  ))/cs.hay.rev0 - 1)*100
cs.hay.rev3.min <- (sum( exp(cs3C.pred_ln_hay_rrev$fit - c(cs3C.pred_ln_hay_rrev_se*1.96) + cs3C.pred_ln_hay_rrev$res  ), na.rm = TRUE)/cs.hay.rev0 - 1)*100
cs.hay.rev3.max <- (sum( exp(cs3C.pred_ln_hay_rrev$fit + c(cs3C.pred_ln_hay_rrev_se*1.96) + cs3C.pred_ln_hay_rrev$res  ), na.rm = TRUE)/cs.hay.rev0 - 1)*100

cs.hay.rev4 <- (sum( exp(cs4C.pred_ln_hay_rrev$fit + cs4C.pred_ln_hay_rrev$res  ))/cs.hay.rev0 - 1)*100
cs.hay.rev4.min <- (sum( exp(cs4C.pred_ln_hay_rrev$fit - c(cs4C.pred_ln_hay_rrev_se*1.96) + cs4C.pred_ln_hay_rrev$res  ), na.rm = TRUE)/cs.hay.rev0 - 1)*100
cs.hay.rev4.max <- (sum( exp(cs4C.pred_ln_hay_rrev$fit + c(cs4C.pred_ln_hay_rrev_se*1.96) + cs4C.pred_ln_hay_rrev$res  ), na.rm = TRUE)/cs.hay.rev0 - 1)*100

cs.hay.rev5 <- (sum( exp(cs5C.pred_ln_hay_rrev$fit + cs5C.pred_ln_hay_rrev$res  ))/cs.hay.rev0 - 1)*100
cs.hay.rev5.min <- (sum( exp(cs5C.pred_ln_hay_rrev$fit - c(cs5C.pred_ln_hay_rrev_se*1.96) + cs5C.pred_ln_hay_rrev$res  ), na.rm = TRUE)/cs.hay.rev0 - 1)*100
cs.hay.rev5.max <- (sum( exp(cs5C.pred_ln_hay_rrev$fit + c(cs5C.pred_ln_hay_rrev_se*1.96) + cs5C.pred_ln_hay_rrev$res  ), na.rm = TRUE)/cs.hay.rev0 - 1)*100


p.hay.rev0 <- (sum( exp(p0C.pred_ln_hay_rrev$fit + p0C.pred_ln_hay_rrev$res + p0C.pred_ln_hay_rrev$effect)))
p.hay.rev1 <- (sum( exp(p1C.pred_ln_hay_rrev$fit + p1C.pred_ln_hay_rrev$res + p1C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100
p.hay.rev1.min <- (sum( exp(p1C.pred_ln_hay_rrev$fit - c(p1C.pred_ln_hay_rrev_se*1.96) + p1C.pred_ln_hay_rrev$res + p1C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100
p.hay.rev1.max <- (sum( exp(p1C.pred_ln_hay_rrev$fit + c(p1C.pred_ln_hay_rrev_se*1.96) + p1C.pred_ln_hay_rrev$res + p1C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100

p.hay.rev2 <- (sum( exp(p2C.pred_ln_hay_rrev$fit + p2C.pred_ln_hay_rrev$res + p2C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100
p.hay.rev2.min <- (sum( exp(p2C.pred_ln_hay_rrev$fit - c(p2C.pred_ln_hay_rrev_se*1.96) + p2C.pred_ln_hay_rrev$res + p2C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100
p.hay.rev2.max <- (sum( exp(p2C.pred_ln_hay_rrev$fit + c(p2C.pred_ln_hay_rrev_se*1.96) + p2C.pred_ln_hay_rrev$res + p2C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100

p.hay.rev3 <- (sum( exp(p3C.pred_ln_hay_rrev$fit + p3C.pred_ln_hay_rrev$res + p3C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100
p.hay.rev3.min <- (sum( exp(p3C.pred_ln_hay_rrev$fit - c(p3C.pred_ln_hay_rrev_se*1.96) + p3C.pred_ln_hay_rrev$res + p3C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100
p.hay.rev3.max <- (sum( exp(p3C.pred_ln_hay_rrev$fit + c(p3C.pred_ln_hay_rrev_se*1.96) + p3C.pred_ln_hay_rrev$res + p3C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100

p.hay.rev4 <- (sum( exp(p4C.pred_ln_hay_rrev$fit + p4C.pred_ln_hay_rrev$res + p4C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100
p.hay.rev4.min <- (sum( exp(p4C.pred_ln_hay_rrev$fit - c(p4C.pred_ln_hay_rrev_se*1.96) + p4C.pred_ln_hay_rrev$res + p4C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100
p.hay.rev4.max <- (sum( exp(p4C.pred_ln_hay_rrev$fit + c(p4C.pred_ln_hay_rrev_se*1.96) + p4C.pred_ln_hay_rrev$res + p4C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100

p.hay.rev5 <- (sum( exp(p5C.pred_ln_hay_rrev$fit + p5C.pred_ln_hay_rrev$res + p5C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100
p.hay.rev5.min <- (sum( exp(p5C.pred_ln_hay_rrev$fit - c(p5C.pred_ln_hay_rrev_se*1.96) + p5C.pred_ln_hay_rrev$res + p5C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100
p.hay.rev5.max <- (sum( exp(p5C.pred_ln_hay_rrev$fit + c(p5C.pred_ln_hay_rrev_se*1.96) + p5C.pred_ln_hay_rrev$res + p5C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100

diff.hay.rev0 <- (sum( exp(diff0C.pred_ln_hay_rrev$fit + diff0C.pred_ln_hay_rrev$res + diff1C.pred_ln_hay_rrev$effect )))
diff.hay.rev1 <- (sum( exp(diff1C.pred_ln_hay_rrev$fit + diff1C.pred_ln_hay_rrev$res + diff1C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100
diff.hay.rev1.min <- (sum( exp(diff1C.pred_ln_hay_rrev$fit - c(diff1C.pred_ln_hay_rrev_se*1.96) + diff1C.pred_ln_hay_rrev$res + diff1C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100
diff.hay.rev1.max <- (sum( exp(diff1C.pred_ln_hay_rrev$fit + c(diff1C.pred_ln_hay_rrev_se*1.96) + diff1C.pred_ln_hay_rrev$res + diff1C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100

diff.hay.rev2 <- (sum( exp(diff2C.pred_ln_hay_rrev$fit + diff2C.pred_ln_hay_rrev$res + diff2C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100
diff.hay.rev2.min <- (sum( exp(diff2C.pred_ln_hay_rrev$fit - c(diff2C.pred_ln_hay_rrev_se*1.96) + diff2C.pred_ln_hay_rrev$res + diff2C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100
diff.hay.rev2.max <- (sum( exp(diff2C.pred_ln_hay_rrev$fit + c(diff2C.pred_ln_hay_rrev_se*1.96) + diff2C.pred_ln_hay_rrev$res + diff2C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100

diff.hay.rev3 <- (sum( exp(diff3C.pred_ln_hay_rrev$fit + diff3C.pred_ln_hay_rrev$res + diff3C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100
diff.hay.rev3.min <- (sum( exp(diff3C.pred_ln_hay_rrev$fit - c(diff3C.pred_ln_hay_rrev_se*1.96) + diff3C.pred_ln_hay_rrev$res + diff3C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100
diff.hay.rev3.max <- (sum( exp(diff3C.pred_ln_hay_rrev$fit + c(diff3C.pred_ln_hay_rrev_se*1.96) + diff3C.pred_ln_hay_rrev$res + diff3C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100

diff.hay.rev4 <- (sum( exp(diff4C.pred_ln_hay_rrev$fit + diff4C.pred_ln_hay_rrev$res + diff4C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100
diff.hay.rev4.min <- (sum( exp(diff4C.pred_ln_hay_rrev$fit - c(diff4C.pred_ln_hay_rrev_se*1.96) + diff4C.pred_ln_hay_rrev$res + diff4C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100
diff.hay.rev4.max <- (sum( exp(diff4C.pred_ln_hay_rrev$fit + c(diff4C.pred_ln_hay_rrev_se*1.96) + diff4C.pred_ln_hay_rrev$res + diff4C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100

diff.hay.rev5 <- (sum( exp(diff5C.pred_ln_hay_rrev$fit + diff5C.pred_ln_hay_rrev$res + diff5C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100
diff.hay.rev5.min <- (sum( exp(diff5C.pred_ln_hay_rrev$fit - c(diff5C.pred_ln_hay_rrev_se*1.96) + diff5C.pred_ln_hay_rrev$res + diff5C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100
diff.hay.rev5.max <- (sum( exp(diff5C.pred_ln_hay_rrev$fit + c(diff5C.pred_ln_hay_rrev_se*1.96) + diff5C.pred_ln_hay_rrev$res + diff5C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100

hay.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.hay.rev1, cs.hay.rev2, cs.hay.rev3, cs.hay.rev4, cs.hay.rev5,
                                   p.hay.rev1, p.hay.rev2, p.hay.rev3, p.hay.rev4, p.hay.rev5,
                                   diff.hay.rev1, diff.hay.rev2, diff.hay.rev3, diff.hay.rev4, diff.hay.rev5),
                           min = c(cs.hay.rev1.min, cs.hay.rev2.min, cs.hay.rev3.min, cs.hay.rev4.min, cs.hay.rev5.min,
                                   p.hay.rev1.min, p.hay.rev2.min, p.hay.rev3.min, p.hay.rev4.min, p.hay.rev5.min,
                                   diff.hay.rev1.min, diff.hay.rev2.min, diff.hay.rev3.min, diff.hay.rev4.min, diff.hay.rev5.min),
                           max = c(cs.hay.rev1.max, cs.hay.rev2.max, cs.hay.rev3.max, cs.hay.rev4.max, cs.hay.rev5.max,
                                   p.hay.rev1.max, p.hay.rev2.max, p.hay.rev3.max, p.hay.rev4.max, p.hay.rev5.max,
                                   diff.hay.rev1.max, diff.hay.rev2.max, diff.hay.rev3.max, diff.hay.rev4.max, diff.hay.rev5.max),
                           reg = rep(c("cross-section", "panel", "diff"), each = 5),
                           crop = "hay")

ggplot(hay.plotdat, aes(temp, rev, group = reg)) + 
  #geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5", alpha = 0.5) + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")

# wheat

cs0C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_0C)
cs0C.pred_ln_wheat_rrev_se <- cc.pred.se(cs.ln_wheat_rrev$X - cs.ln_wheat_rrev_0C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_wheat_rrev$clustervcv, cs.ln_wheat_rrev$weights^2)

cs1C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_1C)
cs1C.pred_ln_wheat_rrev_se <- cc.pred.se(cs.ln_wheat_rrev$X - cs.ln_wheat_rrev_1C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_wheat_rrev$clustervcv, cs.ln_wheat_rrev$weights^2)
cs2C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_2C)
cs2C.pred_ln_wheat_rrev_se <- cc.pred.se(cs.ln_wheat_rrev$X - cs.ln_wheat_rrev_2C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_wheat_rrev$clustervcv, cs.ln_wheat_rrev$weights^2)
cs3C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_3C)
cs3C.pred_ln_wheat_rrev_se <- cc.pred.se(cs.ln_wheat_rrev$X - cs.ln_wheat_rrev_3C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_wheat_rrev$clustervcv, cs.ln_wheat_rrev$weights^2)
cs4C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_4C)
cs4C.pred_ln_wheat_rrev_se <- cc.pred.se(cs.ln_wheat_rrev$X - cs.ln_wheat_rrev_4C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_wheat_rrev$clustervcv, cs.ln_wheat_rrev$weights^2)
cs5C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_5C)
cs5C.pred_ln_wheat_rrev_se <- cc.pred.se(cs.ln_wheat_rrev$X - cs.ln_wheat_rrev_5C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_wheat_rrev$clustervcv, cs.ln_wheat_rrev$weights^2)

p0C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_0C)
p0C.pred_ln_wheat_rrev_se <- cc.pred.se(p.ln_wheat_rrev$X - p.ln_wheat_rrev_0C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_wheat_rrev$clustervcv, p.ln_wheat_rrev$weights^2)

p1C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_1C)
p1C.pred_ln_wheat_rrev_se <- cc.pred.se(p.ln_wheat_rrev$X - p.ln_wheat_rrev_1C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_wheat_rrev$clustervcv, p.ln_wheat_rrev$weights^2)
p2C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_2C)
p2C.pred_ln_wheat_rrev_se <- cc.pred.se(p.ln_wheat_rrev$X - p.ln_wheat_rrev_2C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_wheat_rrev$clustervcv, p.ln_wheat_rrev$weights^2)
p3C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_3C)
p3C.pred_ln_wheat_rrev_se <- cc.pred.se(p.ln_wheat_rrev$X - p.ln_wheat_rrev_3C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_wheat_rrev$clustervcv, p.ln_wheat_rrev$weights^2)
p4C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_4C)
p4C.pred_ln_wheat_rrev_se <- cc.pred.se(p.ln_wheat_rrev$X - p.ln_wheat_rrev_4C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_wheat_rrev$clustervcv, p.ln_wheat_rrev$weights^2)
p5C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_5C)
p5C.pred_ln_wheat_rrev_se <- cc.pred.se(p.ln_wheat_rrev$X - p.ln_wheat_rrev_5C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_wheat_rrev$clustervcv, p.ln_wheat_rrev$weights^2)

diff0C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_0C)
diff1C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_1C)
diff1C.pred_ln_wheat_rrev_se <- cc.pred.se(diff.ln_wheat_rrev$X - diff.ln_wheat_rrev_1C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_wheat_rrev$clustervcv, diff.ln_wheat_rrev$weights^2)
diff2C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_2C)
diff2C.pred_ln_wheat_rrev_se <- cc.pred.se(diff.ln_wheat_rrev$X - diff.ln_wheat_rrev_2C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_wheat_rrev$clustervcv, diff.ln_wheat_rrev$weights^2)
diff3C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_3C)
diff3C.pred_ln_wheat_rrev_se <- cc.pred.se(diff.ln_wheat_rrev$X - diff.ln_wheat_rrev_3C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_wheat_rrev$clustervcv, diff.ln_wheat_rrev$weights^2)
diff4C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_4C)
diff4C.pred_ln_wheat_rrev_se <- cc.pred.se(diff.ln_wheat_rrev$X - diff.ln_wheat_rrev_4C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_wheat_rrev$clustervcv, diff.ln_wheat_rrev$weights^2)
diff5C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_5C)
diff5C.pred_ln_wheat_rrev_se <- cc.pred.se(diff.ln_wheat_rrev$X - diff.ln_wheat_rrev_5C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_wheat_rrev$clustervcv, diff.ln_wheat_rrev$weights^2)


cs.rev1C <- exp(cs1C.pred_ln_wheat_rrev$fit)
cs.rev2C <- exp(cs2C.pred_ln_wheat_rrev$fit + cs2C.pred_ln_wheat_rrev$res )
cs.rev3C <- exp(cs3C.pred_ln_wheat_rrev$fit + cs3C.pred_ln_wheat_rrev$res )
cs.rev4C <- exp(cs4C.pred_ln_wheat_rrev$fit + cs4C.pred_ln_wheat_rrev$res )
cs.rev5C <- exp(cs5C.pred_ln_wheat_rrev$fit + cs5C.pred_ln_wheat_rrev$res )

p.rev1C <- exp(p1C.pred_ln_wheat_rrev$fit + p1C.pred_ln_wheat_rrev$res + p1C.pred_ln_wheat_rrev$effect)
p.rev2C <- exp(p2C.pred_ln_wheat_rrev$fit + p2C.pred_ln_wheat_rrev$res + p2C.pred_ln_wheat_rrev$effect)
p.rev3C <- exp(p3C.pred_ln_wheat_rrev$fit + p3C.pred_ln_wheat_rrev$res + p3C.pred_ln_wheat_rrev$effect)
p.rev4C <- exp(p4C.pred_ln_wheat_rrev$fit + p4C.pred_ln_wheat_rrev$res + p4C.pred_ln_wheat_rrev$effect)
p.rev5C <- exp(p5C.pred_ln_wheat_rrev$fit + p5C.pred_ln_wheat_rrev$res + p5C.pred_ln_wheat_rrev$effect)

diff.rev1C <- exp(diff1C.pred_ln_wheat_rrev$fit + diff1C.pred_ln_wheat_rrev$res + diff1C.pred_ln_wheat_rrev$effect)
diff.rev2C <- exp(diff2C.pred_ln_wheat_rrev$fit + diff2C.pred_ln_wheat_rrev$res + diff2C.pred_ln_wheat_rrev$effect)
diff.rev3C <- exp(diff3C.pred_ln_wheat_rrev$fit + diff3C.pred_ln_wheat_rrev$res + diff3C.pred_ln_wheat_rrev$effect)
diff.rev4C <- exp(diff4C.pred_ln_wheat_rrev$fit + diff4C.pred_ln_wheat_rrev$res + diff4C.pred_ln_wheat_rrev$effect)
diff.rev5C <- exp(diff5C.pred_ln_wheat_rrev$fit + diff5C.pred_ln_wheat_rrev$res + diff5C.pred_ln_wheat_rrev$effect)

cs.rev <- data.frame(rev = c(exp(cs.ln_wheat_rrev), cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
                     change = rep(c("Base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))

p.rev <- data.frame(rev = c(p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
                    change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))

diff.rev <- data.frame(rev = c(diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
                       change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))


ggplot(cs.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")
ggplot(filter(p.rev, rev < 50), aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
ggplot(diff.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")



cs.wheat.rev0 <- (sum( exp(cs0C.pred_ln_wheat_rrev$fit + cs0C.pred_ln_wheat_rrev$res)))
cs.wheat.rev1 <- (sum( exp(cs1C.pred_ln_wheat_rrev$fit + cs1C.pred_ln_wheat_rrev$res  ))/cs.wheat.rev0 - 1)*100
cs.wheat.rev1.min <- (sum( exp(cs1C.pred_ln_wheat_rrev$fit - c(cs1C.pred_ln_wheat_rrev_se*1.96) + cs1C.pred_ln_wheat_rrev$res ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100
cs.wheat.rev1.max <- (sum( exp(cs1C.pred_ln_wheat_rrev$fit + c(cs1C.pred_ln_wheat_rrev_se*1.96) + cs1C.pred_ln_wheat_rrev$res  ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100

cs.wheat.rev2 <- (sum( exp(cs2C.pred_ln_wheat_rrev$fit + cs2C.pred_ln_wheat_rrev$res  ))/cs.wheat.rev0 - 1)*100
cs.wheat.rev2.min <- (sum( exp(cs2C.pred_ln_wheat_rrev$fit - c(cs2C.pred_ln_wheat_rrev_se*1.96) + cs2C.pred_ln_wheat_rrev$res  ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100
cs.wheat.rev2.max <- (sum( exp(cs2C.pred_ln_wheat_rrev$fit + c(cs2C.pred_ln_wheat_rrev_se*1.96) + cs2C.pred_ln_wheat_rrev$res  ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100

cs.wheat.rev3 <- (sum( exp(cs3C.pred_ln_wheat_rrev$fit + cs3C.pred_ln_wheat_rrev$res  ))/cs.wheat.rev0 - 1)*100
cs.wheat.rev3.min <- (sum( exp(cs3C.pred_ln_wheat_rrev$fit - c(cs3C.pred_ln_wheat_rrev_se*1.96) + cs3C.pred_ln_wheat_rrev$res  ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100
cs.wheat.rev3.max <- (sum( exp(cs3C.pred_ln_wheat_rrev$fit + c(cs3C.pred_ln_wheat_rrev_se*1.96) + cs3C.pred_ln_wheat_rrev$res  ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100

cs.wheat.rev4 <- (sum( exp(cs4C.pred_ln_wheat_rrev$fit + cs4C.pred_ln_wheat_rrev$res  ))/cs.wheat.rev0 - 1)*100
cs.wheat.rev4.min <- (sum( exp(cs4C.pred_ln_wheat_rrev$fit - c(cs4C.pred_ln_wheat_rrev_se*1.96) + cs4C.pred_ln_wheat_rrev$res  ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100
cs.wheat.rev4.max <- (sum( exp(cs4C.pred_ln_wheat_rrev$fit + c(cs4C.pred_ln_wheat_rrev_se*1.96) + cs4C.pred_ln_wheat_rrev$res  ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100

cs.wheat.rev5 <- (sum( exp(cs5C.pred_ln_wheat_rrev$fit + cs5C.pred_ln_wheat_rrev$res  ))/cs.wheat.rev0 - 1)*100
cs.wheat.rev5.min <- (sum( exp(cs5C.pred_ln_wheat_rrev$fit - c(cs5C.pred_ln_wheat_rrev_se*1.96) + cs5C.pred_ln_wheat_rrev$res  ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100
cs.wheat.rev5.max <- (sum( exp(cs5C.pred_ln_wheat_rrev$fit + c(cs5C.pred_ln_wheat_rrev_se*1.96) + cs5C.pred_ln_wheat_rrev$res  ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100


p.wheat.rev0 <- (sum( exp(p0C.pred_ln_wheat_rrev$fit + p0C.pred_ln_wheat_rrev$res + p0C.pred_ln_wheat_rrev$effect)))
p.wheat.rev1 <- (sum( exp(p1C.pred_ln_wheat_rrev$fit + p1C.pred_ln_wheat_rrev$res + p1C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100
p.wheat.rev1.min <- (sum( exp(p1C.pred_ln_wheat_rrev$fit - c(p1C.pred_ln_wheat_rrev_se*1.96) + p1C.pred_ln_wheat_rrev$res + p1C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100
p.wheat.rev1.max <- (sum( exp(p1C.pred_ln_wheat_rrev$fit + c(p1C.pred_ln_wheat_rrev_se*1.96) + p1C.pred_ln_wheat_rrev$res + p1C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100

p.wheat.rev2 <- (sum( exp(p2C.pred_ln_wheat_rrev$fit + p2C.pred_ln_wheat_rrev$res + p2C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100
p.wheat.rev2.min <- (sum( exp(p2C.pred_ln_wheat_rrev$fit - c(p2C.pred_ln_wheat_rrev_se*1.96) + p2C.pred_ln_wheat_rrev$res + p2C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100
p.wheat.rev2.max <- (sum( exp(p2C.pred_ln_wheat_rrev$fit + c(p2C.pred_ln_wheat_rrev_se*1.96) + p2C.pred_ln_wheat_rrev$res + p2C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100

p.wheat.rev3 <- (sum( exp(p3C.pred_ln_wheat_rrev$fit + p3C.pred_ln_wheat_rrev$res + p3C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100
p.wheat.rev3.min <- (sum( exp(p3C.pred_ln_wheat_rrev$fit - c(p3C.pred_ln_wheat_rrev_se*1.96) + p3C.pred_ln_wheat_rrev$res + p3C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100
p.wheat.rev3.max <- (sum( exp(p3C.pred_ln_wheat_rrev$fit + c(p3C.pred_ln_wheat_rrev_se*1.96) + p3C.pred_ln_wheat_rrev$res + p3C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100

p.wheat.rev4 <- (sum( exp(p4C.pred_ln_wheat_rrev$fit + p4C.pred_ln_wheat_rrev$res + p4C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100
p.wheat.rev4.min <- (sum( exp(p4C.pred_ln_wheat_rrev$fit - c(p4C.pred_ln_wheat_rrev_se*1.96) + p4C.pred_ln_wheat_rrev$res + p4C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100
p.wheat.rev4.max <- (sum( exp(p4C.pred_ln_wheat_rrev$fit + c(p4C.pred_ln_wheat_rrev_se*1.96) + p4C.pred_ln_wheat_rrev$res + p4C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100

p.wheat.rev5 <- (sum( exp(p5C.pred_ln_wheat_rrev$fit + p5C.pred_ln_wheat_rrev$res + p5C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100
p.wheat.rev5.min <- (sum( exp(p5C.pred_ln_wheat_rrev$fit - c(p5C.pred_ln_wheat_rrev_se*1.96) + p5C.pred_ln_wheat_rrev$res + p5C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100
p.wheat.rev5.max <- (sum( exp(p5C.pred_ln_wheat_rrev$fit + c(p5C.pred_ln_wheat_rrev_se*1.96) + p5C.pred_ln_wheat_rrev$res + p5C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100

diff.wheat.rev0 <- (sum( exp(diff0C.pred_ln_wheat_rrev$fit + diff0C.pred_ln_wheat_rrev$res + diff1C.pred_ln_wheat_rrev$effect )))
diff.wheat.rev1 <- (sum( exp(diff1C.pred_ln_wheat_rrev$fit + diff1C.pred_ln_wheat_rrev$res + diff1C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100
diff.wheat.rev1.min <- (sum( exp(diff1C.pred_ln_wheat_rrev$fit - c(diff1C.pred_ln_wheat_rrev_se*1.96) + diff1C.pred_ln_wheat_rrev$res + diff1C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100
diff.wheat.rev1.max <- (sum( exp(diff1C.pred_ln_wheat_rrev$fit + c(diff1C.pred_ln_wheat_rrev_se*1.96) + diff1C.pred_ln_wheat_rrev$res + diff1C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100

diff.wheat.rev2 <- (sum( exp(diff2C.pred_ln_wheat_rrev$fit + diff2C.pred_ln_wheat_rrev$res + diff2C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100
diff.wheat.rev2.min <- (sum( exp(diff2C.pred_ln_wheat_rrev$fit - c(diff2C.pred_ln_wheat_rrev_se*1.96) + diff2C.pred_ln_wheat_rrev$res + diff2C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100
diff.wheat.rev2.max <- (sum( exp(diff2C.pred_ln_wheat_rrev$fit + c(diff2C.pred_ln_wheat_rrev_se*1.96) + diff2C.pred_ln_wheat_rrev$res + diff2C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100

diff.wheat.rev3 <- (sum( exp(diff3C.pred_ln_wheat_rrev$fit + diff3C.pred_ln_wheat_rrev$res + diff3C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100
diff.wheat.rev3.min <- (sum( exp(diff3C.pred_ln_wheat_rrev$fit - c(diff3C.pred_ln_wheat_rrev_se*1.96) + diff3C.pred_ln_wheat_rrev$res + diff3C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100
diff.wheat.rev3.max <- (sum( exp(diff3C.pred_ln_wheat_rrev$fit + c(diff3C.pred_ln_wheat_rrev_se*1.96) + diff3C.pred_ln_wheat_rrev$res + diff3C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100

diff.wheat.rev4 <- (sum( exp(diff4C.pred_ln_wheat_rrev$fit + diff4C.pred_ln_wheat_rrev$res + diff4C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100
diff.wheat.rev4.min <- (sum( exp(diff4C.pred_ln_wheat_rrev$fit - c(diff4C.pred_ln_wheat_rrev_se*1.96) + diff4C.pred_ln_wheat_rrev$res + diff4C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100
diff.wheat.rev4.max <- (sum( exp(diff4C.pred_ln_wheat_rrev$fit + c(diff4C.pred_ln_wheat_rrev_se*1.96) + diff4C.pred_ln_wheat_rrev$res + diff4C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100

diff.wheat.rev5 <- (sum( exp(diff5C.pred_ln_wheat_rrev$fit + diff5C.pred_ln_wheat_rrev$res + diff5C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100
diff.wheat.rev5.min <- (sum( exp(diff5C.pred_ln_wheat_rrev$fit - c(diff5C.pred_ln_wheat_rrev_se*1.96) + diff5C.pred_ln_wheat_rrev$res + diff5C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100
diff.wheat.rev5.max <- (sum( exp(diff5C.pred_ln_wheat_rrev$fit + c(diff5C.pred_ln_wheat_rrev_se*1.96) + diff5C.pred_ln_wheat_rrev$res + diff5C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100

wheat.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.wheat.rev1, cs.wheat.rev2, cs.wheat.rev3, cs.wheat.rev4, cs.wheat.rev5,
                                   p.wheat.rev1, p.wheat.rev2, p.wheat.rev3, p.wheat.rev4, p.wheat.rev5,
                                   diff.wheat.rev1, diff.wheat.rev2, diff.wheat.rev3, diff.wheat.rev4, diff.wheat.rev5),
                           min = c(cs.wheat.rev1.min, cs.wheat.rev2.min, cs.wheat.rev3.min, cs.wheat.rev4.min, cs.wheat.rev5.min,
                                   p.wheat.rev1.min, p.wheat.rev2.min, p.wheat.rev3.min, p.wheat.rev4.min, p.wheat.rev5.min,
                                   diff.wheat.rev1.min, diff.wheat.rev2.min, diff.wheat.rev3.min, diff.wheat.rev4.min, diff.wheat.rev5.min),
                           max = c(cs.wheat.rev1.max, cs.wheat.rev2.max, cs.wheat.rev3.max, cs.wheat.rev4.max, cs.wheat.rev5.max,
                                   p.wheat.rev1.max, p.wheat.rev2.max, p.wheat.rev3.max, p.wheat.rev4.max, p.wheat.rev5.max,
                                   diff.wheat.rev1.max, diff.wheat.rev2.max, diff.wheat.rev3.max, diff.wheat.rev4.max, diff.wheat.rev5.max),
                           reg = rep(c("cross-section", "panel", "diff"), each = 5),
                           crop = "wheat")

ggplot(wheat.plotdat, aes(temp, rev, group = reg)) + 
  #geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5", alpha = 0.5) + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")

# soybean

cs0C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_0C)
cs0C.pred_ln_soybean_rrev_se <- cc.pred.se(cs.ln_soybean_rrev$X - cs.ln_soybean_rrev_0C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_soybean_rrev$clustervcv, cs.ln_soybean_rrev$weights^2)

cs1C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_1C)
cs1C.pred_ln_soybean_rrev_se <- cc.pred.se(cs.ln_soybean_rrev$X - cs.ln_soybean_rrev_1C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_soybean_rrev$clustervcv, cs.ln_soybean_rrev$weights^2)
cs2C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_2C)
cs2C.pred_ln_soybean_rrev_se <- cc.pred.se(cs.ln_soybean_rrev$X - cs.ln_soybean_rrev_2C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_soybean_rrev$clustervcv, cs.ln_soybean_rrev$weights^2)
cs3C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_3C)
cs3C.pred_ln_soybean_rrev_se <- cc.pred.se(cs.ln_soybean_rrev$X - cs.ln_soybean_rrev_3C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_soybean_rrev$clustervcv, cs.ln_soybean_rrev$weights^2)
cs4C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_4C)
cs4C.pred_ln_soybean_rrev_se <- cc.pred.se(cs.ln_soybean_rrev$X - cs.ln_soybean_rrev_4C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_soybean_rrev$clustervcv, cs.ln_soybean_rrev$weights^2)
cs5C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_5C)
cs5C.pred_ln_soybean_rrev_se <- cc.pred.se(cs.ln_soybean_rrev$X - cs.ln_soybean_rrev_5C[, c("(Intercept)", "dday0_10", "dday10_30", "dday30C", "prec", "prec_sq", "lat", "long", "lat:long")], 
                                        cs.ln_soybean_rrev$clustervcv, cs.ln_soybean_rrev$weights^2)

p0C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_0C)
p0C.pred_ln_soybean_rrev_se <- cc.pred.se(p.ln_soybean_rrev$X - p.ln_soybean_rrev_0C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_soybean_rrev$clustervcv, p.ln_soybean_rrev$weights^2)

p1C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_1C)
p1C.pred_ln_soybean_rrev_se <- cc.pred.se(p.ln_soybean_rrev$X - p.ln_soybean_rrev_1C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_soybean_rrev$clustervcv, p.ln_soybean_rrev$weights^2)
p2C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_2C)
p2C.pred_ln_soybean_rrev_se <- cc.pred.se(p.ln_soybean_rrev$X - p.ln_soybean_rrev_2C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_soybean_rrev$clustervcv, p.ln_soybean_rrev$weights^2)
p3C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_3C)
p3C.pred_ln_soybean_rrev_se <- cc.pred.se(p.ln_soybean_rrev$X - p.ln_soybean_rrev_3C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_soybean_rrev$clustervcv, p.ln_soybean_rrev$weights^2)
p4C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_4C)
p4C.pred_ln_soybean_rrev_se <- cc.pred.se(p.ln_soybean_rrev$X - p.ln_soybean_rrev_4C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_soybean_rrev$clustervcv, p.ln_soybean_rrev$weights^2)
p5C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_5C)
p5C.pred_ln_soybean_rrev_se <- cc.pred.se(p.ln_soybean_rrev$X - p.ln_soybean_rrev_5C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                       p.ln_soybean_rrev$clustervcv, p.ln_soybean_rrev$weights^2)

diff0C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_0C)
diff1C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_1C)
diff1C.pred_ln_soybean_rrev_se <- cc.pred.se(diff.ln_soybean_rrev$X - diff.ln_soybean_rrev_1C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_soybean_rrev$clustervcv, diff.ln_soybean_rrev$weights^2)
diff2C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_2C)
diff2C.pred_ln_soybean_rrev_se <- cc.pred.se(diff.ln_soybean_rrev$X - diff.ln_soybean_rrev_2C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_soybean_rrev$clustervcv, diff.ln_soybean_rrev$weights^2)
diff3C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_3C)
diff3C.pred_ln_soybean_rrev_se <- cc.pred.se(diff.ln_soybean_rrev$X - diff.ln_soybean_rrev_3C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_soybean_rrev$clustervcv, diff.ln_soybean_rrev$weights^2)
diff4C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_4C)
diff4C.pred_ln_soybean_rrev_se <- cc.pred.se(diff.ln_soybean_rrev$X - diff.ln_soybean_rrev_4C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_soybean_rrev$clustervcv, diff.ln_soybean_rrev$weights^2)
diff5C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_5C)
diff5C.pred_ln_soybean_rrev_se <- cc.pred.se(diff.ln_soybean_rrev$X - diff.ln_soybean_rrev_5C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
                                          diff.ln_soybean_rrev$clustervcv, diff.ln_soybean_rrev$weights^2)


cs.rev1C <- exp(cs1C.pred_ln_soybean_rrev$fit)
cs.rev2C <- exp(cs2C.pred_ln_soybean_rrev$fit + cs2C.pred_ln_soybean_rrev$res )
cs.rev3C <- exp(cs3C.pred_ln_soybean_rrev$fit + cs3C.pred_ln_soybean_rrev$res )
cs.rev4C <- exp(cs4C.pred_ln_soybean_rrev$fit + cs4C.pred_ln_soybean_rrev$res )
cs.rev5C <- exp(cs5C.pred_ln_soybean_rrev$fit + cs5C.pred_ln_soybean_rrev$res )

p.rev1C <- exp(p1C.pred_ln_soybean_rrev$fit + p1C.pred_ln_soybean_rrev$res + p1C.pred_ln_soybean_rrev$effect)
p.rev2C <- exp(p2C.pred_ln_soybean_rrev$fit + p2C.pred_ln_soybean_rrev$res + p2C.pred_ln_soybean_rrev$effect)
p.rev3C <- exp(p3C.pred_ln_soybean_rrev$fit + p3C.pred_ln_soybean_rrev$res + p3C.pred_ln_soybean_rrev$effect)
p.rev4C <- exp(p4C.pred_ln_soybean_rrev$fit + p4C.pred_ln_soybean_rrev$res + p4C.pred_ln_soybean_rrev$effect)
p.rev5C <- exp(p5C.pred_ln_soybean_rrev$fit + p5C.pred_ln_soybean_rrev$res + p5C.pred_ln_soybean_rrev$effect)

diff.rev1C <- exp(diff1C.pred_ln_soybean_rrev$fit + diff1C.pred_ln_soybean_rrev$res + diff1C.pred_ln_soybean_rrev$effect)
diff.rev2C <- exp(diff2C.pred_ln_soybean_rrev$fit + diff2C.pred_ln_soybean_rrev$res + diff2C.pred_ln_soybean_rrev$effect)
diff.rev3C <- exp(diff3C.pred_ln_soybean_rrev$fit + diff3C.pred_ln_soybean_rrev$res + diff3C.pred_ln_soybean_rrev$effect)
diff.rev4C <- exp(diff4C.pred_ln_soybean_rrev$fit + diff4C.pred_ln_soybean_rrev$res + diff4C.pred_ln_soybean_rrev$effect)
diff.rev5C <- exp(diff5C.pred_ln_soybean_rrev$fit + diff5C.pred_ln_soybean_rrev$res + diff5C.pred_ln_soybean_rrev$effect)

cs.rev <- data.frame(rev = c(exp(cs.ln_soybean_rrev), cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
                     change = rep(c("Base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))

p.rev <- data.frame(rev = c(p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
                    change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))

diff.rev <- data.frame(rev = c(diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
                       change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))


ggplot(cs.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")
ggplot(filter(p.rev, rev < 50), aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
ggplot(diff.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")



cs.soybean.rev0 <- (sum( exp(cs0C.pred_ln_soybean_rrev$fit + cs0C.pred_ln_soybean_rrev$res)))
cs.soybean.rev1 <- (sum( exp(cs1C.pred_ln_soybean_rrev$fit + cs1C.pred_ln_soybean_rrev$res  ))/cs.soybean.rev0 - 1)*100
cs.soybean.rev1.min <- (sum( exp(cs1C.pred_ln_soybean_rrev$fit - c(cs1C.pred_ln_soybean_rrev_se*1.96) + cs1C.pred_ln_soybean_rrev$res ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100
cs.soybean.rev1.max <- (sum( exp(cs1C.pred_ln_soybean_rrev$fit + c(cs1C.pred_ln_soybean_rrev_se*1.96) + cs1C.pred_ln_soybean_rrev$res  ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100

cs.soybean.rev2 <- (sum( exp(cs2C.pred_ln_soybean_rrev$fit + cs2C.pred_ln_soybean_rrev$res  ))/cs.soybean.rev0 - 1)*100
cs.soybean.rev2.min <- (sum( exp(cs2C.pred_ln_soybean_rrev$fit - c(cs2C.pred_ln_soybean_rrev_se*1.96) + cs2C.pred_ln_soybean_rrev$res  ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100
cs.soybean.rev2.max <- (sum( exp(cs2C.pred_ln_soybean_rrev$fit + c(cs2C.pred_ln_soybean_rrev_se*1.96) + cs2C.pred_ln_soybean_rrev$res  ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100

cs.soybean.rev3 <- (sum( exp(cs3C.pred_ln_soybean_rrev$fit + cs3C.pred_ln_soybean_rrev$res  ))/cs.soybean.rev0 - 1)*100
cs.soybean.rev3.min <- (sum( exp(cs3C.pred_ln_soybean_rrev$fit - c(cs3C.pred_ln_soybean_rrev_se*1.96) + cs3C.pred_ln_soybean_rrev$res  ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100
cs.soybean.rev3.max <- (sum( exp(cs3C.pred_ln_soybean_rrev$fit + c(cs3C.pred_ln_soybean_rrev_se*1.96) + cs3C.pred_ln_soybean_rrev$res  ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100

cs.soybean.rev4 <- (sum( exp(cs4C.pred_ln_soybean_rrev$fit + cs4C.pred_ln_soybean_rrev$res  ))/cs.soybean.rev0 - 1)*100
cs.soybean.rev4.min <- (sum( exp(cs4C.pred_ln_soybean_rrev$fit - c(cs4C.pred_ln_soybean_rrev_se*1.96) + cs4C.pred_ln_soybean_rrev$res  ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100
cs.soybean.rev4.max <- (sum( exp(cs4C.pred_ln_soybean_rrev$fit + c(cs4C.pred_ln_soybean_rrev_se*1.96) + cs4C.pred_ln_soybean_rrev$res  ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100

cs.soybean.rev5 <- (sum( exp(cs5C.pred_ln_soybean_rrev$fit + cs5C.pred_ln_soybean_rrev$res  ))/cs.soybean.rev0 - 1)*100
cs.soybean.rev5.min <- (sum( exp(cs5C.pred_ln_soybean_rrev$fit - c(cs5C.pred_ln_soybean_rrev_se*1.96) + cs5C.pred_ln_soybean_rrev$res  ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100
cs.soybean.rev5.max <- (sum( exp(cs5C.pred_ln_soybean_rrev$fit + c(cs5C.pred_ln_soybean_rrev_se*1.96) + cs5C.pred_ln_soybean_rrev$res  ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100


p.soybean.rev0 <- (sum( exp(p0C.pred_ln_soybean_rrev$fit + p0C.pred_ln_soybean_rrev$res + p0C.pred_ln_soybean_rrev$effect)))
p.soybean.rev1 <- (sum( exp(p1C.pred_ln_soybean_rrev$fit + p1C.pred_ln_soybean_rrev$res + p1C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100
p.soybean.rev1.min <- (sum( exp(p1C.pred_ln_soybean_rrev$fit - c(p1C.pred_ln_soybean_rrev_se*1.96) + p1C.pred_ln_soybean_rrev$res + p1C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100
p.soybean.rev1.max <- (sum( exp(p1C.pred_ln_soybean_rrev$fit + c(p1C.pred_ln_soybean_rrev_se*1.96) + p1C.pred_ln_soybean_rrev$res + p1C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100

p.soybean.rev2 <- (sum( exp(p2C.pred_ln_soybean_rrev$fit + p2C.pred_ln_soybean_rrev$res + p2C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100
p.soybean.rev2.min <- (sum( exp(p2C.pred_ln_soybean_rrev$fit - c(p2C.pred_ln_soybean_rrev_se*1.96) + p2C.pred_ln_soybean_rrev$res + p2C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100
p.soybean.rev2.max <- (sum( exp(p2C.pred_ln_soybean_rrev$fit + c(p2C.pred_ln_soybean_rrev_se*1.96) + p2C.pred_ln_soybean_rrev$res + p2C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100

p.soybean.rev3 <- (sum( exp(p3C.pred_ln_soybean_rrev$fit + p3C.pred_ln_soybean_rrev$res + p3C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100
p.soybean.rev3.min <- (sum( exp(p3C.pred_ln_soybean_rrev$fit - c(p3C.pred_ln_soybean_rrev_se*1.96) + p3C.pred_ln_soybean_rrev$res + p3C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100
p.soybean.rev3.max <- (sum( exp(p3C.pred_ln_soybean_rrev$fit + c(p3C.pred_ln_soybean_rrev_se*1.96) + p3C.pred_ln_soybean_rrev$res + p3C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100

p.soybean.rev4 <- (sum( exp(p4C.pred_ln_soybean_rrev$fit + p4C.pred_ln_soybean_rrev$res + p4C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100
p.soybean.rev4.min <- (sum( exp(p4C.pred_ln_soybean_rrev$fit - c(p4C.pred_ln_soybean_rrev_se*1.96) + p4C.pred_ln_soybean_rrev$res + p4C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100
p.soybean.rev4.max <- (sum( exp(p4C.pred_ln_soybean_rrev$fit + c(p4C.pred_ln_soybean_rrev_se*1.96) + p4C.pred_ln_soybean_rrev$res + p4C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100

p.soybean.rev5 <- (sum( exp(p5C.pred_ln_soybean_rrev$fit + p5C.pred_ln_soybean_rrev$res + p5C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100
p.soybean.rev5.min <- (sum( exp(p5C.pred_ln_soybean_rrev$fit - c(p5C.pred_ln_soybean_rrev_se*1.96) + p5C.pred_ln_soybean_rrev$res + p5C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100
p.soybean.rev5.max <- (sum( exp(p5C.pred_ln_soybean_rrev$fit + c(p5C.pred_ln_soybean_rrev_se*1.96) + p5C.pred_ln_soybean_rrev$res + p5C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100

diff.soybean.rev0 <- (sum( exp(diff0C.pred_ln_soybean_rrev$fit + diff0C.pred_ln_soybean_rrev$res + diff1C.pred_ln_soybean_rrev$effect )))
diff.soybean.rev1 <- (sum( exp(diff1C.pred_ln_soybean_rrev$fit + diff1C.pred_ln_soybean_rrev$res + diff1C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100
diff.soybean.rev1.min <- (sum( exp(diff1C.pred_ln_soybean_rrev$fit - c(diff1C.pred_ln_soybean_rrev_se*1.96) + diff1C.pred_ln_soybean_rrev$res + diff1C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100
diff.soybean.rev1.max <- (sum( exp(diff1C.pred_ln_soybean_rrev$fit + c(diff1C.pred_ln_soybean_rrev_se*1.96) + diff1C.pred_ln_soybean_rrev$res + diff1C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100

diff.soybean.rev2 <- (sum( exp(diff2C.pred_ln_soybean_rrev$fit + diff2C.pred_ln_soybean_rrev$res + diff2C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100
diff.soybean.rev2.min <- (sum( exp(diff2C.pred_ln_soybean_rrev$fit - c(diff2C.pred_ln_soybean_rrev_se*1.96) + diff2C.pred_ln_soybean_rrev$res + diff2C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100
diff.soybean.rev2.max <- (sum( exp(diff2C.pred_ln_soybean_rrev$fit + c(diff2C.pred_ln_soybean_rrev_se*1.96) + diff2C.pred_ln_soybean_rrev$res + diff2C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100

diff.soybean.rev3 <- (sum( exp(diff3C.pred_ln_soybean_rrev$fit + diff3C.pred_ln_soybean_rrev$res + diff3C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100
diff.soybean.rev3.min <- (sum( exp(diff3C.pred_ln_soybean_rrev$fit - c(diff3C.pred_ln_soybean_rrev_se*1.96) + diff3C.pred_ln_soybean_rrev$res + diff3C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100
diff.soybean.rev3.max <- (sum( exp(diff3C.pred_ln_soybean_rrev$fit + c(diff3C.pred_ln_soybean_rrev_se*1.96) + diff3C.pred_ln_soybean_rrev$res + diff3C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100

diff.soybean.rev4 <- (sum( exp(diff4C.pred_ln_soybean_rrev$fit + diff4C.pred_ln_soybean_rrev$res + diff4C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100
diff.soybean.rev4.min <- (sum( exp(diff4C.pred_ln_soybean_rrev$fit - c(diff4C.pred_ln_soybean_rrev_se*1.96) + diff4C.pred_ln_soybean_rrev$res + diff4C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100
diff.soybean.rev4.max <- (sum( exp(diff4C.pred_ln_soybean_rrev$fit + c(diff4C.pred_ln_soybean_rrev_se*1.96) + diff4C.pred_ln_soybean_rrev$res + diff4C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100

diff.soybean.rev5 <- (sum( exp(diff5C.pred_ln_soybean_rrev$fit + diff5C.pred_ln_soybean_rrev$res + diff5C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100
diff.soybean.rev5.min <- (sum( exp(diff5C.pred_ln_soybean_rrev$fit - c(diff5C.pred_ln_soybean_rrev_se*1.96) + diff5C.pred_ln_soybean_rrev$res + diff5C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100
diff.soybean.rev5.max <- (sum( exp(diff5C.pred_ln_soybean_rrev$fit + c(diff5C.pred_ln_soybean_rrev_se*1.96) + diff5C.pred_ln_soybean_rrev$res + diff5C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100

soybean.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.soybean.rev1, cs.soybean.rev2, cs.soybean.rev3, cs.soybean.rev4, cs.soybean.rev5,
                                   p.soybean.rev1, p.soybean.rev2, p.soybean.rev3, p.soybean.rev4, p.soybean.rev5,
                                   diff.soybean.rev1, diff.soybean.rev2, diff.soybean.rev3, diff.soybean.rev4, diff.soybean.rev5),
                           min = c(cs.soybean.rev1.min, cs.soybean.rev2.min, cs.soybean.rev3.min, cs.soybean.rev4.min, cs.soybean.rev5.min,
                                   p.soybean.rev1.min, p.soybean.rev2.min, p.soybean.rev3.min, p.soybean.rev4.min, p.soybean.rev5.min,
                                   diff.soybean.rev1.min, diff.soybean.rev2.min, diff.soybean.rev3.min, diff.soybean.rev4.min, diff.soybean.rev5.min),
                           max = c(cs.soybean.rev1.max, cs.soybean.rev2.max, cs.soybean.rev3.max, cs.soybean.rev4.max, cs.soybean.rev5.max,
                                   p.soybean.rev1.max, p.soybean.rev2.max, p.soybean.rev3.max, p.soybean.rev4.max, p.soybean.rev5.max,
                                   diff.soybean.rev1.max, diff.soybean.rev2.max, diff.soybean.rev3.max, diff.soybean.rev4.max, diff.soybean.rev5.max),
                           reg = rep(c("cross-section", "panel", "diff"), each = 5),
                           crop = "soybean")

ggplot(soybean.plotdat, aes(temp, rev, group = reg)) + 
  #geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5", alpha = 0.5) + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")


#  Full Plot --------------------------------------------------------------


# Merge data
plotdat <- rbind(wheat.plotdat[,c(1,2,5,6)], wheat.plotdat, hay.plotdat, wheat.plotdat, soybean.plotdat[, c(1,2,5,6)])
plotdat <- rbind(corn.plotdat, cotton.plotdat, hay.plotdat, wheat.plotdat, soybean.plotdat)
plotdat2 <- rbind(wheat.plotdat, soybean.plotdat)

p1 <- ggplot(plotdat, aes(temp, rev, color = reg)) + 
  geom_line() + ylab("Impact (% Change) ") + 
  xlab("Change in Temperature (C)") + geom_hline(yintercept = 0, linetype = "dashed")+
  facet_wrap(~crop)

ggplot(filter(plotdat2, crop %in% c("corn", "soybean")), aes(temp, rev, color = reg)) + 
  geom_line() + ylab("Impact (% Change) ") + geom_line(aes(temp, max), linetype = "dashed") +
  xlab("Change in Temperature (C)") + geom_hline(yintercept = 0, linetype = "dashed")+
  facet_wrap(~crop)

p2 <- ggplot(filter(plotdat, crop %in% c("corn", "cotton", "hay", "soybean")), aes(temp, rev, color = reg)) + 
  geom_line() + ylab("Impact (% Change) ") + 
  xlab("Change in Temperature (C)") + geom_hline(yintercept = 0, linetype = "dashed")+
  facet_wrap(~crop)
p2

plot_grid(p1, p2, ncol = 1)
