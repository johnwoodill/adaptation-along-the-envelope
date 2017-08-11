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
cs.ln_corn_rrev <- readRDS("models/cs.dd.ln_corn_rrev")
p.ln_corn_rrev <- readRDS("models/p.dd.ln_corn_rrev")
diff.ln_corn_rrev <- readRDS("models/diff.dd.ln_corn_rrev")

cs.ln_cotton_rrev <- readRDS("models/cs.dd.ln_cotton_rrev")
p.ln_cotton_rrev <- readRDS("models/p.dd.ln_cotton_rrev")
diff.ln_cotton_rrev <- readRDS("models/diff.dd.ln_cotton_rrev")

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
cs.ln_corn_rrev_1C <- filter(cs.1C, !is.na(ln_corn_rrev))
cs.ln_corn_rrev_2C <- filter(cs.2C, !is.na(ln_corn_rrev))
cs.ln_corn_rrev_3C <- filter(cs.3C, !is.na(ln_corn_rrev))
cs.ln_corn_rrev_4C <- filter(cs.4C, !is.na(ln_corn_rrev))
cs.ln_corn_rrev_5C <- filter(cs.5C, !is.na(ln_corn_rrev))

cs.ln_cotton_rrev_1C <- filter(cs.1C, !is.na(ln_cotton_rrev))
cs.ln_cotton_rrev_2C <- filter(cs.2C, !is.na(ln_cotton_rrev))
cs.ln_cotton_rrev_3C <- filter(cs.3C, !is.na(ln_cotton_rrev))
cs.ln_cotton_rrev_4C <- filter(cs.4C, !is.na(ln_cotton_rrev))
cs.ln_cotton_rrev_5C <- filter(cs.5C, !is.na(ln_cotton_rrev))

cs.ln_hay_rrev_1C <- filter(cs.1C, !is.na(ln_hay_rrev))
cs.ln_hay_rrev_2C <- filter(cs.2C, !is.na(ln_hay_rrev))
cs.ln_hay_rrev_3C <- filter(cs.3C, !is.na(ln_hay_rrev))
cs.ln_hay_rrev_4C <- filter(cs.4C, !is.na(ln_hay_rrev))
cs.ln_hay_rrev_5C <- filter(cs.5C, !is.na(ln_hay_rrev))

cs.ln_wheat_rrev_1C <- filter(cs.1C, !is.na(ln_wheat_rrev))
cs.ln_wheat_rrev_2C <- filter(cs.2C, !is.na(ln_wheat_rrev))
cs.ln_wheat_rrev_3C <- filter(cs.3C, !is.na(ln_wheat_rrev))
cs.ln_wheat_rrev_4C <- filter(cs.4C, !is.na(ln_wheat_rrev))
cs.ln_wheat_rrev_5C <- filter(cs.5C, !is.na(ln_wheat_rrev))

cs.ln_soybean_rrev_1C <- filter(cs.1C, !is.na(ln_soybean_rrev))
cs.ln_soybean_rrev_2C <- filter(cs.2C, !is.na(ln_soybean_rrev))
cs.ln_soybean_rrev_3C <- filter(cs.3C, !is.na(ln_soybean_rrev))
cs.ln_soybean_rrev_4C <- filter(cs.4C, !is.na(ln_soybean_rrev))
cs.ln_soybean_rrev_5C <- filter(cs.5C, !is.na(ln_soybean_rrev))

# Panel data
p.ln_corn_rrev_1C <- filter(p.1C, !is.na(ln_corn_rrev))
p.ln_corn_rrev_2C <- filter(p.2C, !is.na(ln_corn_rrev))
p.ln_corn_rrev_3C <- filter(p.3C, !is.na(ln_corn_rrev))
p.ln_corn_rrev_4C <- filter(p.4C, !is.na(ln_corn_rrev))
p.ln_corn_rrev_5C <- filter(p.5C, !is.na(ln_corn_rrev))

p.ln_cotton_rrev_1C <- filter(p.1C, !is.na(ln_cotton_rrev))
p.ln_cotton_rrev_2C <- filter(p.2C, !is.na(ln_cotton_rrev))
p.ln_cotton_rrev_3C <- filter(p.3C, !is.na(ln_cotton_rrev))
p.ln_cotton_rrev_4C <- filter(p.4C, !is.na(ln_cotton_rrev))
p.ln_cotton_rrev_5C <- filter(p.5C, !is.na(ln_cotton_rrev))

p.ln_hay_rrev_1C <- filter(p.1C, !is.na(ln_hay_rrev))
p.ln_hay_rrev_2C <- filter(p.2C, !is.na(ln_hay_rrev))
p.ln_hay_rrev_3C <- filter(p.3C, !is.na(ln_hay_rrev))
p.ln_hay_rrev_4C <- filter(p.4C, !is.na(ln_hay_rrev))
p.ln_hay_rrev_5C <- filter(p.5C, !is.na(ln_hay_rrev))

p.ln_wheat_rrev_1C <- filter(p.1C, !is.na(ln_wheat_rrev))
p.ln_wheat_rrev_2C <- filter(p.2C, !is.na(ln_wheat_rrev))
p.ln_wheat_rrev_3C <- filter(p.3C, !is.na(ln_wheat_rrev))
p.ln_wheat_rrev_4C <- filter(p.4C, !is.na(ln_wheat_rrev))
p.ln_wheat_rrev_5C <- filter(p.5C, !is.na(ln_wheat_rrev))

p.ln_soybean_rrev_1C <- filter(p.1C, !is.na(ln_soybean_rrev))
p.ln_soybean_rrev_2C <- filter(p.2C, !is.na(ln_soybean_rrev))
p.ln_soybean_rrev_3C <- filter(p.3C, !is.na(ln_soybean_rrev))
p.ln_soybean_rrev_4C <- filter(p.4C, !is.na(ln_soybean_rrev))
p.ln_soybean_rrev_5C <- filter(p.5C, !is.na(ln_soybean_rrev))

# Diff data
diff.ln_corn_rrev_1C <- filter(diff.1C, !is.na(ln_corn_rrev))
diff.ln_corn_rrev_2C <- filter(diff.2C, !is.na(ln_corn_rrev))
diff.ln_corn_rrev_3C <- filter(diff.3C, !is.na(ln_corn_rrev))
diff.ln_corn_rrev_4C <- filter(diff.4C, !is.na(ln_corn_rrev))
diff.ln_corn_rrev_5C <- filter(diff.5C, !is.na(ln_corn_rrev))

diff.ln_cotton_rrev_1C <- filter(diff.1C, !is.na(ln_cotton_rrev))
diff.ln_cotton_rrev_2C <- filter(diff.2C, !is.na(ln_cotton_rrev))
diff.ln_cotton_rrev_3C <- filter(diff.3C, !is.na(ln_cotton_rrev))
diff.ln_cotton_rrev_4C <- filter(diff.4C, !is.na(ln_cotton_rrev))
diff.ln_cotton_rrev_5C <- filter(diff.5C, !is.na(ln_cotton_rrev))

diff.ln_hay_rrev_1C <- filter(diff.1C, !is.na(ln_hay_rrev))
diff.ln_hay_rrev_2C <- filter(diff.2C, !is.na(ln_hay_rrev))
diff.ln_hay_rrev_3C <- filter(diff.3C, !is.na(ln_hay_rrev))
diff.ln_hay_rrev_4C <- filter(diff.4C, !is.na(ln_hay_rrev))
diff.ln_hay_rrev_5C <- filter(diff.5C, !is.na(ln_hay_rrev))

diff.ln_wheat_rrev_1C <- filter(diff.1C, !is.na(ln_wheat_rrev))
diff.ln_wheat_rrev_2C <- filter(diff.2C, !is.na(ln_wheat_rrev))
diff.ln_wheat_rrev_3C <- filter(diff.3C, !is.na(ln_wheat_rrev))
diff.ln_wheat_rrev_4C <- filter(diff.4C, !is.na(ln_wheat_rrev))
diff.ln_wheat_rrev_5C <- filter(diff.5C, !is.na(ln_wheat_rrev))

diff.ln_soybean_rrev_1C <- filter(diff.1C, !is.na(ln_soybean_rrev))
diff.ln_soybean_rrev_2C <- filter(diff.2C, !is.na(ln_soybean_rrev))
diff.ln_soybean_rrev_3C <- filter(diff.3C, !is.na(ln_soybean_rrev))
diff.ln_soybean_rrev_4C <- filter(diff.4C, !is.na(ln_soybean_rrev))
diff.ln_soybean_rrev_5C <- filter(diff.5C, !is.na(ln_soybean_rrev))


###########################
# Predictions

# Corn
{
cs1C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_1C)
cs1C.pred_ln_corn_rrev_se <- cc.pred.se(cs.ln_corn_rrev$X - cs.ln_corn_rrev_1C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           cs.ln_corn_rrev$clustervcv, cs.ln_corn_rrev$weights^2)
cs2C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_2C)
cs2C.pred_ln_corn_rrev_se <- cc.pred.se(cs.ln_corn_rrev$X - cs.ln_corn_rrev_2C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           cs.ln_corn_rrev$clustervcv, cs.ln_corn_rrev$weights^2)
cs3C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_3C)
cs3C.pred_ln_corn_rrev_se <- cc.pred.se(cs.ln_corn_rrev$X - cs.ln_corn_rrev_3C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           cs.ln_corn_rrev$clustervcv, cs.ln_corn_rrev$weights^2)
cs4C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_4C)
cs4C.pred_ln_corn_rrev_se <- cc.pred.se(cs.ln_corn_rrev$X - cs.ln_corn_rrev_4C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           cs.ln_corn_rrev$clustervcv, cs.ln_corn_rrev$weights^2)
cs5C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_5C)
cs5C.pred_ln_corn_rrev_se <- cc.pred.se(cs.ln_corn_rrev$X - cs.ln_corn_rrev_5C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           cs.ln_corn_rrev$clustervcv, cs.ln_corn_rrev$weights^2)

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


cs.corn.rev0 <- sum( exp(cs.ln_corn_rrev$response))
cs.corn.rev1 <- (sum( exp(cs1C.pred_ln_corn_rrev$fit + cs1C.pred_ln_corn_rrev$res + cs1C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev1.min <- (sum( exp(cs1C.pred_ln_corn_rrev$fit - c(cs1C.pred_ln_corn_rrev_se*1.96) + cs1C.pred_ln_corn_rrev$res + cs1C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev1.max <- (sum( exp(cs1C.pred_ln_corn_rrev$fit + c(cs1C.pred_ln_corn_rrev_se*1.96) + cs1C.pred_ln_corn_rrev$res + cs1C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100

cs.corn.rev2 <- (sum( exp(cs2C.pred_ln_corn_rrev$fit + cs2C.pred_ln_corn_rrev$res + cs2C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev2.min <- (sum( exp(cs2C.pred_ln_corn_rrev$fit - c(cs2C.pred_ln_corn_rrev_se*1.96) + cs2C.pred_ln_corn_rrev$res + cs2C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev2.max <- (sum( exp(cs2C.pred_ln_corn_rrev$fit + c(cs2C.pred_ln_corn_rrev_se*1.96) + cs2C.pred_ln_corn_rrev$res + cs2C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100

cs.corn.rev3 <- (sum( exp(cs3C.pred_ln_corn_rrev$fit + cs3C.pred_ln_corn_rrev$res + cs3C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev3.min <- (sum( exp(cs3C.pred_ln_corn_rrev$fit - c(cs3C.pred_ln_corn_rrev_se*1.96) + cs3C.pred_ln_corn_rrev$res + cs3C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev3.max <- (sum( exp(cs3C.pred_ln_corn_rrev$fit + c(cs3C.pred_ln_corn_rrev_se*1.96) + cs3C.pred_ln_corn_rrev$res + cs3C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100

cs.corn.rev4 <- (sum( exp(cs4C.pred_ln_corn_rrev$fit + cs4C.pred_ln_corn_rrev$res + cs4C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev4.min <- (sum( exp(cs4C.pred_ln_corn_rrev$fit - c(cs4C.pred_ln_corn_rrev_se*1.96) + cs4C.pred_ln_corn_rrev$res + cs4C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev4.max <- (sum( exp(cs4C.pred_ln_corn_rrev$fit + c(cs4C.pred_ln_corn_rrev_se*1.96) + cs4C.pred_ln_corn_rrev$res + cs4C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100

cs.corn.rev5 <- (sum( exp(cs5C.pred_ln_corn_rrev$fit + cs5C.pred_ln_corn_rrev$res + cs5C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev5.min <- (sum( exp(cs5C.pred_ln_corn_rrev$fit - c(cs5C.pred_ln_corn_rrev_se*1.96) + cs5C.pred_ln_corn_rrev$res + cs5C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev5.max <- (sum( exp(cs5C.pred_ln_corn_rrev$fit + c(cs5C.pred_ln_corn_rrev_se*1.96) + cs5C.pred_ln_corn_rrev$res + cs5C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100

p.corn.rev0 <- sum( exp(p.ln_corn_rrev$response))
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

diff.corn.rev0 <- sum( exp(diff.ln_corn_rrev$response))
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

ggplot(corn.plotdat, aes(temp, rev, color = reg)) + 
  geom_line() + geom_line(aes(temp, min), linetype = "dashed") + 
  geom_line(aes(temp, max), linetype = "dashed")
}

# cotton
{
cs1C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_1C)
cs1C.pred_ln_cotton_rrev_se <- cc.pred.se(cs.ln_cotton_rrev$X - cs.ln_cotton_rrev_1C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           cs.ln_cotton_rrev$clustervcv, cs.ln_cotton_rrev$weights^2)
cs2C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_2C)
cs2C.pred_ln_cotton_rrev_se <- cc.pred.se(cs.ln_cotton_rrev$X - cs.ln_cotton_rrev_2C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           cs.ln_cotton_rrev$clustervcv, cs.ln_cotton_rrev$weights^2)
cs3C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_3C)
cs3C.pred_ln_cotton_rrev_se <- cc.pred.se(cs.ln_cotton_rrev$X - cs.ln_cotton_rrev_3C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           cs.ln_cotton_rrev$clustervcv, cs.ln_cotton_rrev$weights^2)
cs4C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_4C)
cs4C.pred_ln_cotton_rrev_se <- cc.pred.se(cs.ln_cotton_rrev$X - cs.ln_cotton_rrev_4C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           cs.ln_cotton_rrev$clustervcv, cs.ln_cotton_rrev$weights^2)
cs5C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_5C)
cs5C.pred_ln_cotton_rrev_se <- cc.pred.se(cs.ln_cotton_rrev$X - cs.ln_cotton_rrev_5C[, c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")], 
           cs.ln_cotton_rrev$clustervcv, cs.ln_cotton_rrev$weights^2)

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


cs.cotton.rev0 <- sum( exp(cs.ln_cotton_rrev$response))
cs.cotton.rev1 <- (sum( exp(cs1C.pred_ln_cotton_rrev$fit + cs1C.pred_ln_cotton_rrev$res + cs1C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev1.min <- (sum( exp(cs1C.pred_ln_cotton_rrev$fit - c(cs1C.pred_ln_cotton_rrev_se*1.96) + cs1C.pred_ln_cotton_rrev$res + cs1C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev1.max <- (sum( exp(cs1C.pred_ln_cotton_rrev$fit + c(cs1C.pred_ln_cotton_rrev_se*1.96) + cs1C.pred_ln_cotton_rrev$res + cs1C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100

cs.cotton.rev2 <- (sum( exp(cs2C.pred_ln_cotton_rrev$fit + cs2C.pred_ln_cotton_rrev$res + cs2C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev2.min <- (sum( exp(cs2C.pred_ln_cotton_rrev$fit - c(cs2C.pred_ln_cotton_rrev_se*1.96) + cs2C.pred_ln_cotton_rrev$res + cs2C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev2.max <- (sum( exp(cs2C.pred_ln_cotton_rrev$fit + c(cs2C.pred_ln_cotton_rrev_se*1.96) + cs2C.pred_ln_cotton_rrev$res + cs2C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100

cs.cotton.rev3 <- (sum( exp(cs3C.pred_ln_cotton_rrev$fit + cs3C.pred_ln_cotton_rrev$res + cs3C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev3.min <- (sum( exp(cs3C.pred_ln_cotton_rrev$fit - c(cs3C.pred_ln_cotton_rrev_se*1.96) + cs3C.pred_ln_cotton_rrev$res + cs3C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev3.max <- (sum( exp(cs3C.pred_ln_cotton_rrev$fit + c(cs3C.pred_ln_cotton_rrev_se*1.96) + cs3C.pred_ln_cotton_rrev$res + cs3C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100

cs.cotton.rev4 <- (sum( exp(cs4C.pred_ln_cotton_rrev$fit + cs4C.pred_ln_cotton_rrev$res + cs4C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev4.min <- (sum( exp(cs4C.pred_ln_cotton_rrev$fit - c(cs4C.pred_ln_cotton_rrev_se*1.96) + cs4C.pred_ln_cotton_rrev$res + cs4C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev4.max <- (sum( exp(cs4C.pred_ln_cotton_rrev$fit + c(cs4C.pred_ln_cotton_rrev_se*1.96) + cs4C.pred_ln_cotton_rrev$res + cs4C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100

cs.cotton.rev5 <- (sum( exp(cs5C.pred_ln_cotton_rrev$fit + cs5C.pred_ln_cotton_rrev$res + cs5C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev5.min <- (sum( exp(cs5C.pred_ln_cotton_rrev$fit - c(cs5C.pred_ln_cotton_rrev_se*1.96) + cs5C.pred_ln_cotton_rrev$res + cs5C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev5.max <- (sum( exp(cs5C.pred_ln_cotton_rrev$fit + c(cs5C.pred_ln_cotton_rrev_se*1.96) + cs5C.pred_ln_cotton_rrev$res + cs5C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100

p.cotton.rev0 <- sum( exp(p.ln_cotton_rrev$response))
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

diff.cotton.rev0 <- sum( exp(diff.ln_cotton_rrev$response))
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
diff.cotton.rev5.max <- (sum( exp((diff5C.pred_ln_cotton_rrev$fit + c(diff5C.pred_ln_cotton_rrev_se*1.96)) + diff5C.pred_ln_cotton_rrev$res + diff5C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100

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

ggplot(cotton.plotdat, aes(temp, rev, color = reg)) + 
  geom_line() + geom_line(aes(temp, min), linetype = "dashed") + 
  geom_line(aes(temp, max), linetype = "dashed")
}

# Merge data
plotdat <- rbind(corn.plotdat, cotton.plotdat, hay.plotdat, wheat.plotdat, soybean.plotdat)

ggplot(plotdat, aes(temp, rev, color = reg)) + geom_line() + ylab("Impact (% Change) ") + geom_hline(yintercept = 0, linetype = "dashed") + xlab("Change in Temperature (C)") + facet_wrap(~crop)


