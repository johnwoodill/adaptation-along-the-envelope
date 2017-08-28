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

# Difference data
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

# corn 

# Predictions
cs0C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_0C)
cs1C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_1C)
cs2C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_2C)
cs3C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_3C)
cs4C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_4C)
cs5C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_5C)

p0C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_0C)
p1C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_1C)
p2C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_2C)
p3C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_3C)
p4C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_4C)
p5C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_5C)

diff0C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_0C)
diff1C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_1C)
diff2C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_2C)
diff3C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_3C)
diff4C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_4C)
diff5C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_5C)

# Bootstrapped Standard errors

cs0C.pred_ln_corn_rrev_se <- boot.strap(exp(cs0C.pred_ln_corn_rrev$fit + cs0C.pred_ln_corn_rrev$res  ))
cs1C.pred_ln_corn_rrev_se <- boot.strap(exp(cs1C.pred_ln_corn_rrev$fit + cs1C.pred_ln_corn_rrev$res  ))
cs2C.pred_ln_corn_rrev_se <- boot.strap(exp(cs2C.pred_ln_corn_rrev$fit + cs2C.pred_ln_corn_rrev$res  ))
cs3C.pred_ln_corn_rrev_se <- boot.strap(exp(cs3C.pred_ln_corn_rrev$fit + cs3C.pred_ln_corn_rrev$res  ))
cs4C.pred_ln_corn_rrev_se <- boot.strap(exp(cs4C.pred_ln_corn_rrev$fit + cs4C.pred_ln_corn_rrev$res  ))
cs5C.pred_ln_corn_rrev_se <- boot.strap(exp(cs5C.pred_ln_corn_rrev$fit + cs5C.pred_ln_corn_rrev$res  ))

p0C.pred_ln_corn_rrev_se <- boot.strap(exp(p0C.pred_ln_corn_rrev$fit + p0C.pred_ln_corn_rrev$res + p0C.pred_ln_corn_rrev$effect), cluster = p0C.pred_ln_corn_rrev$pred_data$year )
p1C.pred_ln_corn_rrev_se <- boot.strap(exp(p1C.pred_ln_corn_rrev$fit + p1C.pred_ln_corn_rrev$res + p1C.pred_ln_corn_rrev$effect), cluster = p1C.pred_ln_corn_rrev$pred_data$year )
p2C.pred_ln_corn_rrev_se <- boot.strap(exp(p2C.pred_ln_corn_rrev$fit + p2C.pred_ln_corn_rrev$res + p2C.pred_ln_corn_rrev$effect), cluster = p2C.pred_ln_corn_rrev$pred_data$year )
p3C.pred_ln_corn_rrev_se <- boot.strap(exp(p3C.pred_ln_corn_rrev$fit + p3C.pred_ln_corn_rrev$res + p3C.pred_ln_corn_rrev$effect), cluster = p3C.pred_ln_corn_rrev$pred_data$year )
p4C.pred_ln_corn_rrev_se <- boot.strap(exp(p4C.pred_ln_corn_rrev$fit + p4C.pred_ln_corn_rrev$res + p4C.pred_ln_corn_rrev$effect), cluster = p4C.pred_ln_corn_rrev$pred_data$year )
p5C.pred_ln_corn_rrev_se <- boot.strap(exp(p5C.pred_ln_corn_rrev$fit + p5C.pred_ln_corn_rrev$res + p5C.pred_ln_corn_rrev$effect), cluster = p5C.pred_ln_corn_rrev$pred_data$year )

diff0C.pred_ln_corn_rrev_se <- boot.strap(exp(diff0C.pred_ln_corn_rrev$fit + diff0C.pred_ln_corn_rrev$res + diff0C.pred_ln_corn_rrev$effect), cluster = diff0C.pred_ln_corn_rrev$pred_data$year )
diff1C.pred_ln_corn_rrev_se <- boot.strap(exp(diff1C.pred_ln_corn_rrev$fit + diff1C.pred_ln_corn_rrev$res + diff1C.pred_ln_corn_rrev$effect), cluster = diff1C.pred_ln_corn_rrev$pred_data$year )
diff2C.pred_ln_corn_rrev_se <- boot.strap(exp(diff2C.pred_ln_corn_rrev$fit + diff2C.pred_ln_corn_rrev$res + diff2C.pred_ln_corn_rrev$effect), cluster = diff2C.pred_ln_corn_rrev$pred_data$year )
diff3C.pred_ln_corn_rrev_se <- boot.strap(exp(diff3C.pred_ln_corn_rrev$fit + diff3C.pred_ln_corn_rrev$res + diff3C.pred_ln_corn_rrev$effect), cluster = diff3C.pred_ln_corn_rrev$pred_data$year )
diff4C.pred_ln_corn_rrev_se <- boot.strap(exp(diff4C.pred_ln_corn_rrev$fit + diff4C.pred_ln_corn_rrev$res + diff4C.pred_ln_corn_rrev$effect), cluster = diff4C.pred_ln_corn_rrev$pred_data$year )
diff5C.pred_ln_corn_rrev_se <- boot.strap(exp(diff5C.pred_ln_corn_rrev$fit + diff5C.pred_ln_corn_rrev$res + diff5C.pred_ln_corn_rrev$effect), cluster = diff5C.pred_ln_corn_rrev$pred_data$year )

# Predicted revenue
cs.corn.pred_rev0 <- sum(exp(cs0C.pred_ln_corn_rrev$fit + cs0C.pred_ln_corn_rrev$res ))
cs.corn.pred_rev1 <- sum(exp(cs1C.pred_ln_corn_rrev$fit + cs1C.pred_ln_corn_rrev$res ))
cs.corn.pred_rev2 <- sum(exp(cs2C.pred_ln_corn_rrev$fit + cs2C.pred_ln_corn_rrev$res ))
cs.corn.pred_rev3 <- sum(exp(cs3C.pred_ln_corn_rrev$fit + cs3C.pred_ln_corn_rrev$res ))
cs.corn.pred_rev4 <- sum(exp(cs4C.pred_ln_corn_rrev$fit + cs4C.pred_ln_corn_rrev$res ))
cs.corn.pred_rev5 <- sum(exp(cs5C.pred_ln_corn_rrev$fit + cs5C.pred_ln_corn_rrev$res ))

p.corn.pred_rev0 <- sum(exp(p0C.pred_ln_corn_rrev$fit + p0C.pred_ln_corn_rrev$res + p0C.pred_ln_corn_rrev$effect ))
p.corn.pred_rev1 <- sum(exp(p1C.pred_ln_corn_rrev$fit + p1C.pred_ln_corn_rrev$res + p1C.pred_ln_corn_rrev$effect ))
p.corn.pred_rev2 <- sum(exp(p2C.pred_ln_corn_rrev$fit + p2C.pred_ln_corn_rrev$res + p2C.pred_ln_corn_rrev$effect ))
p.corn.pred_rev3 <- sum(exp(p3C.pred_ln_corn_rrev$fit + p3C.pred_ln_corn_rrev$res + p3C.pred_ln_corn_rrev$effect ))
p.corn.pred_rev4 <- sum(exp(p4C.pred_ln_corn_rrev$fit + p4C.pred_ln_corn_rrev$res + p4C.pred_ln_corn_rrev$effect ))
p.corn.pred_rev5 <- sum(exp(p5C.pred_ln_corn_rrev$fit + p5C.pred_ln_corn_rrev$res + p5C.pred_ln_corn_rrev$effect ))

diff.corn.pred_rev0 <- sum(exp(diff0C.pred_ln_corn_rrev$fit + diff0C.pred_ln_corn_rrev$res + diff0C.pred_ln_corn_rrev$effect ))
diff.corn.pred_rev1 <- sum(exp(diff1C.pred_ln_corn_rrev$fit + diff1C.pred_ln_corn_rrev$res + diff1C.pred_ln_corn_rrev$effect ))
diff.corn.pred_rev2 <- sum(exp(diff2C.pred_ln_corn_rrev$fit + diff2C.pred_ln_corn_rrev$res + diff2C.pred_ln_corn_rrev$effect ))
diff.corn.pred_rev3 <- sum(exp(diff3C.pred_ln_corn_rrev$fit + diff3C.pred_ln_corn_rrev$res + diff3C.pred_ln_corn_rrev$effect ))
diff.corn.pred_rev4 <- sum(exp(diff4C.pred_ln_corn_rrev$fit + diff4C.pred_ln_corn_rrev$res + diff4C.pred_ln_corn_rrev$effect ))
diff.corn.pred_rev5 <- sum(exp(diff5C.pred_ln_corn_rrev$fit + diff5C.pred_ln_corn_rrev$res + diff5C.pred_ln_corn_rrev$effect ))

cs.corn.pred_rev0_min <- cs.corn.pred_rev0 - cs0C.pred_ln_corn_rrev_se$se.sum*2.58
cs.corn.pred_rev0_max <- cs.corn.pred_rev0 - cs0C.pred_ln_corn_rrev_se$se.sum*2.58
cs.corn.pred_rev1_min <- cs.corn.pred_rev1 - cs1C.pred_ln_corn_rrev_se$se.sum*2.58
cs.corn.pred_rev1_max <- cs.corn.pred_rev1 + cs1C.pred_ln_corn_rrev_se$se.sum*2.58
cs.corn.pred_rev2_min <- cs.corn.pred_rev2 - cs2C.pred_ln_corn_rrev_se$se.sum*2.58
cs.corn.pred_rev2_max <- cs.corn.pred_rev2 + cs2C.pred_ln_corn_rrev_se$se.sum*2.58
cs.corn.pred_rev3_min <- cs.corn.pred_rev3 - cs3C.pred_ln_corn_rrev_se$se.sum*2.58
cs.corn.pred_rev3_max <- cs.corn.pred_rev3 + cs3C.pred_ln_corn_rrev_se$se.sum*2.58
cs.corn.pred_rev4_min <- cs.corn.pred_rev4 - cs4C.pred_ln_corn_rrev_se$se.sum*2.58
cs.corn.pred_rev4_max <- cs.corn.pred_rev4 + cs4C.pred_ln_corn_rrev_se$se.sum*2.58
cs.corn.pred_rev5_min <- cs.corn.pred_rev5 - cs5C.pred_ln_corn_rrev_se$se.sum*2.58
cs.corn.pred_rev5_max <- cs.corn.pred_rev5 + cs5C.pred_ln_corn_rrev_se$se.sum*2.58

p.corn.pred_rev0_min <- p.corn.pred_rev0 - p0C.pred_ln_corn_rrev_se$se.sum*2.58
p.corn.pred_rev0_max <- p.corn.pred_rev0 + p0C.pred_ln_corn_rrev_se$se.sum*2.58
p.corn.pred_rev1_min <- p.corn.pred_rev1 - p1C.pred_ln_corn_rrev_se$se.sum*2.58
p.corn.pred_rev1_max <- p.corn.pred_rev1 + p1C.pred_ln_corn_rrev_se$se.sum*2.58
p.corn.pred_rev2_min <- p.corn.pred_rev2 - p2C.pred_ln_corn_rrev_se$se.sum*2.58
p.corn.pred_rev2_max <- p.corn.pred_rev2 + p2C.pred_ln_corn_rrev_se$se.sum*2.58
p.corn.pred_rev3_min <- p.corn.pred_rev3 - p3C.pred_ln_corn_rrev_se$se.sum*2.58
p.corn.pred_rev3_max <- p.corn.pred_rev3 + p3C.pred_ln_corn_rrev_se$se.sum*2.58
p.corn.pred_rev4_min <- p.corn.pred_rev4 - p4C.pred_ln_corn_rrev_se$se.sum*2.58
p.corn.pred_rev4_max <- p.corn.pred_rev4 + p4C.pred_ln_corn_rrev_se$se.sum*2.58
p.corn.pred_rev5_min <- p.corn.pred_rev5 - p5C.pred_ln_corn_rrev_se$se.sum*2.58
p.corn.pred_rev5_max <- p.corn.pred_rev5 + p5C.pred_ln_corn_rrev_se$se.sum*2.58

diff.corn.pred_rev0_min <- diff.corn.pred_rev0 - diff0C.pred_ln_corn_rrev_se$se.sum*2.58
diff.corn.pred_rev0_max <- diff.corn.pred_rev0 + diff0C.pred_ln_corn_rrev_se$se.sum*2.58
diff.corn.pred_rev1_min <- diff.corn.pred_rev1 - diff1C.pred_ln_corn_rrev_se$se.sum*2.58
diff.corn.pred_rev1_max <- diff.corn.pred_rev1 + diff1C.pred_ln_corn_rrev_se$se.sum*2.58
diff.corn.pred_rev2_min <- diff.corn.pred_rev2 - diff2C.pred_ln_corn_rrev_se$se.sum*2.58
diff.corn.pred_rev2_max <- diff.corn.pred_rev2 + diff2C.pred_ln_corn_rrev_se$se.sum*2.58
diff.corn.pred_rev3_min <- diff.corn.pred_rev3 - diff3C.pred_ln_corn_rrev_se$se.sum*2.58
diff.corn.pred_rev3_max <- diff.corn.pred_rev3 + diff3C.pred_ln_corn_rrev_se$se.sum*2.58
diff.corn.pred_rev4_min <- diff.corn.pred_rev4 - diff4C.pred_ln_corn_rrev_se$se.sum*2.58
diff.corn.pred_rev4_max <- diff.corn.pred_rev4 + diff4C.pred_ln_corn_rrev_se$se.sum*2.58
diff.corn.pred_rev5_min <- diff.corn.pred_rev5 - diff5C.pred_ln_corn_rrev_se$se.sum*2.58
diff.corn.pred_rev5_max <- diff.corn.pred_rev5 + diff5C.pred_ln_corn_rrev_se$se.sum*2.58

# Get % changes
cs.corn.rev0 <- (sum( exp(cs0C.pred_ln_corn_rrev$fit + cs0C.pred_ln_corn_rrev$res )))

cs.corn.rev1 <- (sum( exp(cs1C.pred_ln_corn_rrev$fit + cs1C.pred_ln_corn_rrev$res  ))/cs.corn.rev0 - 1)*100
cs.corn.rev1.diff <- cs.corn.rev1 - ( (cs.corn.pred_rev1 - cs1C.pred_ln_corn_rrev_se$se.sum*2.58)/cs.corn.rev0 - 1)*100
cs.corn.rev1.min <- cs.corn.rev1 + cs.corn.rev1.diff
cs.corn.rev1.max <- cs.corn.rev1 - cs.corn.rev1.diff

cs.corn.rev2 <- (sum( exp(cs2C.pred_ln_corn_rrev$fit + cs2C.pred_ln_corn_rrev$res ))/cs.corn.rev0 - 1)*100
cs.corn.rev2.diff <- cs.corn.rev2 - ( (cs.corn.pred_rev2 - cs2C.pred_ln_corn_rrev_se$se.sum*2.58)/cs.corn.rev0 - 1)*100
cs.corn.rev2.min <- cs.corn.rev2 + cs.corn.rev2.diff
cs.corn.rev2.max <- cs.corn.rev2 - cs.corn.rev2.diff

cs.corn.rev3 <- (sum( exp(cs3C.pred_ln_corn_rrev$fit + cs3C.pred_ln_corn_rrev$res ))/cs.corn.rev0 - 1)*100
cs.corn.rev3.diff <- cs.corn.rev3 - ( (cs.corn.pred_rev3 - cs3C.pred_ln_corn_rrev_se$se.sum*2.58)/cs.corn.rev0 - 1)*100
cs.corn.rev3.min <- cs.corn.rev3 + cs.corn.rev3.diff
cs.corn.rev3.max <- cs.corn.rev3 - cs.corn.rev3.diff

cs.corn.rev4 <- (sum( exp(cs4C.pred_ln_corn_rrev$fit + cs4C.pred_ln_corn_rrev$res ))/cs.corn.rev0 - 1)*100
cs.corn.rev4.diff <- cs.corn.rev4 - ( (cs.corn.pred_rev4 - cs4C.pred_ln_corn_rrev_se$se.sum*2.58)/cs.corn.rev0 - 1)*100
cs.corn.rev4.min <- cs.corn.rev4 + cs.corn.rev4.diff
cs.corn.rev4.max <- cs.corn.rev4 - cs.corn.rev4.diff

cs.corn.rev5 <- (sum( exp(cs5C.pred_ln_corn_rrev$fit + cs5C.pred_ln_corn_rrev$res ))/cs.corn.rev0 - 1)*100
cs.corn.rev5.diff <- cs.corn.rev5 - ( (cs.corn.pred_rev5 - cs5C.pred_ln_corn_rrev_se$se.sum*2.58)/cs.corn.rev0 - 1)*100
cs.corn.rev5.min <- cs.corn.rev5 + cs.corn.rev5.diff
cs.corn.rev5.max <- cs.corn.rev5 - cs.corn.rev5.diff
cs.corn.rev0 <- (sum( exp(cs0C.pred_ln_corn_rrev$fit + cs0C.pred_ln_corn_rrev$res )))

p.corn.rev0 <- (sum( exp(p0C.pred_ln_corn_rrev$fit + p0C.pred_ln_corn_rrev$res + p0C.pred_ln_corn_rrev$effect )))
p.corn.rev1 <- (sum( exp(p1C.pred_ln_corn_rrev$fit + p1C.pred_ln_corn_rrev$res + p1C.pred_ln_corn_rrev$effect ))/p.corn.rev0 - 1)*100
p.corn.rev1.diff <- p.corn.rev1 - ((p.corn.pred_rev1 - p1C.pred_ln_corn_rrev_se$se.sum*2.58)/p.corn.rev0 - 1)*100
p.corn.rev1.min <- p.corn.rev1 + p.corn.rev1.diff
p.corn.rev1.max <- p.corn.rev1 - p.corn.rev1.diff

p.corn.rev2 <- (sum( exp(p2C.pred_ln_corn_rrev$fit + p2C.pred_ln_corn_rrev$res + p2C.pred_ln_corn_rrev$effect ))/p.corn.rev0 - 1)*100
p.corn.rev2.diff <- p.corn.rev2 - ( (p.corn.pred_rev2 - p2C.pred_ln_corn_rrev_se$se.sum*2.58)/p.corn.rev0 - 1)*100
p.corn.rev2.min <- p.corn.rev2 + p.corn.rev2.diff
p.corn.rev2.max <- p.corn.rev2 - p.corn.rev2.diff

p.corn.rev3 <- (sum( exp(p3C.pred_ln_corn_rrev$fit + p3C.pred_ln_corn_rrev$res+ p3C.pred_ln_corn_rrev$effect ))/p.corn.rev0 - 1)*100
p.corn.rev3.diff <- p.corn.rev3 - ( (p.corn.pred_rev3 - p3C.pred_ln_corn_rrev_se$se.sum*2.58)/p.corn.rev0 - 1)*100
p.corn.rev3.min <- p.corn.rev3 + p.corn.rev3.diff
p.corn.rev3.max <- p.corn.rev3 - p.corn.rev3.diff

p.corn.rev4 <- (sum( exp(p4C.pred_ln_corn_rrev$fit + p4C.pred_ln_corn_rrev$res + p4C.pred_ln_corn_rrev$effect))/p.corn.rev0 - 1)*100
p.corn.rev4.diff <- p.corn.rev4 - ( (p.corn.pred_rev4 - p4C.pred_ln_corn_rrev_se$se.sum*2.58)/p.corn.rev0 - 1)*100
p.corn.rev4.min <- p.corn.rev4 + p.corn.rev4.diff
p.corn.rev4.max <- p.corn.rev4 - p.corn.rev4.diff

p.corn.rev5 <- (sum( exp(p5C.pred_ln_corn_rrev$fit + p5C.pred_ln_corn_rrev$res+ p5C.pred_ln_corn_rrev$effect ))/p.corn.rev0 - 1)*100
p.corn.rev5.diff <- p.corn.rev5 - ( (p.corn.pred_rev5 - p5C.pred_ln_corn_rrev_se$se.sum*2.58)/p.corn.rev0 - 1)*100
p.corn.rev5.min <- p.corn.rev5 + p.corn.rev5.diff
p.corn.rev5.max <- p.corn.rev5 - p.corn.rev5.diff


diff.corn.rev0 <- (sum( exp(diff0C.pred_ln_corn_rrev$fit + diff0C.pred_ln_corn_rrev$res+ diff0C.pred_ln_corn_rrev$effect )))
diff.corn.rev1 <- (sum( exp(diff1C.pred_ln_corn_rrev$fit + diff1C.pred_ln_corn_rrev$res + diff1C.pred_ln_corn_rrev$effect ))/diff.corn.rev0 - 1)*100
diff.corn.rev1.diff <- diff.corn.rev1 - ( (diff.corn.pred_rev1 - diff1C.pred_ln_corn_rrev_se$se.sum*2.58)/diff.corn.rev0 - 1)*100
diff.corn.rev1.min <- diff.corn.rev1 + diff.corn.rev1.diff
diff.corn.rev1.max <- diff.corn.rev1 - diff.corn.rev1.diff

diff.corn.rev2 <- (sum( exp(diff2C.pred_ln_corn_rrev$fit + diff2C.pred_ln_corn_rrev$res+ diff2C.pred_ln_corn_rrev$effect ))/diff.corn.rev0 - 1)*100
diff.corn.rev2.diff <- diff.corn.rev2 - ( (diff.corn.pred_rev2 - diff2C.pred_ln_corn_rrev_se$se.sum*2.58)/diff.corn.rev0 - 1)*100
diff.corn.rev2.min <- diff.corn.rev2 + diff.corn.rev2.diff
diff.corn.rev2.max <- diff.corn.rev2 - diff.corn.rev2.diff

diff.corn.rev3 <- (sum( exp(diff3C.pred_ln_corn_rrev$fit + diff3C.pred_ln_corn_rrev$res + diff3C.pred_ln_corn_rrev$effect))/diff.corn.rev0 - 1)*100
diff.corn.rev3.diff <- diff.corn.rev3 - ( (diff.corn.pred_rev3 - diff3C.pred_ln_corn_rrev_se$se.sum*2.58)/diff.corn.rev0 - 1)*100
diff.corn.rev3.min <- diff.corn.rev3 + diff.corn.rev3.diff
diff.corn.rev3.max <- diff.corn.rev3 - diff.corn.rev3.diff

diff.corn.rev4 <- (sum( exp(diff4C.pred_ln_corn_rrev$fit + diff4C.pred_ln_corn_rrev$res + diff4C.pred_ln_corn_rrev$effect))/diff.corn.rev0 - 1)*100
diff.corn.rev4.diff <- diff.corn.rev4 - ( (diff.corn.pred_rev4 - diff4C.pred_ln_corn_rrev_se$se.sum*2.58)/diff.corn.rev0 - 1)*100
diff.corn.rev4.min <- diff.corn.rev4 + diff.corn.rev4.diff
diff.corn.rev4.max <- diff.corn.rev4 - diff.corn.rev4.diff

diff.corn.rev5 <- (sum( exp(diff5C.pred_ln_corn_rrev$fit + diff5C.pred_ln_corn_rrev$res + diff5C.pred_ln_corn_rrev$effect))/diff.corn.rev0 - 1)*100
diff.corn.rev5.diff <- diff.corn.rev5 - ( (diff.corn.pred_rev5 - diff5C.pred_ln_corn_rrev_se$se.sum*2.58)/diff.corn.rev0 - 1)*100
diff.corn.rev5.min <- diff.corn.rev5 + diff.corn.rev5.diff
diff.corn.rev5.max <- diff.corn.rev5 - diff.corn.rev5.diff


corn.pred_rev <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           rev = c(cs.corn.pred_rev0, cs.corn.pred_rev1, cs.corn.pred_rev2, cs.corn.pred_rev3, cs.corn.pred_rev4, cs.corn.pred_rev5,
                                   p.corn.pred_rev0, p.corn.pred_rev1, p.corn.pred_rev2, p.corn.pred_rev3, p.corn.pred_rev4, p.corn.pred_rev5,
                                   diff.corn.pred_rev0, diff.corn.pred_rev1, diff.corn.pred_rev2, diff.corn.pred_rev3, diff.corn.pred_rev4, diff.corn.pred_rev5),
                           min = c(cs.corn.pred_rev0_min, cs.corn.pred_rev1_min, cs.corn.pred_rev2_min, cs.corn.pred_rev3_min, cs.corn.pred_rev4_min, cs.corn.pred_rev5_min,
                                   p.corn.pred_rev0_min, p.corn.pred_rev1_min, p.corn.pred_rev2_min, p.corn.pred_rev3_min, p.corn.pred_rev4_min, p.corn.pred_rev5_min,
                                   diff.corn.pred_rev0_min, diff.corn.pred_rev1_min, diff.corn.pred_rev2_min, diff.corn.pred_rev3_min, diff.corn.pred_rev4_min, diff.corn.pred_rev5_min),
                           max = c(cs.corn.pred_rev0_max, cs.corn.pred_rev1_max, cs.corn.pred_rev2_max, cs.corn.pred_rev3_max, cs.corn.pred_rev4_max, cs.corn.pred_rev5_max,
                                   p.corn.pred_rev0_max, p.corn.pred_rev1_max, p.corn.pred_rev2_max, p.corn.pred_rev3_max, p.corn.pred_rev4_max, p.corn.pred_rev5_max,
                                   diff.corn.pred_rev0_max, diff.corn.pred_rev1_max, diff.corn.pred_rev2_max, diff.corn.pred_rev3_max, diff.corn.pred_rev4_max, diff.corn.pred_rev5_max),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "corn")

corn.plotdat <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           rev = c(0,cs.corn.rev1, cs.corn.rev2, cs.corn.rev3, cs.corn.rev4, cs.corn.rev5,
                                   0,p.corn.rev1, p.corn.rev2, p.corn.rev3, p.corn.rev4, p.corn.rev5,
                                   0,diff.corn.rev1, diff.corn.rev2, diff.corn.rev3, diff.corn.rev4, diff.corn.rev5),
                           min = c(0,cs.corn.rev1.min, cs.corn.rev2.min, cs.corn.rev3.min, cs.corn.rev4.min, cs.corn.rev5.min,
                                   0,p.corn.rev1.min, p.corn.rev2.min, p.corn.rev3.min, p.corn.rev4.min, p.corn.rev5.min,
                                   0,diff.corn.rev1.min, diff.corn.rev2.min, diff.corn.rev3.min, diff.corn.rev4.min, diff.corn.rev5.min),
                           max = c(0,cs.corn.rev1.max, cs.corn.rev2.max, cs.corn.rev3.max, cs.corn.rev4.max, cs.corn.rev5.max,
                                   0,p.corn.rev1.max, p.corn.rev2.max, p.corn.rev3.max, p.corn.rev4.max, p.corn.rev5.max,
                                   0,diff.corn.rev1.max, diff.corn.rev2.max, diff.corn.rev3.max, diff.corn.rev4.max, diff.corn.rev5.max),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "corn")

ggplot(corn.plotdat, aes(temp, rev, group = reg)) + 
  geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5") + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")



# cotton 

# Predictions
cs0C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_0C)
cs1C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_1C)
cs2C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_2C)
cs3C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_3C)
cs4C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_4C)
cs5C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_5C)

p0C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_0C)
p1C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_1C)
p2C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_2C)
p3C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_3C)
p4C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_4C)
p5C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_5C)

diff0C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_0C)
diff1C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_1C)
diff2C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_2C)
diff3C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_3C)
diff4C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_4C)
diff5C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_5C)

# Bootstrapped Standard errors

cs0C.pred_ln_cotton_rrev_se <- boot.strap(exp(cs0C.pred_ln_cotton_rrev$fit + cs0C.pred_ln_cotton_rrev$res  ))
cs1C.pred_ln_cotton_rrev_se <- boot.strap(exp(cs1C.pred_ln_cotton_rrev$fit + cs1C.pred_ln_cotton_rrev$res  ))
cs2C.pred_ln_cotton_rrev_se <- boot.strap(exp(cs2C.pred_ln_cotton_rrev$fit + cs2C.pred_ln_cotton_rrev$res  ))
cs3C.pred_ln_cotton_rrev_se <- boot.strap(exp(cs3C.pred_ln_cotton_rrev$fit + cs3C.pred_ln_cotton_rrev$res  ))
cs4C.pred_ln_cotton_rrev_se <- boot.strap(exp(cs4C.pred_ln_cotton_rrev$fit + cs4C.pred_ln_cotton_rrev$res  ))
cs5C.pred_ln_cotton_rrev_se <- boot.strap(exp(cs5C.pred_ln_cotton_rrev$fit + cs5C.pred_ln_cotton_rrev$res  ))

p0C.pred_ln_cotton_rrev_se <- boot.strap(exp(p0C.pred_ln_cotton_rrev$fit + p0C.pred_ln_cotton_rrev$res + p0C.pred_ln_cotton_rrev$effect), cluster = p0C.pred_ln_cotton_rrev$pred_data$year )
p1C.pred_ln_cotton_rrev_se <- boot.strap(exp(p1C.pred_ln_cotton_rrev$fit + p1C.pred_ln_cotton_rrev$res + p1C.pred_ln_cotton_rrev$effect), cluster = p1C.pred_ln_cotton_rrev$pred_data$year )
p2C.pred_ln_cotton_rrev_se <- boot.strap(exp(p2C.pred_ln_cotton_rrev$fit + p2C.pred_ln_cotton_rrev$res + p2C.pred_ln_cotton_rrev$effect), cluster = p2C.pred_ln_cotton_rrev$pred_data$year )
p3C.pred_ln_cotton_rrev_se <- boot.strap(exp(p3C.pred_ln_cotton_rrev$fit + p3C.pred_ln_cotton_rrev$res + p3C.pred_ln_cotton_rrev$effect), cluster = p3C.pred_ln_cotton_rrev$pred_data$year )
p4C.pred_ln_cotton_rrev_se <- boot.strap(exp(p4C.pred_ln_cotton_rrev$fit + p4C.pred_ln_cotton_rrev$res + p4C.pred_ln_cotton_rrev$effect), cluster = p4C.pred_ln_cotton_rrev$pred_data$year )
p5C.pred_ln_cotton_rrev_se <- boot.strap(exp(p5C.pred_ln_cotton_rrev$fit + p5C.pred_ln_cotton_rrev$res + p5C.pred_ln_cotton_rrev$effect), cluster = p5C.pred_ln_cotton_rrev$pred_data$year )

diff0C.pred_ln_cotton_rrev_se <- boot.strap(exp(diff0C.pred_ln_cotton_rrev$fit + diff0C.pred_ln_cotton_rrev$res + diff0C.pred_ln_cotton_rrev$effect), cluster = diff0C.pred_ln_cotton_rrev$pred_data$year )
diff1C.pred_ln_cotton_rrev_se <- boot.strap(exp(diff1C.pred_ln_cotton_rrev$fit + diff1C.pred_ln_cotton_rrev$res + diff1C.pred_ln_cotton_rrev$effect), cluster = diff1C.pred_ln_cotton_rrev$pred_data$year )
diff2C.pred_ln_cotton_rrev_se <- boot.strap(exp(diff2C.pred_ln_cotton_rrev$fit + diff2C.pred_ln_cotton_rrev$res + diff2C.pred_ln_cotton_rrev$effect), cluster = diff2C.pred_ln_cotton_rrev$pred_data$year )
diff3C.pred_ln_cotton_rrev_se <- boot.strap(exp(diff3C.pred_ln_cotton_rrev$fit + diff3C.pred_ln_cotton_rrev$res + diff3C.pred_ln_cotton_rrev$effect), cluster = diff3C.pred_ln_cotton_rrev$pred_data$year )
diff4C.pred_ln_cotton_rrev_se <- boot.strap(exp(diff4C.pred_ln_cotton_rrev$fit + diff4C.pred_ln_cotton_rrev$res + diff4C.pred_ln_cotton_rrev$effect), cluster = diff4C.pred_ln_cotton_rrev$pred_data$year )
diff5C.pred_ln_cotton_rrev_se <- boot.strap(exp(diff5C.pred_ln_cotton_rrev$fit + diff5C.pred_ln_cotton_rrev$res + diff5C.pred_ln_cotton_rrev$effect), cluster = diff5C.pred_ln_cotton_rrev$pred_data$year )

# Predicted revenue
cs.cotton.pred_rev0 <- sum(exp(cs0C.pred_ln_cotton_rrev$fit + cs0C.pred_ln_cotton_rrev$res ))
cs.cotton.pred_rev1 <- sum(exp(cs1C.pred_ln_cotton_rrev$fit + cs1C.pred_ln_cotton_rrev$res ))
cs.cotton.pred_rev2 <- sum(exp(cs2C.pred_ln_cotton_rrev$fit + cs2C.pred_ln_cotton_rrev$res ))
cs.cotton.pred_rev3 <- sum(exp(cs3C.pred_ln_cotton_rrev$fit + cs3C.pred_ln_cotton_rrev$res ))
cs.cotton.pred_rev4 <- sum(exp(cs4C.pred_ln_cotton_rrev$fit + cs4C.pred_ln_cotton_rrev$res ))
cs.cotton.pred_rev5 <- sum(exp(cs5C.pred_ln_cotton_rrev$fit + cs5C.pred_ln_cotton_rrev$res ))

p.cotton.pred_rev0 <- sum(exp(p0C.pred_ln_cotton_rrev$fit + p0C.pred_ln_cotton_rrev$res + p0C.pred_ln_cotton_rrev$effect ))
p.cotton.pred_rev1 <- sum(exp(p1C.pred_ln_cotton_rrev$fit + p1C.pred_ln_cotton_rrev$res + p1C.pred_ln_cotton_rrev$effect ))
p.cotton.pred_rev2 <- sum(exp(p2C.pred_ln_cotton_rrev$fit + p2C.pred_ln_cotton_rrev$res + p2C.pred_ln_cotton_rrev$effect ))
p.cotton.pred_rev3 <- sum(exp(p3C.pred_ln_cotton_rrev$fit + p3C.pred_ln_cotton_rrev$res + p3C.pred_ln_cotton_rrev$effect ))
p.cotton.pred_rev4 <- sum(exp(p4C.pred_ln_cotton_rrev$fit + p4C.pred_ln_cotton_rrev$res + p4C.pred_ln_cotton_rrev$effect ))
p.cotton.pred_rev5 <- sum(exp(p5C.pred_ln_cotton_rrev$fit + p5C.pred_ln_cotton_rrev$res + p5C.pred_ln_cotton_rrev$effect ))

diff.cotton.pred_rev0 <- sum(exp(diff0C.pred_ln_cotton_rrev$fit + diff0C.pred_ln_cotton_rrev$res + diff0C.pred_ln_cotton_rrev$effect ))
diff.cotton.pred_rev1 <- sum(exp(diff1C.pred_ln_cotton_rrev$fit + diff1C.pred_ln_cotton_rrev$res + diff1C.pred_ln_cotton_rrev$effect ))
diff.cotton.pred_rev2 <- sum(exp(diff2C.pred_ln_cotton_rrev$fit + diff2C.pred_ln_cotton_rrev$res + diff2C.pred_ln_cotton_rrev$effect ))
diff.cotton.pred_rev3 <- sum(exp(diff3C.pred_ln_cotton_rrev$fit + diff3C.pred_ln_cotton_rrev$res + diff3C.pred_ln_cotton_rrev$effect ))
diff.cotton.pred_rev4 <- sum(exp(diff4C.pred_ln_cotton_rrev$fit + diff4C.pred_ln_cotton_rrev$res + diff4C.pred_ln_cotton_rrev$effect ))
diff.cotton.pred_rev5 <- sum(exp(diff5C.pred_ln_cotton_rrev$fit + diff5C.pred_ln_cotton_rrev$res + diff5C.pred_ln_cotton_rrev$effect ))

cs.cotton.pred_rev0_min <- cs.cotton.pred_rev0 - cs0C.pred_ln_cotton_rrev_se$se.sum*2.58
cs.cotton.pred_rev0_max <- cs.cotton.pred_rev0 - cs0C.pred_ln_cotton_rrev_se$se.sum*2.58
cs.cotton.pred_rev1_min <- cs.cotton.pred_rev1 - cs1C.pred_ln_cotton_rrev_se$se.sum*2.58
cs.cotton.pred_rev1_max <- cs.cotton.pred_rev1 + cs1C.pred_ln_cotton_rrev_se$se.sum*2.58
cs.cotton.pred_rev2_min <- cs.cotton.pred_rev2 - cs2C.pred_ln_cotton_rrev_se$se.sum*2.58
cs.cotton.pred_rev2_max <- cs.cotton.pred_rev2 + cs2C.pred_ln_cotton_rrev_se$se.sum*2.58
cs.cotton.pred_rev3_min <- cs.cotton.pred_rev3 - cs3C.pred_ln_cotton_rrev_se$se.sum*2.58
cs.cotton.pred_rev3_max <- cs.cotton.pred_rev3 + cs3C.pred_ln_cotton_rrev_se$se.sum*2.58
cs.cotton.pred_rev4_min <- cs.cotton.pred_rev4 - cs4C.pred_ln_cotton_rrev_se$se.sum*2.58
cs.cotton.pred_rev4_max <- cs.cotton.pred_rev4 + cs4C.pred_ln_cotton_rrev_se$se.sum*2.58
cs.cotton.pred_rev5_min <- cs.cotton.pred_rev5 - cs5C.pred_ln_cotton_rrev_se$se.sum*2.58
cs.cotton.pred_rev5_max <- cs.cotton.pred_rev5 + cs5C.pred_ln_cotton_rrev_se$se.sum*2.58

p.cotton.pred_rev0_min <- p.cotton.pred_rev0 - p0C.pred_ln_cotton_rrev_se$se.sum*2.58
p.cotton.pred_rev0_max <- p.cotton.pred_rev0 + p0C.pred_ln_cotton_rrev_se$se.sum*2.58
p.cotton.pred_rev1_min <- p.cotton.pred_rev1 - p1C.pred_ln_cotton_rrev_se$se.sum*2.58
p.cotton.pred_rev1_max <- p.cotton.pred_rev1 + p1C.pred_ln_cotton_rrev_se$se.sum*2.58
p.cotton.pred_rev2_min <- p.cotton.pred_rev2 - p2C.pred_ln_cotton_rrev_se$se.sum*2.58
p.cotton.pred_rev2_max <- p.cotton.pred_rev2 + p2C.pred_ln_cotton_rrev_se$se.sum*2.58
p.cotton.pred_rev3_min <- p.cotton.pred_rev3 - p3C.pred_ln_cotton_rrev_se$se.sum*2.58
p.cotton.pred_rev3_max <- p.cotton.pred_rev3 + p3C.pred_ln_cotton_rrev_se$se.sum*2.58
p.cotton.pred_rev4_min <- p.cotton.pred_rev4 - p4C.pred_ln_cotton_rrev_se$se.sum*2.58
p.cotton.pred_rev4_max <- p.cotton.pred_rev4 + p4C.pred_ln_cotton_rrev_se$se.sum*2.58
p.cotton.pred_rev5_min <- p.cotton.pred_rev5 - p5C.pred_ln_cotton_rrev_se$se.sum*2.58
p.cotton.pred_rev5_max <- p.cotton.pred_rev5 + p5C.pred_ln_cotton_rrev_se$se.sum*2.58

diff.cotton.pred_rev0_min <- diff.cotton.pred_rev0 - diff0C.pred_ln_cotton_rrev_se$se.sum*2.58
diff.cotton.pred_rev0_max <- diff.cotton.pred_rev0 + diff0C.pred_ln_cotton_rrev_se$se.sum*2.58
diff.cotton.pred_rev1_min <- diff.cotton.pred_rev1 - diff1C.pred_ln_cotton_rrev_se$se.sum*2.58
diff.cotton.pred_rev1_max <- diff.cotton.pred_rev1 + diff1C.pred_ln_cotton_rrev_se$se.sum*2.58
diff.cotton.pred_rev2_min <- diff.cotton.pred_rev2 - diff2C.pred_ln_cotton_rrev_se$se.sum*2.58
diff.cotton.pred_rev2_max <- diff.cotton.pred_rev2 + diff2C.pred_ln_cotton_rrev_se$se.sum*2.58
diff.cotton.pred_rev3_min <- diff.cotton.pred_rev3 - diff3C.pred_ln_cotton_rrev_se$se.sum*2.58
diff.cotton.pred_rev3_max <- diff.cotton.pred_rev3 + diff3C.pred_ln_cotton_rrev_se$se.sum*2.58
diff.cotton.pred_rev4_min <- diff.cotton.pred_rev4 - diff4C.pred_ln_cotton_rrev_se$se.sum*2.58
diff.cotton.pred_rev4_max <- diff.cotton.pred_rev4 + diff4C.pred_ln_cotton_rrev_se$se.sum*2.58
diff.cotton.pred_rev5_min <- diff.cotton.pred_rev5 - diff5C.pred_ln_cotton_rrev_se$se.sum*2.58
diff.cotton.pred_rev5_max <- diff.cotton.pred_rev5 + diff5C.pred_ln_cotton_rrev_se$se.sum*2.58

# Get % changes
cs.cotton.rev0 <- (sum( exp(cs0C.pred_ln_cotton_rrev$fit + cs0C.pred_ln_cotton_rrev$res )))

cs.cotton.rev1 <- (sum( exp(cs1C.pred_ln_cotton_rrev$fit + cs1C.pred_ln_cotton_rrev$res  ))/cs.cotton.rev0 - 1)*100
cs.cotton.rev1.diff <- cs.cotton.rev1 - ( (cs.cotton.pred_rev1 - cs1C.pred_ln_cotton_rrev_se$se.sum*2.58)/cs.cotton.rev0 - 1)*100
cs.cotton.rev1.min <- cs.cotton.rev1 + cs.cotton.rev1.diff
cs.cotton.rev1.max <- cs.cotton.rev1 - cs.cotton.rev1.diff

cs.cotton.rev2 <- (sum( exp(cs2C.pred_ln_cotton_rrev$fit + cs2C.pred_ln_cotton_rrev$res ))/cs.cotton.rev0 - 1)*100
cs.cotton.rev2.diff <- cs.cotton.rev2 - ( (cs.cotton.pred_rev2 - cs2C.pred_ln_cotton_rrev_se$se.sum*2.58)/cs.cotton.rev0 - 1)*100
cs.cotton.rev2.min <- cs.cotton.rev2 + cs.cotton.rev2.diff
cs.cotton.rev2.max <- cs.cotton.rev2 - cs.cotton.rev2.diff

cs.cotton.rev3 <- (sum( exp(cs3C.pred_ln_cotton_rrev$fit + cs3C.pred_ln_cotton_rrev$res ))/cs.cotton.rev0 - 1)*100
cs.cotton.rev3.diff <- cs.cotton.rev3 - ( (cs.cotton.pred_rev3 - cs3C.pred_ln_cotton_rrev_se$se.sum*2.58)/cs.cotton.rev0 - 1)*100
cs.cotton.rev3.min <- cs.cotton.rev3 + cs.cotton.rev3.diff
cs.cotton.rev3.max <- cs.cotton.rev3 - cs.cotton.rev3.diff

cs.cotton.rev4 <- (sum( exp(cs4C.pred_ln_cotton_rrev$fit + cs4C.pred_ln_cotton_rrev$res ))/cs.cotton.rev0 - 1)*100
cs.cotton.rev4.diff <- cs.cotton.rev4 - ( (cs.cotton.pred_rev4 - cs4C.pred_ln_cotton_rrev_se$se.sum*2.58)/cs.cotton.rev0 - 1)*100
cs.cotton.rev4.min <- cs.cotton.rev4 + cs.cotton.rev4.diff
cs.cotton.rev4.max <- cs.cotton.rev4 - cs.cotton.rev4.diff

cs.cotton.rev5 <- (sum( exp(cs5C.pred_ln_cotton_rrev$fit + cs5C.pred_ln_cotton_rrev$res ))/cs.cotton.rev0 - 1)*100
cs.cotton.rev5.diff <- cs.cotton.rev5 - ( (cs.cotton.pred_rev5 - cs5C.pred_ln_cotton_rrev_se$se.sum*2.58)/cs.cotton.rev0 - 1)*100
cs.cotton.rev5.min <- cs.cotton.rev5 + cs.cotton.rev5.diff
cs.cotton.rev5.max <- cs.cotton.rev5 - cs.cotton.rev5.diff
cs.cotton.rev0 <- (sum( exp(cs0C.pred_ln_cotton_rrev$fit + cs0C.pred_ln_cotton_rrev$res )))

p.cotton.rev0 <- (sum( exp(p0C.pred_ln_cotton_rrev$fit + p0C.pred_ln_cotton_rrev$res + p0C.pred_ln_cotton_rrev$effect )))
p.cotton.rev1 <- (sum( exp(p1C.pred_ln_cotton_rrev$fit + p1C.pred_ln_cotton_rrev$res + p1C.pred_ln_cotton_rrev$effect ))/p.cotton.rev0 - 1)*100
p.cotton.rev1.diff <- p.cotton.rev1 - ((p.cotton.pred_rev1 - p1C.pred_ln_cotton_rrev_se$se.sum*2.58)/p.cotton.rev0 - 1)*100
p.cotton.rev1.min <- p.cotton.rev1 + p.cotton.rev1.diff
p.cotton.rev1.max <- p.cotton.rev1 - p.cotton.rev1.diff

p.cotton.rev2 <- (sum( exp(p2C.pred_ln_cotton_rrev$fit + p2C.pred_ln_cotton_rrev$res + p2C.pred_ln_cotton_rrev$effect ))/p.cotton.rev0 - 1)*100
p.cotton.rev2.diff <- p.cotton.rev2 - ( (p.cotton.pred_rev2 - p2C.pred_ln_cotton_rrev_se$se.sum*2.58)/p.cotton.rev0 - 1)*100
p.cotton.rev2.min <- p.cotton.rev2 + p.cotton.rev2.diff
p.cotton.rev2.max <- p.cotton.rev2 - p.cotton.rev2.diff

p.cotton.rev3 <- (sum( exp(p3C.pred_ln_cotton_rrev$fit + p3C.pred_ln_cotton_rrev$res+ p3C.pred_ln_cotton_rrev$effect ))/p.cotton.rev0 - 1)*100
p.cotton.rev3.diff <- p.cotton.rev3 - ( (p.cotton.pred_rev3 - p3C.pred_ln_cotton_rrev_se$se.sum*2.58)/p.cotton.rev0 - 1)*100
p.cotton.rev3.min <- p.cotton.rev3 + p.cotton.rev3.diff
p.cotton.rev3.max <- p.cotton.rev3 - p.cotton.rev3.diff

p.cotton.rev4 <- (sum( exp(p4C.pred_ln_cotton_rrev$fit + p4C.pred_ln_cotton_rrev$res + p4C.pred_ln_cotton_rrev$effect))/p.cotton.rev0 - 1)*100
p.cotton.rev4.diff <- p.cotton.rev4 - ( (p.cotton.pred_rev4 - p4C.pred_ln_cotton_rrev_se$se.sum*2.58)/p.cotton.rev0 - 1)*100
p.cotton.rev4.min <- p.cotton.rev4 + p.cotton.rev4.diff
p.cotton.rev4.max <- p.cotton.rev4 - p.cotton.rev4.diff

p.cotton.rev5 <- (sum( exp(p5C.pred_ln_cotton_rrev$fit + p5C.pred_ln_cotton_rrev$res+ p5C.pred_ln_cotton_rrev$effect ))/p.cotton.rev0 - 1)*100
p.cotton.rev5.diff <- p.cotton.rev5 - ( (p.cotton.pred_rev5 - p5C.pred_ln_cotton_rrev_se$se.sum*2.58)/p.cotton.rev0 - 1)*100
p.cotton.rev5.min <- p.cotton.rev5 + p.cotton.rev5.diff
p.cotton.rev5.max <- p.cotton.rev5 - p.cotton.rev5.diff


diff.cotton.rev0 <- (sum( exp(diff0C.pred_ln_cotton_rrev$fit + diff0C.pred_ln_cotton_rrev$res+ diff0C.pred_ln_cotton_rrev$effect )))
diff.cotton.rev1 <- (sum( exp(diff1C.pred_ln_cotton_rrev$fit + diff1C.pred_ln_cotton_rrev$res + diff1C.pred_ln_cotton_rrev$effect ))/diff.cotton.rev0 - 1)*100
diff.cotton.rev1.diff <- diff.cotton.rev1 - ( (diff.cotton.pred_rev1 - diff1C.pred_ln_cotton_rrev_se$se.sum*2.58)/diff.cotton.rev0 - 1)*100
diff.cotton.rev1.min <- diff.cotton.rev1 + diff.cotton.rev1.diff
diff.cotton.rev1.max <- diff.cotton.rev1 - diff.cotton.rev1.diff

diff.cotton.rev2 <- (sum( exp(diff2C.pred_ln_cotton_rrev$fit + diff2C.pred_ln_cotton_rrev$res+ diff2C.pred_ln_cotton_rrev$effect ))/diff.cotton.rev0 - 1)*100
diff.cotton.rev2.diff <- diff.cotton.rev2 - ( (diff.cotton.pred_rev2 - diff2C.pred_ln_cotton_rrev_se$se.sum*2.58)/diff.cotton.rev0 - 1)*100
diff.cotton.rev2.min <- diff.cotton.rev2 + diff.cotton.rev2.diff
diff.cotton.rev2.max <- diff.cotton.rev2 - diff.cotton.rev2.diff

diff.cotton.rev3 <- (sum( exp(diff3C.pred_ln_cotton_rrev$fit + diff3C.pred_ln_cotton_rrev$res + diff3C.pred_ln_cotton_rrev$effect))/diff.cotton.rev0 - 1)*100
diff.cotton.rev3.diff <- diff.cotton.rev3 - ( (diff.cotton.pred_rev3 - diff3C.pred_ln_cotton_rrev_se$se.sum*2.58)/diff.cotton.rev0 - 1)*100
diff.cotton.rev3.min <- diff.cotton.rev3 + diff.cotton.rev3.diff
diff.cotton.rev3.max <- diff.cotton.rev3 - diff.cotton.rev3.diff

diff.cotton.rev4 <- (sum( exp(diff4C.pred_ln_cotton_rrev$fit + diff4C.pred_ln_cotton_rrev$res + diff4C.pred_ln_cotton_rrev$effect))/diff.cotton.rev0 - 1)*100
diff.cotton.rev4.diff <- diff.cotton.rev4 - ( (diff.cotton.pred_rev4 - diff4C.pred_ln_cotton_rrev_se$se.sum*2.58)/diff.cotton.rev0 - 1)*100
diff.cotton.rev4.min <- diff.cotton.rev4 + diff.cotton.rev4.diff
diff.cotton.rev4.max <- diff.cotton.rev4 - diff.cotton.rev4.diff

diff.cotton.rev5 <- (sum( exp(diff5C.pred_ln_cotton_rrev$fit + diff5C.pred_ln_cotton_rrev$res + diff5C.pred_ln_cotton_rrev$effect))/diff.cotton.rev0 - 1)*100
diff.cotton.rev5.diff <- diff.cotton.rev5 - ( (diff.cotton.pred_rev5 - diff5C.pred_ln_cotton_rrev_se$se.sum*2.58)/diff.cotton.rev0 - 1)*100
diff.cotton.rev5.min <- diff.cotton.rev5 + diff.cotton.rev5.diff
diff.cotton.rev5.max <- diff.cotton.rev5 - diff.cotton.rev5.diff


cotton.pred_rev <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           rev = c(cs.cotton.pred_rev0, cs.cotton.pred_rev1, cs.cotton.pred_rev2, cs.cotton.pred_rev3, cs.cotton.pred_rev4, cs.cotton.pred_rev5,
                                   p.cotton.pred_rev0, p.cotton.pred_rev1, p.cotton.pred_rev2, p.cotton.pred_rev3, p.cotton.pred_rev4, p.cotton.pred_rev5,
                                   diff.cotton.pred_rev0, diff.cotton.pred_rev1, diff.cotton.pred_rev2, diff.cotton.pred_rev3, diff.cotton.pred_rev4, diff.cotton.pred_rev5),
                           min = c(cs.cotton.pred_rev0_min, cs.cotton.pred_rev1_min, cs.cotton.pred_rev2_min, cs.cotton.pred_rev3_min, cs.cotton.pred_rev4_min, cs.cotton.pred_rev5_min,
                                   p.cotton.pred_rev0_min, p.cotton.pred_rev1_min, p.cotton.pred_rev2_min, p.cotton.pred_rev3_min, p.cotton.pred_rev4_min, p.cotton.pred_rev5_min,
                                   diff.cotton.pred_rev0_min, diff.cotton.pred_rev1_min, diff.cotton.pred_rev2_min, diff.cotton.pred_rev3_min, diff.cotton.pred_rev4_min, diff.cotton.pred_rev5_min),
                           max = c(cs.cotton.pred_rev0_max, cs.cotton.pred_rev1_max, cs.cotton.pred_rev2_max, cs.cotton.pred_rev3_max, cs.cotton.pred_rev4_max, cs.cotton.pred_rev5_max,
                                   p.cotton.pred_rev0_max, p.cotton.pred_rev1_max, p.cotton.pred_rev2_max, p.cotton.pred_rev3_max, p.cotton.pred_rev4_max, p.cotton.pred_rev5_max,
                                   diff.cotton.pred_rev0_max, diff.cotton.pred_rev1_max, diff.cotton.pred_rev2_max, diff.cotton.pred_rev3_max, diff.cotton.pred_rev4_max, diff.cotton.pred_rev5_max),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "cotton")

cotton.plotdat <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           rev = c(0,cs.cotton.rev1, cs.cotton.rev2, cs.cotton.rev3, cs.cotton.rev4, cs.cotton.rev5,
                                   0,p.cotton.rev1, p.cotton.rev2, p.cotton.rev3, p.cotton.rev4, p.cotton.rev5,
                                   0,diff.cotton.rev1, diff.cotton.rev2, diff.cotton.rev3, diff.cotton.rev4, diff.cotton.rev5),
                           min = c(0,cs.cotton.rev1.min, cs.cotton.rev2.min, cs.cotton.rev3.min, cs.cotton.rev4.min, cs.cotton.rev5.min,
                                   0,p.cotton.rev1.min, p.cotton.rev2.min, p.cotton.rev3.min, p.cotton.rev4.min, p.cotton.rev5.min,
                                   0,diff.cotton.rev1.min, diff.cotton.rev2.min, diff.cotton.rev3.min, diff.cotton.rev4.min, diff.cotton.rev5.min),
                           max = c(0,cs.cotton.rev1.max, cs.cotton.rev2.max, cs.cotton.rev3.max, cs.cotton.rev4.max, cs.cotton.rev5.max,
                                   0,p.cotton.rev1.max, p.cotton.rev2.max, p.cotton.rev3.max, p.cotton.rev4.max, p.cotton.rev5.max,
                                   0,diff.cotton.rev1.max, diff.cotton.rev2.max, diff.cotton.rev3.max, diff.cotton.rev4.max, diff.cotton.rev5.max),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "cotton")

ggplot(cotton.plotdat, aes(temp, rev, group = reg)) + 
  geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5") + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")



# hay 

# Predictions
cs0C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_0C)
cs1C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_1C)
cs2C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_2C)
cs3C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_3C)
cs4C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_4C)
cs5C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_5C)

p0C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_0C)
p1C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_1C)
p2C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_2C)
p3C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_3C)
p4C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_4C)
p5C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_5C)

diff0C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_0C)
diff1C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_1C)
diff2C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_2C)
diff3C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_3C)
diff4C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_4C)
diff5C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_5C)

# Bootstrapped Standard errors

cs0C.pred_ln_hay_rrev_se <- boot.strap(exp(cs0C.pred_ln_hay_rrev$fit + cs0C.pred_ln_hay_rrev$res  ))
cs1C.pred_ln_hay_rrev_se <- boot.strap(exp(cs1C.pred_ln_hay_rrev$fit + cs1C.pred_ln_hay_rrev$res  ))
cs2C.pred_ln_hay_rrev_se <- boot.strap(exp(cs2C.pred_ln_hay_rrev$fit + cs2C.pred_ln_hay_rrev$res  ))
cs3C.pred_ln_hay_rrev_se <- boot.strap(exp(cs3C.pred_ln_hay_rrev$fit + cs3C.pred_ln_hay_rrev$res  ))
cs4C.pred_ln_hay_rrev_se <- boot.strap(exp(cs4C.pred_ln_hay_rrev$fit + cs4C.pred_ln_hay_rrev$res  ))
cs5C.pred_ln_hay_rrev_se <- boot.strap(exp(cs5C.pred_ln_hay_rrev$fit + cs5C.pred_ln_hay_rrev$res  ))

p0C.pred_ln_hay_rrev_se <- boot.strap(exp(p0C.pred_ln_hay_rrev$fit + p0C.pred_ln_hay_rrev$res + p0C.pred_ln_hay_rrev$effect), cluster = p0C.pred_ln_hay_rrev$pred_data$year )
p1C.pred_ln_hay_rrev_se <- boot.strap(exp(p1C.pred_ln_hay_rrev$fit + p1C.pred_ln_hay_rrev$res + p1C.pred_ln_hay_rrev$effect), cluster = p1C.pred_ln_hay_rrev$pred_data$year )
p2C.pred_ln_hay_rrev_se <- boot.strap(exp(p2C.pred_ln_hay_rrev$fit + p2C.pred_ln_hay_rrev$res + p2C.pred_ln_hay_rrev$effect), cluster = p2C.pred_ln_hay_rrev$pred_data$year )
p3C.pred_ln_hay_rrev_se <- boot.strap(exp(p3C.pred_ln_hay_rrev$fit + p3C.pred_ln_hay_rrev$res + p3C.pred_ln_hay_rrev$effect), cluster = p3C.pred_ln_hay_rrev$pred_data$year )
p4C.pred_ln_hay_rrev_se <- boot.strap(exp(p4C.pred_ln_hay_rrev$fit + p4C.pred_ln_hay_rrev$res + p4C.pred_ln_hay_rrev$effect), cluster = p4C.pred_ln_hay_rrev$pred_data$year )
p5C.pred_ln_hay_rrev_se <- boot.strap(exp(p5C.pred_ln_hay_rrev$fit + p5C.pred_ln_hay_rrev$res + p5C.pred_ln_hay_rrev$effect), cluster = p5C.pred_ln_hay_rrev$pred_data$year )

diff0C.pred_ln_hay_rrev_se <- boot.strap(exp(diff0C.pred_ln_hay_rrev$fit + diff0C.pred_ln_hay_rrev$res + diff0C.pred_ln_hay_rrev$effect), cluster = diff0C.pred_ln_hay_rrev$pred_data$year )
diff1C.pred_ln_hay_rrev_se <- boot.strap(exp(diff1C.pred_ln_hay_rrev$fit + diff1C.pred_ln_hay_rrev$res + diff1C.pred_ln_hay_rrev$effect), cluster = diff1C.pred_ln_hay_rrev$pred_data$year )
diff2C.pred_ln_hay_rrev_se <- boot.strap(exp(diff2C.pred_ln_hay_rrev$fit + diff2C.pred_ln_hay_rrev$res + diff2C.pred_ln_hay_rrev$effect), cluster = diff2C.pred_ln_hay_rrev$pred_data$year )
diff3C.pred_ln_hay_rrev_se <- boot.strap(exp(diff3C.pred_ln_hay_rrev$fit + diff3C.pred_ln_hay_rrev$res + diff3C.pred_ln_hay_rrev$effect), cluster = diff3C.pred_ln_hay_rrev$pred_data$year )
diff4C.pred_ln_hay_rrev_se <- boot.strap(exp(diff4C.pred_ln_hay_rrev$fit + diff4C.pred_ln_hay_rrev$res + diff4C.pred_ln_hay_rrev$effect), cluster = diff4C.pred_ln_hay_rrev$pred_data$year )
diff5C.pred_ln_hay_rrev_se <- boot.strap(exp(diff5C.pred_ln_hay_rrev$fit + diff5C.pred_ln_hay_rrev$res + diff5C.pred_ln_hay_rrev$effect), cluster = diff5C.pred_ln_hay_rrev$pred_data$year )

# Predicted revenue
cs.hay.pred_rev0 <- sum(exp(cs0C.pred_ln_hay_rrev$fit + cs0C.pred_ln_hay_rrev$res ))
cs.hay.pred_rev1 <- sum(exp(cs1C.pred_ln_hay_rrev$fit + cs1C.pred_ln_hay_rrev$res ))
cs.hay.pred_rev2 <- sum(exp(cs2C.pred_ln_hay_rrev$fit + cs2C.pred_ln_hay_rrev$res ))
cs.hay.pred_rev3 <- sum(exp(cs3C.pred_ln_hay_rrev$fit + cs3C.pred_ln_hay_rrev$res ))
cs.hay.pred_rev4 <- sum(exp(cs4C.pred_ln_hay_rrev$fit + cs4C.pred_ln_hay_rrev$res ))
cs.hay.pred_rev5 <- sum(exp(cs5C.pred_ln_hay_rrev$fit + cs5C.pred_ln_hay_rrev$res ))

p.hay.pred_rev0 <- sum(exp(p0C.pred_ln_hay_rrev$fit + p0C.pred_ln_hay_rrev$res + p0C.pred_ln_hay_rrev$effect ))
p.hay.pred_rev1 <- sum(exp(p1C.pred_ln_hay_rrev$fit + p1C.pred_ln_hay_rrev$res + p1C.pred_ln_hay_rrev$effect ))
p.hay.pred_rev2 <- sum(exp(p2C.pred_ln_hay_rrev$fit + p2C.pred_ln_hay_rrev$res + p2C.pred_ln_hay_rrev$effect ))
p.hay.pred_rev3 <- sum(exp(p3C.pred_ln_hay_rrev$fit + p3C.pred_ln_hay_rrev$res + p3C.pred_ln_hay_rrev$effect ))
p.hay.pred_rev4 <- sum(exp(p4C.pred_ln_hay_rrev$fit + p4C.pred_ln_hay_rrev$res + p4C.pred_ln_hay_rrev$effect ))
p.hay.pred_rev5 <- sum(exp(p5C.pred_ln_hay_rrev$fit + p5C.pred_ln_hay_rrev$res + p5C.pred_ln_hay_rrev$effect ))

diff.hay.pred_rev0 <- sum(exp(diff0C.pred_ln_hay_rrev$fit + diff0C.pred_ln_hay_rrev$res + diff0C.pred_ln_hay_rrev$effect ))
diff.hay.pred_rev1 <- sum(exp(diff1C.pred_ln_hay_rrev$fit + diff1C.pred_ln_hay_rrev$res + diff1C.pred_ln_hay_rrev$effect ))
diff.hay.pred_rev2 <- sum(exp(diff2C.pred_ln_hay_rrev$fit + diff2C.pred_ln_hay_rrev$res + diff2C.pred_ln_hay_rrev$effect ))
diff.hay.pred_rev3 <- sum(exp(diff3C.pred_ln_hay_rrev$fit + diff3C.pred_ln_hay_rrev$res + diff3C.pred_ln_hay_rrev$effect ))
diff.hay.pred_rev4 <- sum(exp(diff4C.pred_ln_hay_rrev$fit + diff4C.pred_ln_hay_rrev$res + diff4C.pred_ln_hay_rrev$effect ))
diff.hay.pred_rev5 <- sum(exp(diff5C.pred_ln_hay_rrev$fit + diff5C.pred_ln_hay_rrev$res + diff5C.pred_ln_hay_rrev$effect ))

cs.hay.pred_rev0_min <- cs.hay.pred_rev0 - cs0C.pred_ln_hay_rrev_se$se.sum*2.58
cs.hay.pred_rev0_max <- cs.hay.pred_rev0 - cs0C.pred_ln_hay_rrev_se$se.sum*2.58
cs.hay.pred_rev1_min <- cs.hay.pred_rev1 - cs1C.pred_ln_hay_rrev_se$se.sum*2.58
cs.hay.pred_rev1_max <- cs.hay.pred_rev1 + cs1C.pred_ln_hay_rrev_se$se.sum*2.58
cs.hay.pred_rev2_min <- cs.hay.pred_rev2 - cs2C.pred_ln_hay_rrev_se$se.sum*2.58
cs.hay.pred_rev2_max <- cs.hay.pred_rev2 + cs2C.pred_ln_hay_rrev_se$se.sum*2.58
cs.hay.pred_rev3_min <- cs.hay.pred_rev3 - cs3C.pred_ln_hay_rrev_se$se.sum*2.58
cs.hay.pred_rev3_max <- cs.hay.pred_rev3 + cs3C.pred_ln_hay_rrev_se$se.sum*2.58
cs.hay.pred_rev4_min <- cs.hay.pred_rev4 - cs4C.pred_ln_hay_rrev_se$se.sum*2.58
cs.hay.pred_rev4_max <- cs.hay.pred_rev4 + cs4C.pred_ln_hay_rrev_se$se.sum*2.58
cs.hay.pred_rev5_min <- cs.hay.pred_rev5 - cs5C.pred_ln_hay_rrev_se$se.sum*2.58
cs.hay.pred_rev5_max <- cs.hay.pred_rev5 + cs5C.pred_ln_hay_rrev_se$se.sum*2.58

p.hay.pred_rev0_min <- p.hay.pred_rev0 - p0C.pred_ln_hay_rrev_se$se.sum*2.58
p.hay.pred_rev0_max <- p.hay.pred_rev0 + p0C.pred_ln_hay_rrev_se$se.sum*2.58
p.hay.pred_rev1_min <- p.hay.pred_rev1 - p1C.pred_ln_hay_rrev_se$se.sum*2.58
p.hay.pred_rev1_max <- p.hay.pred_rev1 + p1C.pred_ln_hay_rrev_se$se.sum*2.58
p.hay.pred_rev2_min <- p.hay.pred_rev2 - p2C.pred_ln_hay_rrev_se$se.sum*2.58
p.hay.pred_rev2_max <- p.hay.pred_rev2 + p2C.pred_ln_hay_rrev_se$se.sum*2.58
p.hay.pred_rev3_min <- p.hay.pred_rev3 - p3C.pred_ln_hay_rrev_se$se.sum*2.58
p.hay.pred_rev3_max <- p.hay.pred_rev3 + p3C.pred_ln_hay_rrev_se$se.sum*2.58
p.hay.pred_rev4_min <- p.hay.pred_rev4 - p4C.pred_ln_hay_rrev_se$se.sum*2.58
p.hay.pred_rev4_max <- p.hay.pred_rev4 + p4C.pred_ln_hay_rrev_se$se.sum*2.58
p.hay.pred_rev5_min <- p.hay.pred_rev5 - p5C.pred_ln_hay_rrev_se$se.sum*2.58
p.hay.pred_rev5_max <- p.hay.pred_rev5 + p5C.pred_ln_hay_rrev_se$se.sum*2.58

diff.hay.pred_rev0_min <- diff.hay.pred_rev0 - diff0C.pred_ln_hay_rrev_se$se.sum*2.58
diff.hay.pred_rev0_max <- diff.hay.pred_rev0 + diff0C.pred_ln_hay_rrev_se$se.sum*2.58
diff.hay.pred_rev1_min <- diff.hay.pred_rev1 - diff1C.pred_ln_hay_rrev_se$se.sum*2.58
diff.hay.pred_rev1_max <- diff.hay.pred_rev1 + diff1C.pred_ln_hay_rrev_se$se.sum*2.58
diff.hay.pred_rev2_min <- diff.hay.pred_rev2 - diff2C.pred_ln_hay_rrev_se$se.sum*2.58
diff.hay.pred_rev2_max <- diff.hay.pred_rev2 + diff2C.pred_ln_hay_rrev_se$se.sum*2.58
diff.hay.pred_rev3_min <- diff.hay.pred_rev3 - diff3C.pred_ln_hay_rrev_se$se.sum*2.58
diff.hay.pred_rev3_max <- diff.hay.pred_rev3 + diff3C.pred_ln_hay_rrev_se$se.sum*2.58
diff.hay.pred_rev4_min <- diff.hay.pred_rev4 - diff4C.pred_ln_hay_rrev_se$se.sum*2.58
diff.hay.pred_rev4_max <- diff.hay.pred_rev4 + diff4C.pred_ln_hay_rrev_se$se.sum*2.58
diff.hay.pred_rev5_min <- diff.hay.pred_rev5 - diff5C.pred_ln_hay_rrev_se$se.sum*2.58
diff.hay.pred_rev5_max <- diff.hay.pred_rev5 + diff5C.pred_ln_hay_rrev_se$se.sum*2.58

# Get % changes
cs.hay.rev0 <- (sum( exp(cs0C.pred_ln_hay_rrev$fit + cs0C.pred_ln_hay_rrev$res )))

cs.hay.rev1 <- (sum( exp(cs1C.pred_ln_hay_rrev$fit + cs1C.pred_ln_hay_rrev$res  ))/cs.hay.rev0 - 1)*100
cs.hay.rev1.diff <- cs.hay.rev1 - ( (cs.hay.pred_rev1 - cs1C.pred_ln_hay_rrev_se$se.sum*2.58)/cs.hay.rev0 - 1)*100
cs.hay.rev1.min <- cs.hay.rev1 + cs.hay.rev1.diff
cs.hay.rev1.max <- cs.hay.rev1 - cs.hay.rev1.diff

cs.hay.rev2 <- (sum( exp(cs2C.pred_ln_hay_rrev$fit + cs2C.pred_ln_hay_rrev$res ))/cs.hay.rev0 - 1)*100
cs.hay.rev2.diff <- cs.hay.rev2 - ( (cs.hay.pred_rev2 - cs2C.pred_ln_hay_rrev_se$se.sum*2.58)/cs.hay.rev0 - 1)*100
cs.hay.rev2.min <- cs.hay.rev2 + cs.hay.rev2.diff
cs.hay.rev2.max <- cs.hay.rev2 - cs.hay.rev2.diff

cs.hay.rev3 <- (sum( exp(cs3C.pred_ln_hay_rrev$fit + cs3C.pred_ln_hay_rrev$res ))/cs.hay.rev0 - 1)*100
cs.hay.rev3.diff <- cs.hay.rev3 - ( (cs.hay.pred_rev3 - cs3C.pred_ln_hay_rrev_se$se.sum*2.58)/cs.hay.rev0 - 1)*100
cs.hay.rev3.min <- cs.hay.rev3 + cs.hay.rev3.diff
cs.hay.rev3.max <- cs.hay.rev3 - cs.hay.rev3.diff

cs.hay.rev4 <- (sum( exp(cs4C.pred_ln_hay_rrev$fit + cs4C.pred_ln_hay_rrev$res ))/cs.hay.rev0 - 1)*100
cs.hay.rev4.diff <- cs.hay.rev4 - ( (cs.hay.pred_rev4 - cs4C.pred_ln_hay_rrev_se$se.sum*2.58)/cs.hay.rev0 - 1)*100
cs.hay.rev4.min <- cs.hay.rev4 + cs.hay.rev4.diff
cs.hay.rev4.max <- cs.hay.rev4 - cs.hay.rev4.diff

cs.hay.rev5 <- (sum( exp(cs5C.pred_ln_hay_rrev$fit + cs5C.pred_ln_hay_rrev$res ))/cs.hay.rev0 - 1)*100
cs.hay.rev5.diff <- cs.hay.rev5 - ( (cs.hay.pred_rev5 - cs5C.pred_ln_hay_rrev_se$se.sum*2.58)/cs.hay.rev0 - 1)*100
cs.hay.rev5.min <- cs.hay.rev5 + cs.hay.rev5.diff
cs.hay.rev5.max <- cs.hay.rev5 - cs.hay.rev5.diff
cs.hay.rev0 <- (sum( exp(cs0C.pred_ln_hay_rrev$fit + cs0C.pred_ln_hay_rrev$res )))

p.hay.rev0 <- (sum( exp(p0C.pred_ln_hay_rrev$fit + p0C.pred_ln_hay_rrev$res + p0C.pred_ln_hay_rrev$effect )))
p.hay.rev1 <- (sum( exp(p1C.pred_ln_hay_rrev$fit + p1C.pred_ln_hay_rrev$res + p1C.pred_ln_hay_rrev$effect ))/p.hay.rev0 - 1)*100
p.hay.rev1.diff <- p.hay.rev1 - ((p.hay.pred_rev1 - p1C.pred_ln_hay_rrev_se$se.sum*2.58)/p.hay.rev0 - 1)*100
p.hay.rev1.min <- p.hay.rev1 + p.hay.rev1.diff
p.hay.rev1.max <- p.hay.rev1 - p.hay.rev1.diff

p.hay.rev2 <- (sum( exp(p2C.pred_ln_hay_rrev$fit + p2C.pred_ln_hay_rrev$res + p2C.pred_ln_hay_rrev$effect ))/p.hay.rev0 - 1)*100
p.hay.rev2.diff <- p.hay.rev2 - ( (p.hay.pred_rev2 - p2C.pred_ln_hay_rrev_se$se.sum*2.58)/p.hay.rev0 - 1)*100
p.hay.rev2.min <- p.hay.rev2 + p.hay.rev2.diff
p.hay.rev2.max <- p.hay.rev2 - p.hay.rev2.diff

p.hay.rev3 <- (sum( exp(p3C.pred_ln_hay_rrev$fit + p3C.pred_ln_hay_rrev$res+ p3C.pred_ln_hay_rrev$effect ))/p.hay.rev0 - 1)*100
p.hay.rev3.diff <- p.hay.rev3 - ( (p.hay.pred_rev3 - p3C.pred_ln_hay_rrev_se$se.sum*2.58)/p.hay.rev0 - 1)*100
p.hay.rev3.min <- p.hay.rev3 + p.hay.rev3.diff
p.hay.rev3.max <- p.hay.rev3 - p.hay.rev3.diff

p.hay.rev4 <- (sum( exp(p4C.pred_ln_hay_rrev$fit + p4C.pred_ln_hay_rrev$res + p4C.pred_ln_hay_rrev$effect))/p.hay.rev0 - 1)*100
p.hay.rev4.diff <- p.hay.rev4 - ( (p.hay.pred_rev4 - p4C.pred_ln_hay_rrev_se$se.sum*2.58)/p.hay.rev0 - 1)*100
p.hay.rev4.min <- p.hay.rev4 + p.hay.rev4.diff
p.hay.rev4.max <- p.hay.rev4 - p.hay.rev4.diff

p.hay.rev5 <- (sum( exp(p5C.pred_ln_hay_rrev$fit + p5C.pred_ln_hay_rrev$res+ p5C.pred_ln_hay_rrev$effect ))/p.hay.rev0 - 1)*100
p.hay.rev5.diff <- p.hay.rev5 - ( (p.hay.pred_rev5 - p5C.pred_ln_hay_rrev_se$se.sum*2.58)/p.hay.rev0 - 1)*100
p.hay.rev5.min <- p.hay.rev5 + p.hay.rev5.diff
p.hay.rev5.max <- p.hay.rev5 - p.hay.rev5.diff


diff.hay.rev0 <- (sum( exp(diff0C.pred_ln_hay_rrev$fit + diff0C.pred_ln_hay_rrev$res+ diff0C.pred_ln_hay_rrev$effect )))
diff.hay.rev1 <- (sum( exp(diff1C.pred_ln_hay_rrev$fit + diff1C.pred_ln_hay_rrev$res + diff1C.pred_ln_hay_rrev$effect ))/diff.hay.rev0 - 1)*100
diff.hay.rev1.diff <- diff.hay.rev1 - ( (diff.hay.pred_rev1 - diff1C.pred_ln_hay_rrev_se$se.sum*2.58)/diff.hay.rev0 - 1)*100
diff.hay.rev1.min <- diff.hay.rev1 + diff.hay.rev1.diff
diff.hay.rev1.max <- diff.hay.rev1 - diff.hay.rev1.diff

diff.hay.rev2 <- (sum( exp(diff2C.pred_ln_hay_rrev$fit + diff2C.pred_ln_hay_rrev$res+ diff2C.pred_ln_hay_rrev$effect ))/diff.hay.rev0 - 1)*100
diff.hay.rev2.diff <- diff.hay.rev2 - ( (diff.hay.pred_rev2 - diff2C.pred_ln_hay_rrev_se$se.sum*2.58)/diff.hay.rev0 - 1)*100
diff.hay.rev2.min <- diff.hay.rev2 + diff.hay.rev2.diff
diff.hay.rev2.max <- diff.hay.rev2 - diff.hay.rev2.diff

diff.hay.rev3 <- (sum( exp(diff3C.pred_ln_hay_rrev$fit + diff3C.pred_ln_hay_rrev$res + diff3C.pred_ln_hay_rrev$effect))/diff.hay.rev0 - 1)*100
diff.hay.rev3.diff <- diff.hay.rev3 - ( (diff.hay.pred_rev3 - diff3C.pred_ln_hay_rrev_se$se.sum*2.58)/diff.hay.rev0 - 1)*100
diff.hay.rev3.min <- diff.hay.rev3 + diff.hay.rev3.diff
diff.hay.rev3.max <- diff.hay.rev3 - diff.hay.rev3.diff

diff.hay.rev4 <- (sum( exp(diff4C.pred_ln_hay_rrev$fit + diff4C.pred_ln_hay_rrev$res + diff4C.pred_ln_hay_rrev$effect))/diff.hay.rev0 - 1)*100
diff.hay.rev4.diff <- diff.hay.rev4 - ( (diff.hay.pred_rev4 - diff4C.pred_ln_hay_rrev_se$se.sum*2.58)/diff.hay.rev0 - 1)*100
diff.hay.rev4.min <- diff.hay.rev4 + diff.hay.rev4.diff
diff.hay.rev4.max <- diff.hay.rev4 - diff.hay.rev4.diff

diff.hay.rev5 <- (sum( exp(diff5C.pred_ln_hay_rrev$fit + diff5C.pred_ln_hay_rrev$res + diff5C.pred_ln_hay_rrev$effect))/diff.hay.rev0 - 1)*100
diff.hay.rev5.diff <- diff.hay.rev5 - ( (diff.hay.pred_rev5 - diff5C.pred_ln_hay_rrev_se$se.sum*2.58)/diff.hay.rev0 - 1)*100
diff.hay.rev5.min <- diff.hay.rev5 + diff.hay.rev5.diff
diff.hay.rev5.max <- diff.hay.rev5 - diff.hay.rev5.diff


hay.pred_rev <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           rev = c(cs.hay.pred_rev0, cs.hay.pred_rev1, cs.hay.pred_rev2, cs.hay.pred_rev3, cs.hay.pred_rev4, cs.hay.pred_rev5,
                                   p.hay.pred_rev0, p.hay.pred_rev1, p.hay.pred_rev2, p.hay.pred_rev3, p.hay.pred_rev4, p.hay.pred_rev5,
                                   diff.hay.pred_rev0, diff.hay.pred_rev1, diff.hay.pred_rev2, diff.hay.pred_rev3, diff.hay.pred_rev4, diff.hay.pred_rev5),
                           min = c(cs.hay.pred_rev0_min, cs.hay.pred_rev1_min, cs.hay.pred_rev2_min, cs.hay.pred_rev3_min, cs.hay.pred_rev4_min, cs.hay.pred_rev5_min,
                                   p.hay.pred_rev0_min, p.hay.pred_rev1_min, p.hay.pred_rev2_min, p.hay.pred_rev3_min, p.hay.pred_rev4_min, p.hay.pred_rev5_min,
                                   diff.hay.pred_rev0_min, diff.hay.pred_rev1_min, diff.hay.pred_rev2_min, diff.hay.pred_rev3_min, diff.hay.pred_rev4_min, diff.hay.pred_rev5_min),
                           max = c(cs.hay.pred_rev0_max, cs.hay.pred_rev1_max, cs.hay.pred_rev2_max, cs.hay.pred_rev3_max, cs.hay.pred_rev4_max, cs.hay.pred_rev5_max,
                                   p.hay.pred_rev0_max, p.hay.pred_rev1_max, p.hay.pred_rev2_max, p.hay.pred_rev3_max, p.hay.pred_rev4_max, p.hay.pred_rev5_max,
                                   diff.hay.pred_rev0_max, diff.hay.pred_rev1_max, diff.hay.pred_rev2_max, diff.hay.pred_rev3_max, diff.hay.pred_rev4_max, diff.hay.pred_rev5_max),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "hay")

hay.plotdat <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           rev = c(0,cs.hay.rev1, cs.hay.rev2, cs.hay.rev3, cs.hay.rev4, cs.hay.rev5,
                                   0,p.hay.rev1, p.hay.rev2, p.hay.rev3, p.hay.rev4, p.hay.rev5,
                                   0,diff.hay.rev1, diff.hay.rev2, diff.hay.rev3, diff.hay.rev4, diff.hay.rev5),
                           min = c(0,cs.hay.rev1.min, cs.hay.rev2.min, cs.hay.rev3.min, cs.hay.rev4.min, cs.hay.rev5.min,
                                   0,p.hay.rev1.min, p.hay.rev2.min, p.hay.rev3.min, p.hay.rev4.min, p.hay.rev5.min,
                                   0,diff.hay.rev1.min, diff.hay.rev2.min, diff.hay.rev3.min, diff.hay.rev4.min, diff.hay.rev5.min),
                           max = c(0,cs.hay.rev1.max, cs.hay.rev2.max, cs.hay.rev3.max, cs.hay.rev4.max, cs.hay.rev5.max,
                                   0,p.hay.rev1.max, p.hay.rev2.max, p.hay.rev3.max, p.hay.rev4.max, p.hay.rev5.max,
                                   0,diff.hay.rev1.max, diff.hay.rev2.max, diff.hay.rev3.max, diff.hay.rev4.max, diff.hay.rev5.max),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "hay")

ggplot(hay.plotdat, aes(temp, rev, group = reg)) + 
  geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5") + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")



# wheat 

# Predictions
cs0C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_0C)
cs1C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_1C)
cs2C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_2C)
cs3C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_3C)
cs4C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_4C)
cs5C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_5C)

p0C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_0C)
p1C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_1C)
p2C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_2C)
p3C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_3C)
p4C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_4C)
p5C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_5C)

diff0C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_0C)
diff1C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_1C)
diff2C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_2C)
diff3C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_3C)
diff4C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_4C)
diff5C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_5C)

# Bootstrapped Standard errors

cs0C.pred_ln_wheat_rrev_se <- boot.strap(exp(cs0C.pred_ln_wheat_rrev$fit + cs0C.pred_ln_wheat_rrev$res  ))
cs1C.pred_ln_wheat_rrev_se <- boot.strap(exp(cs1C.pred_ln_wheat_rrev$fit + cs1C.pred_ln_wheat_rrev$res  ))
cs2C.pred_ln_wheat_rrev_se <- boot.strap(exp(cs2C.pred_ln_wheat_rrev$fit + cs2C.pred_ln_wheat_rrev$res  ))
cs3C.pred_ln_wheat_rrev_se <- boot.strap(exp(cs3C.pred_ln_wheat_rrev$fit + cs3C.pred_ln_wheat_rrev$res  ))
cs4C.pred_ln_wheat_rrev_se <- boot.strap(exp(cs4C.pred_ln_wheat_rrev$fit + cs4C.pred_ln_wheat_rrev$res  ))
cs5C.pred_ln_wheat_rrev_se <- boot.strap(exp(cs5C.pred_ln_wheat_rrev$fit + cs5C.pred_ln_wheat_rrev$res  ))

p0C.pred_ln_wheat_rrev_se <- boot.strap(exp(p0C.pred_ln_wheat_rrev$fit + p0C.pred_ln_wheat_rrev$res + p0C.pred_ln_wheat_rrev$effect), cluster = p0C.pred_ln_wheat_rrev$pred_data$year )
p1C.pred_ln_wheat_rrev_se <- boot.strap(exp(p1C.pred_ln_wheat_rrev$fit + p1C.pred_ln_wheat_rrev$res + p1C.pred_ln_wheat_rrev$effect), cluster = p1C.pred_ln_wheat_rrev$pred_data$year )
p2C.pred_ln_wheat_rrev_se <- boot.strap(exp(p2C.pred_ln_wheat_rrev$fit + p2C.pred_ln_wheat_rrev$res + p2C.pred_ln_wheat_rrev$effect), cluster = p2C.pred_ln_wheat_rrev$pred_data$year )
p3C.pred_ln_wheat_rrev_se <- boot.strap(exp(p3C.pred_ln_wheat_rrev$fit + p3C.pred_ln_wheat_rrev$res + p3C.pred_ln_wheat_rrev$effect), cluster = p3C.pred_ln_wheat_rrev$pred_data$year )
p4C.pred_ln_wheat_rrev_se <- boot.strap(exp(p4C.pred_ln_wheat_rrev$fit + p4C.pred_ln_wheat_rrev$res + p4C.pred_ln_wheat_rrev$effect), cluster = p4C.pred_ln_wheat_rrev$pred_data$year )
p5C.pred_ln_wheat_rrev_se <- boot.strap(exp(p5C.pred_ln_wheat_rrev$fit + p5C.pred_ln_wheat_rrev$res + p5C.pred_ln_wheat_rrev$effect), cluster = p5C.pred_ln_wheat_rrev$pred_data$year )

diff0C.pred_ln_wheat_rrev_se <- boot.strap(exp(diff0C.pred_ln_wheat_rrev$fit + diff0C.pred_ln_wheat_rrev$res + diff0C.pred_ln_wheat_rrev$effect), cluster = diff0C.pred_ln_wheat_rrev$pred_data$year )
diff1C.pred_ln_wheat_rrev_se <- boot.strap(exp(diff1C.pred_ln_wheat_rrev$fit + diff1C.pred_ln_wheat_rrev$res + diff1C.pred_ln_wheat_rrev$effect), cluster = diff1C.pred_ln_wheat_rrev$pred_data$year )
diff2C.pred_ln_wheat_rrev_se <- boot.strap(exp(diff2C.pred_ln_wheat_rrev$fit + diff2C.pred_ln_wheat_rrev$res + diff2C.pred_ln_wheat_rrev$effect), cluster = diff2C.pred_ln_wheat_rrev$pred_data$year )
diff3C.pred_ln_wheat_rrev_se <- boot.strap(exp(diff3C.pred_ln_wheat_rrev$fit + diff3C.pred_ln_wheat_rrev$res + diff3C.pred_ln_wheat_rrev$effect), cluster = diff3C.pred_ln_wheat_rrev$pred_data$year )
diff4C.pred_ln_wheat_rrev_se <- boot.strap(exp(diff4C.pred_ln_wheat_rrev$fit + diff4C.pred_ln_wheat_rrev$res + diff4C.pred_ln_wheat_rrev$effect), cluster = diff4C.pred_ln_wheat_rrev$pred_data$year )
diff5C.pred_ln_wheat_rrev_se <- boot.strap(exp(diff5C.pred_ln_wheat_rrev$fit + diff5C.pred_ln_wheat_rrev$res + diff5C.pred_ln_wheat_rrev$effect), cluster = diff5C.pred_ln_wheat_rrev$pred_data$year )

# Predicted revenue
cs.wheat.pred_rev0 <- sum(exp(cs0C.pred_ln_wheat_rrev$fit + cs0C.pred_ln_wheat_rrev$res ))
cs.wheat.pred_rev1 <- sum(exp(cs1C.pred_ln_wheat_rrev$fit + cs1C.pred_ln_wheat_rrev$res ))
cs.wheat.pred_rev2 <- sum(exp(cs2C.pred_ln_wheat_rrev$fit + cs2C.pred_ln_wheat_rrev$res ))
cs.wheat.pred_rev3 <- sum(exp(cs3C.pred_ln_wheat_rrev$fit + cs3C.pred_ln_wheat_rrev$res ))
cs.wheat.pred_rev4 <- sum(exp(cs4C.pred_ln_wheat_rrev$fit + cs4C.pred_ln_wheat_rrev$res ))
cs.wheat.pred_rev5 <- sum(exp(cs5C.pred_ln_wheat_rrev$fit + cs5C.pred_ln_wheat_rrev$res ))

p.wheat.pred_rev0 <- sum(exp(p0C.pred_ln_wheat_rrev$fit + p0C.pred_ln_wheat_rrev$res + p0C.pred_ln_wheat_rrev$effect ))
p.wheat.pred_rev1 <- sum(exp(p1C.pred_ln_wheat_rrev$fit + p1C.pred_ln_wheat_rrev$res + p1C.pred_ln_wheat_rrev$effect ))
p.wheat.pred_rev2 <- sum(exp(p2C.pred_ln_wheat_rrev$fit + p2C.pred_ln_wheat_rrev$res + p2C.pred_ln_wheat_rrev$effect ))
p.wheat.pred_rev3 <- sum(exp(p3C.pred_ln_wheat_rrev$fit + p3C.pred_ln_wheat_rrev$res + p3C.pred_ln_wheat_rrev$effect ))
p.wheat.pred_rev4 <- sum(exp(p4C.pred_ln_wheat_rrev$fit + p4C.pred_ln_wheat_rrev$res + p4C.pred_ln_wheat_rrev$effect ))
p.wheat.pred_rev5 <- sum(exp(p5C.pred_ln_wheat_rrev$fit + p5C.pred_ln_wheat_rrev$res + p5C.pred_ln_wheat_rrev$effect ))

diff.wheat.pred_rev0 <- sum(exp(diff0C.pred_ln_wheat_rrev$fit + diff0C.pred_ln_wheat_rrev$res + diff0C.pred_ln_wheat_rrev$effect ))
diff.wheat.pred_rev1 <- sum(exp(diff1C.pred_ln_wheat_rrev$fit + diff1C.pred_ln_wheat_rrev$res + diff1C.pred_ln_wheat_rrev$effect ))
diff.wheat.pred_rev2 <- sum(exp(diff2C.pred_ln_wheat_rrev$fit + diff2C.pred_ln_wheat_rrev$res + diff2C.pred_ln_wheat_rrev$effect ))
diff.wheat.pred_rev3 <- sum(exp(diff3C.pred_ln_wheat_rrev$fit + diff3C.pred_ln_wheat_rrev$res + diff3C.pred_ln_wheat_rrev$effect ))
diff.wheat.pred_rev4 <- sum(exp(diff4C.pred_ln_wheat_rrev$fit + diff4C.pred_ln_wheat_rrev$res + diff4C.pred_ln_wheat_rrev$effect ))
diff.wheat.pred_rev5 <- sum(exp(diff5C.pred_ln_wheat_rrev$fit + diff5C.pred_ln_wheat_rrev$res + diff5C.pred_ln_wheat_rrev$effect ))

cs.wheat.pred_rev0_min <- cs.wheat.pred_rev0 - cs0C.pred_ln_wheat_rrev_se$se.sum*2.58
cs.wheat.pred_rev0_max <- cs.wheat.pred_rev0 - cs0C.pred_ln_wheat_rrev_se$se.sum*2.58
cs.wheat.pred_rev1_min <- cs.wheat.pred_rev1 - cs1C.pred_ln_wheat_rrev_se$se.sum*2.58
cs.wheat.pred_rev1_max <- cs.wheat.pred_rev1 + cs1C.pred_ln_wheat_rrev_se$se.sum*2.58
cs.wheat.pred_rev2_min <- cs.wheat.pred_rev2 - cs2C.pred_ln_wheat_rrev_se$se.sum*2.58
cs.wheat.pred_rev2_max <- cs.wheat.pred_rev2 + cs2C.pred_ln_wheat_rrev_se$se.sum*2.58
cs.wheat.pred_rev3_min <- cs.wheat.pred_rev3 - cs3C.pred_ln_wheat_rrev_se$se.sum*2.58
cs.wheat.pred_rev3_max <- cs.wheat.pred_rev3 + cs3C.pred_ln_wheat_rrev_se$se.sum*2.58
cs.wheat.pred_rev4_min <- cs.wheat.pred_rev4 - cs4C.pred_ln_wheat_rrev_se$se.sum*2.58
cs.wheat.pred_rev4_max <- cs.wheat.pred_rev4 + cs4C.pred_ln_wheat_rrev_se$se.sum*2.58
cs.wheat.pred_rev5_min <- cs.wheat.pred_rev5 - cs5C.pred_ln_wheat_rrev_se$se.sum*2.58
cs.wheat.pred_rev5_max <- cs.wheat.pred_rev5 + cs5C.pred_ln_wheat_rrev_se$se.sum*2.58

p.wheat.pred_rev0_min <- p.wheat.pred_rev0 - p0C.pred_ln_wheat_rrev_se$se.sum*2.58
p.wheat.pred_rev0_max <- p.wheat.pred_rev0 + p0C.pred_ln_wheat_rrev_se$se.sum*2.58
p.wheat.pred_rev1_min <- p.wheat.pred_rev1 - p1C.pred_ln_wheat_rrev_se$se.sum*2.58
p.wheat.pred_rev1_max <- p.wheat.pred_rev1 + p1C.pred_ln_wheat_rrev_se$se.sum*2.58
p.wheat.pred_rev2_min <- p.wheat.pred_rev2 - p2C.pred_ln_wheat_rrev_se$se.sum*2.58
p.wheat.pred_rev2_max <- p.wheat.pred_rev2 + p2C.pred_ln_wheat_rrev_se$se.sum*2.58
p.wheat.pred_rev3_min <- p.wheat.pred_rev3 - p3C.pred_ln_wheat_rrev_se$se.sum*2.58
p.wheat.pred_rev3_max <- p.wheat.pred_rev3 + p3C.pred_ln_wheat_rrev_se$se.sum*2.58
p.wheat.pred_rev4_min <- p.wheat.pred_rev4 - p4C.pred_ln_wheat_rrev_se$se.sum*2.58
p.wheat.pred_rev4_max <- p.wheat.pred_rev4 + p4C.pred_ln_wheat_rrev_se$se.sum*2.58
p.wheat.pred_rev5_min <- p.wheat.pred_rev5 - p5C.pred_ln_wheat_rrev_se$se.sum*2.58
p.wheat.pred_rev5_max <- p.wheat.pred_rev5 + p5C.pred_ln_wheat_rrev_se$se.sum*2.58

diff.wheat.pred_rev0_min <- diff.wheat.pred_rev0 - diff0C.pred_ln_wheat_rrev_se$se.sum*2.58
diff.wheat.pred_rev0_max <- diff.wheat.pred_rev0 + diff0C.pred_ln_wheat_rrev_se$se.sum*2.58
diff.wheat.pred_rev1_min <- diff.wheat.pred_rev1 - diff1C.pred_ln_wheat_rrev_se$se.sum*2.58
diff.wheat.pred_rev1_max <- diff.wheat.pred_rev1 + diff1C.pred_ln_wheat_rrev_se$se.sum*2.58
diff.wheat.pred_rev2_min <- diff.wheat.pred_rev2 - diff2C.pred_ln_wheat_rrev_se$se.sum*2.58
diff.wheat.pred_rev2_max <- diff.wheat.pred_rev2 + diff2C.pred_ln_wheat_rrev_se$se.sum*2.58
diff.wheat.pred_rev3_min <- diff.wheat.pred_rev3 - diff3C.pred_ln_wheat_rrev_se$se.sum*2.58
diff.wheat.pred_rev3_max <- diff.wheat.pred_rev3 + diff3C.pred_ln_wheat_rrev_se$se.sum*2.58
diff.wheat.pred_rev4_min <- diff.wheat.pred_rev4 - diff4C.pred_ln_wheat_rrev_se$se.sum*2.58
diff.wheat.pred_rev4_max <- diff.wheat.pred_rev4 + diff4C.pred_ln_wheat_rrev_se$se.sum*2.58
diff.wheat.pred_rev5_min <- diff.wheat.pred_rev5 - diff5C.pred_ln_wheat_rrev_se$se.sum*2.58
diff.wheat.pred_rev5_max <- diff.wheat.pred_rev5 + diff5C.pred_ln_wheat_rrev_se$se.sum*2.58

# Get % changes
cs.wheat.rev0 <- (sum( exp(cs0C.pred_ln_wheat_rrev$fit + cs0C.pred_ln_wheat_rrev$res )))

cs.wheat.rev1 <- (sum( exp(cs1C.pred_ln_wheat_rrev$fit + cs1C.pred_ln_wheat_rrev$res  ))/cs.wheat.rev0 - 1)*100
cs.wheat.rev1.diff <- cs.wheat.rev1 - ( (cs.wheat.pred_rev1 - cs1C.pred_ln_wheat_rrev_se$se.sum*2.58)/cs.wheat.rev0 - 1)*100
cs.wheat.rev1.min <- cs.wheat.rev1 + cs.wheat.rev1.diff
cs.wheat.rev1.max <- cs.wheat.rev1 - cs.wheat.rev1.diff

cs.wheat.rev2 <- (sum( exp(cs2C.pred_ln_wheat_rrev$fit + cs2C.pred_ln_wheat_rrev$res ))/cs.wheat.rev0 - 1)*100
cs.wheat.rev2.diff <- cs.wheat.rev2 - ( (cs.wheat.pred_rev2 - cs2C.pred_ln_wheat_rrev_se$se.sum*2.58)/cs.wheat.rev0 - 1)*100
cs.wheat.rev2.min <- cs.wheat.rev2 + cs.wheat.rev2.diff
cs.wheat.rev2.max <- cs.wheat.rev2 - cs.wheat.rev2.diff

cs.wheat.rev3 <- (sum( exp(cs3C.pred_ln_wheat_rrev$fit + cs3C.pred_ln_wheat_rrev$res ))/cs.wheat.rev0 - 1)*100
cs.wheat.rev3.diff <- cs.wheat.rev3 - ( (cs.wheat.pred_rev3 - cs3C.pred_ln_wheat_rrev_se$se.sum*2.58)/cs.wheat.rev0 - 1)*100
cs.wheat.rev3.min <- cs.wheat.rev3 + cs.wheat.rev3.diff
cs.wheat.rev3.max <- cs.wheat.rev3 - cs.wheat.rev3.diff

cs.wheat.rev4 <- (sum( exp(cs4C.pred_ln_wheat_rrev$fit + cs4C.pred_ln_wheat_rrev$res ))/cs.wheat.rev0 - 1)*100
cs.wheat.rev4.diff <- cs.wheat.rev4 - ( (cs.wheat.pred_rev4 - cs4C.pred_ln_wheat_rrev_se$se.sum*2.58)/cs.wheat.rev0 - 1)*100
cs.wheat.rev4.min <- cs.wheat.rev4 + cs.wheat.rev4.diff
cs.wheat.rev4.max <- cs.wheat.rev4 - cs.wheat.rev4.diff

cs.wheat.rev5 <- (sum( exp(cs5C.pred_ln_wheat_rrev$fit + cs5C.pred_ln_wheat_rrev$res ))/cs.wheat.rev0 - 1)*100
cs.wheat.rev5.diff <- cs.wheat.rev5 - ( (cs.wheat.pred_rev5 - cs5C.pred_ln_wheat_rrev_se$se.sum*2.58)/cs.wheat.rev0 - 1)*100
cs.wheat.rev5.min <- cs.wheat.rev5 + cs.wheat.rev5.diff
cs.wheat.rev5.max <- cs.wheat.rev5 - cs.wheat.rev5.diff
cs.wheat.rev0 <- (sum( exp(cs0C.pred_ln_wheat_rrev$fit + cs0C.pred_ln_wheat_rrev$res )))

p.wheat.rev0 <- (sum( exp(p0C.pred_ln_wheat_rrev$fit + p0C.pred_ln_wheat_rrev$res + p0C.pred_ln_wheat_rrev$effect )))
p.wheat.rev1 <- (sum( exp(p1C.pred_ln_wheat_rrev$fit + p1C.pred_ln_wheat_rrev$res + p1C.pred_ln_wheat_rrev$effect ))/p.wheat.rev0 - 1)*100
p.wheat.rev1.diff <- p.wheat.rev1 - ((p.wheat.pred_rev1 - p1C.pred_ln_wheat_rrev_se$se.sum*2.58)/p.wheat.rev0 - 1)*100
p.wheat.rev1.min <- p.wheat.rev1 + p.wheat.rev1.diff
p.wheat.rev1.max <- p.wheat.rev1 - p.wheat.rev1.diff

p.wheat.rev2 <- (sum( exp(p2C.pred_ln_wheat_rrev$fit + p2C.pred_ln_wheat_rrev$res + p2C.pred_ln_wheat_rrev$effect ))/p.wheat.rev0 - 1)*100
p.wheat.rev2.diff <- p.wheat.rev2 - ( (p.wheat.pred_rev2 - p2C.pred_ln_wheat_rrev_se$se.sum*2.58)/p.wheat.rev0 - 1)*100
p.wheat.rev2.min <- p.wheat.rev2 + p.wheat.rev2.diff
p.wheat.rev2.max <- p.wheat.rev2 - p.wheat.rev2.diff

p.wheat.rev3 <- (sum( exp(p3C.pred_ln_wheat_rrev$fit + p3C.pred_ln_wheat_rrev$res+ p3C.pred_ln_wheat_rrev$effect ))/p.wheat.rev0 - 1)*100
p.wheat.rev3.diff <- p.wheat.rev3 - ( (p.wheat.pred_rev3 - p3C.pred_ln_wheat_rrev_se$se.sum*2.58)/p.wheat.rev0 - 1)*100
p.wheat.rev3.min <- p.wheat.rev3 + p.wheat.rev3.diff
p.wheat.rev3.max <- p.wheat.rev3 - p.wheat.rev3.diff

p.wheat.rev4 <- (sum( exp(p4C.pred_ln_wheat_rrev$fit + p4C.pred_ln_wheat_rrev$res + p4C.pred_ln_wheat_rrev$effect))/p.wheat.rev0 - 1)*100
p.wheat.rev4.diff <- p.wheat.rev4 - ( (p.wheat.pred_rev4 - p4C.pred_ln_wheat_rrev_se$se.sum*2.58)/p.wheat.rev0 - 1)*100
p.wheat.rev4.min <- p.wheat.rev4 + p.wheat.rev4.diff
p.wheat.rev4.max <- p.wheat.rev4 - p.wheat.rev4.diff

p.wheat.rev5 <- (sum( exp(p5C.pred_ln_wheat_rrev$fit + p5C.pred_ln_wheat_rrev$res+ p5C.pred_ln_wheat_rrev$effect ))/p.wheat.rev0 - 1)*100
p.wheat.rev5.diff <- p.wheat.rev5 - ( (p.wheat.pred_rev5 - p5C.pred_ln_wheat_rrev_se$se.sum*2.58)/p.wheat.rev0 - 1)*100
p.wheat.rev5.min <- p.wheat.rev5 + p.wheat.rev5.diff
p.wheat.rev5.max <- p.wheat.rev5 - p.wheat.rev5.diff


diff.wheat.rev0 <- (sum( exp(diff0C.pred_ln_wheat_rrev$fit + diff0C.pred_ln_wheat_rrev$res+ diff0C.pred_ln_wheat_rrev$effect )))
diff.wheat.rev1 <- (sum( exp(diff1C.pred_ln_wheat_rrev$fit + diff1C.pred_ln_wheat_rrev$res + diff1C.pred_ln_wheat_rrev$effect ))/diff.wheat.rev0 - 1)*100
diff.wheat.rev1.diff <- diff.wheat.rev1 - ( (diff.wheat.pred_rev1 - diff1C.pred_ln_wheat_rrev_se$se.sum*2.58)/diff.wheat.rev0 - 1)*100
diff.wheat.rev1.min <- diff.wheat.rev1 + diff.wheat.rev1.diff
diff.wheat.rev1.max <- diff.wheat.rev1 - diff.wheat.rev1.diff

diff.wheat.rev2 <- (sum( exp(diff2C.pred_ln_wheat_rrev$fit + diff2C.pred_ln_wheat_rrev$res+ diff2C.pred_ln_wheat_rrev$effect ))/diff.wheat.rev0 - 1)*100
diff.wheat.rev2.diff <- diff.wheat.rev2 - ( (diff.wheat.pred_rev2 - diff2C.pred_ln_wheat_rrev_se$se.sum*2.58)/diff.wheat.rev0 - 1)*100
diff.wheat.rev2.min <- diff.wheat.rev2 + diff.wheat.rev2.diff
diff.wheat.rev2.max <- diff.wheat.rev2 - diff.wheat.rev2.diff

diff.wheat.rev3 <- (sum( exp(diff3C.pred_ln_wheat_rrev$fit + diff3C.pred_ln_wheat_rrev$res + diff3C.pred_ln_wheat_rrev$effect))/diff.wheat.rev0 - 1)*100
diff.wheat.rev3.diff <- diff.wheat.rev3 - ( (diff.wheat.pred_rev3 - diff3C.pred_ln_wheat_rrev_se$se.sum*2.58)/diff.wheat.rev0 - 1)*100
diff.wheat.rev3.min <- diff.wheat.rev3 + diff.wheat.rev3.diff
diff.wheat.rev3.max <- diff.wheat.rev3 - diff.wheat.rev3.diff

diff.wheat.rev4 <- (sum( exp(diff4C.pred_ln_wheat_rrev$fit + diff4C.pred_ln_wheat_rrev$res + diff4C.pred_ln_wheat_rrev$effect))/diff.wheat.rev0 - 1)*100
diff.wheat.rev4.diff <- diff.wheat.rev4 - ( (diff.wheat.pred_rev4 - diff4C.pred_ln_wheat_rrev_se$se.sum*2.58)/diff.wheat.rev0 - 1)*100
diff.wheat.rev4.min <- diff.wheat.rev4 + diff.wheat.rev4.diff
diff.wheat.rev4.max <- diff.wheat.rev4 - diff.wheat.rev4.diff

diff.wheat.rev5 <- (sum( exp(diff5C.pred_ln_wheat_rrev$fit + diff5C.pred_ln_wheat_rrev$res + diff5C.pred_ln_wheat_rrev$effect))/diff.wheat.rev0 - 1)*100
diff.wheat.rev5.diff <- diff.wheat.rev5 - ( (diff.wheat.pred_rev5 - diff5C.pred_ln_wheat_rrev_se$se.sum*2.58)/diff.wheat.rev0 - 1)*100
diff.wheat.rev5.min <- diff.wheat.rev5 + diff.wheat.rev5.diff
diff.wheat.rev5.max <- diff.wheat.rev5 - diff.wheat.rev5.diff


wheat.pred_rev <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           rev = c(cs.wheat.pred_rev0, cs.wheat.pred_rev1, cs.wheat.pred_rev2, cs.wheat.pred_rev3, cs.wheat.pred_rev4, cs.wheat.pred_rev5,
                                   p.wheat.pred_rev0, p.wheat.pred_rev1, p.wheat.pred_rev2, p.wheat.pred_rev3, p.wheat.pred_rev4, p.wheat.pred_rev5,
                                   diff.wheat.pred_rev0, diff.wheat.pred_rev1, diff.wheat.pred_rev2, diff.wheat.pred_rev3, diff.wheat.pred_rev4, diff.wheat.pred_rev5),
                           min = c(cs.wheat.pred_rev0_min, cs.wheat.pred_rev1_min, cs.wheat.pred_rev2_min, cs.wheat.pred_rev3_min, cs.wheat.pred_rev4_min, cs.wheat.pred_rev5_min,
                                   p.wheat.pred_rev0_min, p.wheat.pred_rev1_min, p.wheat.pred_rev2_min, p.wheat.pred_rev3_min, p.wheat.pred_rev4_min, p.wheat.pred_rev5_min,
                                   diff.wheat.pred_rev0_min, diff.wheat.pred_rev1_min, diff.wheat.pred_rev2_min, diff.wheat.pred_rev3_min, diff.wheat.pred_rev4_min, diff.wheat.pred_rev5_min),
                           max = c(cs.wheat.pred_rev0_max, cs.wheat.pred_rev1_max, cs.wheat.pred_rev2_max, cs.wheat.pred_rev3_max, cs.wheat.pred_rev4_max, cs.wheat.pred_rev5_max,
                                   p.wheat.pred_rev0_max, p.wheat.pred_rev1_max, p.wheat.pred_rev2_max, p.wheat.pred_rev3_max, p.wheat.pred_rev4_max, p.wheat.pred_rev5_max,
                                   diff.wheat.pred_rev0_max, diff.wheat.pred_rev1_max, diff.wheat.pred_rev2_max, diff.wheat.pred_rev3_max, diff.wheat.pred_rev4_max, diff.wheat.pred_rev5_max),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "wheat")

wheat.plotdat <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           rev = c(0,cs.wheat.rev1, cs.wheat.rev2, cs.wheat.rev3, cs.wheat.rev4, cs.wheat.rev5,
                                   0,p.wheat.rev1, p.wheat.rev2, p.wheat.rev3, p.wheat.rev4, p.wheat.rev5,
                                   0,diff.wheat.rev1, diff.wheat.rev2, diff.wheat.rev3, diff.wheat.rev4, diff.wheat.rev5),
                           min = c(0,cs.wheat.rev1.min, cs.wheat.rev2.min, cs.wheat.rev3.min, cs.wheat.rev4.min, cs.wheat.rev5.min,
                                   0,p.wheat.rev1.min, p.wheat.rev2.min, p.wheat.rev3.min, p.wheat.rev4.min, p.wheat.rev5.min,
                                   0,diff.wheat.rev1.min, diff.wheat.rev2.min, diff.wheat.rev3.min, diff.wheat.rev4.min, diff.wheat.rev5.min),
                           max = c(0,cs.wheat.rev1.max, cs.wheat.rev2.max, cs.wheat.rev3.max, cs.wheat.rev4.max, cs.wheat.rev5.max,
                                   0,p.wheat.rev1.max, p.wheat.rev2.max, p.wheat.rev3.max, p.wheat.rev4.max, p.wheat.rev5.max,
                                   0,diff.wheat.rev1.max, diff.wheat.rev2.max, diff.wheat.rev3.max, diff.wheat.rev4.max, diff.wheat.rev5.max),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "wheat")

ggplot(wheat.plotdat, aes(temp, rev, group = reg)) + 
  geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5") + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")




# soybean 

# Predictions
cs0C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_0C)
cs1C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_1C)
cs2C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_2C)
cs3C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_3C)
cs4C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_4C)
cs5C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_5C)

p0C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_0C)
p1C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_1C)
p2C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_2C)
p3C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_3C)
p4C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_4C)
p5C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_5C)

diff0C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_0C)
diff1C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_1C)
diff2C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_2C)
diff3C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_3C)
diff4C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_4C)
diff5C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_5C)

# Bootstrapped Standard errors

cs0C.pred_ln_soybean_rrev_se <- boot.strap(exp(cs0C.pred_ln_soybean_rrev$fit + cs0C.pred_ln_soybean_rrev$res  ))
cs1C.pred_ln_soybean_rrev_se <- boot.strap(exp(cs1C.pred_ln_soybean_rrev$fit + cs1C.pred_ln_soybean_rrev$res  ))
cs2C.pred_ln_soybean_rrev_se <- boot.strap(exp(cs2C.pred_ln_soybean_rrev$fit + cs2C.pred_ln_soybean_rrev$res  ))
cs3C.pred_ln_soybean_rrev_se <- boot.strap(exp(cs3C.pred_ln_soybean_rrev$fit + cs3C.pred_ln_soybean_rrev$res  ))
cs4C.pred_ln_soybean_rrev_se <- boot.strap(exp(cs4C.pred_ln_soybean_rrev$fit + cs4C.pred_ln_soybean_rrev$res  ))
cs5C.pred_ln_soybean_rrev_se <- boot.strap(exp(cs5C.pred_ln_soybean_rrev$fit + cs5C.pred_ln_soybean_rrev$res  ))

p0C.pred_ln_soybean_rrev_se <- boot.strap(exp(p0C.pred_ln_soybean_rrev$fit + p0C.pred_ln_soybean_rrev$res + p0C.pred_ln_soybean_rrev$effect), cluster = p0C.pred_ln_soybean_rrev$pred_data$year )
p1C.pred_ln_soybean_rrev_se <- boot.strap(exp(p1C.pred_ln_soybean_rrev$fit + p1C.pred_ln_soybean_rrev$res + p1C.pred_ln_soybean_rrev$effect), cluster = p1C.pred_ln_soybean_rrev$pred_data$year )
p2C.pred_ln_soybean_rrev_se <- boot.strap(exp(p2C.pred_ln_soybean_rrev$fit + p2C.pred_ln_soybean_rrev$res + p2C.pred_ln_soybean_rrev$effect), cluster = p2C.pred_ln_soybean_rrev$pred_data$year )
p3C.pred_ln_soybean_rrev_se <- boot.strap(exp(p3C.pred_ln_soybean_rrev$fit + p3C.pred_ln_soybean_rrev$res + p3C.pred_ln_soybean_rrev$effect), cluster = p3C.pred_ln_soybean_rrev$pred_data$year )
p4C.pred_ln_soybean_rrev_se <- boot.strap(exp(p4C.pred_ln_soybean_rrev$fit + p4C.pred_ln_soybean_rrev$res + p4C.pred_ln_soybean_rrev$effect), cluster = p4C.pred_ln_soybean_rrev$pred_data$year )
p5C.pred_ln_soybean_rrev_se <- boot.strap(exp(p5C.pred_ln_soybean_rrev$fit + p5C.pred_ln_soybean_rrev$res + p5C.pred_ln_soybean_rrev$effect), cluster = p5C.pred_ln_soybean_rrev$pred_data$year )

diff0C.pred_ln_soybean_rrev_se <- boot.strap(exp(diff0C.pred_ln_soybean_rrev$fit + diff0C.pred_ln_soybean_rrev$res + diff0C.pred_ln_soybean_rrev$effect), cluster = diff0C.pred_ln_soybean_rrev$pred_data$year )
diff1C.pred_ln_soybean_rrev_se <- boot.strap(exp(diff1C.pred_ln_soybean_rrev$fit + diff1C.pred_ln_soybean_rrev$res + diff1C.pred_ln_soybean_rrev$effect), cluster = diff1C.pred_ln_soybean_rrev$pred_data$year )
diff2C.pred_ln_soybean_rrev_se <- boot.strap(exp(diff2C.pred_ln_soybean_rrev$fit + diff2C.pred_ln_soybean_rrev$res + diff2C.pred_ln_soybean_rrev$effect), cluster = diff2C.pred_ln_soybean_rrev$pred_data$year )
diff3C.pred_ln_soybean_rrev_se <- boot.strap(exp(diff3C.pred_ln_soybean_rrev$fit + diff3C.pred_ln_soybean_rrev$res + diff3C.pred_ln_soybean_rrev$effect), cluster = diff3C.pred_ln_soybean_rrev$pred_data$year )
diff4C.pred_ln_soybean_rrev_se <- boot.strap(exp(diff4C.pred_ln_soybean_rrev$fit + diff4C.pred_ln_soybean_rrev$res + diff4C.pred_ln_soybean_rrev$effect), cluster = diff4C.pred_ln_soybean_rrev$pred_data$year )
diff5C.pred_ln_soybean_rrev_se <- boot.strap(exp(diff5C.pred_ln_soybean_rrev$fit + diff5C.pred_ln_soybean_rrev$res + diff5C.pred_ln_soybean_rrev$effect), cluster = diff5C.pred_ln_soybean_rrev$pred_data$year )

# Predicted revenue
cs.soybean.pred_rev0 <- sum(exp(cs0C.pred_ln_soybean_rrev$fit + cs0C.pred_ln_soybean_rrev$res ))
cs.soybean.pred_rev1 <- sum(exp(cs1C.pred_ln_soybean_rrev$fit + cs1C.pred_ln_soybean_rrev$res ))
cs.soybean.pred_rev2 <- sum(exp(cs2C.pred_ln_soybean_rrev$fit + cs2C.pred_ln_soybean_rrev$res ))
cs.soybean.pred_rev3 <- sum(exp(cs3C.pred_ln_soybean_rrev$fit + cs3C.pred_ln_soybean_rrev$res ))
cs.soybean.pred_rev4 <- sum(exp(cs4C.pred_ln_soybean_rrev$fit + cs4C.pred_ln_soybean_rrev$res ))
cs.soybean.pred_rev5 <- sum(exp(cs5C.pred_ln_soybean_rrev$fit + cs5C.pred_ln_soybean_rrev$res ))

p.soybean.pred_rev0 <- sum(exp(p0C.pred_ln_soybean_rrev$fit + p0C.pred_ln_soybean_rrev$res + p0C.pred_ln_soybean_rrev$effect ))
p.soybean.pred_rev1 <- sum(exp(p1C.pred_ln_soybean_rrev$fit + p1C.pred_ln_soybean_rrev$res + p1C.pred_ln_soybean_rrev$effect ))
p.soybean.pred_rev2 <- sum(exp(p2C.pred_ln_soybean_rrev$fit + p2C.pred_ln_soybean_rrev$res + p2C.pred_ln_soybean_rrev$effect ))
p.soybean.pred_rev3 <- sum(exp(p3C.pred_ln_soybean_rrev$fit + p3C.pred_ln_soybean_rrev$res + p3C.pred_ln_soybean_rrev$effect ))
p.soybean.pred_rev4 <- sum(exp(p4C.pred_ln_soybean_rrev$fit + p4C.pred_ln_soybean_rrev$res + p4C.pred_ln_soybean_rrev$effect ))
p.soybean.pred_rev5 <- sum(exp(p5C.pred_ln_soybean_rrev$fit + p5C.pred_ln_soybean_rrev$res + p5C.pred_ln_soybean_rrev$effect ))

diff.soybean.pred_rev0 <- sum(exp(diff0C.pred_ln_soybean_rrev$fit + diff0C.pred_ln_soybean_rrev$res + diff0C.pred_ln_soybean_rrev$effect ))
diff.soybean.pred_rev1 <- sum(exp(diff1C.pred_ln_soybean_rrev$fit + diff1C.pred_ln_soybean_rrev$res + diff1C.pred_ln_soybean_rrev$effect ))
diff.soybean.pred_rev2 <- sum(exp(diff2C.pred_ln_soybean_rrev$fit + diff2C.pred_ln_soybean_rrev$res + diff2C.pred_ln_soybean_rrev$effect ))
diff.soybean.pred_rev3 <- sum(exp(diff3C.pred_ln_soybean_rrev$fit + diff3C.pred_ln_soybean_rrev$res + diff3C.pred_ln_soybean_rrev$effect ))
diff.soybean.pred_rev4 <- sum(exp(diff4C.pred_ln_soybean_rrev$fit + diff4C.pred_ln_soybean_rrev$res + diff4C.pred_ln_soybean_rrev$effect ))
diff.soybean.pred_rev5 <- sum(exp(diff5C.pred_ln_soybean_rrev$fit + diff5C.pred_ln_soybean_rrev$res + diff5C.pred_ln_soybean_rrev$effect ))

cs.soybean.pred_rev0_min <- cs.soybean.pred_rev0 - cs0C.pred_ln_soybean_rrev_se$se.sum*2.58
cs.soybean.pred_rev0_max <- cs.soybean.pred_rev0 - cs0C.pred_ln_soybean_rrev_se$se.sum*2.58
cs.soybean.pred_rev1_min <- cs.soybean.pred_rev1 - cs1C.pred_ln_soybean_rrev_se$se.sum*2.58
cs.soybean.pred_rev1_max <- cs.soybean.pred_rev1 + cs1C.pred_ln_soybean_rrev_se$se.sum*2.58
cs.soybean.pred_rev2_min <- cs.soybean.pred_rev2 - cs2C.pred_ln_soybean_rrev_se$se.sum*2.58
cs.soybean.pred_rev2_max <- cs.soybean.pred_rev2 + cs2C.pred_ln_soybean_rrev_se$se.sum*2.58
cs.soybean.pred_rev3_min <- cs.soybean.pred_rev3 - cs3C.pred_ln_soybean_rrev_se$se.sum*2.58
cs.soybean.pred_rev3_max <- cs.soybean.pred_rev3 + cs3C.pred_ln_soybean_rrev_se$se.sum*2.58
cs.soybean.pred_rev4_min <- cs.soybean.pred_rev4 - cs4C.pred_ln_soybean_rrev_se$se.sum*2.58
cs.soybean.pred_rev4_max <- cs.soybean.pred_rev4 + cs4C.pred_ln_soybean_rrev_se$se.sum*2.58
cs.soybean.pred_rev5_min <- cs.soybean.pred_rev5 - cs5C.pred_ln_soybean_rrev_se$se.sum*2.58
cs.soybean.pred_rev5_max <- cs.soybean.pred_rev5 + cs5C.pred_ln_soybean_rrev_se$se.sum*2.58

p.soybean.pred_rev0_min <- p.soybean.pred_rev0 - p0C.pred_ln_soybean_rrev_se$se.sum*2.58
p.soybean.pred_rev0_max <- p.soybean.pred_rev0 + p0C.pred_ln_soybean_rrev_se$se.sum*2.58
p.soybean.pred_rev1_min <- p.soybean.pred_rev1 - p1C.pred_ln_soybean_rrev_se$se.sum*2.58
p.soybean.pred_rev1_max <- p.soybean.pred_rev1 + p1C.pred_ln_soybean_rrev_se$se.sum*2.58
p.soybean.pred_rev2_min <- p.soybean.pred_rev2 - p2C.pred_ln_soybean_rrev_se$se.sum*2.58
p.soybean.pred_rev2_max <- p.soybean.pred_rev2 + p2C.pred_ln_soybean_rrev_se$se.sum*2.58
p.soybean.pred_rev3_min <- p.soybean.pred_rev3 - p3C.pred_ln_soybean_rrev_se$se.sum*2.58
p.soybean.pred_rev3_max <- p.soybean.pred_rev3 + p3C.pred_ln_soybean_rrev_se$se.sum*2.58
p.soybean.pred_rev4_min <- p.soybean.pred_rev4 - p4C.pred_ln_soybean_rrev_se$se.sum*2.58
p.soybean.pred_rev4_max <- p.soybean.pred_rev4 + p4C.pred_ln_soybean_rrev_se$se.sum*2.58
p.soybean.pred_rev5_min <- p.soybean.pred_rev5 - p5C.pred_ln_soybean_rrev_se$se.sum*2.58
p.soybean.pred_rev5_max <- p.soybean.pred_rev5 + p5C.pred_ln_soybean_rrev_se$se.sum*2.58

diff.soybean.pred_rev0_min <- diff.soybean.pred_rev0 - diff0C.pred_ln_soybean_rrev_se$se.sum*2.58
diff.soybean.pred_rev0_max <- diff.soybean.pred_rev0 + diff0C.pred_ln_soybean_rrev_se$se.sum*2.58
diff.soybean.pred_rev1_min <- diff.soybean.pred_rev1 - diff1C.pred_ln_soybean_rrev_se$se.sum*2.58
diff.soybean.pred_rev1_max <- diff.soybean.pred_rev1 + diff1C.pred_ln_soybean_rrev_se$se.sum*2.58
diff.soybean.pred_rev2_min <- diff.soybean.pred_rev2 - diff2C.pred_ln_soybean_rrev_se$se.sum*2.58
diff.soybean.pred_rev2_max <- diff.soybean.pred_rev2 + diff2C.pred_ln_soybean_rrev_se$se.sum*2.58
diff.soybean.pred_rev3_min <- diff.soybean.pred_rev3 - diff3C.pred_ln_soybean_rrev_se$se.sum*2.58
diff.soybean.pred_rev3_max <- diff.soybean.pred_rev3 + diff3C.pred_ln_soybean_rrev_se$se.sum*2.58
diff.soybean.pred_rev4_min <- diff.soybean.pred_rev4 - diff4C.pred_ln_soybean_rrev_se$se.sum*2.58
diff.soybean.pred_rev4_max <- diff.soybean.pred_rev4 + diff4C.pred_ln_soybean_rrev_se$se.sum*2.58
diff.soybean.pred_rev5_min <- diff.soybean.pred_rev5 - diff5C.pred_ln_soybean_rrev_se$se.sum*2.58
diff.soybean.pred_rev5_max <- diff.soybean.pred_rev5 + diff5C.pred_ln_soybean_rrev_se$se.sum*2.58

# Get % changes
cs.soybean.rev0 <- (sum( exp(cs0C.pred_ln_soybean_rrev$fit + cs0C.pred_ln_soybean_rrev$res )))

cs.soybean.rev1 <- (sum( exp(cs1C.pred_ln_soybean_rrev$fit + cs1C.pred_ln_soybean_rrev$res  ))/cs.soybean.rev0 - 1)*100
cs.soybean.rev1.diff <- cs.soybean.rev1 - ( (cs.soybean.pred_rev1 - cs1C.pred_ln_soybean_rrev_se$se.sum*2.58)/cs.soybean.rev0 - 1)*100
cs.soybean.rev1.min <- cs.soybean.rev1 + cs.soybean.rev1.diff
cs.soybean.rev1.max <- cs.soybean.rev1 - cs.soybean.rev1.diff

cs.soybean.rev2 <- (sum( exp(cs2C.pred_ln_soybean_rrev$fit + cs2C.pred_ln_soybean_rrev$res ))/cs.soybean.rev0 - 1)*100
cs.soybean.rev2.diff <- cs.soybean.rev2 - ( (cs.soybean.pred_rev2 - cs2C.pred_ln_soybean_rrev_se$se.sum*2.58)/cs.soybean.rev0 - 1)*100
cs.soybean.rev2.min <- cs.soybean.rev2 + cs.soybean.rev2.diff
cs.soybean.rev2.max <- cs.soybean.rev2 - cs.soybean.rev2.diff

cs.soybean.rev3 <- (sum( exp(cs3C.pred_ln_soybean_rrev$fit + cs3C.pred_ln_soybean_rrev$res ))/cs.soybean.rev0 - 1)*100
cs.soybean.rev3.diff <- cs.soybean.rev3 - ( (cs.soybean.pred_rev3 - cs3C.pred_ln_soybean_rrev_se$se.sum*2.58)/cs.soybean.rev0 - 1)*100
cs.soybean.rev3.min <- cs.soybean.rev3 + cs.soybean.rev3.diff
cs.soybean.rev3.max <- cs.soybean.rev3 - cs.soybean.rev3.diff

cs.soybean.rev4 <- (sum( exp(cs4C.pred_ln_soybean_rrev$fit + cs4C.pred_ln_soybean_rrev$res ))/cs.soybean.rev0 - 1)*100
cs.soybean.rev4.diff <- cs.soybean.rev4 - ( (cs.soybean.pred_rev4 - cs4C.pred_ln_soybean_rrev_se$se.sum*2.58)/cs.soybean.rev0 - 1)*100
cs.soybean.rev4.min <- cs.soybean.rev4 + cs.soybean.rev4.diff
cs.soybean.rev4.max <- cs.soybean.rev4 - cs.soybean.rev4.diff

cs.soybean.rev5 <- (sum( exp(cs5C.pred_ln_soybean_rrev$fit + cs5C.pred_ln_soybean_rrev$res ))/cs.soybean.rev0 - 1)*100
cs.soybean.rev5.diff <- cs.soybean.rev5 - ( (cs.soybean.pred_rev5 - cs5C.pred_ln_soybean_rrev_se$se.sum*2.58)/cs.soybean.rev0 - 1)*100
cs.soybean.rev5.min <- cs.soybean.rev5 + cs.soybean.rev5.diff
cs.soybean.rev5.max <- cs.soybean.rev5 - cs.soybean.rev5.diff
cs.soybean.rev0 <- (sum( exp(cs0C.pred_ln_soybean_rrev$fit + cs0C.pred_ln_soybean_rrev$res )))

p.soybean.rev0 <- (sum( exp(p0C.pred_ln_soybean_rrev$fit + p0C.pred_ln_soybean_rrev$res + p0C.pred_ln_soybean_rrev$effect )))
p.soybean.rev1 <- (sum( exp(p1C.pred_ln_soybean_rrev$fit + p1C.pred_ln_soybean_rrev$res + p1C.pred_ln_soybean_rrev$effect ))/p.soybean.rev0 - 1)*100
p.soybean.rev1.diff <- p.soybean.rev1 - ((p.soybean.pred_rev1 - p1C.pred_ln_soybean_rrev_se$se.sum*2.58)/p.soybean.rev0 - 1)*100
p.soybean.rev1.min <- p.soybean.rev1 + p.soybean.rev1.diff
p.soybean.rev1.max <- p.soybean.rev1 - p.soybean.rev1.diff

p.soybean.rev2 <- (sum( exp(p2C.pred_ln_soybean_rrev$fit + p2C.pred_ln_soybean_rrev$res + p2C.pred_ln_soybean_rrev$effect ))/p.soybean.rev0 - 1)*100
p.soybean.rev2.diff <- p.soybean.rev2 - ( (p.soybean.pred_rev2 - p2C.pred_ln_soybean_rrev_se$se.sum*2.58)/p.soybean.rev0 - 1)*100
p.soybean.rev2.min <- p.soybean.rev2 + p.soybean.rev2.diff
p.soybean.rev2.max <- p.soybean.rev2 - p.soybean.rev2.diff

p.soybean.rev3 <- (sum( exp(p3C.pred_ln_soybean_rrev$fit + p3C.pred_ln_soybean_rrev$res+ p3C.pred_ln_soybean_rrev$effect ))/p.soybean.rev0 - 1)*100
p.soybean.rev3.diff <- p.soybean.rev3 - ( (p.soybean.pred_rev3 - p3C.pred_ln_soybean_rrev_se$se.sum*2.58)/p.soybean.rev0 - 1)*100
p.soybean.rev3.min <- p.soybean.rev3 + p.soybean.rev3.diff
p.soybean.rev3.max <- p.soybean.rev3 - p.soybean.rev3.diff

p.soybean.rev4 <- (sum( exp(p4C.pred_ln_soybean_rrev$fit + p4C.pred_ln_soybean_rrev$res + p4C.pred_ln_soybean_rrev$effect))/p.soybean.rev0 - 1)*100
p.soybean.rev4.diff <- p.soybean.rev4 - ( (p.soybean.pred_rev4 - p4C.pred_ln_soybean_rrev_se$se.sum*2.58)/p.soybean.rev0 - 1)*100
p.soybean.rev4.min <- p.soybean.rev4 + p.soybean.rev4.diff
p.soybean.rev4.max <- p.soybean.rev4 - p.soybean.rev4.diff

p.soybean.rev5 <- (sum( exp(p5C.pred_ln_soybean_rrev$fit + p5C.pred_ln_soybean_rrev$res+ p5C.pred_ln_soybean_rrev$effect ))/p.soybean.rev0 - 1)*100
p.soybean.rev5.diff <- p.soybean.rev5 - ( (p.soybean.pred_rev5 - p5C.pred_ln_soybean_rrev_se$se.sum*2.58)/p.soybean.rev0 - 1)*100
p.soybean.rev5.min <- p.soybean.rev5 + p.soybean.rev5.diff
p.soybean.rev5.max <- p.soybean.rev5 - p.soybean.rev5.diff


diff.soybean.rev0 <- (sum( exp(diff0C.pred_ln_soybean_rrev$fit + diff0C.pred_ln_soybean_rrev$res+ diff0C.pred_ln_soybean_rrev$effect )))
diff.soybean.rev1 <- (sum( exp(diff1C.pred_ln_soybean_rrev$fit + diff1C.pred_ln_soybean_rrev$res + diff1C.pred_ln_soybean_rrev$effect ))/diff.soybean.rev0 - 1)*100
diff.soybean.rev1.diff <- diff.soybean.rev1 - ( (diff.soybean.pred_rev1 - diff1C.pred_ln_soybean_rrev_se$se.sum*2.58)/diff.soybean.rev0 - 1)*100
diff.soybean.rev1.min <- diff.soybean.rev1 + diff.soybean.rev1.diff
diff.soybean.rev1.max <- diff.soybean.rev1 - diff.soybean.rev1.diff

diff.soybean.rev2 <- (sum( exp(diff2C.pred_ln_soybean_rrev$fit + diff2C.pred_ln_soybean_rrev$res+ diff2C.pred_ln_soybean_rrev$effect ))/diff.soybean.rev0 - 1)*100
diff.soybean.rev2.diff <- diff.soybean.rev2 - ( (diff.soybean.pred_rev2 - diff2C.pred_ln_soybean_rrev_se$se.sum*2.58)/diff.soybean.rev0 - 1)*100
diff.soybean.rev2.min <- diff.soybean.rev2 + diff.soybean.rev2.diff
diff.soybean.rev2.max <- diff.soybean.rev2 - diff.soybean.rev2.diff

diff.soybean.rev3 <- (sum( exp(diff3C.pred_ln_soybean_rrev$fit + diff3C.pred_ln_soybean_rrev$res + diff3C.pred_ln_soybean_rrev$effect))/diff.soybean.rev0 - 1)*100
diff.soybean.rev3.diff <- diff.soybean.rev3 - ( (diff.soybean.pred_rev3 - diff3C.pred_ln_soybean_rrev_se$se.sum*2.58)/diff.soybean.rev0 - 1)*100
diff.soybean.rev3.min <- diff.soybean.rev3 + diff.soybean.rev3.diff
diff.soybean.rev3.max <- diff.soybean.rev3 - diff.soybean.rev3.diff

diff.soybean.rev4 <- (sum( exp(diff4C.pred_ln_soybean_rrev$fit + diff4C.pred_ln_soybean_rrev$res + diff4C.pred_ln_soybean_rrev$effect))/diff.soybean.rev0 - 1)*100
diff.soybean.rev4.diff <- diff.soybean.rev4 - ( (diff.soybean.pred_rev4 - diff4C.pred_ln_soybean_rrev_se$se.sum*2.58)/diff.soybean.rev0 - 1)*100
diff.soybean.rev4.min <- diff.soybean.rev4 + diff.soybean.rev4.diff
diff.soybean.rev4.max <- diff.soybean.rev4 - diff.soybean.rev4.diff

diff.soybean.rev5 <- (sum( exp(diff5C.pred_ln_soybean_rrev$fit + diff5C.pred_ln_soybean_rrev$res + diff5C.pred_ln_soybean_rrev$effect))/diff.soybean.rev0 - 1)*100
diff.soybean.rev5.diff <- diff.soybean.rev5 - ( (diff.soybean.pred_rev5 - diff5C.pred_ln_soybean_rrev_se$se.sum*2.58)/diff.soybean.rev0 - 1)*100
diff.soybean.rev5.min <- diff.soybean.rev5 + diff.soybean.rev5.diff
diff.soybean.rev5.max <- diff.soybean.rev5 - diff.soybean.rev5.diff


soybean.pred_rev <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           rev = c(cs.soybean.pred_rev0, cs.soybean.pred_rev1, cs.soybean.pred_rev2, cs.soybean.pred_rev3, cs.soybean.pred_rev4, cs.soybean.pred_rev5,
                                   p.soybean.pred_rev0, p.soybean.pred_rev1, p.soybean.pred_rev2, p.soybean.pred_rev3, p.soybean.pred_rev4, p.soybean.pred_rev5,
                                   diff.soybean.pred_rev0, diff.soybean.pred_rev1, diff.soybean.pred_rev2, diff.soybean.pred_rev3, diff.soybean.pred_rev4, diff.soybean.pred_rev5),
                           min = c(cs.soybean.pred_rev0_min, cs.soybean.pred_rev1_min, cs.soybean.pred_rev2_min, cs.soybean.pred_rev3_min, cs.soybean.pred_rev4_min, cs.soybean.pred_rev5_min,
                                   p.soybean.pred_rev0_min, p.soybean.pred_rev1_min, p.soybean.pred_rev2_min, p.soybean.pred_rev3_min, p.soybean.pred_rev4_min, p.soybean.pred_rev5_min,
                                   diff.soybean.pred_rev0_min, diff.soybean.pred_rev1_min, diff.soybean.pred_rev2_min, diff.soybean.pred_rev3_min, diff.soybean.pred_rev4_min, diff.soybean.pred_rev5_min),
                           max = c(cs.soybean.pred_rev0_max, cs.soybean.pred_rev1_max, cs.soybean.pred_rev2_max, cs.soybean.pred_rev3_max, cs.soybean.pred_rev4_max, cs.soybean.pred_rev5_max,
                                   p.soybean.pred_rev0_max, p.soybean.pred_rev1_max, p.soybean.pred_rev2_max, p.soybean.pred_rev3_max, p.soybean.pred_rev4_max, p.soybean.pred_rev5_max,
                                   diff.soybean.pred_rev0_max, diff.soybean.pred_rev1_max, diff.soybean.pred_rev2_max, diff.soybean.pred_rev3_max, diff.soybean.pred_rev4_max, diff.soybean.pred_rev5_max),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "soybean")

soybean.plotdat <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           rev = c(0,cs.soybean.rev1, cs.soybean.rev2, cs.soybean.rev3, cs.soybean.rev4, cs.soybean.rev5,
                                   0,p.soybean.rev1, p.soybean.rev2, p.soybean.rev3, p.soybean.rev4, p.soybean.rev5,
                                   0,diff.soybean.rev1, diff.soybean.rev2, diff.soybean.rev3, diff.soybean.rev4, diff.soybean.rev5),
                           min = c(0,cs.soybean.rev1.min, cs.soybean.rev2.min, cs.soybean.rev3.min, cs.soybean.rev4.min, cs.soybean.rev5.min,
                                   0,p.soybean.rev1.min, p.soybean.rev2.min, p.soybean.rev3.min, p.soybean.rev4.min, p.soybean.rev5.min,
                                   0,diff.soybean.rev1.min, diff.soybean.rev2.min, diff.soybean.rev3.min, diff.soybean.rev4.min, diff.soybean.rev5.min),
                           max = c(0,cs.soybean.rev1.max, cs.soybean.rev2.max, cs.soybean.rev3.max, cs.soybean.rev4.max, cs.soybean.rev5.max,
                                   0,p.soybean.rev1.max, p.soybean.rev2.max, p.soybean.rev3.max, p.soybean.rev4.max, p.soybean.rev5.max,
                                   0,diff.soybean.rev1.max, diff.soybean.rev2.max, diff.soybean.rev3.max, diff.soybean.rev4.max, diff.soybean.rev5.max),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = "soybean")

ggplot(soybean.plotdat, aes(temp, rev, group = reg)) + 
  geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5") + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")



#  Full Plot --------------------------------------------------------------
library(ggrepel)
library(ggthemes)
library(ggplot2)

pred_rev_dat <- rbind(corn.pred_rev, cotton.pred_rev, hay.pred_rev, wheat.pred_rev, soybean.pred_rev)
saveRDS(pred_rev_dat, "data/pred_rev_dat.rds")

# Merge data
plotdat <- rbind(corn.plotdat, cotton.plotdat, hay.plotdat, wheat.plotdat, soybean.plotdat)
saveRDS(plotdat, "data/pred_plot_dat.rds")
plotdat <- readRDS("data/pred_plot_dat.rds")
plotdat$rev <- round(plotdat$rev, 2)
plotdat$min <- round(plotdat$min, 2)
plotdat$max <- round(plotdat$max, 2)

p1 <- ggplot(plotdat, aes(temp, rev, color = reg)) + 
  geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5", size = 0) + 
  geom_line() + ylab("Revenue/Acre Impact (% Change) ") + 
  geom_point(size = 0.5) +
  xlab("Change in Temperature (C)") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
  facet_wrap(~crop) + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
  theme_tufte(base_size = 14) +
  theme(legend.position = "top",
        legend.title = element_blank()) + 
  geom_text_repel(aes(temp, rev, label = round(rev)), show.legend  = FALSE, alpha = 0.5) 
p1

p1 <- ggplot(filter(plotdat, crop %in% c("Corn", "cotton", "hay", "wheat")), aes(temp, rev, color = reg)) + 
  ylab("Revenue/Acre Impact (% Change) ") + 
  geom_ribbon(aes(ymin = min, ymax = max), size = 0, fill = "#C0CCD5") + 
  geom_line() +
  ylim(-100, 50) +
  geom_point(size = 0.5) +
  xlab("Change in Temperature (C)") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
  facet_wrap(~crop) + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
  theme_tufte(base_size = 14) +
  theme(legend.position = "top",
        legend.title = element_blank()) + 
  geom_text_repel(aes(temp, rev, label = rev), show.legend  = FALSE, alpha = 0.5) 
p1
# 
# df.labeled <- plotdat %>%
#   ungroup() %>% group_by(crop, rev) %>%
#   mutate(label = c(rev[1], rep(NA, length(rev) - 1)))
# 
# df.labeled = df.labeled %>% 
#   group_by(crop, is.na(label)) %>% 
#   arrange(rev) %>%
#   mutate(xval = rep(c(-0.1,-0.35), ceiling(n()/2))[1:n()])
# 
# 
# ggplot(plotdat, aes(temp, rev, color = reg)) + 
#   ylab("Revenue Impact (% Change) ") + xlab("Change in Temperature (C)") + 
#   geom_text(aes(label = rev), size=3, show.legend=FALSE) + theme_tufte(base_size = 12) +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   geom_line(alpha=0.3, size=0.7) + 
#   facet_wrap(~crop) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
#   scale_x_continuous( labels = c("+1", "+2", "+3", "+4", "+5")) +
#   #theme_bw() + 
#   theme(panel.grid = element_blank(),legend.position = "top",
#         legend.title = element_blank()) +
#   guides(colour=guide_legend(override.aes=list(alpha = 1, size=1)))
# 
# 
# 
# ggplot(df.labeled, aes(temp, rev, color = reg)) +
#   geom_segment(aes(xend = 0, yend = rev), linetype = "dashed", color = "grey") +
#   geom_text(aes(label = label, x = -0.1), colour = "black", hjust = 1) +
#   geom_vline(xintercept = 0) +
#   geom_point() + geom_line() + facet_wrap(~crop, ncol = 3) +
#   scale_y_continuous(breaks = NULL) +
#   scale_x_continuous(limits = c(-0.5, NA)) +
#   theme(panel.grid = element_blank()) + theme_base()
# 
# 
# p2 <- ggplot(filter(plotdat, crop %in% c("corn", "cotton", "hay", "soybean")), aes(temp, rev, color = reg)) +
#   geom_line() + ylab("Impact (% Change) ") +
#   xlab("Change in Temperature (C)") + geom_hline(yintercept = 0, linetype = "dashed")+
#   facet_wrap(~crop)
# p2
# 
# plot_grid(p1, p2, ncol = 1)
# 
# c1 <- ggplot(filter(plotdat, reg == "cross-section"), aes(temp, rev, color = reg)) +
#   geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5", alpha = 0.5) +
#   geom_line() + ylab("Revenue Impact (% Change) ") + geom_point() +
#   xlab("Change in Temperature (C)") + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
#    facet_wrap(~crop, ncol = 5, scale = "free") + scale_y_continuous(breaks = plotdat$rev)
# c1
# 
# c2 <- ggplot(filter(plotdat, reg == "panel"), aes(temp, rev, color = reg)) +
#   geom_line() + ylab("Revenue Impact (% Change) ") + geom_point() +
#   xlab("Change in Temperature (C)") + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
#    facet_wrap(~crop, ncol = 5) + scale_y_continuous(breaks = plotdat$rev)
# c2
# 
# c3 <- ggplot(filter(plotdat, reg == "diff"), aes(temp, rev, color = reg)) +
#   geom_line() + ylab("Revenue Impact (% Change) ") + geom_point() +
#   xlab("Change in Temperature (C)") + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
#    facet_wrap(~crop, ncol = 5) + scale_y_continuous(breaks = plotdat$rev)
# c3
# 
# plot_grid(c1, c2, c3, ncol = 1)
# 
# # Prediction tables
# pred.table <- data.frame(temp = rep(c("+1C", "+2C", "+3C", "+4C", "+5C"), 15),
#                          crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"),3, each = 5),
#                          reg = rep(c("Cross-Section", "Panel", "Difference"), each = 25),
#                          impact = c(cs.corn.rev1, cs.corn.rev2, cs.corn.rev3, cs.corn.rev4, cs.corn.rev5,
#                                     cs.cotton.rev1, cs.cotton.rev2, cs.cotton.rev3, cs.cotton.rev4, cs.cotton.rev5,
#                                     cs.hay.rev1, cs.hay.rev2, cs.hay.rev3, cs.hay.rev4, cs.hay.rev5,
#                                     cs.wheat.rev1, cs.wheat.rev2, cs.wheat.rev3, cs.wheat.rev4, cs.wheat.rev5,
#                                     cs.soybean.rev1, cs.soybean.rev2, cs.soybean.rev3, cs.soybean.rev4, cs.soybean.rev5,
#                                     p.corn.rev1, p.corn.rev2, p.corn.rev3, p.corn.rev4, p.corn.rev5,
#                                     p.cotton.rev1, p.cotton.rev2, p.cotton.rev3, p.cotton.rev4, p.cotton.rev5,
#                                     p.hay.rev1, p.hay.rev2, p.hay.rev3, p.hay.rev4, p.hay.rev5,
#                                     p.wheat.rev1, p.wheat.rev2, p.wheat.rev3, p.wheat.rev4, p.wheat.rev5,
#                                     p.soybean.rev1, p.soybean.rev2, p.soybean.rev3, p.soybean.rev4, p.soybean.rev5,
#                                     diff.corn.rev1, diff.corn.rev2, diff.corn.rev3, diff.corn.rev4, diff.corn.rev5,
#                                     diff.cotton.rev1, diff.cotton.rev2, diff.cotton.rev3, diff.cotton.rev4, diff.cotton.rev5,
#                                     diff.hay.rev1, diff.hay.rev2, diff.hay.rev3, diff.hay.rev4, diff.hay.rev5,
#                                     diff.wheat.rev1, diff.wheat.rev2, diff.wheat.rev3, diff.wheat.rev4, diff.wheat.rev5,
#                                     diff.soybean.rev1, diff.soybean.rev2, diff.soybean.rev3, diff.soybean.rev4, diff.soybean.rev5),
#                          se = c(cs1C.pred_ln_corn_rrev_se, cs2C.pred_ln_corn_rrev_se, cs3C.pred_ln_corn_rrev_se, cs4C.pred_ln_corn_rrev_se, cs5C.pred_ln_corn_rrev_se,
#                                   cs1C.pred_ln_cotton_rrev_se, cs2C.pred_ln_cotton_rrev_se, cs3C.pred_ln_cotton_rrev_se, cs4C.pred_ln_cotton_rrev_se, cs5C.pred_ln_cotton_rrev_se,
#                                   cs1C.pred_ln_hay_rrev_se, cs2C.pred_ln_hay_rrev_se, cs3C.pred_ln_hay_rrev_se, cs4C.pred_ln_hay_rrev_se, cs5C.pred_ln_hay_rrev_se,
#                                   cs1C.pred_ln_wheat_rrev_se, cs2C.pred_ln_wheat_rrev_se, cs3C.pred_ln_wheat_rrev_se, cs4C.pred_ln_wheat_rrev_se, cs5C.pred_ln_wheat_rrev_se,
#                                   cs1C.pred_ln_soybean_rrev_se, cs2C.pred_ln_soybean_rrev_se, cs3C.pred_ln_soybean_rrev_se, cs4C.pred_ln_soybean_rrev_se, cs5C.pred_ln_soybean_rrev_se,
#                                   p1C.pred_ln_corn_rrev_se, p2C.pred_ln_corn_rrev_se, p3C.pred_ln_corn_rrev_se, p4C.pred_ln_corn_rrev_se, p5C.pred_ln_corn_rrev_se,
#                                   p1C.pred_ln_cotton_rrev_se, p2C.pred_ln_cotton_rrev_se, p3C.pred_ln_cotton_rrev_se, p4C.pred_ln_cotton_rrev_se, p5C.pred_ln_cotton_rrev_se,
#                                   p1C.pred_ln_hay_rrev_se, p2C.pred_ln_hay_rrev_se, p3C.pred_ln_hay_rrev_se, p4C.pred_ln_hay_rrev_se, p5C.pred_ln_hay_rrev_se,
#                                   p1C.pred_ln_wheat_rrev_se, p2C.pred_ln_wheat_rrev_se, p3C.pred_ln_wheat_rrev_se, p4C.pred_ln_wheat_rrev_se, p5C.pred_ln_wheat_rrev_se,
#                                   p1C.pred_ln_soybean_rrev_se, p2C.pred_ln_soybean_rrev_se, p3C.pred_ln_soybean_rrev_se, p4C.pred_ln_soybean_rrev_se, p5C.pred_ln_soybean_rrev_se,
#                                   diff1C.pred_ln_corn_rrev_se, diff2C.pred_ln_corn_rrev_se, diff3C.pred_ln_corn_rrev_se, diff4C.pred_ln_corn_rrev_se, diff5C.pred_ln_corn_rrev_se,
#                                   diff1C.pred_ln_cotton_rrev_se, diff2C.pred_ln_cotton_rrev_se, diff3C.pred_ln_cotton_rrev_se, diff4C.pred_ln_cotton_rrev_se, diff5C.pred_ln_cotton_rrev_se,
#                                   diff1C.pred_ln_hay_rrev_se, diff2C.pred_ln_hay_rrev_se, diff3C.pred_ln_hay_rrev_se, diff4C.pred_ln_hay_rrev_se, diff5C.pred_ln_hay_rrev_se,
#                                   diff1C.pred_ln_wheat_rrev_se, diff2C.pred_ln_wheat_rrev_se, diff3C.pred_ln_wheat_rrev_se, diff4C.pred_ln_wheat_rrev_se, diff5C.pred_ln_wheat_rrev_se,
#                                   diff1C.pred_ln_soybean_rrev_se, diff2C.pred_ln_soybean_rrev_se, diff3C.pred_ln_soybean_rrev_se, diff4C.pred_ln_soybean_rrev_se, diff5C.pred_ln_soybean_rrev_se))
# head(pred.table)
# pred.table
# setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
# saveRDS(pred.table, "tables/predictions.rds")
# 
# head(pred.table)
# library()
# tab <- tabular
# 
# 
# 
# 
# 
# 
# 
# 
