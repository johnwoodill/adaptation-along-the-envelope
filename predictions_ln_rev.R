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
cs2C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_2C)
cs3C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_3C)
cs4C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_4C)
cs5C.pred_ln_corn_rrev <- predictFelm(felm.fit = cs.ln_corn_rrev, newdata = cs.ln_corn_rrev_5C)

p1C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_1C)
p2C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_2C)
p3C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_3C)
p4C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_4C)
p5C.pred_ln_corn_rrev <- predictFelm(felm.fit = p.ln_corn_rrev, newdata = p.ln_corn_rrev_5C)

diff1C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_1C)
diff2C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_2C)
diff3C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_3C)
diff4C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_4C)
diff5C.pred_ln_corn_rrev <- predictFelm(felm.fit = diff.ln_corn_rrev, newdata = diff.ln_corn_rrev_5C)


cs.rev1C <- exp(cs1C.pred_ln_corn_rrev$fit)
cs.rev2C <- exp(cs2C.pred_ln_corn_rrev$fit + cs2C.pred_ln_corn_rrev$res + cs2C.pred_ln_corn_rrev$effect)
cs.rev3C <- exp(cs3C.pred_ln_corn_rrev$fit + cs3C.pred_ln_corn_rrev$res + cs3C.pred_ln_corn_rrev$effect)
cs.rev4C <- exp(cs4C.pred_ln_corn_rrev$fit + cs4C.pred_ln_corn_rrev$res + cs4C.pred_ln_corn_rrev$effect)
cs.rev5C <- exp(cs5C.pred_ln_corn_rrev$fit + cs5C.pred_ln_corn_rrev$res + cs5C.pred_ln_corn_rrev$effect)

p.rev1C <- exp(p1C.pred_ln_corn_rrev$fit + p1C.pred_ln_corn_rrev$res + p1C.pred_ln_corn_rrev$effect)
p.rev2C <- exp(p2C.pred_ln_corn_rrev$fit + p2C.pred_ln_corn_rrev$res + p2C.pred_ln_corn_rrev$effect)
p.rev3C <- exp(p3C.pred_ln_corn_rrev$fit + p3C.pred_ln_corn_rrev$res + p3C.pred_ln_corn_rrev$effect)
p.rev4C <- exp(p4C.pred_ln_corn_rrev$fit + p4C.pred_ln_corn_rrev$res + p4C.pred_ln_corn_rrev$effect)
p.rev5C <- exp(p5C.pred_ln_corn_rrev$fit + p5C.pred_ln_corn_rrev$res + p5C.pred_ln_corn_rrev$effect)

diff.rev1C <- exp(diff1C.pred_ln_corn_rrev$fit + diff1C.pred_ln_corn_rrev$res + diff1C.pred_ln_corn_rrev$effect)
diff.rev2C <- exp(diff2C.pred_ln_corn_rrev$fit + diff2C.pred_ln_corn_rrev$res + diff2C.pred_ln_corn_rrev$effect)
diff.rev3C <- exp(diff3C.pred_ln_corn_rrev$fit + diff3C.pred_ln_corn_rrev$res + diff3C.pred_ln_corn_rrev$effect)
diff.rev4C <- exp(diff4C.pred_ln_corn_rrev$fit + diff4C.pred_ln_corn_rrev$res + diff4C.pred_ln_corn_rrev$effect)
diff.rev5C <- exp(diff5C.pred_ln_corn_rrev$fit + diff5C.pred_ln_corn_rrev$res + diff5C.pred_ln_corn_rrev$effect)

cs.rev <- data.frame(rev = c(exp(cs.ln_corn_rrev), cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
                     change = rep(c("Base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))

p.rev <- data.frame(rev = c(p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
                     change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))

diff.rev <- data.frame(rev = c(diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
                     change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))


ggplot(cs.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")
ggplot(filter(p.rev, rev < 50), aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
ggplot(diff.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")


cs.corn.rev0 <- sum( exp(cs.ln_corn_rrev$response))
cs.corn.rev1 <- (sum( exp(cs1C.pred_ln_corn_rrev$fit + cs1C.pred_ln_corn_rrev$res + cs1C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev2 <- (sum( exp(cs2C.pred_ln_corn_rrev$fit + cs2C.pred_ln_corn_rrev$res + cs2C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev3 <- (sum( exp(cs3C.pred_ln_corn_rrev$fit + cs3C.pred_ln_corn_rrev$res + cs3C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev4 <- (sum( exp(cs4C.pred_ln_corn_rrev$fit + cs4C.pred_ln_corn_rrev$res + cs4C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100
cs.corn.rev5 <- (sum( exp(cs5C.pred_ln_corn_rrev$fit + cs5C.pred_ln_corn_rrev$res + cs5C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/cs.corn.rev0 - 1)*100

p.corn.rev0 <- sum( exp(p.ln_corn_rrev$response))
p.corn.rev1 <- (sum( exp(p1C.pred_ln_corn_rrev$fit + p1C.pred_ln_corn_rrev$res + p1C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev2 <- (sum( exp(p2C.pred_ln_corn_rrev$fit + p2C.pred_ln_corn_rrev$res + p2C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev3 <- (sum( exp(p3C.pred_ln_corn_rrev$fit + p3C.pred_ln_corn_rrev$res + p3C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev4 <- (sum( exp(p4C.pred_ln_corn_rrev$fit + p4C.pred_ln_corn_rrev$res + p4C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100
p.corn.rev5 <- (sum( exp(p5C.pred_ln_corn_rrev$fit + p5C.pred_ln_corn_rrev$res + p5C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/p.corn.rev0 - 1)*100

diff.corn.rev0 <- sum( exp(diff.ln_corn_rrev$response))
diff.corn.rev1 <- (sum( exp(diff1C.pred_ln_corn_rrev$fit + diff1C.pred_ln_corn_rrev$res + diff1C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev2 <- (sum( exp(diff2C.pred_ln_corn_rrev$fit + diff2C.pred_ln_corn_rrev$res + diff2C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev3 <- (sum( exp(diff3C.pred_ln_corn_rrev$fit + diff3C.pred_ln_corn_rrev$res + diff3C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev4 <- (sum( exp(diff4C.pred_ln_corn_rrev$fit + diff4C.pred_ln_corn_rrev$res + diff4C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100
diff.corn.rev5 <- (sum( exp(diff5C.pred_ln_corn_rrev$fit + diff5C.pred_ln_corn_rrev$res + diff5C.pred_ln_corn_rrev$effect ), na.rm = TRUE)/diff.corn.rev0 - 1)*100

corn.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.corn.rev1, cs.corn.rev2, cs.corn.rev3, cs.corn.rev4, cs.corn.rev5,
                                   p.corn.rev1, p.corn.rev2, p.corn.rev3, p.corn.rev4, p.corn.rev5,
                                   diff.corn.rev1, diff.corn.rev2, diff.corn.rev3, diff.corn.rev4, diff.corn.rev5),
                           reg = rep(c("cross-section", "panel", "diff"), each = 5),
                           crop = "corn")

ggplot(corn.plotdat, aes(temp, rev, color = reg)) + geom_line()
}

# Cotton
{
cs1C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_1C)
cs2C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_2C)
cs3C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_3C)
cs4C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_4C)
cs5C.pred_ln_cotton_rrev <- predictFelm(felm.fit = cs.ln_cotton_rrev, newdata = cs.ln_cotton_rrev_5C)

p1C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_1C)
p2C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_2C)
p3C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_3C)
p4C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_4C)
p5C.pred_ln_cotton_rrev <- predictFelm(felm.fit = p.ln_cotton_rrev, newdata = p.ln_cotton_rrev_5C)

diff1C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_1C)
diff2C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_2C)
diff3C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_3C)
diff4C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_4C)
diff5C.pred_ln_cotton_rrev <- predictFelm(felm.fit = diff.ln_cotton_rrev, newdata = diff.ln_cotton_rrev_5C)


cs.rev1C <- exp(cs1C.pred_ln_cotton_rrev$fit + cs1C.pred_ln_cotton_rrev$res + cs1C.pred_ln_cotton_rrev$effect)
cs.rev2C <- exp(cs2C.pred_ln_cotton_rrev$fit + cs2C.pred_ln_cotton_rrev$res + cs2C.pred_ln_cotton_rrev$effect)
cs.rev3C <- exp(cs3C.pred_ln_cotton_rrev$fit + cs3C.pred_ln_cotton_rrev$res + cs3C.pred_ln_cotton_rrev$effect)
cs.rev4C <- exp(cs4C.pred_ln_cotton_rrev$fit + cs4C.pred_ln_cotton_rrev$res + cs4C.pred_ln_cotton_rrev$effect)
cs.rev5C <- exp(cs5C.pred_ln_cotton_rrev$fit + cs5C.pred_ln_cotton_rrev$res + cs5C.pred_ln_cotton_rrev$effect)

p.rev1C <- exp(p1C.pred_ln_cotton_rrev$fit + p1C.pred_ln_cotton_rrev$res + p1C.pred_ln_cotton_rrev$effect)
p.rev2C <- exp(p2C.pred_ln_cotton_rrev$fit + p2C.pred_ln_cotton_rrev$res + p2C.pred_ln_cotton_rrev$effect)
p.rev3C <- exp(p3C.pred_ln_cotton_rrev$fit + p3C.pred_ln_cotton_rrev$res + p3C.pred_ln_cotton_rrev$effect)
p.rev4C <- exp(p4C.pred_ln_cotton_rrev$fit + p4C.pred_ln_cotton_rrev$res + p4C.pred_ln_cotton_rrev$effect)
p.rev5C <- exp(p5C.pred_ln_cotton_rrev$fit + p5C.pred_ln_cotton_rrev$res + p5C.pred_ln_cotton_rrev$effect)

diff.rev1C <- exp(diff1C.pred_ln_cotton_rrev$fit + diff1C.pred_ln_cotton_rrev$res + diff1C.pred_ln_cotton_rrev$effect)
diff.rev2C <- exp(diff2C.pred_ln_cotton_rrev$fit + diff2C.pred_ln_cotton_rrev$res + diff1C.pred_ln_cotton_rrev$effect)
diff.rev3C <- exp(diff3C.pred_ln_cotton_rrev$fit + diff3C.pred_ln_cotton_rrev$res + diff1C.pred_ln_cotton_rrev$effect)
diff.rev4C <- exp(diff4C.pred_ln_cotton_rrev$fit + diff4C.pred_ln_cotton_rrev$res + diff1C.pred_ln_cotton_rrev$effect)
diff.rev5C <- exp(diff5C.pred_ln_cotton_rrev$fit + diff5C.pred_ln_cotton_rrev$res + diff1C.pred_ln_cotton_rrev$effect)

cs.rev <- data.frame(rev = c(cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
                     change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))

p.rev <- data.frame(rev = c(p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
                     change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))

diff.rev <- data.frame(rev = c(diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
                     change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))


ggplot(cs.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")
ggplot(p.rev, aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
ggplot(diff.rev, aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")


cs.cotton.rev0 <- sum( exp(cs.ln_cotton_rrev$response))
cs.cotton.rev1 <- (sum( exp(cs1C.pred_ln_cotton_rrev$fit + cs1C.pred_ln_cotton_rrev$res + cs1C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev2 <- (sum( exp(cs2C.pred_ln_cotton_rrev$fit + cs2C.pred_ln_cotton_rrev$res + cs2C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev3 <- (sum( exp(cs3C.pred_ln_cotton_rrev$fit + cs3C.pred_ln_cotton_rrev$res + cs3C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev4 <- (sum( exp(cs4C.pred_ln_cotton_rrev$fit + cs4C.pred_ln_cotton_rrev$res + cs4C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100
cs.cotton.rev5 <- (sum( exp(cs5C.pred_ln_cotton_rrev$fit + cs5C.pred_ln_cotton_rrev$res + cs5C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/cs.cotton.rev0 - 1)*100

p.cotton.rev0 <- sum( exp(p.ln_cotton_rrev$response))
p.cotton.rev1 <- (sum( exp(p1C.pred_ln_cotton_rrev$fit + p1C.pred_ln_cotton_rrev$res + p1C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100
p.cotton.rev2 <- (sum( exp(p2C.pred_ln_cotton_rrev$fit + p2C.pred_ln_cotton_rrev$res + p2C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100
p.cotton.rev3 <- (sum( exp(p3C.pred_ln_cotton_rrev$fit + p3C.pred_ln_cotton_rrev$res + p3C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100
p.cotton.rev4 <- (sum( exp(p4C.pred_ln_cotton_rrev$fit + p4C.pred_ln_cotton_rrev$res + p4C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100
p.cotton.rev5 <- (sum( exp(p5C.pred_ln_cotton_rrev$fit + p5C.pred_ln_cotton_rrev$res + p5C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/p.cotton.rev0 - 1)*100

diff.cotton.rev0 <- sum( exp(diff.ln_cotton_rrev$response))
diff.cotton.rev1 <- (sum( exp(diff1C.pred_ln_cotton_rrev$fit + diff1C.pred_ln_cotton_rrev$res + diff1C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100
diff.cotton.rev2 <- (sum( exp(diff2C.pred_ln_cotton_rrev$fit + diff2C.pred_ln_cotton_rrev$res + diff2C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100
diff.cotton.rev3 <- (sum( exp(diff3C.pred_ln_cotton_rrev$fit + diff3C.pred_ln_cotton_rrev$res + diff3C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100
diff.cotton.rev4 <- (sum( exp(diff4C.pred_ln_cotton_rrev$fit + diff4C.pred_ln_cotton_rrev$res + diff4C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100
diff.cotton.rev5 <- (sum( exp(diff5C.pred_ln_cotton_rrev$fit + diff5C.pred_ln_cotton_rrev$res + diff5C.pred_ln_cotton_rrev$effect ), na.rm = TRUE)/diff.cotton.rev0 - 1)*100

cotton.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.cotton.rev1, cs.cotton.rev2, cs.cotton.rev3, cs.cotton.rev4, cs.cotton.rev5,
                                   p.cotton.rev1, p.cotton.rev2, p.cotton.rev3, p.cotton.rev4, p.cotton.rev5,
                                   diff.cotton.rev1, diff.cotton.rev2, diff.cotton.rev3, diff.cotton.rev4, diff.cotton.rev5),
                           reg = rep(c("cross-section", "panel", "diff"), each = 5),
                           crop = "cotton")

ggplot(cotton.plotdat, aes(temp, rev, color = reg)) + geom_line()
}
# Hay
{
cs1C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_1C)
cs2C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_2C)
cs3C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_3C)
cs4C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_4C)
cs5C.pred_ln_hay_rrev <- predictFelm(felm.fit = cs.ln_hay_rrev, newdata = cs.ln_hay_rrev_5C)

p1C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_1C)
p2C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_2C)
p3C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_3C)
p4C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_4C)
p5C.pred_ln_hay_rrev <- predictFelm(felm.fit = p.ln_hay_rrev, newdata = p.ln_hay_rrev_5C)

diff1C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_1C)
diff2C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_2C)
diff3C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_3C)
diff4C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_4C)
diff5C.pred_ln_hay_rrev <- predictFelm(felm.fit = diff.ln_hay_rrev, newdata = diff.ln_hay_rrev_5C)


cs.rev1C <- exp(cs1C.pred_ln_hay_rrev$fit + cs1C.pred_ln_hay_rrev$res + cs1C.pred_ln_hay_rrev$effect)
cs.rev2C <- exp(cs2C.pred_ln_hay_rrev$fit + cs2C.pred_ln_hay_rrev$res + cs2C.pred_ln_hay_rrev$effect)
cs.rev3C <- exp(cs3C.pred_ln_hay_rrev$fit + cs3C.pred_ln_hay_rrev$res + cs3C.pred_ln_hay_rrev$effect)
cs.rev4C <- exp(cs4C.pred_ln_hay_rrev$fit + cs4C.pred_ln_hay_rrev$res + cs4C.pred_ln_hay_rrev$effect)
cs.rev5C <- exp(cs5C.pred_ln_hay_rrev$fit + cs5C.pred_ln_hay_rrev$res + cs5C.pred_ln_hay_rrev$effect)

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

cs.rev <- data.frame(rev = c(cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
                     change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))

p.rev <- data.frame(rev = c(p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
                     change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))

diff.rev <- data.frame(rev = c(diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
                     change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))


ggplot(cs.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")
ggplot(filter(p.rev, rev < 50), aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
ggplot(diff.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")


cs.hay.rev0 <- sum( exp(cs.ln_hay_rrev$response))
cs.hay.rev1 <- (sum( exp(cs1C.pred_ln_hay_rrev$fit + cs1C.pred_ln_hay_rrev$res + cs1C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/cs.hay.rev0 - 1)*100
cs.hay.rev2 <- (sum( exp(cs2C.pred_ln_hay_rrev$fit + cs2C.pred_ln_hay_rrev$res + cs2C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/cs.hay.rev0 - 1)*100
cs.hay.rev3 <- (sum( exp(cs3C.pred_ln_hay_rrev$fit + cs3C.pred_ln_hay_rrev$res + cs3C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/cs.hay.rev0 - 1)*100
cs.hay.rev4 <- (sum( exp(cs4C.pred_ln_hay_rrev$fit + cs4C.pred_ln_hay_rrev$res + cs4C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/cs.hay.rev0 - 1)*100
cs.hay.rev5 <- (sum( exp(cs5C.pred_ln_hay_rrev$fit + cs5C.pred_ln_hay_rrev$res + cs5C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/cs.hay.rev0 - 1)*100

p.hay.rev0 <- sum( exp(p.ln_hay_rrev$response))
p.hay.rev1 <- (sum( exp(p1C.pred_ln_hay_rrev$fit + p1C.pred_ln_hay_rrev$res + p1C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100
p.hay.rev2 <- (sum( exp(p2C.pred_ln_hay_rrev$fit + p2C.pred_ln_hay_rrev$res + p2C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100
p.hay.rev3 <- (sum( exp(p3C.pred_ln_hay_rrev$fit + p3C.pred_ln_hay_rrev$res + p3C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100
p.hay.rev4 <- (sum( exp(p4C.pred_ln_hay_rrev$fit + p4C.pred_ln_hay_rrev$res + p4C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100
p.hay.rev5 <- (sum( exp(p5C.pred_ln_hay_rrev$fit + p5C.pred_ln_hay_rrev$res + p5C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/p.hay.rev0 - 1)*100

diff.hay.rev0 <- sum( exp(diff.ln_hay_rrev$response))
diff.hay.rev1 <- (sum( exp(diff1C.pred_ln_hay_rrev$fit + diff1C.pred_ln_hay_rrev$res + diff1C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100
diff.hay.rev2 <- (sum( exp(diff2C.pred_ln_hay_rrev$fit + diff2C.pred_ln_hay_rrev$res + diff2C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100
diff.hay.rev3 <- (sum( exp(diff3C.pred_ln_hay_rrev$fit + diff3C.pred_ln_hay_rrev$res + diff3C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100
diff.hay.rev4 <- (sum( exp(diff4C.pred_ln_hay_rrev$fit + diff4C.pred_ln_hay_rrev$res + diff4C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100
diff.hay.rev5 <- (sum( exp(diff5C.pred_ln_hay_rrev$fit + diff5C.pred_ln_hay_rrev$res + diff5C.pred_ln_hay_rrev$effect ), na.rm = TRUE)/diff.hay.rev0 - 1)*100

hay.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.hay.rev1, cs.hay.rev2, cs.hay.rev3, cs.hay.rev4, cs.hay.rev5,
                                   p.hay.rev1, p.hay.rev2, p.hay.rev3, p.hay.rev4, p.hay.rev5,
                                   diff.hay.rev1, diff.hay.rev2, diff.hay.rev3, diff.hay.rev4, diff.hay.rev5),
                           reg = rep(c("cross-section", "panel", "diff"), each = 5),
                          crop = "hay")

ggplot(hay.plotdat, aes(temp, rev, color = reg)) + geom_line()
}

# Wheat
{
cs1C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_1C)
cs2C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_2C)
cs3C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_3C)
cs4C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_4C)
cs5C.pred_ln_wheat_rrev <- predictFelm(felm.fit = cs.ln_wheat_rrev, newdata = cs.ln_wheat_rrev_5C)

p1C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_1C)
p2C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_2C)
p3C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_3C)
p4C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_4C)
p5C.pred_ln_wheat_rrev <- predictFelm(felm.fit = p.ln_wheat_rrev, newdata = p.ln_wheat_rrev_5C)

diff1C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_1C)
diff2C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_2C)
diff3C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_3C)
diff4C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_4C)
diff5C.pred_ln_wheat_rrev <- predictFelm(felm.fit = diff.ln_wheat_rrev, newdata = diff.ln_wheat_rrev_5C)


cs.rev1C <- exp(cs1C.pred_ln_wheat_rrev$fit + cs1C.pred_ln_wheat_rrev$res + cs1C.pred_ln_wheat_rrev$effect)
cs.rev2C <- exp(cs2C.pred_ln_wheat_rrev$fit + cs2C.pred_ln_wheat_rrev$res + cs2C.pred_ln_wheat_rrev$effect)
cs.rev3C <- exp(cs3C.pred_ln_wheat_rrev$fit + cs3C.pred_ln_wheat_rrev$res + cs3C.pred_ln_wheat_rrev$effect)
cs.rev4C <- exp(cs4C.pred_ln_wheat_rrev$fit + cs4C.pred_ln_wheat_rrev$res + cs4C.pred_ln_wheat_rrev$effect)
cs.rev5C <- exp(cs5C.pred_ln_wheat_rrev$fit + cs5C.pred_ln_wheat_rrev$res + cs5C.pred_ln_wheat_rrev$effect)

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

cs.rev <- data.frame(rev = c(exp(cs.ln_wheat_rrev$response), cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
                     change = rep(c("Base", "1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))

p.rev <- data.frame(rev = c(p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
                     change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))

diff.rev <- data.frame(rev = c(exp(diff.ln_wheat_rrev$response), diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
                     change = rep(c("Base", "1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))


ggplot(cs.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")

ggplot(filter(p.rev, rev < 50), aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")

ggplot(diff.rev, aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")

cs.wheat.rev0 <- sum( exp(cs.ln_wheat_rrev$response))
cs.wheat.rev1 <- (sum( exp(cs1C.pred_ln_wheat_rrev$fit + cs1C.pred_ln_wheat_rrev$res + cs1C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100
cs.wheat.rev2 <- (sum( exp(cs2C.pred_ln_wheat_rrev$fit + cs2C.pred_ln_wheat_rrev$res + cs2C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100
cs.wheat.rev3 <- (sum( exp(cs3C.pred_ln_wheat_rrev$fit + cs3C.pred_ln_wheat_rrev$res + cs3C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100
cs.wheat.rev4 <- (sum( exp(cs4C.pred_ln_wheat_rrev$fit + cs4C.pred_ln_wheat_rrev$res + cs4C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100
cs.wheat.rev5 <- (sum( exp(cs5C.pred_ln_wheat_rrev$fit + cs5C.pred_ln_wheat_rrev$res + cs5C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/cs.wheat.rev0 - 1)*100

p.wheat.rev0 <- sum( exp(p.ln_wheat_rrev$response))
p.wheat.rev1 <- (sum( exp(p1C.pred_ln_wheat_rrev$fit + p1C.pred_ln_wheat_rrev$res + p1C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100
p.wheat.rev2 <- (sum( exp(p2C.pred_ln_wheat_rrev$fit + p2C.pred_ln_wheat_rrev$res + p2C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100
p.wheat.rev3 <- (sum( exp(p3C.pred_ln_wheat_rrev$fit + p3C.pred_ln_wheat_rrev$res + p3C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100
p.wheat.rev4 <- (sum( exp(p4C.pred_ln_wheat_rrev$fit + p4C.pred_ln_wheat_rrev$res + p4C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100
p.wheat.rev5 <- (sum( exp(p5C.pred_ln_wheat_rrev$fit + p5C.pred_ln_wheat_rrev$res + p5C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/p.wheat.rev0 - 1)*100

diff.wheat.rev0 <- sum( exp(diff.ln_wheat_rrev$response))
diff.wheat.rev1 <- (sum( exp(diff1C.pred_ln_wheat_rrev$fit + diff1C.pred_ln_wheat_rrev$res + diff1C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100
diff.wheat.rev2 <- (sum( exp(diff2C.pred_ln_wheat_rrev$fit + diff2C.pred_ln_wheat_rrev$res + diff2C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100
diff.wheat.rev3 <- (sum( exp(diff3C.pred_ln_wheat_rrev$fit + diff3C.pred_ln_wheat_rrev$res + diff3C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100
diff.wheat.rev4 <- (sum( exp(diff4C.pred_ln_wheat_rrev$fit + diff4C.pred_ln_wheat_rrev$res + diff4C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100
diff.wheat.rev5 <- (sum( exp(diff5C.pred_ln_wheat_rrev$fit + diff5C.pred_ln_wheat_rrev$res + diff5C.pred_ln_wheat_rrev$effect ), na.rm = TRUE)/diff.wheat.rev0 - 1)*100

wheat.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.wheat.rev1, cs.wheat.rev2, cs.wheat.rev3, cs.wheat.rev4, cs.wheat.rev5,
                                   p.wheat.rev1, p.wheat.rev2, p.wheat.rev3, p.wheat.rev4, p.wheat.rev5,
                                   diff.wheat.rev1, diff.wheat.rev2, diff.wheat.rev3, diff.wheat.rev4, diff.wheat.rev5),
                           reg = rep(c("cross-section", "panel", "diff"), each = 5),
                           crop = "wheat")

ggplot(wheat.plotdat, aes(temp, rev, color = reg)) + geom_line()
}

# Soybean
{
cs1C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_1C)
cs2C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_2C)
cs3C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_3C)
cs4C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_4C)
cs5C.pred_ln_soybean_rrev <- predictFelm(felm.fit = cs.ln_soybean_rrev, newdata = cs.ln_soybean_rrev_5C)

p1C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_1C)
p2C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_2C)
p3C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_3C)
p4C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_4C)
p5C.pred_ln_soybean_rrev <- predictFelm(felm.fit = p.ln_soybean_rrev, newdata = p.ln_soybean_rrev_5C)

diff1C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_1C)
diff2C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_2C)
diff3C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_3C)
diff4C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_4C)
diff5C.pred_ln_soybean_rrev <- predictFelm(felm.fit = diff.ln_soybean_rrev, newdata = diff.ln_soybean_rrev_5C)


cs.rev1C <- exp(cs1C.pred_ln_soybean_rrev$fit + cs1C.pred_ln_soybean_rrev$res + cs1C.pred_ln_soybean_rrev$effect)
cs.rev2C <- exp(cs2C.pred_ln_soybean_rrev$fit + cs2C.pred_ln_soybean_rrev$res + cs2C.pred_ln_soybean_rrev$effect)
cs.rev3C <- exp(cs3C.pred_ln_soybean_rrev$fit + cs3C.pred_ln_soybean_rrev$res + cs3C.pred_ln_soybean_rrev$effect)
cs.rev4C <- exp(cs4C.pred_ln_soybean_rrev$fit + cs4C.pred_ln_soybean_rrev$res + cs4C.pred_ln_soybean_rrev$effect)
cs.rev5C <- exp(cs5C.pred_ln_soybean_rrev$fit + cs5C.pred_ln_soybean_rrev$res + cs5C.pred_ln_soybean_rrev$effect)

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

cs.rev <- data.frame(rev = c(cs.rev1C, cs.rev2C, cs.rev3C, cs.rev4C, cs.rev5C),
                     change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(cs.rev1C)))

p.rev <- data.frame(rev = c(p.rev1C, p.rev2C, p.rev3C, p.rev4C, p.rev5C),
                     change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(p.rev1C)))

diff.rev <- data.frame(rev = c(diff.rev1C, diff.rev2C, diff.rev3C, diff.rev4C, diff.rev5C),
                     change = rep(c("1C", "2C", "3C", "4C", "5C"), each = length(diff.rev1C)))


ggplot(cs.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")
ggplot(filter(p.rev, rev < 50), aes(rev, fill = change)) + geom_histogram(bins = 100) + scale_fill_brewer(palette = "OrRd")
ggplot(diff.rev, aes(rev, fill = change)) + geom_histogram() + scale_fill_brewer(palette = "OrRd")


cs.soybean.rev0 <- sum( exp(cs.ln_soybean_rrev$response))
cs.soybean.rev1 <- (sum( exp(cs1C.pred_ln_soybean_rrev$fit + cs1C.pred_ln_soybean_rrev$res + cs1C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100
cs.soybean.rev2 <- (sum( exp(cs2C.pred_ln_soybean_rrev$fit + cs2C.pred_ln_soybean_rrev$res + cs2C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100
cs.soybean.rev3 <- (sum( exp(cs3C.pred_ln_soybean_rrev$fit + cs3C.pred_ln_soybean_rrev$res + cs3C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100
cs.soybean.rev4 <- (sum( exp(cs4C.pred_ln_soybean_rrev$fit + cs4C.pred_ln_soybean_rrev$res + cs4C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100
cs.soybean.rev5 <- (sum( exp(cs5C.pred_ln_soybean_rrev$fit + cs5C.pred_ln_soybean_rrev$res + cs5C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/cs.soybean.rev0 - 1)*100

p.soybean.rev0 <- sum( exp(p.ln_soybean_rrev$response))
p.soybean.rev1 <- (sum( exp(p1C.pred_ln_soybean_rrev$fit + p1C.pred_ln_soybean_rrev$res + p1C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100
p.soybean.rev2 <- (sum( exp(p2C.pred_ln_soybean_rrev$fit + p2C.pred_ln_soybean_rrev$res + p2C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100
p.soybean.rev3 <- (sum( exp(p3C.pred_ln_soybean_rrev$fit + p3C.pred_ln_soybean_rrev$res + p3C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100
p.soybean.rev4 <- (sum( exp(p4C.pred_ln_soybean_rrev$fit + p4C.pred_ln_soybean_rrev$res + p4C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100
p.soybean.rev5 <- (sum( exp(p5C.pred_ln_soybean_rrev$fit + p5C.pred_ln_soybean_rrev$res + p5C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/p.soybean.rev0 - 1)*100

diff.soybean.rev0 <- sum( exp(diff.ln_soybean_rrev$response))
diff.soybean.rev1 <- (sum( exp(diff1C.pred_ln_soybean_rrev$fit + diff1C.pred_ln_soybean_rrev$res + diff1C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100
diff.soybean.rev2 <- (sum( exp(diff2C.pred_ln_soybean_rrev$fit + diff2C.pred_ln_soybean_rrev$res + diff2C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100
diff.soybean.rev3 <- (sum( exp(diff3C.pred_ln_soybean_rrev$fit + diff3C.pred_ln_soybean_rrev$res + diff3C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100
diff.soybean.rev4 <- (sum( exp(diff4C.pred_ln_soybean_rrev$fit + diff4C.pred_ln_soybean_rrev$res + diff4C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100
diff.soybean.rev5 <- (sum( exp(diff5C.pred_ln_soybean_rrev$fit + diff5C.pred_ln_soybean_rrev$res + diff5C.pred_ln_soybean_rrev$effect ), na.rm = TRUE)/diff.soybean.rev0 - 1)*100

soybean.plotdat <- data.frame(temp = rep(c(1,2,3,4,5), 3),
                           rev = c(cs.soybean.rev1, cs.soybean.rev2, cs.soybean.rev3, cs.soybean.rev4, cs.soybean.rev5,
                                   p.soybean.rev1, p.soybean.rev2, p.soybean.rev3, p.soybean.rev4, p.soybean.rev5,
                                   diff.soybean.rev1, diff.soybean.rev2, diff.soybean.rev3, diff.soybean.rev4, diff.soybean.rev5),
                           reg = rep(c("cross-section", "panel", "diff"), each = 5),
                           crop = "soybean")

ggplot(soybean.plotdat, aes(temp, rev, color = reg)) + geom_line()
}

# Merge data
plotdat <- rbind(corn.plotdat, cotton.plotdat, hay.plotdat, wheat.plotdat, soybean.plotdat)

ggplot(plotdat, aes(temp, rev, color = reg)) + 
  geom_line() + ylab("Impact (% Change) ") + 
  xlab("Change in Temperature (C)") + geom_hline(yintercept = 0, linetype = "dashed")+
  facet_wrap(~crop)

ggplot(filter(plotdat, crop %in% c("corn", "soybean")), aes(temp, rev, color = reg)) + 
  geom_line() + ylab("Impact (% Change) ") + 
  xlab("Change in Temperature (C)") + geom_hline(yintercept = 0, linetype = "dashed")+
  facet_wrap(~crop)

