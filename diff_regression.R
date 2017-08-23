library(tidyverse)
library(AER)
library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

decadedat <- readRDS("data/diff_regression_data.rds")

# Setup data sets for regression
corndat <- filter(decadedat, !is.na(ln_corn_rrev))
cottondat <- filter(decadedat, !is.na(ln_cotton_rrev))
haydat <- filter(decadedat, !is.na(ln_hay_rrev))
wheatdat <- filter(decadedat, !is.na(ln_wheat_rrev))
soybeandat <- filter(decadedat, !is.na(ln_soybean_rrev))

# Log Revenue

# Corn
diff.corn.mod2 <- felm(ln_corn_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = corndat, weights = corndat$corn_w)
summary(diff.corn.mod2)

# Cotton
diff.cotton.mod2 <- felm(ln_cotton_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = cottondat, weights = cottondat$cotton_w)
summary(diff.cotton.mod2)

# Hay
diff.hay.mod2 <- felm(ln_hay_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = haydat, weights = haydat$hay_w)
summary(diff.hay.mod2)

# Wheat
diff.wheat.mod2 <- felm(ln_wheat_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = wheatdat, weights = wheatdat$wheat_w)
summary(diff.wheat.mod2)

# Soybean
diff.soybean.mod2 <- felm(ln_soybean_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = soybeandat, weights = soybeandat$soybean_w)
summary(diff.soybean.mod2)

# Save regression data
saveRDS(diff.corn.mod2, "models/diff.dd.ln_corn_rrev")
saveRDS(diff.cotton.mod2, "models/diff.dd.ln_cotton_rrev")
saveRDS(diff.hay.mod2, "models/diff.dd.ln_hay_rrev")
saveRDS(diff.wheat.mod2, "models/diff.dd.ln_wheat_rrev")
saveRDS(diff.soybean.mod2, "models/diff.dd.ln_soybean_rrev")

# Proportion of acreage regressions

decadedat <- readRDS("data/tobit_diff_regression_data.rds")

corndat <- filter(decadedat, !is.na(ln_corn_rrev))
cottondat <- filter(decadedat, !is.na(ln_cotton_rrev))
haydat <- filter(decadedat, !is.na(ln_hay_rrev))
wheatdat <- filter(decadedat, !is.na(ln_wheat_rrev))
soybeandat <- filter(decadedat, !is.na(ln_soybean_rrev))

# Corn
diff.corn.mod2 <- tobit(p_corn_share ~  dday0_10 + dday10_30  + dday30C + prec + prec_sq + lat + long + lat:long + 
                          factor(year) + cluster(state),
                  data = corndat, weights = corndat$total_w)
summary(diff.corn.mod2)


# Cotton
diff.cotton.mod2 <- tobit(p_cotton_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + factor(year) + cluster(state),
                  data = cottondat, weights = cottondat$total_w)
summary(diff.cotton.mod2)


# Hay
diff.hay.mod2 <- tobit(p_hay_share ~ dday0_10 +  dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + factor(year)  + cluster(state),
                  data = haydat, weights = haydat$total_w)
summary(diff.hay.mod2)

# Wheat
diff.wheat.mod2 <- tobit(p_wheat_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + factor(year) + cluster(state),
                  data = wheatdat, weights = wheatdat$total_w)
summary(diff.wheat.mod2)

# Soybean
diff.soybean.mod2 <- tobit(p_soybean_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + factor(year) + cluster(state),
                  data = soybeandat, weights = soybeandat$total_w)
summary(diff.soybean.mod2)



saveRDS(diff.corn.mod2, "models/diff.dd.p_corn_share")
saveRDS(diff.cotton.mod2, "models/diff.dd.p_cotton_share")
saveRDS(diff.hay.mod2, "models/diff.dd.p_hay_share")
saveRDS(diff.wheat.mod2, "models/diff.dd.p_wheat_share")
saveRDS(diff.soybean.mod2, "models/diff.dd.p_soybean_share")

