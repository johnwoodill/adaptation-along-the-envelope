library(tidyverse)
library(lfe)
library(AER)

cropdat <- readRDS("data/cross_section_regression_data.rds")

# Cross Section: Revenue per acre -----------------------------------------

corndat <- filter(cropdat, !is.na(ln_corn_rrev))
cottondat <- filter(cropdat, !is.na(ln_cotton_rrev))
haydat <- filter(cropdat, !is.na(ln_hay_rrev))
wheatdat <- filter(cropdat, !is.na(ln_wheat_rrev))
soybeandat <- filter(cropdat, !is.na(ln_soybean_rrev))

# Corn 
cs.corn.mod2 <- felm(ln_corn_rrev ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq + lat + long | 0 | 0 | state,
                  data = corndat, weights = corndat$corn_w)
summary(cs.corn.mod2)

# Cotton
cs.cotton.mod2 <- felm(ln_cotton_rrev ~ dday0_10  + dday10_30 + dday30C + prec + prec_sq + lat + long + lat:long  | 0 | 0 | state,
                  data = cottondat, weights = cottondat$cotton_w)
summary(cs.cotton.mod2)

# Hay
cs.hay.mod2 <- felm(ln_hay_rrev ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq + lat + long + lat:long | 0 | 0 | state,
                  data = haydat, weights = haydat$hay_w)
summary(cs.hay.mod2)

# Wheat
cs.wheat.mod2 <- felm(ln_wheat_rrev ~ dday0_10  + dday10_30 + dday30C + prec + prec_sq + lat + long + lat:long  | 0 | 0 | state,
                  data = wheatdat, weights = wheatdat$wheat_w)
summary(cs.wheat.mod2)

# Soybean
cs.soybean.mod2 <- felm(ln_soybean_rrev ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq + lat + long + lat:long | 0 | 0 | state,
                  data = soybeandat, weights = soybeandat$soybean_w)
summary(cs.soybean.mod2)




# Save models
saveRDS(cs.corn.mod2, "models/cs.dd.ln_corn_rrev")
saveRDS(cs.cotton.mod2, "models/cs.dd.ln_cotton_rrev")
saveRDS(cs.hay.mod2, "models/cs.dd.ln_hay_rrev")
saveRDS(cs.wheat.mod2, "models/cs.dd.ln_wheat_rrev")
saveRDS(cs.soybean.mod2, "models/cs.dd.ln_soybean_rrev")

# Cross-section (Crop Choice): Corn Acres -----------------------------------------------

cropdat <- readRDS("data/baseline_cross_section_regression_data.rds")

corndat <- filter(cropdat, !is.na(ln_corn_rrev))
cottondat <- filter(cropdat, !is.na(ln_cotton_rrev))
haydat <- filter(cropdat, !is.na(ln_hay_rrev))
wheatdat <- filter(cropdat, !is.na(ln_wheat_rrev))
soybeandat <- filter(cropdat, !is.na(ln_soybean_rrev))

# Corn
cc.corn.mod2 <- tobit(p_corn_share ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq + lat + long + lat:long + cluster(state),
                  data = corndat, weights = corndat$total_w)
summary(cc.corn.mod2)

# Cotton
cc.cotton.mod2 <- tobit(p_cotton_share ~ dday0_10 + dday10_30 + dday30C +  prec + prec_sq + lat + long + lat:long  +cluster(state),
                  data = cottondat, weights = cottondat$total_w)
summary(cc.cotton.mod2)


# Hay
cc.hay.mod2 <- tobit(p_hay_share ~ dday0_10 +  dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long  +cluster(state),
                  data = haydat, weights = haydat$total_w)
summary(cc.hay.mod2)

# Wheat
cc.wheat.mod2 <- tobit(p_wheat_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long  + cluster(state),
                  data = wheatdat, weights = wheatdat$total_w)
summary(cc.wheat.mod2)

# Soybean
cc.soybean.mod2 <- tobit(p_soybean_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + cluster(state),
                  data = soybeandat, weights = soybeandat$total_w)
summary(cc.soybean.mod2)




# Save models
saveRDS(cc.corn.mod2, "models/cs.dd.p_corn_share")
saveRDS(cc.cotton.mod2, "models/cs.dd.p_cotton_share")
saveRDS(cc.hay.mod2, "models/cs.dd.p_hay_share")
saveRDS(cc.wheat.mod2, "models/cs.dd.p_wheat_share")
saveRDS(cc.soybean.mod2, "models/cs.dd.p_soybean_share")

