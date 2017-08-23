library(tidyverse)
library(lfe)
library(AER)

cropdat <- readRDS("data/cross_section_regression_data.rds")

# Cross-section (Crop Choice): Corn Acres -----------------------------------------------

cropdat <- readRDS("data/tobit_cross_section_regression_data.rds")

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

sum(predict(cc.corn.mod2))




# Save models
saveRDS(cc.corn.mod2, "models/cs.dd.p_corn_share")
saveRDS(cc.cotton.mod2, "models/cs.dd.p_cotton_share")
saveRDS(cc.hay.mod2, "models/cs.dd.p_hay_share")
saveRDS(cc.wheat.mod2, "models/cs.dd.p_wheat_share")
saveRDS(cc.soybean.mod2, "models/cs.dd.p_soybean_share")

