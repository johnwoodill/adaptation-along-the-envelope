library(tidyverse)
library(cowplot)
library(lfe)
library(AER)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
cropdat <- readRDS("data/cross_section_regression_data.rds")

# Cross Section: Revenue per acre -----------------------------------------

# Exposure weighted values equal zero
cropdat$tavg <- cropdat$tavg - mean(cropdat$tavg, na.rm = TRUE)
cropdat$dday0_10 <- cropdat$dday0_10 - mean(cropdat$dday0_10, na.rm = TRUE)
cropdat$dday10_30 <- cropdat$dday10_30 - mean(cropdat$dday10_30, na.rm = TRUE)
cropdat$dday30C <- cropdat$dday30C - mean(cropdat$dday30C, na.rm = TRUE)
cropdat$prec <- cropdat$prec - mean(cropdat$prec, na.rm = TRUE)
cropdat$prec_sq <- cropdat$prec^2

# Corn 
cs.corn.mod1 <- felm(ln_corn_rrev ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq  | state | 0 | state,
                  data = cropdat)
summary(cs.corn.mod1)

# Cotton
cs.cotton.mod1 <- felm(ln_cotton_rrev ~ dday0_10  + dday10_30 + dday30C + prec + prec_sq  | state | 0 | state,
                  data = cropdat)
summary(cs.cotton.mod1)

# Hay
cs.hay.mod1 <- felm(ln_hay_rrev ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq | state | 0 | state,
                  data = cropdat)
summary(cs.hay.mod1)

# Wheat
cs.wheat.mod1 <- felm(ln_wheat_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq  | state | 0 | state,
                  data = cropdat)

summary(cs.wheat.mod1)

# Soybean
cs.soybean.mod1 <- felm(ln_soybean_rrev ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq | state | 0 | state,
                  data = cropdat)
summary(cs.soybean.mod1)


# Save models
saveRDS(cs.corn.mod1, "models/cs.dd.ln_corn_rrev")
saveRDS(cs.cotton.mod1, "models/cs.dd.ln_cotton_rrev")
saveRDS(cs.hay.mod1, "models/cs.dd.ln_hay_rrev")
saveRDS(cs.wheat.mod1, "models/cs.dd.ln_wheat_rrev")
saveRDS(cs.soybean.mod1, "models/cs.dd.ln_soybean_rrev")

# Cross-section (Crop Choice): Corn Acres -----------------------------------------------

# # Corn
# cs.corn.mod2 <- tobit(p_corn_share ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq + lat + long + lat:long + cluster(state),
#                   data = cropdat, weights = (cropdat$total_w))
# summary(cs.corn.mod2)
# 
# # Cotton
# cs.cotton.mod2 <- tobit(p_cotton_share ~ dday0_10 + dday10_30 + dday30C +  prec + prec_sq + lat + long + lat:long + cluster(state),
#                   data = cropdat, weights = (cropdat$total_w))
# summary(cs.cotton.mod2)
# 
# 
# # Hay
# cs.hay.mod2 <- tobit(p_hay_share ~ dday0_10 +  dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + cluster(state),
#                   data = cropdat, weights = (cropdat$total_w))
# summary(cs.hay.mod2)
# 
# # Wheat
# cs.wheat.mod2 <- lm(p_wheat_share ~ factor(state) + dday0_10 + dday10_30 + dday30C + prec + prec_sq,
#                        data = cropdat)
# summary(cs.wheat.mod2)
# 
# # Soybean
# cs.soybean.mod2 <- tobit(p_soybean_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + cluster(state),
#                   data = cropdat, weights = (cropdat$total_w))
# summary(cs.soybean.mod2)
# 
# 
# 
# 
# # Save models
# saveRDS(cs.corn.mod2, "models/cs.dd.p_corn_share")
# saveRDS(cs.cotton.mod2, "models/cs.dd.p_cotton_share")
# saveRDS(cs.hay.mod2, "models/cs.dd.p_hay_share")
# saveRDS(cs.wheat.mod2, "models/cs.dd.p_wheat_share")
# saveRDS(cs.soybean.mod2, "models/cs.dd.p_soybean_share")

