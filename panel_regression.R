library(dplyr)
library(lfe)
library(AER)


# Panel for Corn Revenue per Acre -----------------------------------------

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

cropdat <- readRDS("data/panel_regression_data.rds")

# Exposure weighted values equal zero
cropdat$tavg <- cropdat$tavg - mean(cropdat$tavg, na.rm = TRUE)
cropdat$dday0_10 <- cropdat$dday0_10 - mean(cropdat$dday0_10, na.rm = TRUE)
cropdat$dday10_30 <- cropdat$dday10_30 - mean(cropdat$dday10_30, na.rm = TRUE)
cropdat$dday30C <- cropdat$dday30C - mean(cropdat$dday30C, na.rm = TRUE)
cropdat$prec <- cropdat$prec - mean(cropdat$prec, na.rm = TRUE)
cropdat$prec_sq <- cropdat$prec^2



# Corn
p.corn.mod2 <- felm(ln_corn_rrev ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = cropdat, weights = cropdat$corn_w)
summary(p.corn.mod2)

# Cotton
p.cotton.mod2 <- felm(ln_cotton_rrev ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = cropdat, weights = cropdat$cotton_w)
summary(p.cotton.mod2)

# Hay
p.hay.mod2 <- felm(ln_hay_rrev ~ dday0_10 +  dday10_30  + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = cropdat, weights = cropdat$hay_w)

summary(p.hay.mod2)

# Wheat
p.wheat.mod2 <- felm(ln_wheat_rrev ~ dday0_10 +  dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = cropdat, weights = cropdat$wheat_w)
summary(p.wheat.mod2)


# Soybean
p.soybean.mod2 <- felm(ln_soybean_rrev ~ dday0_10 +  dday10_30  + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = cropdat, weights = cropdat$soybean_w)
summary(p.soybean.mod2)


# Save Models
saveRDS(p.corn.mod2, "models/p.dd.ln_corn_rrev")
saveRDS(p.cotton.mod2, "models/p.dd.ln_cotton_rrev")
saveRDS(p.hay.mod2, "models/p.dd.ln_hay_rrev")
saveRDS(p.wheat.mod2, "models/p.dd.ln_wheat_rrev")
saveRDS(p.soybean.mod2, "models/p.dd.ln_soybean_rrev")


# Panel Acres --------------------------------------------------------


# Corn
p.corn.mod2 <- tobit(p_corn_share ~  dday0_10 + dday10_30  + dday30C + prec + prec_sq + lat + long + lat:long + cluster(state),
                  data = cropdat, weights = (cropdat$total_w))
summary(p.corn.mod2)


# Cotton
p.cotton.mod2 <- tobit(p_cotton_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + cluster(state),
                  data = cropdat, weights = (cropdat$total_w))
summary(p.cotton.mod2)


# Hay
p.hay.mod2 <- tobit(p_hay_share ~ dday0_10 +  dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + cluster(state),
                  data = cropdat, weights = (cropdat$total_w))
summary(p.hay.mod2)

# Wheat
p.wheat.mod2 <- tobit(p_wheat_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + cluster(state),
                  data = cropdat, weights = (cropdat$total_w ))
summary(p.wheat.mod2)

# Soybean
p.soybean.mod2 <- tobit(p_soybean_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + cluster(state),
                  data = cropdat, weights = (cropdat$total_w ))
summary(p.soybean.mod2)

saveRDS(p.corn.mod2, "models/p.dd.p_corn_share")
saveRDS(p.cotton.mod2, "models/p.dd.p_cotton_share")
saveRDS(p.hay.mod2, "models/p.dd.p_hay_share")
saveRDS(p.wheat.mod2, "models/p.dd.p_wheat_share")
saveRDS(p.soybean.mod2, "models/p.dd.p_soybean_share")




