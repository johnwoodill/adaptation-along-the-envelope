library(tidyverse)
library(AER)
library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

decadedat <- readRDS("data/diff_regression_data.rds")

# Exposure weighted values equal zero
decadedat$tavg <- decadedat$tavg - mean(decadedat$tavg, na.rm = TRUE)
decadedat$dday0_10 <- decadedat$dday0_10 - mean(decadedat$dday0_10, na.rm = TRUE)
decadedat$dday10_30 <- decadedat$dday10_30 - mean(decadedat$dday10_30, na.rm = TRUE)
decadedat$dday30C <- decadedat$dday30C - mean(decadedat$dday30C, na.rm = TRUE)
decadedat$prec <- decadedat$prec - mean(decadedat$prec, na.rm = TRUE)
decadedat$prec_sq <- decadedat$prec^2

# Log Revenue

# Corn
diff.corn.mod1 <- felm(ln_corn_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = decadedat, weights = decadedat$corn_w)
summary(diff.corn.mod1)

# Cotton
diff.cotton.mod1 <- felm(ln_cotton_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = decadedat, weights = decadedat$cotton_w)
summary(diff.cotton.mod1)

# Hay
diff.hay.mod1 <- felm(ln_hay_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = decadedat, weights = decadedat$hay_w)
summary(diff.hay.mod1)

# Wheat
diff.wheat.mod1 <- felm(ln_wheat_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = decadedat, weights = decadedat$wheat_w)
summary(diff.wheat.mod1)

# Soybean
diff.soybean.mod1 <- felm(ln_soybean_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = decadedat, weights = decadedat$soybean_w)
summary(diff.soybean.mod1)

# Save regression data
saveRDS(diff.corn.mod1, "models/diff.dd.ln_corn_rrev")
saveRDS(diff.cotton.mod1, "models/diff.dd.ln_cotton_rrev")
saveRDS(diff.hay.mod1, "models/diff.dd.ln_hay_rrev")
saveRDS(diff.wheat.mod1, "models/diff.dd.ln_wheat_rrev")
saveRDS(diff.soybean.mod1, "models/diff.dd.ln_soybean_rrev")

# Proportion of acreage regressions

# Corn
diff.corn.mod2 <- tobit(p_corn_share ~  dday0_10 + dday10_30  + dday30C + prec + prec_sq + lat + long + lat:long,
                  data = decadedat, weights = (decadedat$total_w ))
summary(diff.corn.mod2)


# Cotton
diff.cotton.mod2 <- tobit(p_cotton_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq+ lat + long + lat:long,
                  data = decadedat, weights = (decadedat$total_w ))
summary(diff.cotton.mod2)


# Hay
diff.hay.mod2 <- tobit(p_hay_share ~ dday0_10 +  dday10_30  + dday30C +  prec + prec_sq+ lat + long + lat:long,
                  data = decadedat, weights = (decadedat$total_w ))
summary(diff.hay.mod2)

# Wheat
diff.wheat.mod2 <- tobit(p_wheat_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq,
                  data = decadedat, weights = (decadedat$total_w ))
summary(diff.wheat.mod2)

# Soybean
diff.soybean.mod2 <- tobit(p_soybean_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq+ lat + long + lat:long,
                  data = decadedat, weights = (decadedat$total_w ))
summary(diff.soybean.mod2)



saveRDS(diff.corn.mod2, "models/diff.dd.p_corn_share")
saveRDS(diff.cotton.mod2, "models/diff.dd.p_cotton_share")
saveRDS(diff.hay.mod2, "models/diff.dd.p_hay_share")
saveRDS(diff.wheat.mod2, "models/diff.dd.p_wheat_share")
saveRDS(diff.soybean.mod2, "models/diff.dd.p_soybean_share")

