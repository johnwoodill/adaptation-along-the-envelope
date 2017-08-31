library(dplyr)
library(lfe)
library(AER)


# Panel for Corn Revenue per Acre -----------------------------------------

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

cropdat <- readRDS("data/panel_regression_data.rds")


corndat <- filter(cropdat, !is.na(ln_corn_rrev))
cottondat <- filter(cropdat, !is.na(ln_cotton_rrev))
haydat <- filter(cropdat, !is.na(ln_hay_rrev))
wheatdat <- filter(cropdat, !is.na(ln_wheat_rrev))
soybeandat <- filter(cropdat, !is.na(ln_soybean_rrev))

# Corn
p.corn.mod2 <- felm(ln_corn_rrev ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = corndat, weights = corndat$corn_w)
summary(p.corn.mod2)

# Cotton
p.cotton.mod2 <- felm(ln_cotton_rrev ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = cottondat, weights = cottondat$cotton_w)
summary(p.cotton.mod2)

# Hay
p.hay.mod2 <- felm(ln_hay_rrev ~ dday0_10 +  dday10_30  + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = haydat, weights = haydat$hay_w)

summary(p.hay.mod2)

# Wheat
p.wheat.mod2 <- felm(ln_wheat_rrev ~ dday0_10 +  dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = wheatdat, weights = wheatdat$wheat_w)
summary(p.wheat.mod2)



# Soybean
p.soybean.mod2 <- felm(ln_soybean_rrev ~ dday0_10 +  dday10_30  + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = soybeandat, weights = soybeandat$soybean_w)
summary(p.soybean.mod2)


# Save Models
saveRDS(p.corn.mod2, "models/p.dd.ln_corn_rrev")
saveRDS(p.cotton.mod2, "models/p.dd.ln_cotton_rrev")
saveRDS(p.hay.mod2, "models/p.dd.ln_hay_rrev")
saveRDS(p.wheat.mod2, "models/p.dd.ln_wheat_rrev")
saveRDS(p.soybean.mod2, "models/p.dd.ln_soybean_rrev")


# Panel Acres --------------------------------------------------------

cropdat <- readRDS("data/panel_regression_data.rds")

corndat <- filter(cropdat, !is.na(p_corn_share))
cottondat <- filter(cropdat, !is.na(p_cotton_share))
haydat <- filter(cropdat, !is.na(p_hay_share))
wheatdat <- filter(cropdat, !is.na(p_wheat_share))
soybeandat <- filter(cropdat, !is.na(p_soybean_share))

# Corn
p.corn.mod2 <- tobit(p_corn_share ~  dday0_10 + dday10_30  + dday30C + prec + prec_sq + lat + long + lat:long + cluster(state),
                  data = corndat, weights = (corndat$total_w + 1))
summary(p.corn.mod2)


# Cotton
p.cotton.mod2 <- tobit(p_cotton_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + cluster(state),
                  data = cottondat, weights = (cottondat$total_w + 1))
summary(p.cotton.mod2)


# Hay
p.hay.mod2 <- tobit(p_hay_share ~ dday0_10 +  dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + cluster(state),
                  data = haydat, weights = (haydat$total_w + 1))
summary(p.hay.mod2)

# Wheat
p.wheat.mod2 <- tobit(p_wheat_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + cluster(state),
                  data = wheatdat, weights = (wheatdat$total_w + 1))
summary(p.wheat.mod2)

# Soybean
p.soybean.mod2 <- tobit(p_soybean_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq + lat + long + lat:long + cluster(state),
                  data = soybeandat, weights = (soybeandat$total_w + 1))
summary(p.soybean.mod2)

saveRDS(p.corn.mod2, "models/p.dd.p_corn_share")
saveRDS(p.cotton.mod2, "models/p.dd.p_cotton_share")
saveRDS(p.hay.mod2, "models/p.dd.p_hay_share")
saveRDS(p.wheat.mod2, "models/p.dd.p_wheat_share")
saveRDS(p.soybean.mod2, "models/p.dd.p_soybean_share")




