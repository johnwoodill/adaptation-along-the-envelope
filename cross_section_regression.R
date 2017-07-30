library(plm)
library(tidyverse)
library(stargazer)
library(rms)
library(cowplot)
library(multiwayvcov)
library(lmtest)
library(lfe)

cropdat <- readRDS("data/cross_section_regression_data.rds")

# Cross Section: Revenue per acre -----------------------------------------

corndat <- filter(cropdat, !is.na(ln_corn_rrev))
cottondat <- filter(cropdat, !is.na(ln_cotton_rrev))
haydat <- filter(cropdat, !is.na(ln_hay_rrev))
wheatdat <- filter(cropdat, !is.na(ln_wheat_rrev))
soybeandat <- filter(cropdat, !is.na(ln_soybean_rrev))

# Corn
cs.corn.mod1 <- felm(ln_corn_rrev ~ tavg + tavg_sq + prec + prec_sq | state, 
                   data = corndat, weights = corndat$corn_grain_a)
summary(cs.corn.mod1)

cs.corn.mod2 <- felm(ln_corn_rrev ~ I(dday0C - dday10C) +dday10_30  + dday30C + prec + prec_sq | state,
                  data = corndat, weights = corndat$corn_grain_a)
summary(cs.corn.mod2)

# Cotton
cs.cotton.mod1 <- felm(ln_cotton_rrev ~ tavg + tavg_sq + prec + prec_sq | state, 
                   data = cottondat, weights = cottondat$cotton_a)
summary(cs.cotton.mod1)

cs.cotton.mod2 <- felm(ln_cotton_rrev ~ I(dday0C - dday10C) +dday10_30  + dday30C + prec + prec_sq | state,
                  data = cottondat, weights = cottondat$cotton_a)
summary(cs.cotton.mod2)

# Hay
cs.hay.mod1 <- felm(ln_hay_rrev ~ tavg + tavg_sq + prec + prec_sq | state, 
                   data = haydat, weights = haydat$hay_a)
summary(cs.hay.mod1)

cs.hay.mod2 <- felm(ln_hay_rrev ~I(dday0C - dday10C) + dday10_30  + dday30C + prec + prec_sq | state,
                  data = haydat, weights = haydat$hay_a)
summary(cs.hay.mod2)

# Wheat
cs.wheat.mod1 <- felm(ln_wheat_rrev ~ tavg + tavg_sq + prec + prec_sq | state, 
                   data = wheatdat, weights = wheatdat$wheat_a)
summary(cs.wheat.mod1)

cs.wheat.mod2 <- felm(ln_wheat_rrev ~I(dday0C - dday10C) + dday10_30  + dday30C + prec + prec_sq | state,
                  data = wheatdat, weights = wheatdat$wheat_a)
summary(cs.wheat.mod2)

# Soybean
cs.soybean.mod1 <- felm(ln_soybean_rrev ~ tavg + tavg_sq + prec + prec_sq | state, 
                   data = soybeandat, weights = soybeandat$soybean_a)
summary(cs.soybean.mod1)

cs.soybean.mod2 <- felm(ln_soybean_rrev ~I(dday0C - dday10C) + dday10_30  + dday30C + prec + prec_sq | state,
                  data = soybeandat, weights = soybeandat$soybean_a)
summary(cs.soybean.mod2)




# Save models
saveRDS(cs.corn.mod1, "models/cs.temp.ln_corn_rrev")
saveRDS(cs.corn.mod2, "models/cs.dd.ln_corn_rrev")

saveRDS(cs.cotton.mod1, "models/cs.temp.ln_cotton_rrev")
saveRDS(cs.cotton.mod2, "models/cs.dd.ln_cotton_rrev")

saveRDS(cs.hay.mod1, "models/cs.temp.ln_hay_rrev")
saveRDS(cs.hay.mod2, "models/cs.dd.ln_hay_rrev")

saveRDS(cs.wheat.mod1, "models/cs.temp.ln_wheat_rrev")
saveRDS(cs.wheat.mod2, "models/cs.dd.ln_wheat_rrev")

saveRDS(cs.soybean.mod1, "models/cs.temp.ln_soybean_rrev")
saveRDS(cs.soybean.mod2, "models/cs.dd.ln_soybean_rrev")

# saveRDS(corn_cropdat, "models/cs.corn_cropdat_ln_corn_rrev")
# saveRDS(cotton_cropdat, "models/cs.cotton_cropdat_ln_cotton_rrev")
# saveRDS(hay_cropdat, "models/cs.hay_cropdat_ln_hay_rrev")
# saveRDS(wheat_cropdat, "models/cs.wheat_cropdat_ln_wheat_rrev")
# saveRDS(soybean_cropdat, "models/cs.soybean_cropdat_ln_soybean_rrev")


# Cross-section (Crop Choice): Corn Acres -----------------------------------------------


cropdat <- filter(cropdat, corn_grain_a != 0 & cotton_a != 0 & hay_a != 0 & wheat_a != 0 & soybean_a != 0)

corndat <- filter(cropdat, !is.na(ln_corn_rrev))
cottondat <- filter(cropdat, !is.na(ln_cotton_rrev))
haydat <- filter(cropdat, !is.na(ln_hay_rrev))
wheatdat <- filter(cropdat, !is.na(ln_wheat_rrev))
soybeandat <- filter(cropdat, !is.na(ln_soybean_rrev))


# Corn
cc.corn.mod1 <- felm(p_corn_share ~ tavg + tavg_sq + prec + prec_sq | state, 
                   data = corndat, weights = corndat$total_a)
summary(cc.corn.mod1)

cc.corn.mod2 <- felm(p_corn_share ~ dday10_30  + dday30C + prec + prec_sq | state,
                  data = corndat, weights = corndat$total_a)
summary(cc.corn.mod2)

# Cotton
cc.cotton.mod1 <- felm(p_cotton_share ~ tavg + tavg_sq + prec + prec_sq | state, 
                   data = cottondat, weights = cottondat$total_a)
summary(cc.cotton.mod1)

cc.cotton.mod2 <- felm(p_cotton_share ~ dday10_30  + dday30C + prec + prec_sq | state,
                  data = cottondat, weights = cottondat$total_a)
summary(cc.cotton.mod2)

# Hay
cc.hay.mod1 <- felm(p_hay_share ~ tavg + tavg_sq + prec + prec_sq | state, 
                   data = haydat, weights = haydat$total_a)
summary(cc.hay.mod1)

cc.hay.mod2 <- felm(p_hay_share ~ dday10_30  + dday30C + prec + prec_sq | state,
                  data = haydat, weights = haydat$total_a)
summary(cc.hay.mod2)

# Wheat
cc.wheat.mod1 <- felm(p_wheat_share ~ tavg + tavg_sq + prec + prec_sq | state, 
                   data = wheatdat, weights = wheatdat$total_a)
summary(cc.wheat.mod1)

cc.wheat.mod2 <- felm(p_wheat_share ~ dday10_30  + dday30C + prec + prec_sq | state,
                  data = wheatdat, weights = wheatdat$total_a)
summary(cc.wheat.mod2)

# Soybean
cc.soybean.mod1 <- felm(p_soybean_share ~ tavg + tavg_sq + prec + prec_sq | state, 
                   data = soybeandat, weights = soybeandat$total_a)
summary(cc.soybean.mod1)

cc.soybean.mod2 <- felm(p_soybean_share ~ dday10_30  + dday30C + prec + prec_sq | state,
                  data = soybeandat, weights = soybeandat$total_a)
summary(cc.soybean.mod2)



# Save models

saveRDS(cc.corn.mod1, "models/cs.temp.p_corn_share")
saveRDS(cc.corn.mod2, "models/cs.dd.p_corn_share")

saveRDS(cc.cotton.mod1, "models/cs.temp.p_cotton_share")
saveRDS(cc.cotton.mod2, "models/cs.dd.p_cotton_share")

saveRDS(cc.hay.mod1, "models/cs.temp.p_hay_share")
saveRDS(cc.hay.mod2, "models/cs.dd.p_hay_share")

saveRDS(cc.wheat.mod1, "models/cs.temp.p_wheat_share")
saveRDS(cc.wheat.mod2, "models/cs.dd.p_wheat_share")

saveRDS(cc.soybean.mod1, "models/cs.temp.p_soybean_share")
saveRDS(cc.soybean.mod2, "models/cs.dd.p_soybean_share")

