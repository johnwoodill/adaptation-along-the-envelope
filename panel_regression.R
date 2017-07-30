library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)
library(lfe)
library(cowplot)
library(plm)
library(lfe)


# Panel for Corn Revenue per Acre -----------------------------------------


cropdat <- readRDS("data/panel_regression_data.rds")

corndat <- filter(cropdat, !is.na(ln_corn_rrev))
cottondat <- filter(cropdat, !is.na(ln_cotton_rrev))
haydat <- filter(cropdat, !is.na(ln_hay_rrev))
wheatdat <- filter(cropdat, !is.na(ln_wheat_rrev))
soybeandat <- filter(cropdat, !is.na(ln_soybean_rrev))

# Corn
p.corn.mod1 <- felm(ln_corn_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = corndat, weights = corndat$corn_grain_a)
summary(p.corn.mod1)

p.corn.mod2 <- felm(ln_corn_rrev ~ I(dday0C - dday10C) +dday10_30 + dday30C + 
                      prec + prec_sq | fips + year | 0 | state,
                  data = corndat, weights = corndat$corn_grain_a)
summary(p.corn.mod2)

# Cotton
p.cotton.mod1 <- felm(ln_cotton_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = cottondat, weights = cottondat$cotton_a)
summary(p.cotton.mod1)

p.cotton.mod2 <- felm(ln_cotton_rrev ~I(dday0C - dday10C) + dday10_30  + dday30C + 
                        prec + prec_sq | fips + year | 0 | state,
                  data = cottondat, weights = cottondat$cotton_a)
summary(p.cotton.mod2)

# Hay
p.hay.mod1 <- felm(ln_hay_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = haydat, weights = haydat$hay_a)
summary(p.hay.mod1)

p.hay.mod2 <- felm(ln_hay_rrev ~I(dday0C - dday10C) + dday10_30  + dday30C + 
                     prec + prec_sq | fips + year | 0 | state,
                  data = haydat, weights = haydat$hay_a)
summary(p.hay.mod2)

# Wheat
p.wheat.mod1 <- felm(ln_wheat_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = wheatdat, weights = wheatdat$wheat_a)
summary(p.wheat.mod1)

p.wheat.mod2 <- felm(ln_wheat_rrev ~ I(dday0C - dday10C) + dday10_30 + dday30C +
                       prec + prec_sq | fips + year | 0 | state,
                  data = wheatdat, weights = wheatdat$wheat_a)
summary(p.wheat.mod2)

# Soybean
p.soybean.mod1 <- felm(ln_soybean_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = soybeandat, weights = soybeandat$soybean_a)
summary(p.soybean.mod1)

p.soybean.mod2 <- felm(ln_soybean_rrev ~I(dday0C - dday10C) + dday10_30  + dday30C +
                         prec + prec_sq | fips + year | 0 | state,
                  data = soybeandat, weights = soybeandat$soybean_a)
summary(p.soybean.mod2)


saveRDS(p.corn.mod1, "models/p.temp.ln_corn_rrev")
saveRDS(p.corn.mod2, "models/p.dd.ln_corn_rrev")

saveRDS(p.cotton.mod1, "models/p.temp.ln_cotton_rrev")
saveRDS(p.cotton.mod2, "models/p.dd.ln_cotton_rrev")

saveRDS(p.hay.mod1, "models/p.temp.ln_hay_rrev")
saveRDS(p.hay.mod2, "models/p.dd.ln_hay_rrev")

saveRDS(p.wheat.mod1, "models/p.temp.ln_wheat_rrev")
saveRDS(p.wheat.mod2, "models/p.dd.ln_wheat_rrev")

saveRDS(p.soybean.mod1, "models/p.temp.ln_soybean_rrev")
saveRDS(p.soybean.mod2, "models/p.dd.ln_soybean_rrev")


# Panel Corn Acres --------------------------------------------------------

cropdat <- filter(cropdat, corn_grain_a != 0 & cotton_a != 0 & hay_a != 0 & wheat_a != 0 & soybean_a != 0)

corndat <- filter(cropdat, !is.na(ln_corn_rrev))
cottondat <- filter(cropdat, !is.na(ln_cotton_rrev))
haydat <- filter(cropdat, !is.na(ln_hay_rrev))
wheatdat <- filter(cropdat, !is.na(ln_wheat_rrev))
soybeandat <- filter(cropdat, !is.na(ln_soybean_rrev))

# Corn
p.corn.mod1 <- felm(p_corn_share ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = corndat, weights = corndat$total_a)
summary(p.corn.mod1)

p.corn.mod2 <- felm(p_corn_share ~ dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = corndat, weights = corndat$total_a)
summary(p.corn.mod2)

# Cotton
p.cotton.mod1 <- felm(p_cotton_share ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = cottondat, weights = cottondat$total_a)
summary(p.cotton.mod1)

p.cotton.mod2 <- felm(ln_cotton_rrev ~ dday10_30  + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = cottondat, weights = cottondat$total_a)
summary(p.cotton.mod2)

# Hay
p.hay.mod1 <- felm(ln_hay_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = haydat, weights = haydat$total_a)
summary(p.hay.mod1)

p.hay.mod2 <- felm(ln_hay_rrev ~ dday10_30  + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = haydat, weights = haydat$total_a)
summary(p.hay.mod2)

# Wheat
p.wheat.mod1 <- felm(ln_wheat_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = wheatdat, weights = wheatdat$total_a)
summary(p.wheat.mod1)

p.wheat.mod2 <- felm(ln_wheat_rrev ~ dday10_30  + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = wheatdat, weights = wheatdat$total_a)
summary(p.wheat.mod2)

# Soybean
p.soybean.mod1 <- felm(ln_soybean_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = soybeandat, weights = soybeandat$total_a)
summary(p.soybean.mod1)

p.soybean.mod2 <- felm(ln_soybean_rrev ~ dday10_30  + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = soybeandat, weights = soybeandat$total_a)
summary(p.soybean.mod2)


saveRDS(p.corn.mod1, "models/p.temp.p_corn_share")
saveRDS(p.corn.mod2, "models/p.dd.p_corn_share")

saveRDS(p.cotton.mod1, "models/p.temp.p_cotton_share")
saveRDS(p.cotton.mod2, "models/p.dd.p_cotton_share")

saveRDS(p.hay.mod1, "models/p.temp.p_hay_share")
saveRDS(p.hay.mod2, "models/p.dd.p_hay_share")

saveRDS(p.wheat.mod1, "models/p.temp.p_wheat_share")
saveRDS(p.wheat.mod2, "models/p.dd.p_wheat_share")

saveRDS(p.soybean.mod1, "models/p.temp.p_soybean_share")
saveRDS(p.soybean.mod2, "models/p.dd.p_soybean_share")




