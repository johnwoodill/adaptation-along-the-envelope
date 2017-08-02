library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)
library(lfe)
library(cowplot)
library(plm)
library(lfe)


# Panel for Corn Revenue per Acre -----------------------------------------

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

cropdat <- readRDS("data/panel_regression_data.rds")

corndat <- filter(cropdat, !is.na(ln_corn_rrev))
cottondat <- filter(cropdat, !is.na(ln_cotton_rrev))
haydat <- filter(cropdat, !is.na(ln_hay_rrev))
wheatdat <- filter(cropdat, !is.na(ln_wheat_rrev))
soybeandat <- filter(cropdat, !is.na(ln_soybean_rrev))

# Corn
p.corn.mod1 <- felm(ln_corn_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = corndat, weights = corndat$corn_w)
summary(p.corn.mod1)

p.corn.mod2 <- felm(ln_corn_rrev ~ dday0_10 + dday10_30 + dday30C + 
                      prec + prec_sq | fips + year | 0 | state,
                  data = corndat, weights = corndat$corn_w)
summary(p.corn.mod2)

# Cotton
p.cotton.mod1 <- felm(ln_cotton_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = cottondat, weights = cottondat$cotton_w)
summary(p.cotton.mod1)

p.cotton.mod2 <- felm(ln_cotton_rrev ~dday0_10 + dday10_30  + dday30C + 
                        prec + prec_sq | fips + year | 0 | state,
                  data = cottondat, weights = cottondat$cotton_w)
summary(p.cotton.mod2)

# Hay
p.hay.mod1 <- felm(ln_hay_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = haydat, weights = haydat$hay_w)
summary(p.hay.mod1)

p.hay.mod2 <- felm(ln_hay_rrev ~ dday0_10 + dday10_30  + dday30C + 
                     prec + prec_sq | fips + year | 0 | state,
                  data = haydat, weights = haydat$hay_w)
summary(p.hay.mod2)

# Wheat
p.wheat.mod1 <- felm(ln_wheat_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = wheatdat, weights = wheatdat$wheat_w)
summary(p.wheat.mod1)

p.wheat.mod2 <- felm(ln_wheat_rrev ~ dday0_10  + dday10_30 + dday30C +
                       prec + prec_sq | fips + year | 0 | state,
                  data = wheatdat, weights = wheatdat$wheat_w)
summary(p.wheat.mod2)

# Soybean
p.soybean.mod1 <- felm(ln_soybean_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = soybeandat, weights = soybeandat$soybean_w)
summary(p.soybean.mod1)

p.soybean.mod2 <- felm(ln_soybean_rrev ~ dday0_10 + dday10_30  + dday30C +
                         prec + prec_sq | fips + year | 0 | state,
                  data = soybeandat, weights = soybeandat$soybean_w)
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


# Panel Acres --------------------------------------------------------

# Corn
p.corn.mod1 <- felm(log(p_corn_share) ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = corndat, weights = corndat$total_a)
summary(p.corn.mod1)

p.corn.mod2 <- felm(p_corn_share ~  dday0_10 + dday10_30  + dday30C + prec + prec_sq | fips + year | 0 | state,
                  data = corndat, weights = corndat$total_a)
summary(p.corn.mod2)

# Cotton
p.cotton.mod1 <- felm(p_cotton_share ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = cottondat, weights = cottondat$total_a)
summary(p.cotton.mod1)

p.cotton.mod2 <- felm(p_cotton_share ~ dday0_10 +dday10_30  + dday30C +  prec + prec_sq | fips + year | 0 | state,
                  data = cottondat, weights = cottondat$total_a)
summary(p.cotton.mod2)

# Hay
p.hay.mod1 <- felm(p_hay_share ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = haydat, weights = haydat$total_a)
summary(p.hay.mod1)

p.hay.mod2 <- felm(p_hay_share ~ dday0_10 +  dday10_30  + dday30C +  prec + prec_sq | fips + year | 0 | 0,
                  data = haydat, weights = haydat$total_a)
summary(p.hay.mod2)

# Wheat
p.wheat.mod1 <- felm(p_wheat_share ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = wheatdat, weights = wheatdat$total_a)
summary(p.wheat.mod1)

p.wheat.mod2 <- felm(p_wheat_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq | fips + year | 0 | state,
                  data = wheatdat, weights = wheatdat$total_a)
summary(p.wheat.mod2)

# Soybean
p.soybean.mod1 <- felm(p_soybean_share ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = soybeandat, weights = soybeandat$total_a)
summary(p.soybean.mod1)

p.soybean.mod2 <- felm(p_soybean_share ~ dday0_10 + dday10_30  + dday30C +  prec + prec_sq | fips + year | 0 | state,
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




