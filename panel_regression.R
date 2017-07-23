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


# Remove fixed-effects fips and year
corn_cropdat <- filter(cropdat, !is.na(ln_corn_rrev))
corn_moddat <- demeanlist(
  mtx = as.matrix(corn_cropdat[,7:64]), 
  fl = list(fips = corn_cropdat$fips,
            year = corn_cropdat$year),
  weights = sqrt(corn_cropdat$corn_grain_a))
corn_moddat <- as.data.frame(corn_moddat)

cotton_cropdat <- filter(cropdat, !is.na(ln_cotton_rrev))
cotton_moddat <- demeanlist(
  mtx = as.matrix(cotton_cropdat[,7:64]), 
  fl = list(fips = cotton_cropdat$fips,
            year = cotton_cropdat$year),
  weights = sqrt(cotton_cropdat$cotton_a))
cotton_moddat <- as.data.frame(cotton_moddat)

hay_cropdat <- filter(cropdat, !is.na(ln_hay_rrev))
hay_moddat <- demeanlist(
  mtx = as.matrix(hay_cropdat[,7:64]), 
  fl = list(fips = hay_cropdat$fips,
            year = hay_cropdat$year),
  weights = sqrt(hay_cropdat$hay_a))
hay_moddat <- as.data.frame(hay_moddat)

wheat_cropdat <- filter(cropdat, !is.na(ln_wheat_rrev))
wheat_moddat <- demeanlist(
  mtx = as.matrix(wheat_cropdat[,7:64]), 
  fl = list(fips = wheat_cropdat$fips,
            year = wheat_cropdat$year),
  weights = sqrt(wheat_cropdat$wheat_a))
wheat_moddat <- as.data.frame(wheat_moddat)

soybean_cropdat <- filter(cropdat, !is.na(ln_soybean_rrev))
soybean_moddat <- demeanlist(
  mtx = as.matrix(soybean_cropdat[,7:64]), 
  fl = list(fips = soybean_cropdat$fips,
            year = soybean_cropdat$year),
  weights = sqrt(soybean_cropdat$soybean_a))
soybean_moddat <- as.data.frame(soybean_moddat)


 # p.corn.mod1 <- felm(ln_corn_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year, 
 #                     data = corn_cropdat, weights = corn_cropdat$corn_grain_a)
 # summary(p.corn.mod1)

# Corn
p.corn.mod1 <- lm(ln_corn_rrev ~ tavg + tavg_sq + prec + prec_sq - 1, 
                data = corn_cropdat)
summary(p.corn.mod1)

p.corn.mod2 <- lm(ln_corn_rrev ~ dday10C_30C  + dday30C +
               prec + prec_sq - 1, data = corn_cropdat)
summary(p.corn.mod2)

# Cotton
p.cotton.mod1 <- update(p.corn.mod1, ln_cotton_rrev ~ ., data = cotton_moddat, weights = NULL)
summary(p.cotton.mod1)

p.cotton.mod2 <- update(p.corn.mod2, ln_cotton_rrev ~ ., weights = NULL, data = cotton_moddat)
summary(p.cotton.mod2)

# Hay
p.hay.mod1 <- update(p.corn.mod1, ln_hay_rrev ~ ., weights = NULL, data = hay_moddat)
summary(p.hay.mod1)

p.hay.mod2 <- update(p.corn.mod2, ln_hay_rrev ~ ., weights = NULL, data = hay_moddat)
summary(p.hay.mod2)

# Wheat
p.wheat.mod1 <- update(p.corn.mod1, ln_wheat_rrev ~ ., weights = wheat_cropdat$wheat_a, data = wheat_moddat)
summary(p.wheat.mod1)

p.wheat.mod2 <- update(p.corn.mod2, ln_wheat_rrev ~ ., weights = NULL, data = wheat_moddat)
summary(p.wheat.mod2)

# Soybean
p.soybean.mod1 <- update(p.corn.mod1, ln_soybean_rrev ~ ., weights = soybean_cropdat$soybean_a, data = soybean_moddat)
summary(p.soybean.mod1)

p.soybean.mod2 <- update(p.corn.mod2, ln_soybean_rrev ~ ., weights = NULL, data = soybean_moddat)
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

# Remove fixed-effects fips and year
corn_cropdat <- filter(cropdat, !is.na(p_corn_share))
corn_moddat <- demeanlist(
  mtx = as.matrix(corn_cropdat[,7:70]), 
  fl = list(fips = corn_cropdat$fips,
            year = corn_cropdat$year),
  weights = sqrt(corn_cropdat$total_a))
corn_moddat <- as.data.frame(corn_moddat)

cotton_cropdat <- filter(cropdat, !is.na(p_cotton_share))
cotton_moddat <- demeanlist(
  mtx = as.matrix(cotton_cropdat[,7:70]), 
  fl = list(fips = cotton_cropdat$fips,
            year = cotton_cropdat$year),
  weights = sqrt(cotton_cropdat$total_a))
cotton_moddat <- as.data.frame(cotton_moddat)

hay_cropdat <- filter(cropdat, !is.na(p_hay_share))
hay_moddat <- demeanlist(
  mtx = as.matrix(hay_cropdat[,7:70]), 
  fl = list(fips = hay_cropdat$fips,
            year = hay_cropdat$year),
  weights = sqrt(hay_cropdat$total_a))
hay_moddat <- as.data.frame(hay_moddat)

wheat_cropdat <- filter(cropdat, !is.na(p_wheat_share))
wheat_moddat <- demeanlist(
  mtx = as.matrix(wheat_cropdat[,7:70]), 
  fl = list(fips = wheat_cropdat$fips,
            year = wheat_cropdat$year),
  weights = sqrt(wheat_cropdat$total_a))
wheat_moddat <- as.data.frame(wheat_moddat)

soybean_cropdat <- filter(cropdat, !is.na(p_soybean_share))
soybean_moddat <- demeanlist(
  mtx = as.matrix(soybean_cropdat[,7:70]), 
  fl = list(fips = soybean_cropdat$fips,
            year = soybean_cropdat$year),
  weights = sqrt(soybean_cropdat$total_a))
soybean_moddat <- as.data.frame(soybean_moddat)


# Corn
p.corn.mod1 <- lm(p_corn_share ~ tavg + tavg_sq + prec + prec_sq - 1, 
                data = corn_moddat, weights = corn_cropdat$corn_grain_a)
summary(p.corn.mod1)


p.corn.mod2 <- lm(p_corn_share ~ dday10C_30C  + dday10C_30C_sq + dday30C +
               prec + prec_sq - 1, data = corn_cropdat, weights = corn_cropdat$total_a)
summary(p.corn.mod2)

# Cotton
p.cotton.mod1 <- update(p.corn.mod1, p_cotton_share ~ ., weights = cotton_cropdat$total_a, data = cotton_moddat)
summary(p.cotton.mod1)

p.cotton.mod2 <- update(p.corn.mod2, p_cotton_share ~ ., weights = cotton_cropdat$total_a, data = cotton_moddat)
summary(p.cotton.mod2)

# Hay
p.hay.mod1 <- update(p.corn.mod1, p_hay_share ~ ., weights = hay_cropdat$total_a, data = hay_moddat)
summary(p.hay.mod1)

p.hay.mod2 <- update(p.corn.mod2, p_hay_share ~ ., weights = hay_cropdat$total_a, data = hay_moddat)
summary(p.hay.mod2)

# Wheat
p.wheat.mod1 <- update(p.corn.mod1, p_wheat_share ~ ., weights = wheat_cropdat$total_a, data = wheat_moddat)
summary(p.wheat.mod1)

p.wheat.mod2 <- update(p.corn.mod2, p_wheat_share ~ ., weights = wheat_cropdat$total_a, data = wheat_moddat)
summary(p.wheat.mod2)

# Soybean
p.soybean.mod1 <- update(p.corn.mod1, p_soybean_share ~ ., weights = soybean_cropdat$total_a, data = soybean_moddat)
summary(p.soybean.mod1)

p.soybean.mod2 <- update(p.corn.mod2, p_soybean_share ~ ., weights = soybean_cropdat$total_a, data = soybean_moddat)
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




