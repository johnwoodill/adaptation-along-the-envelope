library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)
library(lfe)
library(cowplot)
library(plm)
library(lfe)


# Panel for Corn Revenue per Acre -----------------------------------------


cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1950 & year <= 2010)

cropdat$prec_sq <- cropdat$prec^2
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$fips <- factor(cropdat$fips)
cropdat$state <- factor(cropdat$state)
cropdat$year <- factor(cropdat$year)
cropdat$dday8C_32C <- cropdat$dday8C - cropdat$dday32C
cropdat$dday10C_30C <- cropdat$dday10C - cropdat$dday30C
cropdat$dday10C_30C_sq <- cropdat$dday10C_30C^2
cropdat$dday8C_32C_sq <- cropdat$dday8C_32C^2
cropdat$dday34C_sqrt <- sqrt(cropdat$dday34C)
cropdat$ln_corn_rrev <- log(1 + cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(1 + cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(1 + cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(1 + cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(1 + cropdat$soybean_rrev)



# Remove fixed-effects fips and year
corn_cropdat <- filter(cropdat, !is.na(cropdat$ln_corn_rrev))
corn_moddat <- demeanlist(
  mtx = as.matrix(corn_cropdat[,7:64]), 
  fl = list(fips = corn_cropdat$fips,
            year = corn_cropdat$year),
  weights = sqrt(corn_cropdat$corn_grain_a))
corn_moddat <- as.data.frame(corn_moddat)

cotton_cropdat <- filter(cropdat, !is.na(cropdat$ln_cotton_rrev))
cotton_moddat <- demeanlist(
  mtx = as.matrix(cotton_cropdat[,7:64]), 
  fl = list(fips = cotton_cropdat$fips,
            year = cotton_cropdat$year),
  weights = sqrt(cotton_cropdat$cotton_a))
cotton_moddat <- as.data.frame(cotton_moddat)

hay_cropdat <- filter(cropdat, !is.na(cropdat$ln_hay_rrev))
hay_moddat <- demeanlist(
  mtx = as.matrix(hay_cropdat[,7:64]), 
  fl = list(fips = hay_cropdat$fips,
            year = hay_cropdat$year),
  weights = sqrt(hay_cropdat$hay_a))
hay_moddat <- as.data.frame(hay_moddat)

wheat_cropdat <- filter(cropdat, !is.na(cropdat$ln_wheat_rrev))
wheat_moddat <- demeanlist(
  mtx = as.matrix(wheat_cropdat[,7:64]), 
  fl = list(fips = wheat_cropdat$fips,
            year = wheat_cropdat$year),
  weights = sqrt(wheat_cropdat$wheat_a))
wheat_moddat <- as.data.frame(wheat_moddat)

soybean_cropdat <- filter(cropdat, !is.na(cropdat$ln_soybean_rrev))
soybean_moddat <- demeanlist(
  mtx = as.matrix(soybean_cropdat[,7:64]), 
  fl = list(fips = soybean_cropdat$fips,
            year = soybean_cropdat$year),
  weights = sqrt(soybean_cropdat$soybean_a))
soybean_moddat <- as.data.frame(soybean_moddat)


# p.cropdat <- pdata.frame(cropdat, index = c("fips", "year"))
# 
# p.corn.mod1 <- plm(ln_corn_rrev ~ factor(year) + tavg + tavg_sq + prec + prec_sq, 
#                     data = p.cropdat, model = "within")

# summary(p.corn.mod1)


# p.corn.mod1 <- felm(ln_corn_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year, 
#                     data = cropdat, weights = cropdat$corn_grain_a)
# summary(p.corn.mod1)

# Corn
p.corn.mod1 <- lm(ln_corn_rrev ~ tavg + tavg_sq + prec + prec_sq - 1, 
                data = corn_moddat, weights = corn_cropdat$corn_grain_a)
summary(mod)


p.corn.mod2 <- lm(ln_corn_rrev ~ dday10C_30C  + dday10C_30C_sq + dday30C +
               prec + prec_sq - 1, data = p.cropdat)
summary(p.corn.mod2)

# Cotton
p.cotton.mod1 <- update(p.corn.mod1, ln_cotton_rrev ~ ., weights = cotton_cropdat$cotton_a, data = cotton_moddat)
summary(p.cotton.mod1)

p.cotton.mod2 <- update(p.corn.mod2, ln_cotton_rrev ~ ., weights = cotton_cropdat$cotton_a, data = cotton_moddat)
summary(p.cotton.mod1)

# Hay
p.hay.mod1 <- update(p.corn.mod1, ln_hay_rrev ~ ., weights = hay_cropdat$hay_a, data = hay_moddat)
summary(p.hay.mod1)

p.hay.mod2 <- update(p.corn.mod2, ln_hay_rrev ~ ., weights = hay_cropdat$hay_a, data = hay_moddat)
summary(p.hay.mod1)

# Wheat
p.wheat.mod1 <- update(p.corn.mod1, ln_wheat_rrev ~ ., weights = wheat_cropdat$wheat_a, data = wheat_moddat)
summary(p.wheat.mod1)

p.wheat.mod2 <- update(p.corn.mod2, ln_wheat_rrev ~ ., weights = wheat_cropdat$wheat_a, data = wheat_moddat)
summary(p.wheat.mod1)

# Soybean
p.soybean.mod1 <- update(p.corn.mod1, ln_soybean_rrev ~ ., weights = soybean_cropdat$soybean_a, data = soybean_moddat)
summary(p.soybean.mod1)

p.soybean.mod2 <- update(p.corn.mod2, ln_soybean_rrev ~ ., weights = soybean_cropdat$soybean_a, data = soybean_moddat)
summary(p.soybean.mod1)

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

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1950 & year <= 2010)

cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)
cropdat$dday8_32 <- cropdat$dday8C - cropdat$dday32C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

cropdat <- cropdat %>% 
  group_by(year) %>% 
  mutate(p_corn_a = corn_grain_a/sum(total_a, na.rm = TRUE))

cropdat$trend <- cropdat$year - 1959
cropdat$trendsq <- cropdat$trend^2
cropdat$precsq <- cropdat$prec^2
cropdat$tavgsq <- cropdat$tavg^2
cropdat$ffips <- as.factor(cropdat$fips)
cropdat$fstate <- as.factor(cropdat$state)
cropdat$dday8C_32C <- cropdat$dday8C - cropdat$dday32C
cropdat$dday10C_30C <- cropdat$dday10C - cropdat$dday30C
cropdat$dday10C_30C_sq <- cropdat$dday10C_30C^2
cropdat$dday8C_32C_sq <- cropdat$dday8C_32C^2
cropdat$dday34C_sqrt <- sqrt(cropdat$dday34C)
cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)

cropdat <- filter(cropdat, !is.na(ln_corn_rrev))

p.cropdat <- plm.data(cropdat, index = c("fips", "year"))

p.corn.mod3 <- plm(p_corn_a ~ factor(year) + tavg + I(tavg^2) + prec + I(prec^2), 
                    data = p.cropdat, model = "within")
summary(p.corn.mod3)

p.corn.mod4 <- plm(p_corn_a ~ factor(year) + dday10C_30C + I(dday10C_30C^2) + dday30C +
               prec + I(prec^2), data = p.cropdat, model = "within")
summary(p.corn.mod4)

saveRDS(p.corn.mod3, "models/p.temp.p_corn_share")
saveRDS(p.corn.mod4, "models/p.dd.p_corn_share")


