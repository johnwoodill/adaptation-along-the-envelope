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

# Demean values
corn_cropdat <- as.data.frame(filter(cropdat, !is.na(ln_corn_rrev)))
corn_moddat <- demeanlist(
  mtx = as.matrix(corn_cropdat[,3:36]), 
  fl = list(state = corn_cropdat$state),
  weights = sqrt(corn_cropdat$corn_grain_a))
corn_moddat <- as.data.frame(corn_moddat)

cotton_cropdat <- filter(cropdat, !is.na(ln_cotton_rrev))
cotton_moddat <- demeanlist(
  mtx = as.matrix(cotton_cropdat[,3:36]), 
  fl = list(state = cotton_cropdat$state),
  weights = sqrt(cotton_cropdat$cotton_a))
cotton_moddat <- as.data.frame(cotton_moddat)

hay_cropdat <- filter(cropdat, !is.na(ln_hay_rrev))
hay_moddat <- demeanlist(
  mtx = as.matrix(hay_cropdat[,3:36]), 
  fl = list(state = hay_cropdat$state),
  weights = sqrt(hay_cropdat$hay_a))
hay_moddat <- as.data.frame(hay_moddat)

wheat_cropdat <- filter(cropdat, !is.na(ln_wheat_rrev))
wheat_moddat <- demeanlist(
  mtx = as.matrix(wheat_cropdat[,3:36]), 
  fl = list(state = wheat_cropdat$state),
  weights = sqrt(wheat_cropdat$wheat_a))
wheat_moddat <- as.data.frame(wheat_moddat)

soybean_cropdat <- filter(cropdat, !is.na(ln_soybean_rrev))
soybean_moddat <- demeanlist(
  mtx = as.matrix(soybean_cropdat[,3:36]), 
  fl = list(state = soybean_cropdat$state),
  weights = sqrt(soybean_cropdat$soybean_a))
soybean_moddat <- as.data.frame(soybean_moddat)

cs.corn.mod1 <- lm(ln_corn_rrev ~ tavg + tavg_sq + prec + prec_sq + 
                      lat + ipc + pop_dens + pop_dens_sq + 
              waterCapacity + percentClay + minPermeability + kFactor + bestSoil - 1, 
              data = corn_moddat, weights = corn_cropdat$corn_grain_a)
summary(cs.corn.mod1)


 # mod1 <-lm(ln_corn_rrev ~ factor(state) + dm_tavg + dm_tavg_sq + dm_prec + dm_prec_sq + 
 #                       lat + dm_ipc + dm_pop_dens + dm_pop_dens_sq + 
 #               waterCapacity + percentClay + minPermeability + kFactor + bestSoil, 
 #               data = corn_cropdat, weights = corn_cropdat$corn_grain_a)
 # 
 # summary(mod1)
# 
# mod2 <- felm(ln_corn_rrev ~ dm_tavg + dm_tavg_sq + dm_prec + dm_prec_sq + 
#                       lat + dm_ipc + dm_pop_dens + dm_pop_dens_sq + 
#               waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, 
#              data = corn_cropdat, weights = corn_cropdat$corn_grain_a)

# summary(mod2)

cs.corn.mod2 <- lm(ln_corn_rrev ~ dday10_30 + dday34C + prec + prec_sq +
                    lat + ipc + pop_dens + pop_dens_sq + 
                    waterCapacity + percentClay + minPermeability + kFactor + bestSoil- 1,
                  data = corn_moddat, weights = corn_cropdat$corn_grain_a)

summary(cs.corn.mod2)

# Cotton
cs.cotton.mod1 <- update(cs.corn.mod1, ln_cotton_rrev ~ ., weights = cotton_cropdat$cotton_a, data = cotton_moddat)
summary(cs.cotton.mod1)

cs.cotton.mod2 <- update(cs.corn.mod2, ln_cotton_rrev ~ ., weights = cotton_cropdat$cotton_a, data = cotton_moddat)
summary(cs.cotton.mod2)

# Hay
cs.hay.mod1 <- update(cs.corn.mod1, ln_hay_rrev ~ ., weights = hay_cropdat$hay_a, data = hay_moddat)
summary(cs.hay.mod1)

cs.hay.mod2 <- update(cs.corn.mod2, ln_hay_rrev ~ ., weights = hay_cropdat$hay_a,  data = hay_moddat)
summary(cs.hay.mod2)

# Wheat
cs.wheat.mod1 <- update(cs.corn.mod1, ln_wheat_rrev ~ ., weights = wheat_cropdat$wheat_a, data = wheat_moddat)
summary(cs.wheat.mod1)

cs.wheat.mod2 <- update(cs.corn.mod2, ln_wheat_rrev ~ ., weights = wheat_cropdat$wheat_a, data = wheat_moddat)
summary(cs.wheat.mod2)

# Soybean
cs.soybean.mod1 <- update(cs.corn.mod1, ln_soybean_rrev ~ ., weights = soybean_cropdat$soybean_a, data = soybean_moddat)
summary(cs.soybean.mod1)

cs.soybean.mod2 <- update(cs.corn.mod2, ln_soybean_rrev ~ ., data = soybean_moddat)
summary(cs.soybean.mod2)



# Cluster by state
# vcov_state <- cluster.vcov(cs.corn.mod2, cluster = cropdat$state)
# cs.corn.mod2 <- coeftest(cs.corn.mod2, vcov_state)



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

saveRDS(corn_cropdat, "models/cs.corn_cropdat_ln_corn_rrev")
saveRDS(cotton_cropdat, "models/cs.cotton_cropdat_ln_cotton_rrev")
saveRDS(hay_cropdat, "models/cs.hay_cropdat_ln_hay_rrev")
saveRDS(wheat_cropdat, "models/cs.wheat_cropdat_ln_wheat_rrev")
saveRDS(soybean_cropdat, "models/cs.soybean_cropdat_ln_soybean_rrev")


# Cross-section (Crop Choice): Corn Acres -----------------------------------------------


cropdat <- filter(cropdat, corn_grain_a != 0 & cotton_a != 0 & hay_a != 0 & wheat_a != 0 & soybean_a != 0)

corn_cropdat <- filter(cropdat, !is.na(p_corn_share))
corn_moddat <- demeanlist(
  mtx = as.matrix(corn_cropdat[,3:36]), 
  fl = list(state = corn_cropdat$state),
  weights = sqrt(corn_cropdat$total_a))
corn_moddat <- as.data.frame(corn_moddat)

cotton_cropdat <- filter(cropdat, !is.na(p_cotton_share))
cotton_moddat <- demeanlist(
  mtx = as.matrix(cotton_cropdat[,3:36]), 
  fl = list(state = cotton_cropdat$state),
  weights = sqrt(cotton_cropdat$total_a))
cotton_moddat <- as.data.frame(cotton_moddat)

hay_cropdat <- filter(cropdat, !is.na(p_hay_share))
hay_moddat <- demeanlist(
  mtx = as.matrix(hay_cropdat[,3:36]), 
  fl = list(state = hay_cropdat$state),
  weights = sqrt(hay_cropdat$total_a))
hay_moddat <- as.data.frame(hay_moddat)

wheat_cropdat <- filter(cropdat, !is.na(p_wheat_share))
wheat_moddat <- demeanlist(
  mtx = as.matrix(wheat_cropdat[,3:36]), 
  fl = list(state = wheat_cropdat$state),
  weights = sqrt(wheat_cropdat$total_a))
wheat_moddat <- as.data.frame(wheat_moddat)

soybean_cropdat <- filter(cropdat, !is.na(p_soybean_share))
soybean_moddat <- demeanlist(
  mtx = as.matrix(soybean_cropdat[,3:36]), 
  fl = list(state = soybean_cropdat$state),
  weights = sqrt(soybean_cropdat$total_a))
soybean_moddat <- as.data.frame(soybean_moddat)

cc.corn.mod1 <- lm(p_corn_share ~ tavg + tavg_sq + prec + prec_sq + 
                     lat + ipc + pop_dens + pop_dens_sq + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil - 1, 
                   data = corn_moddat, weights = corn_cropdat$total_a)
summary(cc.corn.mod1)


cc.corn.mod2 <- lm(p_corn_share ~ dday10_30  + dday30C + prec + prec_sq + 
                    lat + ipc + pop_dens + pop_dens_sq + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil - 1, 
                   data = corn_moddat, weights = cropdat$total_a)
summary(cc.corn.mod2)

# Cotton
cc.cotton.mod1 <- update(cc.corn.mod1, p_cotton_share ~ ., weights = cotton_cropdat$total_a, data = cotton_moddat)
summary(cc.cotton.mod1)

cc.cotton.mod2 <- update(cc.corn.mod2, p_cotton_share ~ ., weights = cotton_cropdat$total_a, data = cotton_moddat)
summary(cc.cotton.mod2)

# Hay
cc.hay.mod1 <- update(cc.corn.mod1, p_hay_share ~ ., weights = hay_cropdat$total_a, data = hay_moddat)
summary(cc.hay.mod1)

cc.hay.mod2 <- update(cc.corn.mod2, p_hay_share ~ ., weights = hay_cropdat$total_a, data = hay_moddat)
summary(cc.hay.mod2)

# Wheat
cc.wheat.mod1 <- update(cc.corn.mod1, p_wheat_share ~ ., weights = wheat_cropdat$total_a, data = wheat_moddat)
summary(cc.wheat.mod1)

cc.wheat.mod2 <- update(cc.corn.mod2, p_wheat_share ~ ., weights = wheat_cropdat$total_a, data = wheat_moddat)
summary(cc.wheat.mod2)

# Soybean
cc.soybean.mod1 <- update(cc.corn.mod1, p_soybean_share ~ ., weights = soybean_cropdat$total_a, data = soybean_moddat)
summary(cc.soybean.mod1)

cc.soybean.mod2 <- update(cc.corn.mod2, p_soybean_share ~ ., weights = soybean_cropdat$total_a, data = soybean_moddat)
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

saveRDS(cropdat, "models/cc.corn.dat")

