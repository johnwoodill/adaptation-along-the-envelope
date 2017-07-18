library(plm)
library(tidyverse)
library(stargazer)
library(rms)
library(cowplot)
library(multiwayvcov)
library(lmtest)
library(lfe)

# Cross-section: Log(Corn Rev) --------------------------------------------

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

# Crop data
cropdat <- readRDS("data/full_ag_data.rds")

# Soil data
#soil <- read_dta("data/soilData.dta")
soil <- readRDS("data/soilData.rds")
soil$fips <- as.numeric(soil$fips)


# East of 100th meridian
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1970 & year <= 2010)

cropdat$ln_corn_rrev <- log(1 + cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(1 + cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(1 + cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(1 + cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(1 + cropdat$soybean_rrev)
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C
cropdat$dday10_30_sq <- cropdat$dday10_30^2
cropdat$dday8_32 <- cropdat$dday8C - cropdat$dday32C
cropdat$dday8_32_sq <- cropdat$dday8_32^2
cropdat$pop_dens_sq <- cropdat$pop_dens^2
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$prec_sq <- cropdat$prec^2
cropdat$dday34C_sqrt <- sqrt(cropdat$dday34C)

# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))


# Need to remove mean from weather variables too
cropdat <- cropdat %>%
  group_by(year) %>%
  mutate(dm_ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
         dm_ln_cotton_rrev = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
         dm_ln_hay_rrev = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
         dm_ln_wheat_rrev = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
         dm_ln_soybean_rrev = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE),
        dm_tavg = tavg - mean(tavg, na.rm = TRUE),
        dm_tavg_sq = tavg - mean(tavg^2, na.rm = TRUE),
        dm_prec = prec - mean(prec, na.rm = TRUE),
        dm_prec_sq = prec - mean(prec^2, na.rm = TRUE),
        dm_dday10_30 = dday10_30 - mean(dday10_30, na.rm = TRUE),
        dm_dday10_30_sq = dday10_30_sq - mean(dday10_30_sq, na.rm = TRUE),
        dm_dday10 = dday10C - mean(dday10C, na.rm = TRUE),
        dm_dday30 = dday30C - mean(dday30C, na.rm = TRUE),
        dm_dday8_32 = dday8_32 - mean(dday8_32, na.rm = TRUE),
        dm_dday34C = dday34C - mean(dday34C, na.rm = TRUE),
        dm_ipc = ipc - mean(ipc, na.rm = TRUE),
        dm_pop_dens = pop_dens - mean(pop_dens, na.rm = TRUE),
        dm_pop_dens_sq = pop_dens - mean(pop_dens^2, na.rm = TRUE)) %>% 
    group_by(state, fips) %>%
    summarise(ln_corn_rrev = mean(dm_ln_corn_rrev, na.rm = TRUE),
              ln_cotton_rrev = mean(dm_ln_cotton_rrev, na.rm = TRUE),
              ln_hay_rrev = mean(dm_ln_hay_rrev, na.rm = TRUE),
              ln_wheat_rrev = mean(dm_ln_wheat_rrev, na.rm = TRUE),
              ln_soybean_rrev = mean(dm_ln_soybean_rrev, na.rm = TRUE),
              tavg = mean(tavg, na.rm = TRUE),
              tavg_sq = mean(tavg^2, na.rm = TRUE),
              prec = mean(prec, na.rm = TRUE),
              dm_tavg = mean(dm_tavg, na.rm = TRUE),
              dm_tavg_sq = mean(dm_tavg^2, na.rm = TRUE),
              dm_prec = mean(dm_prec, na.rm = TRUE),
              dm_prec_sq = mean(dm_prec^2, na.rm = TRUE),
              corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
              cotton_a = mean(corn_grain_a, na.rm = TRUE),
              hay_a = mean(corn_grain_a, na.rm = TRUE),
              wheat_a = mean(corn_grain_a, na.rm = TRUE),
              soybean_a = mean(corn_grain_a, na.rm = TRUE),
              lat = mean(lat, na.rm = TRUE),
              dm_dday10_30 = mean(dm_dday10_30, na.rm = TRUE),
              dm_dday10_30_sq = mean(dm_dday10_30^2, na.rm = TRUE),
              dm_dday30 = mean(dm_dday30, na.rm = TRUE),
              dm_dday10 = mean(dm_dday10, na.rm = TRUE),
              dm_dday8_32 = mean(dm_dday8_32, na.rm = TRUE),
              dm_dday34C = mean(dm_dday34C, na.rm = TRUE),
              dm_ipc = mean(dm_ipc, na.rm = TRUE),
              dm_pop_dens = mean(dm_pop_dens, na.rm = TRUE),
              dm_pop_dens_sq = mean(dm_pop_dens^2, na.rm = TRUE)) %>% 
  ungroup()

cropdat <- left_join(cropdat, soil, by = "fips")
#cropdat$state <- factor(cropdat$state)
#cropdat <- filter(cropdat, !is.na(ln_corn_rrev) & !is.na(waterCapacity) & !is.na(percentClay) & !is.na(minPermeability) & !is.na(kFactor) & !is.na(bestSoil))
# cropdat <- cropdat %>% 
#   group_by(state) %>%
#   mutate(ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
#               ln_cotton_rrev = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
#               ln_hay_rrev = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
#               ln_wheat_rrev = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
#               ln_soybean_rrev = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE),
#               dm_tavg = dm_tavg - mean(dm_tavg, na.rm = TRUE),
#               dm_prec = dm_prec - mean(dm_prec, na.rm = TRUE),
#               dm_dday10_30 = dm_dday10_30 - mean(dm_dday10_30, na.rm = TRUE),
#               dm_dday30 = dm_dday30 - mean(dm_dday30, na.rm = TRUE),
#               dm_dday10 = dm_dday10 - mean(dm_dday10, na.rm = TRUE),
#               dm_dday8_32 = dm_dday8_32 - mean(dm_dday8_32, na.rm = TRUE),
#               dm_dday34C = dm_dday34C - mean(dm_dday34C, na.rm = TRUE),
#               dm_ipc = dm_ipc - mean(dm_ipc, na.rm = TRUE),
#               dm_pop_dens = dm_pop_dens - mean(dm_pop_dens, na.rm = TRUE))
# 
# cropdat$dm_tavg_sq <- cropdat$dm_tavg^2
# cropdat$dm_prec_sq <- cropdat$dm_prec^2
# cropdat$dm_pop_dens_sq <- cropdat$dm_pop_dens^2

# p.cropdat <- plm.data(cropdat, index = c("state"))

#Corn
# cs.corn.mod1  <- lm(ln_corn_rrev ~ factor(state) + dm_tavg + I(dm_tavg^2) + dm_prec + I(dm_prec^2) +
#                        lat + dm_ipc + dm_pop_dens + I(dm_pop_dens^2) +
#                        waterCapacity + percentClay + minPermeability + kFactor + bestSoil,
#                      data = cropdat, weights = cropdat$corn_grain_a)
# 
# summary(cs.corn.mod1)
# 
# cs.corn.mod1  <- plm(ln_corn_rrev ~ factor(state) + dm_tavg + dm_tavg_sq + dm_prec + dm_prec_sq + 
#                      lat + dm_ipc + dm_pop_dens + dm_pop_dens_sq + 
#                      waterCapacity + percentClay + minPermeability + kFactor + bestSoil,
#                    data = cropdat, model = "within")
# 
# summary(cs.corn.mod1)
# 
# 
# cs.corn.mod1  <- felm(ln_corn_rrev ~ dm_tavg + dm_tavg_sq + dm_prec + dm_prec_sq +
#                       lat + dm_ipc + dm_pop_dens + dm_pop_dens_sq +
#               waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state,
#                     data = cropdat, weights = cropdat$corn_grain_a)
# summary(cs.corn.mod1 )


# Weights are not making equivalent, but lm_felm_dmeanlist...R is showing equivalence
cropdat <- ungroup(cropdat)
cropdat <- as.data.frame(cropdat)


corn_moddat <- demeanlist(
  mtx = as.matrix(cropdat[,3:34]), 
  fl = list(state = cropdat$state),
  weights = sqrt(cropdat$corn_grain_a))
corn_moddat <- as.data.frame(corn_moddat)

cotton_moddat <- demeanlist(
  mtx = as.matrix(cropdat[,3:34]), 
  fl = list(state = cropdat$state),
  weights = sqrt(cropdat$cotton_a))
cotton_moddat <- as.data.frame(cotton_moddat)

hay_moddat <- demeanlist(
  mtx = as.matrix(cropdat[,3:34]), 
  fl = list(state = cropdat$state),
  weights = sqrt(cropdat$hay_a))
hay_moddat <- as.data.frame(hay_moddat)

wheat_moddat <- demeanlist(
  mtx = as.matrix(cropdat[,3:34]), 
  fl = list(state = cropdat$state),
  weights = sqrt(cropdat$wheat_a))
wheat_moddat <- as.data.frame(wheat_moddat)

soybean_moddat <- demeanlist(
  mtx = as.matrix(cropdat[,3:34]), 
  fl = list(state = cropdat$state),
  weights = sqrt(cropdat$soybean_a))
soybean_moddat <- as.data.frame(soybean_moddat)

cs.corn.mod1 <- lm(ln_corn_rrev ~ dm_tavg + dm_tavg_sq + dm_prec + dm_prec_sq + 
                      lat + dm_ipc + dm_pop_dens + dm_pop_dens_sq + 
              waterCapacity + percentClay + minPermeability + kFactor + bestSoil, 
              data = corn_moddat, weights = cropdat$corn_grain_a)
summary(cs.corn.mod1)

cs.corn.mod2 <- lm(ln_corn_rrev ~ dm_dday10_30 + dm_dday10_30_sq + dm_dday30 + dm_prec + dm_prec_sq +
                    lat + dm_ipc + dm_pop_dens + dm_pop_dens_sq + waterCapacity + percentClay + minPermeability + kFactor + bestSoil,
                  data = corn_moddat, weights = cropdat$corn_grain_a)
summary(cs.corn.mod2)

# Cotton
cs.cotton.mod1 <- update(cs.corn.mod1, ln_cotton_rrev ~ ., weights = cropdat$cotton_a, data = cotton_moddat)
summary(cs.cotton.mod1)

cs.cotton.mod2 <- update(cs.corn.mod2, ln_cotton_rrev ~ ., weights = cropdat$cotton_a, data = cotton_moddat)
summary(cs.cotton.mod1)

# Hay
cs.hay.mod1 <- update(cs.corn.mod1, ln_hay_rrev ~ ., weights = cropdat$hay_a, data = hay_moddat)
summary(cs.hay.mod1)

cs.hay.mod2 <- update(cs.corn.mod2, ln_hay_rrev ~ ., weights = cropdat$hay_a, data = hay_moddat)
summary(cs.hay.mod1)

# Wheat
cs.wheat.mod1 <- update(cs.corn.mod1, ln_wheat_rrev ~ ., weights = cropdat$wheat_a, data = wheat_moddat)
summary(cs.wheat.mod1)

cs.wheat.mod2 <- update(cs.corn.mod2, ln_wheat_rrev ~ ., weights = cropdat$wheat_a, data = wheat_moddat)
summary(cs.wheat.mod1)

# Soybean
cs.soybean.mod1 <- update(cs.corn.mod1, ln_soybean_rrev ~ ., weights = cropdat$soybean_a, data = soybean_moddat)
summary(cs.soybean.mod1)

cs.soybean.mod2 <- update(cs.corn.mod2, ln_soybean_rrev ~ ., weights = cropdat$soybean_a, data = soybean_moddat)
summary(cs.soybean.mod1)



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

saveRDS(cropdat, "models/cs.corn.dat")
# 
# 

#Predict(cs.corn.mod1, ln_corn_rrev)
#cs.corn.mod1$fitted.values
#plot(x = (cs.corn.mod1$model$dm_tavg - cs.corn.mod1$residuals), y = cs.corn.mod1$model$ln_corn_rrev)



# Cross-section (Crop Choice): Corn Acres -----------------------------------------------

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1970 & year <= 2010)
soil <- readRDS("data/soilData.rds")
soil$fips <- as.numeric(soil$fips)

#cropdat[is.na(cropdat)] <- 0
cropdat <- filter(cropdat, corn_grain_a != 0 & cotton_a != 0 & hay_a != 0 & wheat_a != 0 & soybean_a != 0)
cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")])
cropdat$dday8_32 <- cropdat$dday8C - cropdat$dday32C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C
cropdat$dday10_30_sq <- cropdat$dday10_30^2
cropdat$dday8_32sq <- cropdat$dday8_32^2

cropdat$pop_dens_sq <- cropdat$pop_dens^2
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$prec_sq <- cropdat$prec^2
cropdat$dday34C_sqrt <- sqrt(cropdat$dday34C)
# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))

cropdat <- cropdat %>%
  group_by(year) %>%
  mutate(p_corn_share = corn_grain_a/sum(total_a, na.rm = TRUE),
         p_cotton_share = cotton_a/sum(total_a, na.rm = TRUE),
         p_hay_share = hay_a/sum(total_a, na.rm = TRUE),
         p_wheat_share = wheat_a/sum(total_a, na.rm = TRUE),
         p_soybean_share = soybean_a/sum(total_a, na.rm = TRUE),
         p_corn_share = p_corn_share - mean(p_corn_share, na.rm = TRUE),
         p_cotton_share = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
         p_hay_share = p_hay_share - mean(p_hay_share, na.rm = TRUE),
         p_wheat_share = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
         p_soybean_share = p_soybean_share - mean(p_soybean_share, na.rm = TRUE),
         dm_tavg = tavg - mean(tavg, na.rm = TRUE),
        dm_tavg_sq = tavg - mean(tavg^2, na.rm = TRUE),
        dm_prec = prec - mean(prec, na.rm = TRUE),
        dm_prec_sq = prec - mean(prec^2, na.rm = TRUE),
        dm_dday10_30 = dday10_30 - mean(dday10_30, na.rm = TRUE),
        dm_dday10_30_sq = dday10_30_sq - mean(dday10_30_sq, na.rm = TRUE),
        dm_dday10 = dday10C - mean(dday10C, na.rm = TRUE),
        dm_dday30 = dday30C - mean(dday30C, na.rm = TRUE),
        dm_dday8_32 = dday8_32 - mean(dday8_32, na.rm = TRUE),
        dm_dday34C = dday34C - mean(dday34C, na.rm = TRUE),
        dm_ipc = ipc - mean(ipc, na.rm = TRUE),
        dm_pop_dens = pop_dens - mean(pop_dens, na.rm = TRUE),
        dm_pop_dens_sq = pop_dens - mean(pop_dens^2, na.rm = TRUE)) %>% 
  group_by(state, fips) %>%
  summarise(p_corn_share = mean(p_corn_share, na.rm = TRUE),
             p_cotton_share = mean(p_cotton_share, na.rm = TRUE),
             p_hay_share = mean(p_hay_share, na.rm = TRUE),
             p_wheat_share = mean(p_wheat_share, na.rm = TRUE),
             p_soybean_share = mean(p_soybean_share, na.rm = TRUE),
              tavg = mean(tavg, na.rm = TRUE),
              tavg_sq = mean(tavg^2, na.rm = TRUE),
              prec = mean(prec, na.rm = TRUE),
              dm_tavg = mean(dm_tavg, na.rm = TRUE),
              dm_tavg_sq = mean(dm_tavg^2, na.rm = TRUE),
              dm_prec = mean(dm_prec, na.rm = TRUE),
              dm_prec_sq = mean(dm_prec^2, na.rm = TRUE),
              corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
              cotton_a = mean(corn_grain_a, na.rm = TRUE),
              hay_a = mean(corn_grain_a, na.rm = TRUE),
              wheat_a = mean(corn_grain_a, na.rm = TRUE),
              soybean_a = mean(corn_grain_a, na.rm = TRUE),
              lat = mean(lat, na.rm = TRUE),
              dm_dday10_30 = mean(dm_dday10_30, na.rm = TRUE),
              dm_dday10_30_sq = mean(dm_dday10_30^2, na.rm = TRUE),
              dm_dday30 = mean(dm_dday30, na.rm = TRUE),
              dm_dday10 = mean(dm_dday10, na.rm = TRUE),
              dm_dday8_32 = mean(dm_dday8_32, na.rm = TRUE),
              dm_dday34C = mean(dm_dday34C, na.rm = TRUE),
              dm_ipc = mean(dm_ipc, na.rm = TRUE),
              dm_pop_dens = mean(dm_pop_dens, na.rm = TRUE),
              dm_pop_dens_sq = mean(dm_pop_dens^2, na.rm = TRUE),
              total_a = mean(total_a, na.rm = TRUE))

cropdat <- left_join(cropdat, soil, by = "fips")

# Weights are not making equivalent, but lm_felm_dmeanlist...R is showing equivalence
cropdat <- ungroup(cropdat)
cropdat <- as.data.frame(cropdat)


corn_moddat <- demeanlist(
  mtx = as.matrix(cropdat[,3:35]), 
  fl = list(state = cropdat$state),
  weights = sqrt(cropdat$total_a))
corn_moddat <- as.data.frame(corn_moddat)

cotton_moddat <- demeanlist(
  mtx = as.matrix(cropdat[,3:35]), 
  fl = list(state = cropdat$state),
  weights = sqrt(cropdat$total_a))
cotton_moddat <- as.data.frame(cotton_moddat)

hay_moddat <- demeanlist(
  mtx = as.matrix(cropdat[,3:35]), 
  fl = list(state = cropdat$state),
  weights = sqrt(cropdat$total_a))
hay_moddat <- as.data.frame(hay_moddat)

wheat_moddat <- demeanlist(
  mtx = as.matrix(cropdat[,3:35]), 
  fl = list(state = cropdat$state),
  weights = sqrt(cropdat$total_a))
wheat_moddat <- as.data.frame(wheat_moddat)

soybean_moddat <- demeanlist(
  mtx = as.matrix(cropdat[,3:35]), 
  fl = list(state = cropdat$state),
  weights = sqrt(cropdat$total_a))
soybean_moddat <- as.data.frame(soybean_moddat)

cc.corn.mod1 <- lm(p_corn_share ~ dm_tavg + dm_tavg_sq + prec + dm_prec_sq + 
                     lat + dm_ipc + dm_pop_dens + dm_pop_dens_sq + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil, 
                   data = corn_moddat, weights = cropdat$total_a)
summary(cc.corn.mod1)

# Cluster by state
#vcov_state <- cluster.vcov(cc.corn.mod1, cluster = cropdat$state)
#cc.corn.mod1 <- coeftest(cc.corn.mod1, vcov_state)

# l.corn.mod1 <- glm(p_corn_a ~ tavg + tavgsq + prec + precsq, data = cropdat, family = "quasibinomial")
# summary(l.corn.mod1)

cc.corn.mod2 <- lm(p_corn_share ~ dm_dday10_30  + dm_dday10_30_sq + dm_dday30 + dm_prec + dm_prec_sq + 
                    lat + dm_ipc + dm_pop_dens + dm_pop_dens_sq + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil, 
                   data = corn_moddat, weights = cropdat$total_a)
summary(cc.corn.mod2)

# Cotton
cc.cotton.mod1 <- update(cc.corn.mod1, p_cotton_share ~ ., weights = cropdat$total_a, data = cotton_moddat)
summary(cc.cotton.mod1)

cc.cotton.mod2 <- update(cc.corn.mod2, p_cotton_share ~ ., weights = cropdat$total_a, data = cotton_moddat)
summary(cc.cotton.mod2)

# Hay
cc.hay.mod1 <- update(cc.corn.mod1, p_hay_share ~ ., weights = cropdat$total_a, data = hay_moddat)
summary(cc.hay.mod1)

cc.hay.mod2 <- update(cc.corn.mod2, p_hay_share ~ ., weights = cropdat$total_a, data = hay_moddat)
summary(cc.hay.mod2)

# Wheat
cc.wheat.mod1 <- update(cc.corn.mod1, p_wheat_share ~ ., weights = cropdat$total_a, data = wheat_moddat)
summary(cc.wheat.mod1)

cc.wheat.mod2 <- update(cc.corn.mod2, p_wheat_share ~ ., weights = cropdat$total_a, data = wheat_moddat)
summary(cc.wheat.mod2)

# Soybean
cc.soybean.mod1 <- update(cc.corn.mod1, p_soybean_share ~ ., weights = cropdat$total_a, data = soybean_moddat)
summary(cc.soybean.mod1)

cc.soybean.mod2 <- update(cc.corn.mod2, p_soybean_share ~ ., weights = cropdat$total_a, data = soybean_moddat)
summary(cc.soybean.mod2)

# Cluster by state
#vcov_state <- cluster.vcov(cc.corn.mod2, cluster = cropdat$state)
#cc.corn.mod2 <- coeftest(cc.corn.mod2, vcov_state)


saveRDS(cc.corn.mod1, "models/cs.temp.p_corn_share")
saveRDS(cc.corn.mod2, "models/cs.dd.p_corn_share")

# Save models

saveRDS(cc.cotton.mod1, "models/cs.temp.p_cotton_share")
saveRDS(cc.cotton.mod2, "models/cs.dd.p_cotton_share")

saveRDS(cc.hay.mod1, "models/cs.temp.p_hay_share")
saveRDS(cc.hay.mod2, "models/cs.dd.p_hay_share")

saveRDS(cc.wheat.mod1, "models/cs.temp.p_wheat_share")
saveRDS(cc.wheat.mod2, "models/cs.dd.p_wheat_share")

saveRDS(cc.soybean.mod1, "models/cs.temp.p_soybean_share")
saveRDS(cc.soybean.mod2, "models/cs.dd.p_soybean_share")

saveRDS(cropdat, "models/cc.corn.dat")
