library(plm)
library(tidyverse)
library(stargazer)
library(rms)
library(cowplot)

# Crop data
cropdat <- readRDS("data/full_ag_data.rds")

# Soil data
#soil <- read_dta("data/soilData.dta")
soil <- readRDS("data/soilData.rds")
soil$fips <- as.numeric(soil$fips)


# East of 100th meridian
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, !is.na(corn_rrev))
cropdat <- filter(cropdat, year >= 1970 & year <= 2010)

cropdat$ln_corn_rrev <- log(1 + cropdat$corn_rrev)
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
        dm_tavg = tavg - mean(tavg, na.rm = TRUE),
        dm_prec = prec - mean(prec, na.rm = TRUE),
        dm_dday10_30 = dday10_30 - mean(dday10_30, na.rm = TRUE),
        dm_dday10_30_sq = dday10_30_sq - mean(dday10_30_sq, na.rm = TRUE),
        dm_dday10 = dday10C - mean(dday10C, na.rm = TRUE),
        dm_dday30 = dday30C - mean(dday30C, na.rm = TRUE),
        dm_dday8_32 = dday8_32 - mean(dday8_32, na.rm = TRUE),
        dm_dday34C = dday34C - mean(dday34C, na.rm = TRUE),
        dm_ipc = ipc - mean(ipc, na.rm = TRUE),
        dm_pop_dens = pop_dens - mean(pop_dens, na.rm = TRUE)) %>% 
    group_by(state, fips) %>%
    summarise(ln_corn_rrev = mean(dm_ln_corn_rrev, na.rm = TRUE),
              tavg = mean(tavg, na.rm = TRUE),
              prec = mean(prec, na.rm = TRUE),
              dm_tavg = mean(dm_tavg, na.rm = TRUE),
              dm_prec = mean(dm_prec, na.rm = TRUE),
              corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
              lat = mean(lat, na.rm = TRUE),
              dm_dday10_30 = mean(dm_dday10_30, na.rm = TRUE),
              dm_dday30 = mean(dm_dday30, na.rm = TRUE),
              dm_dday10 = mean(dm_dday10, na.rm = TRUE),
              dm_dday8_32 = mean(dm_dday8_32, na.rm = TRUE),
              dm_dday34C = mean(dm_dday34C, na.rm = TRUE),
              dm_ipc = mean(dm_ipc, na.rm = TRUE),
              dm_pop_dens = mean(dm_pop_dens, na.rm = TRUE))

mean(cropdat$dm_dday10_30, na.rm = TRUE)
cropdat$state <- factor(cropdat$state) 
cropdat <- left_join(cropdat, soil, by = "fips")

cs.corn.mod1  <- lm(ln_corn_rrev ~ factor(state) + dm_tavg + I(dm_tavg^2) + dm_prec + I(dm_prec^2) + 
                      lat + dm_ipc + dm_pop_dens + I(dm_pop_dens^2) + waterCapacity + percentClay + minPermeability + kFactor + bestSoil,
                    data = cropdat, weights = cropdat$corn_grain_a)
summary(cs.corn.mod1)

cs.corn.mod2 <- lm(ln_corn_rrev ~ factor(state) + dm_dday10_30 + I(dm_dday10_30^2) + dm_dday30 + dm_prec + I(dm_prec^2) + 
                    lat + dm_ipc + dm_pop_dens + I(dm_pop_dens^2) + waterCapacity + percentClay + minPermeability + kFactor + bestSoil,
                  data = cropdat, weights = cropdat$corn_grain_a, x = TRUE, y = TRUE)
summary(cs.corn.mod2)

# Save models
saveRDS(cs.corn.mod1, "models/cs.corn.mod1")
saveRDS(cs.corn.mod2, "models/cs.corn.mod2")
saveRDS(cropdat, "models/cs.corn.dat")
# 
# 

#Predict(cs.corn.mod1, ln_corn_rrev)
#cs.corn.mod1$fitted.values
#plot(x = (cs.corn.mod1$model$dm_tavg - cs.corn.mod1$residuals), y = cs.corn.mod1$model$ln_corn_rrev)
