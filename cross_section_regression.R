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

cropdat <- filter(cropdat, year >= 1970 & year <= 2010)
# cropdat <- filter(cropdat, corn_rrev > 0)
#cropdat$corn_rrev <- cropdat$corn_rrev + cropdat$cotton_rrev + cropdat$hay_rrev + cropdat$wheat_rrev + cropdat$soybean_rrev
cropdat$ln_corn_rrev <- log(1 + cropdat$corn_rrev)
# cropdat$ln_cotton_rrev <- log(1 + cropdat$cotton_rrev)
# cropdat$ln_hay_rrev <- log(1 + cropdat$hay_rrev)

# cropdat$tavg_sq <- cropdat$tavg^2
# cropdat$prec_sq <- cropdat$prec^2
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))

# d <- cropdat

# Don't think i need to detrend weather data, just take the average
# cropdat <- cropdat %>% 
#   group_by(year) %>% 
#   mutate(dm_ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
#         dm_tavg = tavg - mean(tavg, na.rm = TRUE),
#         dm_prec = prec - mean(prec, na.rm = TRUE),
#         dm_corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
#         dm_dday10_30 = dday10_30 - mean(dday10_30, na.rm = TRUE),
#         dm_dday34C = dday34C - mean(dday34C, na.rm = TRUE),
#         dm_ipc = ipc - mean(ipc, na.rm = TRUE),
#         dm_pop_dens = pop_dens - mean(pop_dens, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   group_by(state, fips) %>% 
# 
#     summarise(dm_ln_corn_rrev = mean(dm_ln_corn_rrev, na.rm = TRUE),
#         dm_tavg = mean(dm_tavg, na.rm = TRUE),
#         dm_prec = mean(dm_prec, na.rm = TRUE),
#         dm_corn_grain_a = mean(dm_corn_grain_a, na.rm = TRUE),
#         lat = mean(lat, na.rm = TRUE),
#         dm_dday10_30 = mean(dm_dday10_30, na.rm = TRUE),
#         dm_dday34C = mean(dm_dday34C, na.rm = TRUE),
#         dm_ipc = mean(dm_ipc, na.rm = TRUE),
#         dm_pop_dens = mean(dm_pop_dens, na.rm = TRUE)) 

cropdat <- cropdat %>%
  group_by(year) %>%
  mutate(ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
         dm_ipc = ipc - mean(ipc, na.rm = TRUE),
         dm_pop_dens = pop_dens - mean(pop_dens, na.rm = TRUE)) %>% 
  ungroup() %>%
  group_by(state, fips) %>%

    summarise(ln_corn_rrev = mean(dm_ln_corn_rrev, na.rm = TRUE),
        dm_tavg = mean(tavg, na.rm = TRUE),
        dm_prec = mean(prec, na.rm = TRUE),
        lat = mean(lat, na.rm = TRUE),
        dm_dday10_30 = mean(dday10_30, na.rm = TRUE),
        dm_dday34C = mean(dday34C, na.rm = TRUE),
        dm_ipc = mean(dm_ipc, na.rm = TRUE),
        dm_pop_dens = mean(dm_pop_dens, na.rm = TRUE))
# 
cropdat <- left_join(cropdat, soil, by = "fips")
cropdat <- filter(cropdat, !is.na(dm_ln_corn_rrev))
# # Corn

# cs.corn.mod1  <- lm(dm_ln_corn_rrev ~ dm_tavg + I(dm_tavg^2) + dm_prec + I(dm_prec^2), data = cropdat)
# summary(cs.corn.mod1)

cs.corn.mod1  <- lm(ln_corn_rrev ~ dm_tavg + I(dm_tavg^2) + dm_prec + I(dm_prec^2), data = cropdat)
summary(cs.corn.mod1)

#cs.corn.mod1  <- lm(dm_ln_corn_rrev ~ dm_tavg + I(dm_tavg^2) + dm_prec + I(dm_prec^2), data = cropdat)
#summary(cs.corn.mod1)

# cs.corn.mod2<- lm(dm_ln_corn_rrev ~ dm_dday10_30 + I(dm_dday10_30^2) + sqrt(dm_dday34C) + dm_prec + I(dm_prec^2), data = cropdat)
# summary(cs.corn.mod2)


cs.corn.mod2<- lm(ln_corn_rrev ~ dm_dday10_30 + sqrt(dm_dday34C) + dm_prec + I(dm_prec^2), data = cropdat)
summary(cs.corn.mod2)

# Save models
saveRDS(cs.corn.mod1, "models/cs.corn.mod1")
saveRDS(cs.corn.mod2, "models/cs.corn.mod2")
# 
# 
# # Check assumptions
# gtest <- gvlma(mod1)
# summary(gtest)
# mean(mod1$residuals)   # Mean of residuals is 0: [1] 2.370616e-18
# 
# plot(mod1) # Homoscedasticity of residuals or equal variance
# 
# acf(mod1$residuals) # no time so no autocorrection, check with dwtest
# dwtest(mod1)
# 
# # Check for multicollinearity
# car::vif(mod1)
# 
# mod1 <- lm(dm_corn_rev ~ dm_dd8C_32C + + I(dm_dd8C_32C^2) + dm_dd34C + dm_prec + dm_precsq, data = corn_reg)
# summary(mod1)
# 
# # Cotton
# mod2 <- lm(dm_cotton_rev ~ dm_tavg + dm_tavgsq + dm_prec + dm_precsq, data = dat)
# summary(mod2)
# 
# # Hay
# mod3 <- lm(dm_hay_rev ~ dm_tavg + dm_tavgsq + dm_prec + dm_precsq, data = dat)
# summary(mod3)
# 
# # Wheat
# mod4 <- lm(dm_wheat_rev ~ dm_tavg + dm_tavgsq + dm_prec + dm_precsq, data = dat)
# summary(mod4)
# 
# # Soybean
# mod5 <- lm(dm_soybean_rev ~ dm_tavg + dm_tavgsq + dm_prec + dm_precsq, data = dat)
# summary(mod5)
# 
# 
# 
