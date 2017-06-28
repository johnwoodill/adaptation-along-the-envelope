library(plm)
library(tidyverse)
library(stargazer)
library(rms)
library(cowplot)

cropdat <- readRDS("data/full_ag_data.rds")

# East of 100th meridian
cropdat <- filter(cropdat, abs(long) <= 100)

cropdat <- filter(cropdat, year >= 1960 & year <= 2010)

cropdat$ln_corn_rrev <- log(1 + cropdat$corn_rrev)
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$prec_sq <- cropdat$prec^2

# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))

# d <- cropdat

d <- cropdat %>% 
  group_by(state) %>% 
  mutate(dm_ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
        dm_tavg = tavg - mean(tavg, na.rm = TRUE),
        dm_prec = prec - mean(prec, na.rm = TRUE),
        dm_corn_grain_a = corn_grain_a - mean(corn_grain_a, na.rm = TRUE))

# for (i in unique(d$fips)){
#    timemeancorn <- mean(d[d$fips == i, "ln_corn_rrev"], na.rm = TRUE)
#    timemeantavg <- mean(d[d$fips == i, "tavg"], na.rm = TRUE)
#    timemeanprec <- mean(d[d$fips == i, "prec"], na.rm = TRUE)
#    cropdat$dm_ln_corn_rrev[d$fips == i] <- d$ln_corn_rrev[d$fips == i] - timemeancorn
#    cropdat$dm_tavg[d$fips == i] <- d$tavg[d$fips == i] - timemeantavg
#    cropdat$dm_prec[d$fips == i] <- d$prec[d$fips == i] - timemeanprec
#  }

# cropdat <- cropdat %>% 
#   group_by(fips) %>% 
#   summarise(ln_corn_rev = mean(ln_corn_rev, na.rm = TRUE),
#             tavg = mean(tavg, na.rm = TRUE),
#             prec = mean(prec, na.rm = TRUE))

# Corn
mod1  <- lm(dm_ln_corn_rrev ~ dm_tavg + I(dm_tavg^2) + dm_prec + I(dm_prec^2) + lat, data = d)
summary(mod1)

mod2 <- plm(ln_corn_rrev ~ tavg + I(tavg^2) + prec + I(prec^2), data = cropdat, index = "state")
summary(mod2)

# Check assumptions
gtest <- gvlma(mod1)
summary(gtest)
mean(mod1$residuals)   # Mean of residuals is 0: [1] 2.370616e-18

plot(mod1) # Homoscedasticity of residuals or equal variance

acf(mod1$residuals) # no time so no autocorrection, check with dwtest
dwtest(mod1)

# Check for multicollinearity
car::vif(mod1)

mod1 <- lm(dm_corn_rev ~ dm_dd8C_32C + + I(dm_dd8C_32C^2) + dm_dd34C + dm_prec + dm_precsq, data = corn_reg)
summary(mod1)

# Cotton
mod2 <- lm(dm_cotton_rev ~ dm_tavg + dm_tavgsq + dm_prec + dm_precsq, data = dat)
summary(mod2)

# Hay
mod3 <- lm(dm_hay_rev ~ dm_tavg + dm_tavgsq + dm_prec + dm_precsq, data = dat)
summary(mod3)

# Wheat
mod4 <- lm(dm_wheat_rev ~ dm_tavg + dm_tavgsq + dm_prec + dm_precsq, data = dat)
summary(mod4)

# Soybean
mod5 <- lm(dm_soybean_rev ~ dm_tavg + dm_tavgsq + dm_prec + dm_precsq, data = dat)
summary(mod5)



