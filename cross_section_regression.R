library(tidyverse)
library(stargazer)
library(rms)
library(cowplot)

cropdat <- readRDS("data/full_ag_data.rds")

# East of 100th meridian
cropdat <- filter(cropdat, abs(long) <= 100)

cropdat <- filter(cropdat, year >= 1960 & year <= 2010)

# Crop rev
cropdat$corn_rev <- (cropdat$corn_grain_p*cropdat$corn_rprice)/cropdat$corn_grain_a
cropdat$cotton_rev <- (cropdat$cotton_p*cropdat$cotton_rprice)/cropdat$cotton_a
cropdat$hay_rev <- (cropdat$hay_p*cropdat$hay_rprice)/cropdat$hay_a 
cropdat$wheat_rev <- (cropdat$wheat_p*cropdat$wheat_rprice)/cropdat$wheat_a
cropdat$soybean_rev <- (cropdat$soybean_p*cropdat$soybean_rprice)/cropdat$soybean_a

cropdat$dd8C_32C <-cropdat$dday8C - cropdat$dday32C

cropdat$ln_corn_rev <- log(1 + cropdat$corn_rev)
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$prec_sq <- cropdat$prec^2

# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))

d <- cropdat

for (i in unique(d$fips)){
  timemeancorn <- mean(d[d$fips == i, "ln_corn_rev"], na.rm = TRUE)
  timemeantavg <- mean(d[d$fips == i, "tavg"], na.rm = TRUE)
  timemeanprec <- mean(d[d$fips == i, "prec"], na.rm = TRUE)
  timemeantavg_sq <- mean(d[d$fips == i, "tavg_sq"], na.rm = TRUE)
  timemeanprec_sq <- mean(d[d$fips == i, "prec_sq"], na.rm = TRUE)
  cropdat$ln_corn_rev[d$fips == i] <- d$ln_corn_rev[d$fips == i] - timemeancorn
  cropdat$tavg[d$fips == i] <- d$tavg[d$fips == i] - timemeantavg
  cropdat$prec[d$fips == i] <- d$prec[d$fips == i] - timemeanprec
  cropdat$tavg_sq[d$fips == i] <- d$tavg_sq[d$fips == i] - timemeantavg_sq
  cropdat$prec_sq[d$fips == i] <- d$prec_sq[d$fips == i] - timemeanprec_sq
}

cropdat <- cropdat %>% 
  group_by(fips) %>% 
  summarise(ln_corn_rev = mean(ln_corn_rev, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE),
            prec = mean(prec, na.rm = TRUE))

# Corn
mod1  <- lm(ln_corn_rev ~ tavg + I(tavg^2) + prec + I(prec^2), data = cropdat)
summary(mod1)

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



