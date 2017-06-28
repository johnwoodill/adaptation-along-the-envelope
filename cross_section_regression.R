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

cropdat$total_rev <- cropdat$corn_rev + cropdat$cotton_rev + cropdat$hay_rev + cropdat$wheat_rev + cropdat$soybean_rev
cropdat$total_a <- cropdat$corn_grain_a + cropdat$cotton_a + cropdat$hay_a + cropdat$wheat_a + cropdat$soybean_a

cropdat$dd8C_32C <-cropdat$dday8C - cropdat$dday32C

# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))

# Get annual averages
dat <- cropdat %>%
  group_by(year) %>%
  summarise(annual_corn_rev = mean(corn_rev, na.rm = TRUE),
            annual_cotton_rev = mean(cotton_rev, na.rm = TRUE),
            annual_hay_rev = mean(hay_rev, na.rm = TRUE),
            annual_wheat_rev = mean(wheat_rev, na.rm = TRUE),
            annual_soybean_rev = mean(soybean_rev, na.rm = TRUE),
            annual_tavg = mean(tavg, na.rm = TRUE),
            annual_prec = mean(prec, na.rm = TRUE),
            annual_dd8C_32C = mean(dd8C_32C, na.rm = TRUE),
            annual_dday34C = mean(dday34C, na.rm = TRUE)) %>%  
    ungroup()

dat <- left_join(cropdat, dat, by = "year")

# Demean each crop and temps
#dat$dm_corn_rev <- log(1 + dat$corn_rev) - log(1 + dat$annual_corn_rev)

dat$dm_corn_rev <- dat$corn_rev - dat$annual_corn_rev
dat$dm_cotton_rev <- log(1 + dat$cotton_rev) - log(1 + dat$annual_cotton_rev)
dat$dm_hay_rev <- log(1 + dat$hay_rev) - log(1 + dat$annual_hay_rev)
dat$dm_wheat_rev <- log(1 + dat$wheat_rev) - log(1 + dat$annual_wheat_rev)
dat$dm_soybean_rev <- log(1 + dat$soybean_rev) - log(1 + dat$annual_soybean_rev)
dat$dm_tavg <- dat$tavg - dat$annual_tavg  
dat$dm_prec <- dat$prec - dat$annual_prec
dat$dm_dd8C_32C <- dat$dd8C_32C - dat$annual_dd8C_32C
dat$dm_dd34C <- dat$dday34C - dat$annual_dday34C

dat$dm_tavg <- dat$tavg
dat$dm_prec <- dat$prec
dat$dm_dd8C_32C <- dat$dd8C_32C
dat$dm_dd34C <- dat$dday34C


# Aggregate to county level
dat <- dat %>%
   group_by(fips) %>% 
   summarise(dm_corn_rev = mean(dm_corn_rev, na.rm = TRUE),
             corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
             dm_cotton_rev = mean(dm_cotton_rev, na.rm = TRUE),
             dm_hay_rev = mean(dm_hay_rev, na.rm = TRUE),
             dm_wheat_rev = mean(dm_wheat_rev, na.rm = TRUE),
             dm_soybean_rev = mean(dm_soybean_rev, na.rm = TRUE),
             dm_tavg = mean(dm_tavg, na.rm = TRUE),
             dm_prec = mean(dm_prec, na.rm = TRUE),
             dm_dd8C_32C = mean(dm_dd8C_32C, na.rm = TRUE),
             dm_dd34C = mean(dm_dd34C, na.rm = TRUE))

dat[is.infinite(dat$dm_corn_rev),] <- NA
dat$dm_tavgsq <- dat$dm_tavg^2
dat$dm_precsq <- dat$dm_prec^2

# Corn
corn_reg <- filter(dat, !is.na(dm_corn_rev))
mod1 <- lm(dm_corn_rev ~ dm_tavg + dm_tavgsq + dm_precsq + dm_prec, data = dat)
summary(mod1)

# Check assumptions
gtest <- gvlma(mod1)
summary(gtest)
mean(mod1$residuals)   # Mean of residuals is 0: [1] 2.370616e-18

plot(mod1) # Homoscedasticity of residuals or equal variance

acf(mod1$residuals) # no time so no autocorrection, check with dwtest
dwtest(mod1)

cor.test(corn_reg$dm_tavg, mod1$residuals)
cor.test(corn_reg$dm_tavgsq, mod1$residuals)
cor.test(corn_reg$dm_prec, mod1$residuals)
cor.test(corn_reg$dm_precsq, mod1$residuals)

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



