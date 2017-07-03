library(tidyverse)
library(stargazer)
library(rms)
library(cowplot)

# Comparison across cross section and panel estimates

cropdat <- readRDS("data/full_ag_data.rds")

# East of 100th meridian
cropdat <- filter(cropdat, abs(long) <= 100)

cropdat <- filter(cropdat, year >= 1960 & year <= 2010)
#cropdat <- filter(cropdat, year >= 2000 & year <= 2003)

# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))

d <- cropdat
g <- cropdat

# Removes nation wide differences
g <- cropdat %>%
   group_by(year) %>% 
   mutate(dm_corn_rev = log(1 + corn_rev) - mean(log(1 + corn_rev), na.rm = TRUE),
             dm_tavg = tavg - mean(tavg, na.rm = TRUE),
             dm_prec = prec - mean(prec, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(fips) %>% 
  summarise(dm_corn_rev = mean(dm_corn_rev, na.rm = TRUE),
            dm_tavg = mean(dm_tavg, na.rm = TRUE),
            dm_prec = mean(dm_prec, na.rm = TRUE))

g <- cropdat %>%
   group_by(year) %>% 
   mutate(dm_corn_rev = log(1 + corn_rev) - mean(log(1 + corn_rev), na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(fips) %>% 
  summarise(dm_corn_rev = mean(dm_corn_rev, na.rm = TRUE),
            dm_tavg = mean(tavg, na.rm = TRUE),
            dm_prec = mean(prec, na.rm = TRUE))

# Removes time-invariant differences (within-group)
g <- cropdat %>%
   group_by(fips) %>% 
   mutate(dm_corn_rrev = log(1 + corn_rrev) - mean(log(1 + corn_rrev), na.rm = TRUE),
             dm_tavg = tavg - mean(tavg, na.rm = TRUE),
             dm_prec = prec - mean(prec, na.rm = TRUE))

# Within group estimator
for (i in unique(d$fips)){
  timemeancorn <- log(1+mean(d[d$fips == i, "corn_rrev"], na.rm = TRUE))
  timemeantavg <- mean(d[d$fips == i, "tavg"], na.rm = TRUE)
  timemeanprec <- mean(d[d$fips == i, "prec"], na.rm = TRUE)
  g$corn_rrev[d$fips == i] <- log(1+d$corn_rrev[d$fips == i]) - timemeancorn
  g$tavg[d$fips == i] <- d$tavg[d$fips == i] - timemeantavg
  g$prec[d$fips == i] <- d$prec[d$fips == i] - timemeanprec
}


mod1 <-lm(dm_corn_rev ~ dm_tavg + dm_prec, data = g)
mod1 <-lm(corn_rrev ~ tavg + prec, data = g)
summary(mod1)

mod1 <- segmented(mod1, seg.Z = ~ dm_tavg, psi = list(dm_tavg = c(4, 7, 10, 13, 16, 19, 20)))

summary(mod1)
slope(mod1)
mod2 <- plm(log(1+corn_rrev) ~ tavg + prec,data = cropdat, index = c("fips"))
summary(mod2)


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



