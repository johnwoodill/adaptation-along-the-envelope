library(tidyverse)
library(stargazer)
library(rms)
library(cowplot)

# Comparison across cross section and panel estimates

cropdat <- readRDS("data/full_ag_data.rds")

# East of 100th meridian
cropdat <- filter(cropdat, abs(long) <= 100)

cropdat <- filter(cropdat, year >= 1950 & year <= 2010)
#cropdat <- filter(cropdat, year >= 2000 & year <= 2003)
cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)
# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))
cropdat$ln_corn_rrev <- log(1 + cropdat$corn_rrev)

d <- cropdat
g <- cropdat


g <- cropdat %>%
   group_by(fips) %>%
   mutate(corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
             tavg = tavg - mean(tavg, na.rm = TRUE),
             prec = prec - mean(prec, na.rm = TRUE))

# Removes time-invariant differences (within-group) using dplyr

g <- cropdat %>%
   group_by(fips) %>%
   mutate(corn_rrev = log(1 + corn_rrev) - mean(log(1 + corn_rrev), na.rm = TRUE),
             tavg = tavg - mean(tavg, na.rm = TRUE),
             prec = prec - mean(prec, na.rm = TRUE))

# # Within group estimator using for loop (same as above)
# for (i in unique(d$fips)){
#   timemeancorn <- log(1+mean(d[d$fips == i, "corn_rrev"], na.rm = TRUE))
#   timemeantavg <- mean(d[d$fips == i, "tavg"], na.rm = TRUE)
#   timemeanprec <- mean(d[d$fips == i, "prec"], na.rm = TRUE)
#   g$corn_rrev[d$fips == i] <- log(1+d$corn_rrev[d$fips == i]) - timemeancorn
#   g$tavg[d$fips == i] <- d$tavg[d$fips == i] - timemeantavg
#   g$prec[d$fips == i] <- d$prec[d$fips == i] - timemeanprec
# }

mod1 <-lm(corn_rrev ~ tavg + prec, data = g)
summary(mod1)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.1451817  0.0036121  -40.19   <2e-16 ***
# tavg         0.1562292  0.0054325   28.76   <2e-16 ***
# prec         0.0129865  0.0002019   64.31   <2e-16 ***

p.cropdat <- plm.data(cropdat, index = c("fips", "year"))
mod2 <- plm(log(1+corn_rrev) ~ tavg + prec, data = p.cropdat, model = "within")
summary(mod2)

# Coefficients :
#       Estimate Std. Error t-value  Pr(>|t|)    
# tavg 0.1610887  0.0054994  29.292 < 2.2e-16 ***
# prec 0.0130834  0.0002043  64.041 < 2.2e-16 ***



#-----------------------------------------
# Removes time-invariant differences (between-group) using dplyr

g <- cropdat %>%
   group_by(year) %>%
   mutate(corn_rrev = log(1 + corn_rrev) - mean(log(1 + corn_rrev), na.rm = TRUE),
             tavg = tavg - mean(tavg, na.rm = TRUE),
             prec = prec - mean(prec, na.rm = TRUE))


mod1 <-lm(corn_rrev ~ tavg + prec, data = g)
summary(mod1)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.1451817  0.0036121  -40.19   <2e-16 ***
# tavg         0.1562292  0.0054325   28.76   <2e-16 ***
# prec         0.0129865  0.0002019   64.31   <2e-16 ***

p.cropdat <- plm.data(cropdat, index = c("fips", "year"))
mod2 <- plm(log(1+corn_rrev) ~ tavg + prec, data = p.cropdat, model = "between")
summary(mod2)
