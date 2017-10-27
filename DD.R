library(lfe)
library(tidyverse)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, year < 2010)
cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

#cropdat <- filter(cropdat, state == "wi")
#cropdat
# # Constant prices
cropdat$corn_rprice <- mean(cropdat$corn_rprice, na.rm = TRUE)
cropdat$cotton_rprice <- mean(cropdat$cotton_rprice, na.rm = TRUE)
cropdat$hay_rprice <- mean(cropdat$hay_rprice, na.rm = TRUE)
cropdat$wheat_rprice <- mean(cropdat$wheat_rprice, na.rm = TRUE)
cropdat$soybean_rprice <- mean(cropdat$soybean_rprice, na.rm = TRUE)

 
# Total Activity
cropdat$corn <- cropdat$corn_yield*cropdat$corn_rprice
cropdat$cotton <- cropdat$cotton_yield*cropdat$cotton_rprice
cropdat$hay <- cropdat$hay_yield*cropdat$hay_rprice
cropdat$wheat <- cropdat$wheat_yield*cropdat$wheat_rprice
cropdat$soybean <- cropdat$soybean_yield*cropdat$soybean_rprice



cropdat$rev <- rowSums(cropdat[, c("corn", "cotton", "hay", "soybean", "wheat")], na.rm = TRUE)
cropdat$ln_rev <- log(1 + cropdat$rev)
cropdat$prec_sq <- cropdat$prec^2

# Find warmest counties
dat1950 <- filter(cropdat, year >= 1950 & year <= 1979)
dat1950 <- dat1950 %>% 
  group_by(state, fips) %>% 
  summarise(dday30C_1950 = mean(dday30C, na.rm = TRUE))

dat2000 <- filter(cropdat, year >= 1980 & year <= 2009)
dat2000 <- dat2000 %>% 
  group_by(state, fips) %>% 
  summarise(dday30C_2000 = mean(dday30C, na.rm = TRUE))

dat <- left_join(dat1950, dat2000, by = c("state", "fips"))
dat$tdiff <- dat$dday30C_2000 - dat$dday30C_1950

diff <- arrange(dat, -tdiff)
head(diff)

# Split into thirds by state
diff <- diff %>% 
   group_by(state) %>% 
   mutate(thirds = dplyr::ntile(tdiff, 3))

spdiff <- filter(diff, thirds == 3) # Warmest
wfips <- spdiff$fips

tpdiff <- filter(diff, thirds == 1) # Coolest
cfips <- tpdiff$fips

moddat1 <- filter(cropdat, fips %in% wfips)
moddat2 <- filter(cropdat, fips %in% cfips)

moddat1$omega <- 1
moddat2$omega <- 0

moddat <- rbind(moddat1, moddat2)
head(moddat)

moddat$tau <- ifelse(moddat$year >= 1980, 1, 0)

moddat$did <- moddat$tau*moddat$omega
moddat$trend <- moddat$year - 1949
moddat$state_trend <- as.numeric(factor(moddat$state, levels = unique(moddat$state)))*moddat$trend

mod <- felm(log(1 + rev) ~ dday0_10 + dday10_30 + dday30C + prec + I(prec^2) + 
            state_trend + tau + omega + did | fips, data = moddat)
summary(mod)


# Hand computer data
a = sapply(subset(moddat, tau == 0 & omega == 0, select = rev), mean)
b = sapply(subset(moddat, tau == 0 & omega == 1, select = rev), mean)
c = sapply(subset(moddat, tau == 1 & omega == 0, select = rev), mean)
d = sapply(subset(moddat, tau == 1 & omega == 1, select = rev), mean)

# average difference
(d - c) - (b - a)
# -10.88443

# percentage difference
((d - c) / c)* 100 - ((b - a) / a)*100
#-3.037302


mod0 <- felm(ln_rev ~ tau + omega + tau + did, data = moddat)

mod1 <- felm(ln_rev ~ tau + omega + tau + did + state_trend | fips, data = moddat)

mod2 <- felm(ln_rev ~ tau + omega + tau + did | fips + year, data = moddat)

mod3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + omega + tau + did, data = moddat)

mod4 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + omega + tau + did + state_trend | fips, data = moddat)

mod5 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
              tau + omega + tau + did | fips + year, data = moddat)


# summary(mod1)
# summary(mod2)
# summary(mod3)
# summary(mod4)


 saveRDS(mod0, "models/dd_mod0.rds")
 saveRDS(mod1, "models/dd_mod1.rds")
 saveRDS(mod2, "models/dd_mod2.rds")
 saveRDS(mod3, "models/dd_mod3.rds")
 saveRDS(mod4, "models/dd_mod4.rds")
 saveRDS(mod5, "models/dd_mod5.rds")
 