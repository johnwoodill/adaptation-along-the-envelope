library(tidyverse)
library(plm)

# 10-year interval 1960's and 2000's --------------------------------------
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)

cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)

cropdat <- cropdat %>% 
  group_by(year) %>% 
  mutate(p_corn_share = corn_grain_a/sum(total_a, na.rm = TRUE))
  
cropdat1950 <- filter(cropdat, year >= 1950 & year < 1960)
cropdat1960 <- filter(cropdat, year >= 1960 & year < 1970)
cropdat1970 <- filter(cropdat, year >= 1970 & year < 1980)
cropdat1980 <- filter(cropdat, year >= 1980 & year < 1990)
cropdat1990 <- filter(cropdat, year >= 1990 & year < 2000)
cropdat2000 <- filter(cropdat, year >= 2000 & year < 2010)

cropdat1950 <- cropdat1950 %>% 
  group_by(state, fips) %>% 
  summarise(p_corn_share_1950 = mean(p_corn_share, na.rm = TRUE),
            corn_rrev_1950 = mean(corn_rrev, na.rm = TRUE),
            corn_grain_a_1950 = mean(corn_grain_a, na.rm = TRUE),
            tavg_1950 = mean(tavg, na.rm = TRUE),
            dday10_1950 = mean(dday10C, na.rm = TRUE),
            dday30_1950 = mean(dday30C, na.rm = TRUE),
            prec_1950 = mean(prec, na.rm = TRUE))

cropdat1960 <- cropdat1960 %>% 
  group_by(state, fips) %>% 
  summarise(p_corn_share_1960 = mean(p_corn_share, na.rm = TRUE),
            corn_rrev_1960 = mean(corn_rrev, na.rm = TRUE),
            corn_grain_a_1960 = mean(corn_grain_a, na.rm = TRUE),
            tavg_1960 = mean(tavg, na.rm = TRUE),
            dday10_1960 = mean(dday10C, na.rm = TRUE),
            dday30_1960 = mean(dday30C, na.rm = TRUE),
            prec_1960 = mean(prec, na.rm = TRUE))

cropdat1970 <- cropdat1970 %>% 
  group_by(state, fips) %>% 
  summarise(p_corn_share_1970 = mean(p_corn_share, na.rm = TRUE),
            corn_rrev_1970 = mean(corn_rrev, na.rm = TRUE),
            corn_grain_a_1970 = mean(corn_grain_a, na.rm = TRUE),
            tavg_1970 = mean(tavg, na.rm = TRUE),
            dday10_1970 = mean(dday10C, na.rm = TRUE),
            dday30_1970 = mean(dday30C, na.rm = TRUE),
            prec_1970 = mean(prec, na.rm = TRUE))

cropdat1980 <- cropdat1980 %>% 
  group_by(state, fips) %>% 
  summarise(p_corn_share_1980 = mean(p_corn_share, na.rm = TRUE),
            corn_rrev_1980 = mean(corn_rrev, na.rm = TRUE),
            corn_grain_a_1980 = mean(corn_grain_a, na.rm = TRUE),
            tavg_1980 = mean(tavg, na.rm = TRUE),
            dday10_1980 = mean(dday10C, na.rm = TRUE),
            dday30_1980 = mean(dday30C, na.rm = TRUE),
            prec_1980 = mean(prec, na.rm = TRUE))

cropdat1990 <- cropdat1990 %>% 
  group_by(state, fips) %>% 
  summarise(p_corn_share_1990 = mean(p_corn_share, na.rm = TRUE),
            corn_rrev_1990 = mean(corn_rrev, na.rm = TRUE),
            corn_grain_a_1990 = mean(corn_grain_a, na.rm = TRUE),
            tavg_1990 = mean(tavg, na.rm = TRUE),
            dday10_1990 = mean(dday10C, na.rm = TRUE),
            dday30_1990 = mean(dday30C, na.rm = TRUE),
            prec_1990 = mean(prec, na.rm = TRUE))

cropdat2000 <- cropdat2000 %>% 
  group_by(state, fips) %>% 
  summarise(p_corn_share_2000 = mean(p_corn_share, na.rm = TRUE),
            corn_rrev_2000 = mean(corn_rrev, na.rm = TRUE),
            corn_grain_a_2000 = mean(corn_grain_a, na.rm = TRUE),
            tavg_2000 = mean(tavg, na.rm = TRUE),
            dday10_2000 = mean(dday10C, na.rm = TRUE),
            dday30_2000 = mean(dday30C, na.rm = TRUE),
            prec_2000 = mean(prec, na.rm = TRUE))

# 20-year intervals (1980-1960 & 2000 - 1980)
dat1960 <- left_join(cropdat1960, cropdat1980, by = c("state", "fips"))
dat1960$dday10_30 <- (dat1960$dday10_1980 - dat1960$dday30_1980) - (dat1960$dday10_1960 - dat1960$dday30_1960)
dat1960$dday30 <- dat1960$dday30_1980 - dat1960$dday30_1960
dat1960$corn_grain_a <- dat1960$corn_grain_a_1960
dat1960$tavg <- dat1960$tavg_1980 - dat1960$tavg_1960
dat1960$prec <- dat1960$prec_1980 - dat1960$prec_1960
dat1960$ln_corn_rrev <- log(dat1960$corn_rrev_1980) - log(dat1960$corn_rrev_1960)
dat1960$p_corn_share <- dat1960$p_corn_share_1980 - dat1960$p_corn_share_1960
dat1960$year <- 1960

dat1980 <- left_join(cropdat1980, cropdat2000, by = c("state", "fips"))
dat1980$dday10_30 <- (dat1980$dday10_2000 - dat1980$dday30_2000) - (dat1980$dday10_1980 - dat1980$dday30_1980)
dat1980$dday30 <- dat1980$dday30_2000 - dat1980$dday30_1980
dat1980$corn_grain_a <- dat1980$corn_grain_a_1980
dat1980$tavg <- dat1980$tavg_2000 - dat1980$tavg_1980
dat1980$prec <- dat1980$prec_2000 - dat1980$prec_1980
dat1980$ln_corn_rrev <- log(dat1980$corn_rrev_2000) - log(dat1980$corn_rrev_1980)
dat1980$p_corn_share <- dat1980$p_corn_share_2000 - dat1980$p_corn_share_1980
dat1980$year <- 1980

newcropdat <- rbind(dat1960, dat1980)

diff.corn.mod1 <- lm(ln_corn_rrev ~ factor(year) + factor(fips) + tavg + I(tavg^2) + prec + I(prec^2), 
                   data = newcropdat, weights = newcropdat$corn_grain_a)
summary(diff.corn.mod1)

diff.corn.mod2 <- lm(ln_corn_rrev ~ factor(year) + factor(fips) + dday10_30 + I(dday10_30^2) + dday30 + prec + I(prec^2), 
                   data = newcropdat, weights = newcropdat$corn_grain_a)
summary(diff.corn.mod2)

diff.corn.mod3 <- plm(p_corn_share ~ factor(year) + tavg + I(tavg^2) + prec + I(prec^2), 
                   data = newcropdat, index = c("fips", "year"), model = "within")
summary(diff.corn.mod3)

diff.corn.mod4 <- plm(p_corn_share ~ factor(year) + dday10_30 + I(dday10_30^2) + dday30 + prec + I(prec^2), 
                   data = newcropdat, index = c("fips", "year"), model = "within")
summary(diff.corn.mod4)

saveRDS(diff.corn.mod1, "models/diff.temp.ln_corn_rrev")
saveRDS(diff.corn.mod2, "models/diff.dd.ln_corn_rrev")
saveRDS(diff.corn.mod3, "models/diff.temp.p_corn_share")
saveRDS(diff.corn.mod4, "models/diff.dd.p_corn_share")




