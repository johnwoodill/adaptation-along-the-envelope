library(tidyverse)
library(plm)

# 10-year interval 1960's and 2000's --------------------------------------
setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)

cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)
cropdat$p_corn_share <- cropdat$corn_grain_a/cropdat$total_a
cropdat$p_cotton_share <- cropdat$cotton_a/cropdat$total_a
cropdat$p_hay_share <- cropdat$hay_a/cropdat$total_a
cropdat$p_wheat_share <- cropdat$wheat_a/cropdat$total_a
cropdat$p_soybean_share <- cropdat$soybean_a/cropdat$total_a
cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(cropdat$soybean_rrev)

decade_merge <- function(dat, begd, endd, int){
  mergdat <- data.frame()
  decades <- seq(begd, endd, int)
  for (i in decades){
    int.dat <- filter(dat, year >= i & year < (i + int))
    
    int.dat <- int.dat %>% 
    mutate(p_corn_share = p_corn_share - mean(p_corn_share, na.rm = TRUE),
         ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
         p_cotton_share = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
         ln_cotton_rrev = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
         p_hay_share = p_hay_share - mean(p_hay_share, na.rm = TRUE),
         ln_hay_rrev = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
         p_wheat_share = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
         ln_wheat_rrev = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
         p_soybean_share = p_soybean_share - mean(p_soybean_share, na.rm = TRUE),
         ln_soybean_rrev = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE)) %>% 
   group_by(state, fips) %>% 
   summarise(p_corn_share = mean(p_corn_share, na.rm = TRUE),
            ln_corn_rrev = mean(ln_corn_rrev, na.rm = TRUE),
            corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
            p_cotton_share = mean(p_cotton_share, na.rm = TRUE),
            ln_cotton_rrev = mean(ln_cotton_rrev, na.rm = TRUE),
            cotton_a = mean(cotton_a, na.rm = TRUE),
            p_hay_share = mean(p_hay_share, na.rm = TRUE),
            ln_hay_rrev = mean(ln_hay_rrev, na.rm = TRUE),
            hay_a = mean(hay_a, na.rm = TRUE),
            p_wheat_share = mean(p_wheat_share, na.rm = TRUE),
            ln_wheat_rrev = mean(ln_wheat_rrev, na.rm = TRUE),
            wheat_a = mean(wheat_a, na.rm = TRUE),
            p_soybean_share = mean(p_soybean_share, na.rm = TRUE),
            ln_soybean_rrev = mean(ln_soybean_rrev, na.rm = TRUE),
            soybean_a = mean(soybean_a, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE),
            dday0C = mean(dday0C, na.rm = TRUE),
            dday10C = mean(dday10C, na.rm = TRUE),
            dday15C = mean(dday15C, na.rm = TRUE),
            dday17C = mean(dday17C, na.rm = TRUE),
            dday30C = mean(dday30C, na.rm = TRUE),
            dday0_10 = dday0C - dday10C,
            dday10_30 = dday10C - dday30C,
            prec = mean(prec, na.rm = TRUE),
            prec_sq = prec^2,
            total_a = mean(total_a, na.rm = TRUE)) %>% 
      ungroup()
    int.dat$year <- i
    mergdat <- rbind(mergdat, int.dat)
  }
  return(mergdat)
}

decadedat <- decade_merge(cropdat, 1930, 2000, 20)

# Setup data sets for regression
corndat <- filter(decadedat, !is.na(ln_corn_rrev))
cottondat <- filter(decadedat, !is.na(ln_cotton_rrev))
haydat <- filter(decadedat, !is.na(ln_hay_rrev))
wheatdat <- filter(decadedat, !is.na(ln_wheat_rrev))
soybeandat <- filter(decadedat, !is.na(ln_soybean_rrev))

# Log Revenue

# Corn
diff.corn.mod1 <- felm(ln_corn_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | state + year | 0 | state, 
                   data = corndat, weights = corndat$corn_grain_a)
summary(diff.corn.mod1)

diff.corn.mod2 <- felm(ln_corn_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | state + year | 0 | state, 
                   data = corndat, weights = corndat$corn_grain_a)
summary(diff.corn.mod2)

# Cotton
diff.cotton.mod1 <- felm(ln_cotton_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | state + year | 0 | state, 
                   data = cottondat, weights = cottondat$cotton_a)
summary(diff.cotton.mod1)

diff.cotton.mod2 <- felm(ln_cotton_rrev ~ dday0_10 +dday10_30 + dday30C + prec + prec_sq | state + year | 0 | state, 
                   data = cottondat, weights = cottondat$cotton_a)
summary(diff.cotton.mod2)

# Hay
diff.hay.mod1 <- felm(ln_hay_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | state + year | 0 | state, 
                   data = haydat, weights = haydat$hay_a)
summary(diff.hay.mod1)

diff.hay.mod2 <- felm(ln_hay_rrev ~ dday0_10 +dday10_30 + dday30C + prec + prec_sq | state + year | 0 | state, 
                   data = haydat, weights = haydat$hay_a)
summary(diff.hay.mod2)

# Wheat
diff.wheat.mod1 <- felm(ln_wheat_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | state + year | 0 | state, 
                   data = wheatdat, weights = wheatdat$wheat_a)
summary(diff.wheat.mod1)

diff.wheat.mod2 <- felm(ln_wheat_rrev ~ dday0_10  + dday10_30 + dday30C + prec + prec_sq | state + year | 0 | state, 
                   data = wheatdat, weights = wheatdat$wheat_a)
summary(diff.wheat.mod2)

# Soybean
diff.soybean.mod1 <- felm(ln_soybean_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | state + year | 0 | state, 
                   data = soybeandat, weights = soybeandat$soybean_a)
summary(diff.soybean.mod1)

diff.soybean.mod2 <- felm(ln_soybean_rrev ~ dday0_10 +dday10_30 + dday30C + prec + prec_sq | state + year | 0 | state, 
                   data = soybeandat, weights = soybeandat$soybean_a)
summary(diff.soybean.mod2)

# Save regression data
saveRDS(diff.corn.mod1, "models/diff.temp.ln_corn_rrev")
saveRDS(diff.corn.mod2, "models/diff.dd.ln_corn_rrev")
saveRDS(diff.cotton.mod1, "models/diff.temp.ln_cotton_rrev")
saveRDS(diff.cotton.mod2, "models/diff.dd.ln_cotton_rrev")
saveRDS(diff.hay.mod1, "models/diff.temp.ln_hay_rrev")
saveRDS(diff.hay.mod2, "models/diff.dd.ln_hay_rrev")
saveRDS(diff.wheat.mod1, "models/diff.temp.ln_wheat_rrev")
saveRDS(diff.wheat.mod2, "models/diff.dd.ln_wheat_rrev")
saveRDS(diff.soybean.mod1, "models/diff.temp.ln_soybean_rrev")
saveRDS(diff.soybean.mod2, "models/diff.dd.ln_soybean_rrev")

saveRDS(decadedat, "data/diff_regression_data.rds")


# Proportion of acreage regressions

# 10-year interval 1960's and 2000's --------------------------------------
setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
#cropdat <- filter(cropdat, corn_grain_a != 0 & cotton_a != 0 & hay_a != 0 &  soybean_a != 0 & wheat_a != 0)
cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)
cropdat <- filter(cropdat, total_a > 0)
cropdat$p_corn_share <- cropdat$corn_grain_a/cropdat$total_a
cropdat$p_cotton_share <- cropdat$cotton_a/cropdat$total_a
cropdat$p_hay_share <- cropdat$hay_a/cropdat$total_a
cropdat$p_wheat_share <- cropdat$wheat_a/cropdat$total_a
cropdat$p_soybean_share <- cropdat$soybean_a/cropdat$total_a
cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(cropdat$soybean_rrev)

cropdat <- decade_merge(cropdat, 1930, 2000, 20)

# cropdat <- decade_merge(cropdat, 1930, 2000, 20)

# Setup data sets for regression
corndat <- filter(cropdat, !is.na(p_corn_share))
cottondat <- filter(cropdat, !is.na(p_cotton_share))
haydat <- filter(cropdat, !is.na(p_hay_share))
wheatdat <- filter(cropdat, !is.na(p_wheat_share))
soybeandat <- filter(cropdat, !is.na(p_soybean_share))

# Corn
diff.corn.mod1 <- felm(p_corn_share ~ tavg + I(tavg^2) + prec + I(prec^2) | state + year | 0 | state, 
                   data = corndat, weights = corndat$total_a)
summary(diff.corn.mod1)

diff.corn.mod2 <- felm(p_corn_share ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = corndat, weights = corndat$total_a)
summary(diff.corn.mod2)

# Cotton
diff.cotton.mod1 <- felm(p_cotton_share ~ tavg + I(tavg^2) + prec + I(prec^2) | state + year | 0 | state, 
                   data = cottondat, weights = cottondat$total_a)
summary(diff.cotton.mod1)

diff.cotton.mod2 <- felm(p_cotton_share ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | state + year | 0 | state, 
                   data = cottondat, weights = cottondat$total_a)
summary(diff.cotton.mod2)

# Hay
diff.hay.mod1 <- felm(p_hay_share ~ tavg + I(tavg^2) + prec + I(prec^2) | state + year | 0 | state, 
                   data = haydat, weights = haydat$total_a)
summary(diff.hay.mod1)

diff.hay.mod2 <- felm(p_hay_share ~dday0_10 + dday10_30 + dday30C + prec + prec_sq | state + year | 0 | state, 
                   data = haydat, weights = haydat$total_a)
summary(diff.hay.mod2)

# Wheat
diff.wheat.mod1 <- felm(p_wheat_share ~ tavg + I(tavg^2) + prec + I(prec^2) | state + year | 0 | state, 
                   data = wheatdat, weights = wheatdat$total_a)
summary(diff.wheat.mod1)

diff.wheat.mod2 <- felm(p_wheat_share ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | state + year | 0 | state, 
                   data = wheatdat, weights = wheatdat$total_a)
summary(diff.wheat.mod2)

# Soybean
diff.soybean.mod1 <- felm(p_soybean_share ~ tavg + I(tavg^2) + prec + I(prec^2) | state + year | 0 | state, 
                   data = soybeandat, weights = soybeandat$total_a)
summary(diff.soybean.mod1)

diff.soybean.mod2 <- felm(p_soybean_share ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | state + year | 0 | state, 
                   data = soybeandat, weights = soybeandat$total_a)
summary(diff.soybean.mod2)


saveRDS(diff.corn.mod1, "models/diff.temp.p_corn_share")
saveRDS(diff.corn.mod2, "models/diff.dd.p_corn_share")
saveRDS(diff.cotton.mod1, "models/diff.temp.p_cotton_share")
saveRDS(diff.cotton.mod2, "models/diff.dd.p_cotton_share")
saveRDS(diff.hay.mod1, "models/diff.temp.p_hay_share")
saveRDS(diff.hay.mod2, "models/diff.dd.p_hay_share")
saveRDS(diff.wheat.mod1, "models/diff.temp.p_wheat_share")
saveRDS(diff.wheat.mod2, "models/diff.dd.p_wheat_share")
saveRDS(diff.soybean.mod1, "models/diff.temp.p_soybean_share")
saveRDS(diff.soybean.mod2, "models/diff.dd.p_soybean_share")










#################################
#################################
# cropdat1950 <- filter(cropdat, year >= 1950 & year < 1960)
# cropdat1960 <- filter(cropdat, year >= 1960 & year < 1970)
# cropdat1970 <- filter(cropdat, year >= 1970 & year < 1980)
# cropdat1980 <- filter(cropdat, year >= 1980 & year < 1990)
# cropdat1990 <- filter(cropdat, year >= 1990 & year < 2000)
# cropdat2000 <- filter(cropdat, year >= 2000 & year < 2010)
# 
# 
# 
# cropdat1950 <- cropdat1950 %>% 
#   group_by(year) %>% 
#   mutate(p_corn_share_1950 = p_corn_share - mean(p_corn_share, na.rm = TRUE),
#          ln_corn_rrev_1950 = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
#          p_cotton_share_1950 = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
#          ln_cotton_rrev_1950 = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
#          p_hay_share_1950 = p_hay_share - mean(p_hay_share, na.rm = TRUE),
#          ln_hay_rrev_1950 = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
#          p_wheat_share_1950 = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
#          ln_wheat_rrev_1950 = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
#          p_soybean_share_1950 = p_soybean_share - mean(p_soybean_share, na.rm = TRUE),
#          ln_soybean_rrev_1950 = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE)) %>% 
#   group_by(state, fips) %>% 
#   summarise(p_corn_share_1950 = mean(p_corn_share, na.rm = TRUE),
#             ln_corn_rrev_1950 = mean(ln_corn_rrev_1950, na.rm = TRUE),
#             corn_grain_a_1950 = mean(corn_grain_a, na.rm = TRUE),
#             p_cotton_share_1950 = mean(p_cotton_share, na.rm = TRUE),
#             ln_cotton_rrev_1950 = mean(ln_cotton_rrev_1950, na.rm = TRUE),
#             cotton_a_1950 = mean(cotton_a, na.rm = TRUE),
#             p_hay_share_1950 = mean(p_hay_share, na.rm = TRUE),
#             ln_hay_rrev_1950 = mean(ln_hay_rrev_1950, na.rm = TRUE),
#             hay_a_1950 = mean(hay_a, na.rm = TRUE),
#             p_wheat_share_1950 = mean(p_wheat_share, na.rm = TRUE),
#             ln_wheat_rrev_1950 = mean(ln_wheat_rrev_1950, na.rm = TRUE),
#             wheat_a_1950 = mean(wheat_a, na.rm = TRUE),
#             p_soybean_share_1950 = mean(p_soybean_share, na.rm = TRUE),
#             ln_soybean_rrev_1950 = mean(ln_soybean_rrev_1950, na.rm = TRUE),
#             soybean_a_1950 = mean(soybean_a, na.rm = TRUE),
#             tavg_1950 = mean(tavg, na.rm = TRUE),
#             dday10_1950 = mean(dday10C, na.rm = TRUE),
#             dday30_1950 = mean(dday30C, na.rm = TRUE),
#             prec_1950 = mean(prec, na.rm = TRUE),
#             total_a_1950 = mean(total_a, na.rm = TRUE))
# 
# cropdat1960 <- cropdat1960 %>% 
#   group_by(year) %>% 
#   mutate(p_corn_share_1960 = p_corn_share - mean(p_corn_share, na.rm = TRUE),
#          ln_corn_rrev_1960 = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
#          p_cotton_share_1960 = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
#          ln_cotton_rrev_1960 = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
#          p_hay_share_1960 = p_hay_share - mean(p_hay_share, na.rm = TRUE),
#          ln_hay_rrev_1960 = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
#          p_wheat_share_1960 = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
#          ln_wheat_rrev_1960 = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
#          p_soybean_share_1960 = p_soybean_share - mean(p_soybean_share, na.rm = TRUE),
#          ln_soybean_rrev_1960 = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE)) %>% 
#   group_by(state, fips) %>% 
#   summarise(p_corn_share_1960 = mean(p_corn_share, na.rm = TRUE),
#             ln_corn_rrev_1960 = mean(ln_corn_rrev_1960, na.rm = TRUE),
#             corn_grain_a_1960 = mean(corn_grain_a, na.rm = TRUE),
#             p_cotton_share_1960 = mean(p_cotton_share, na.rm = TRUE),
#             ln_cotton_rrev_1960 = mean(ln_cotton_rrev_1960, na.rm = TRUE),
#             cotton_a_1960 = mean(cotton_a, na.rm = TRUE),
#             p_hay_share_1960 = mean(p_hay_share, na.rm = TRUE),
#             ln_hay_rrev_1960 = mean(ln_hay_rrev_1960, na.rm = TRUE),
#             hay_a_1960 = mean(hay_a, na.rm = TRUE),
#             p_wheat_share_1960 = mean(p_wheat_share, na.rm = TRUE),
#             ln_wheat_rrev_1960 = mean(ln_wheat_rrev_1960, na.rm = TRUE),
#             wheat_a_1960 = mean(wheat_a, na.rm = TRUE),
#             p_soybean_share_1960 = mean(p_soybean_share, na.rm = TRUE),
#             ln_soybean_rrev_1960 = mean(ln_soybean_rrev_1960, na.rm = TRUE),
#             soybean_a_1960 = mean(soybean_a, na.rm = TRUE),
#             tavg_1960 = mean(tavg, na.rm = TRUE),
#             dday10_1960 = mean(dday10C, na.rm = TRUE),
#             dday30_1960 = mean(dday30C, na.rm = TRUE),
#             prec_1960 = mean(prec, na.rm = TRUE),
#             total_a_1960 = mean(total_a, na.rm = TRUE))
# 
# cropdat1970 <- cropdat1970 %>% 
#   group_by(year) %>% 
#   mutate(p_corn_share_1970 = p_corn_share - mean(p_corn_share, na.rm = TRUE),
#          ln_corn_rrev_1970 = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
#          p_cotton_share_1970 = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
#          ln_cotton_rrev_1970 = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
#          p_hay_share_1970 = p_hay_share - mean(p_hay_share, na.rm = TRUE),
#          ln_hay_rrev_1970 = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
#          p_wheat_share_1970 = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
#          ln_wheat_rrev_1970 = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
#          p_soybean_share_1970 = p_soybean_share - mean(p_soybean_share, na.rm = TRUE),
#          ln_soybean_rrev_1970 = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE)) %>% 
#   group_by(state, fips) %>% 
#   summarise(p_corn_share_1970 = mean(p_corn_share, na.rm = TRUE),
#             ln_corn_rrev_1970 = mean(ln_corn_rrev_1970, na.rm = TRUE),
#             corn_grain_a_1970 = mean(corn_grain_a, na.rm = TRUE),
#             p_cotton_share_1970 = mean(p_cotton_share, na.rm = TRUE),
#             ln_cotton_rrev_1970 = mean(ln_cotton_rrev_1970, na.rm = TRUE),
#             cotton_a_1970 = mean(cotton_a, na.rm = TRUE),
#             p_hay_share_1970 = mean(p_hay_share, na.rm = TRUE),
#             ln_hay_rrev_1970 = mean(ln_hay_rrev_1970, na.rm = TRUE),
#             hay_a_1970 = mean(hay_a, na.rm = TRUE),
#             p_wheat_share_1970 = mean(p_wheat_share, na.rm = TRUE),
#             ln_wheat_rrev_1970 = mean(ln_wheat_rrev_1970, na.rm = TRUE),
#             wheat_a_1970 = mean(wheat_a, na.rm = TRUE),
#             p_soybean_share_1970 = mean(p_soybean_share, na.rm = TRUE),
#             ln_soybean_rrev_1970 = mean(ln_soybean_rrev_1970, na.rm = TRUE),
#             soybean_a_1970 = mean(soybean_a, na.rm = TRUE),
#             tavg_1970 = mean(tavg, na.rm = TRUE),
#             dday10_1970 = mean(dday10C, na.rm = TRUE),
#             dday30_1970 = mean(dday30C, na.rm = TRUE),
#             prec_1970 = mean(prec, na.rm = TRUE),
#             total_a_1970 = mean(total_a, na.rm = TRUE))
# 
# cropdat1980 <- cropdat1980 %>% 
#   group_by(year) %>% 
#   mutate(p_corn_share_1980 = p_corn_share - mean(p_corn_share, na.rm = TRUE),
#          ln_corn_rrev_1980 = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
#          p_cotton_share_1980 = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
#          ln_cotton_rrev_1980 = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
#          p_hay_share_1980 = p_hay_share - mean(p_hay_share, na.rm = TRUE),
#          ln_hay_rrev_1980 = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
#          p_wheat_share_1980 = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
#          ln_wheat_rrev_1980 = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
#          p_soybean_share_1980 = p_soybean_share - mean(p_soybean_share, na.rm = TRUE),
#          ln_soybean_rrev_1980 = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE)) %>% 
#   group_by(state, fips) %>% 
#   summarise(p_corn_share_1980 = mean(p_corn_share, na.rm = TRUE),
#             ln_corn_rrev_1980 = mean(ln_corn_rrev_1980, na.rm = TRUE),
#             corn_grain_a_1980 = mean(corn_grain_a, na.rm = TRUE),
#             p_cotton_share_1980 = mean(p_cotton_share, na.rm = TRUE),
#             ln_cotton_rrev_1980 = mean(ln_cotton_rrev_1980, na.rm = TRUE),
#             cotton_a_1980 = mean(cotton_a, na.rm = TRUE),
#             p_hay_share_1980 = mean(p_hay_share, na.rm = TRUE),
#             ln_hay_rrev_1980 = mean(ln_hay_rrev_1980, na.rm = TRUE),
#             hay_a_1980 = mean(hay_a, na.rm = TRUE),
#             p_wheat_share_1980 = mean(p_wheat_share, na.rm = TRUE),
#             ln_wheat_rrev_1980 = mean(ln_wheat_rrev_1980, na.rm = TRUE),
#             wheat_a_1980 = mean(wheat_a, na.rm = TRUE),
#             p_soybean_share_1980 = mean(p_soybean_share, na.rm = TRUE),
#             ln_soybean_rrev_1980 = mean(ln_soybean_rrev_1980, na.rm = TRUE),
#             soybean_a_1980 = mean(soybean_a, na.rm = TRUE),
#             tavg_1980 = mean(tavg, na.rm = TRUE),
#             dday10_1980 = mean(dday10C, na.rm = TRUE),
#             dday30_1980 = mean(dday30C, na.rm = TRUE),
#             prec_1980 = mean(prec, na.rm = TRUE),
#             total_a_1980 = mean(total_a, na.rm = TRUE))
# 
# cropdat1990 <- cropdat1990 %>% 
#   group_by(year) %>% 
#   mutate(p_corn_share_1990 = p_corn_share - mean(p_corn_share, na.rm = TRUE),
#          ln_corn_rrev_1990 = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
#          p_cotton_share_1990 = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
#          ln_cotton_rrev_1990 = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
#          p_hay_share_1990 = p_hay_share - mean(p_hay_share, na.rm = TRUE),
#          ln_hay_rrev_1990 = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
#          p_wheat_share_1990 = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
#          ln_wheat_rrev_1990 = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
#          p_soybean_share_1990 = p_soybean_share - mean(p_soybean_share, na.rm = TRUE),
#          ln_soybean_rrev_1990 = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE)) %>% 
#   group_by(state, fips) %>% 
#   summarise(p_corn_share_1990 = mean(p_corn_share, na.rm = TRUE),
#             ln_corn_rrev_1990 = mean(ln_corn_rrev_1990, na.rm = TRUE),
#             corn_grain_a_1990 = mean(corn_grain_a, na.rm = TRUE),
#             p_cotton_share_1990 = mean(p_cotton_share, na.rm = TRUE),
#             ln_cotton_rrev_1990 = mean(ln_cotton_rrev_1990, na.rm = TRUE),
#             cotton_a_1990 = mean(cotton_a, na.rm = TRUE),
#             p_hay_share_1990 = mean(p_hay_share, na.rm = TRUE),
#             ln_hay_rrev_1990 = mean(ln_hay_rrev_1990, na.rm = TRUE),
#             hay_a_1990 = mean(hay_a, na.rm = TRUE),
#             p_wheat_share_1990 = mean(p_wheat_share, na.rm = TRUE),
#             ln_wheat_rrev_1990 = mean(ln_wheat_rrev_1990, na.rm = TRUE),
#             wheat_a_1990 = mean(wheat_a, na.rm = TRUE),
#             p_soybean_share_1990 = mean(p_soybean_share, na.rm = TRUE),
#             ln_soybean_rrev_1990 = mean(ln_soybean_rrev_1990, na.rm = TRUE),
#             soybean_a_1990 = mean(soybean_a, na.rm = TRUE),
#             tavg_1990 = mean(tavg, na.rm = TRUE),
#             dday10_1990 = mean(dday10C, na.rm = TRUE),
#             dday30_1990 = mean(dday30C, na.rm = TRUE),
#             prec_1990 = mean(prec, na.rm = TRUE),
#             total_a_1990 = mean(total_a, na.rm = TRUE))
# 
# cropdat2000 <- cropdat2000 %>% 
#   group_by(year) %>% 
#   mutate(p_corn_share_2000 = p_corn_share - mean(p_corn_share, na.rm = TRUE),
#          ln_corn_rrev_2000 = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
#          p_cotton_share_2000 = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
#          ln_cotton_rrev_2000 = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
#          p_hay_share_2000 = p_hay_share - mean(p_hay_share, na.rm = TRUE),
#          ln_hay_rrev_2000 = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
#          p_wheat_share_2000 = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
#          ln_wheat_rrev_2000 = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
#          p_soybean_share_2000 = p_soybean_share - mean(p_soybean_share, na.rm = TRUE),
#          ln_soybean_rrev_2000 = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE)) %>% 
#   group_by(state, fips) %>% 
#   summarise(p_corn_share_2000 = mean(p_corn_share, na.rm = TRUE),
#             ln_corn_rrev_2000 = mean(ln_corn_rrev_2000, na.rm = TRUE),
#             corn_grain_a_2000 = mean(corn_grain_a, na.rm = TRUE),
#             p_cotton_share_2000 = mean(p_cotton_share, na.rm = TRUE),
#             ln_cotton_rrev_2000 = mean(ln_cotton_rrev_2000, na.rm = TRUE),
#             cotton_a_2000 = mean(cotton_a, na.rm = TRUE),
#             p_hay_share_2000 = mean(p_hay_share, na.rm = TRUE),
#             ln_hay_rrev_2000 = mean(ln_hay_rrev_2000, na.rm = TRUE),
#             hay_a_2000 = mean(hay_a, na.rm = TRUE),
#             p_wheat_share_2000 = mean(p_wheat_share, na.rm = TRUE),
#             ln_wheat_rrev_2000 = mean(ln_wheat_rrev_2000, na.rm = TRUE),
#             wheat_a_2000 = mean(wheat_a, na.rm = TRUE),
#             p_soybean_share_2000 = mean(p_soybean_share, na.rm = TRUE),
#             ln_soybean_rrev_2000 = mean(ln_soybean_rrev_2000, na.rm = TRUE),
#             soybean_a_2000 = mean(soybean_a, na.rm = TRUE),
#             tavg_2000 = mean(tavg, na.rm = TRUE),
#             dday10_2000 = mean(dday10C, na.rm = TRUE),
#             dday30_2000 = mean(dday30C, na.rm = TRUE),
#             prec_2000 = mean(prec, na.rm = TRUE),
#             total_a_2000 = mean(total_a, na.rm = TRUE))
# 
# # 20-year intervals (1980-1960 & 2000 - 1980)
# 
# # 1960 - 1980
# dat1960 <- left_join(cropdat1960, cropdat1980, by = c("state", "fips"))
# 
# dat1960$dday10 <- (dat1960$dday10_1980 + dat1960$dday10_1960) / 2
# dat1960$dday30 <- (dat1960$dday30_1980 + dat1960$dday30_1960) / 2
# dat1960$dday10_30 <- dat1960$dday10 - dat1960$dday30
# dat1960$tavg <- (dat1960$tavg_1980 + dat1960$tavg_1960) / 2
# dat1960$prec <- (dat1960$prec_1980 + dat1960$prec_1960) / 2
# 
# # dat1960$dday10_30 <- (dat1960$dday10_1980 - dat1960$dday30_1980) - (dat1960$dday10_1960 - dat1960$dday30_1960)
# # dat1960$dday30 <- dat1960$dday30_1980 - dat1960$dday30_1960
# # dat1960$tavg <- dat1960$tavg_1980 - dat1960$tavg_1960
# # dat1960$prec <- dat1960$prec_1980 - dat1960$prec_1960
# 
# 
#   # Acre weights 1960
#   dat1960$corn_grain_a <- dat1960$corn_grain_a_1960
#   dat1960$cotton_a <- dat1960$cotton_a_1960
#   dat1960$hay_a <- dat1960$hay_a_1960
#   dat1960$wheat_a <- dat1960$wheat_a_1960
#   dat1960$soybean_a <- dat1960$soybean_a_1960
#   dat1960$total_a <- dat1960$total_a_1960
# 
#   # Depedent variables
#   dat1960$ln_corn_rrev <- dat1960$ln_corn_rrev_1980 - dat1960$ln_corn_rrev_1960
#   dat1960$p_corn_share <- dat1960$p_corn_share_1980 - dat1960$p_corn_share_1960
#   dat1960$ln_cotton_rrev <- dat1960$ln_cotton_rrev_1980 - dat1960$ln_cotton_rrev_1960
#   dat1960$p_cotton_share <- dat1960$p_cotton_share_1980 - dat1960$p_cotton_share_1960
#   dat1960$ln_hay_rrev <- dat1960$ln_hay_rrev_1980 - dat1960$ln_hay_rrev_1960
#   dat1960$p_hay_share <- dat1960$p_hay_share_1980 - dat1960$p_hay_share_1960
#   dat1960$ln_wheat_rrev <- dat1960$ln_wheat_rrev_1980 - dat1960$ln_wheat_rrev_1960
#   dat1960$p_wheat_share <- dat1960$p_wheat_share_1980 - dat1960$p_wheat_share_1960
#   dat1960$ln_soybean_rrev <- dat1960$ln_soybean_rrev_1980 - dat1960$ln_soybean_rrev_1960
#   dat1960$p_soybean_share <- dat1960$p_soybean_share_1980 - dat1960$p_soybean_share_1960
# dat1960$year <- 1960
# 
# # 1980 - 2000
# dat1980 <- left_join(cropdat1980, cropdat2000, by = c("state", "fips"))
# 
# dat1980$dday10 <- (dat1980$dday10_1980 + dat1980$dday10_2000) / 2
# dat1980$dday30 <- (dat1980$dday30_1980 + dat1980$dday30_2000) / 2
# dat1980$dday10_30 <- dat1980$dday10 - dat1980$dday30
# dat1980$tavg <- (dat1980$tavg_1980 + dat1980$tavg_2000) / 2
# dat1980$prec <- (dat1980$prec_1980 + dat1980$prec_2000) / 2
# 
# # dat1980$dday10_30 <- (dat1980$dday10_2000 - dat1980$dday30_2000) - (dat1980$dday10_1980 - dat1980$dday30_1980)
# # dat1980$dday30 <- dat1980$dday30_2000 - dat1980$dday30_1980
# # dat1980$tavg <- dat1980$tavg_2000 - dat1980$tavg_1980
# # dat1980$prec <- dat1980$prec_2000 - dat1980$prec_1980
# 
#   # Acre weights 1980
#   dat1980$corn_grain_a <- dat1980$corn_grain_a_1980
#   dat1980$cotton_a <- dat1980$cotton_a_1980
#   dat1980$hay_a <- dat1980$hay_a_1980
#   dat1980$wheat_a <- dat1980$wheat_a_1980
#   dat1980$soybean_a <- dat1980$soybean_a_1980
#   dat1980$total_a <- dat1960$total_a_1980
#   
#   # Dependent variables
#   dat1980$ln_corn_rrev <- dat1980$ln_corn_rrev_2000 - dat1980$ln_corn_rrev_1980
#   dat1980$p_corn_share <- dat1980$p_corn_share_2000 - dat1980$p_corn_share_1980
#   dat1980$ln_cotton_rrev <- dat1980$ln_cotton_rrev_2000 - dat1980$ln_cotton_rrev_1980
#   dat1980$p_cotton_share <- dat1980$p_cotton_share_2000 - dat1980$p_cotton_share_1980
#   dat1980$ln_hay_rrev <- dat1980$ln_hay_rrev_2000 - dat1980$ln_hay_rrev_1980
#   dat1980$p_hay_share <- dat1980$p_hay_share_2000 - dat1980$p_hay_share_1980
#   dat1980$ln_wheat_rrev <- dat1980$ln_wheat_rrev_2000 - dat1980$ln_wheat_rrev_1980
#   dat1980$p_wheat_share <- dat1980$p_wheat_share_2000 - dat1980$p_wheat_share_1980
#   dat1980$ln_soybean_rrev <- dat1980$ln_soybean_rrev_2000 - dat1980$ln_soybean_rrev_1980
#   dat1980$p_soybean_share <- dat1980$p_soybean_share_2000 - dat1980$p_soybean_share_1980
# dat1980$year <- 1980
# 
# 
# # Merge data
# newcropdat <- rbind(dat1960, dat1980)
# newcropdat$prec_sq <- newcropdat$prec^2
# 
# # Setup data sets for regression
# corndat <- filter(newcropdat, !is.na(ln_corn_rrev))
# cottondat <- filter(newcropdat, !is.na(ln_cotton_rrev))
# haydat <- filter(newcropdat, !is.na(ln_hay_rrev))
# wheatdat <- filter(newcropdat, !is.na(ln_wheat_rrev))
# soybeandat <- filter(newcropdat, !is.na(ln_soybean_rrev))
# 
# 
# # Log Revenue
# 
# # Corn
# diff.corn.mod1 <- felm(ln_corn_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
#                    data = corndat, weights = corndat$corn_grain_a)
# summary(diff.corn.mod1)
# 
# diff.corn.mod2 <- felm(ln_corn_rrev ~ dday10_30 + dday30 + prec + prec_sq | fips + year | 0 | state, 
#                    data = corndat, weights = corndat$corn_grain_a)
# summary(diff.corn.mod2)
# 
# # Cotton
# diff.cotton.mod1 <- felm(ln_cotton_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
#                    data = cottondat, weights = cottondat$cotton_a)
# summary(diff.cotton.mod1)
# 
# diff.cotton.mod2 <- felm(ln_cotton_rrev ~ dday10_30 + dday30 + prec + prec_sq | fips + year | 0 | state, 
#                    data = cottondat, weights = cottondat$cotton_a)
# summary(diff.cotton.mod2)
# 
# # Hay
# diff.hay.mod1 <- felm(ln_hay_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
#                    data = haydat, weights = haydat$hay_a)
# summary(diff.hay.mod1)
# 
# diff.hay.mod2 <- felm(ln_hay_rrev ~ dday10_30 + dday30 + prec + prec_sq | fips + year | 0 | state, 
#                    data = haydat, weights = haydat$hay_a)
# summary(diff.hay.mod2)
# 
# # Wheat
# diff.wheat.mod1 <- felm(ln_wheat_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
#                    data = wheatdat, weights = wheatdat$wheat_a)
# summary(diff.wheat.mod1)
# 
# diff.wheat.mod2 <- felm(ln_wheat_rrev ~ dday10_30 + dday30 + prec + prec_sq | fips + year | 0 | state, 
#                    data = wheatdat, weights = wheatdat$wheat_a)
# summary(diff.wheat.mod2)
# 
# # Soybean
# diff.soybean.mod1 <- felm(ln_soybean_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
#                    data = soybeandat, weights = soybeandat$soybean_a)
# summary(diff.soybean.mod1)
# 
# diff.soybean.mod2 <- felm(ln_soybean_rrev ~ dday10_30 + dday30 + prec + prec_sq | fips + year | 0 | state, 
#                    data = soybeandat, weights = soybeandat$soybean_a)
# summary(diff.soybean.mod2)
# 
# # Save regression data
# saveRDS(diff.corn.mod1, "models/diff.temp.ln_corn_rrev")
# saveRDS(diff.corn.mod2, "models/diff.dd.ln_corn_rrev")
# saveRDS(diff.cotton.mod1, "models/diff.temp.ln_cotton_rrev")
# saveRDS(diff.cotton.mod2, "models/diff.dd.ln_cotton_rrev")
# saveRDS(diff.hay.mod1, "models/diff.temp.ln_hay_rrev")
# saveRDS(diff.hay.mod2, "models/diff.dd.ln_hay_rrev")
# saveRDS(diff.wheat.mod1, "models/diff.temp.ln_wheat_rrev")
# saveRDS(diff.wheat.mod2, "models/diff.dd.ln_wheat_rrev")
# saveRDS(diff.soybean.mod1, "models/diff.temp.ln_soybean_rrev")
# saveRDS(diff.soybean.mod2, "models/diff.dd.ln_soybean_rrev")
# 
# 
# # Proportion of acreage regressions
# cropdat <- filter(newcropdat, corn_grain_a != 0 & cotton_a != 0 & hay_a != 0 & wheat_a != 0 & soybean_a != 0)
# 
# corndat <- filter(newcropdat, !is.na(ln_corn_rrev))
# cottondat <- filter(newcropdat, !is.na(ln_cotton_rrev))
# haydat <- filter(newcropdat, !is.na(ln_hay_rrev))
# wheatdat <- filter(newcropdat, !is.na(ln_wheat_rrev))
# soybeandat <- filter(newcropdat, !is.na(ln_soybean_rrev))
# 
# # Corn
# diff.corn.mod1 <- felm(p_corn_share ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
#                    data = corndat, weights = corndat$total_a)
# summary(diff.corn.mod1)
# 
# diff.corn.mod2 <- felm(p_corn_share ~ dday10_30 + dday30 + prec + prec_sq | fips + year | 0 | state, 
#                    data = corndat, weights = corndat$total_a)
# summary(diff.corn.mod2)
# 
# # Cotton
# diff.cotton.mod1 <- felm(p_cotton_share ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
#                    data = cottondat, weights = cottondat$total_a)
# summary(diff.cotton.mod1)
# 
# diff.cotton.mod2 <- felm(p_cotton_share ~ dday10_30 + dday30 + prec + prec_sq | fips + year | 0 | state, 
#                    data = cottondat, weights = cottondat$total_a)
# summary(diff.cotton.mod2)
# 
# # Hay
# diff.hay.mod1 <- felm(p_hay_share ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
#                    data = haydat, weights = haydat$total_a)
# summary(diff.hay.mod1)
# 
# diff.hay.mod2 <- felm(p_hay_share ~ dday10_30 + dday30 + prec + prec_sq | fips + year | 0 | state, 
#                    data = haydat, weights = haydat$total_a)
# summary(diff.hay.mod2)
# 
# # Wheat
# diff.wheat.mod1 <- felm(p_wheat_share ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
#                    data = wheatdat, weights = wheatdat$total_a)
# summary(diff.wheat.mod1)
# 
# diff.wheat.mod2 <- felm(p_wheat_share ~ dday10_30 + dday30 + prec + prec_sq | fips + year | 0 | state, 
#                    data = wheatdat, weights = wheatdat$total_a)
# summary(diff.wheat.mod2)
# 
# # Soybean
# diff.soybean.mod1 <- felm(p_soybean_share ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
#                    data = soybeandat, weights = soybeandat$total_a)
# summary(diff.soybean.mod1)
# 
# diff.soybean.mod2 <- felm(p_soybean_share ~ dday10_30 + dday30 + prec + prec_sq | fips + year | 0 | state, 
#                    data = soybeandat, weights = soybeandat$total_a)
# summary(diff.soybean.mod2)
# 
# 
# saveRDS(diff.corn.mod1, "models/diff.temp.p_corn_share")
# saveRDS(diff.corn.mod2, "models/diff.dd.p_corn_share")
# saveRDS(diff.cotton.mod1, "models/diff.temp.p_cotton_share")
# saveRDS(diff.cotton.mod2, "models/diff.dd.p_cotton_share")
# saveRDS(diff.hay.mod1, "models/diff.temp.p_hay_share")
# saveRDS(diff.hay.mod2, "models/diff.dd.p_hay_share")
# saveRDS(diff.wheat.mod1, "models/diff.temp.p_wheat_share")
# saveRDS(diff.wheat.mod2, "models/diff.dd.p_wheat_share")
# saveRDS(diff.soybean.mod1, "models/diff.temp.p_soybean_share")
# saveRDS(diff.soybean.mod2, "models/diff.dd.p_soybean_share")
# 
# 
