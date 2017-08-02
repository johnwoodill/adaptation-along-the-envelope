library(tidyverse)
library(plm)

# 10-year interval 1960's and 2000's --------------------------------------
setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)

cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(cropdat$soybean_rrev)

cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)

cropdat$p_corn_share <- cropdat$corn_grain_a/cropdat$total_a
cropdat$p_cotton_share <- cropdat$cotton_a/cropdat$total_a
cropdat$p_hay_share <- cropdat$hay_a/cropdat$total_a
cropdat$p_wheat_share <- cropdat$wheat_a/cropdat$total_a
cropdat$p_soybean_share <- cropdat$soybean_a/cropdat$total_a

cropdat$p_corn_share <- ifelse(is.na(cropdat$p_corn_share), 0, cropdat$p_corn_share)
cropdat$p_cotton_share <- ifelse(is.na(cropdat$p_cotton_share), 0, cropdat$p_cotton_share)
cropdat$p_hay_share <- ifelse(is.na(cropdat$p_hay_share), 0, cropdat$p_hay_share)
cropdat$p_wheat_share <- ifelse(is.na(cropdat$p_wheat_share), 0, cropdat$p_wheat_share)
cropdat$p_soybean_share <- ifelse(is.na(cropdat$p_soybean_share), 0, cropdat$p_soybean_share)

decade_merge <- function(dat, begd, endd, int){
  mergdat <- data.frame()
  decades <- seq(begd, endd, int)
  for (i in decades){
    int.dat <- filter(dat, year >= i & year < (i + int))
    int.dat <- int.dat %>%
    mutate(ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
         ln_cotton_rrev = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
         ln_hay_rrev = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
         ln_wheat_rrev = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
         ln_soybean_rrev = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE),
         p_corn_share = p_corn_share - mean(p_corn_share, na.rm = TRUE),
         p_cotton_share = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
         p_hay_share = p_hay_share - mean(p_hay_share, na.rm = TRUE),
         p_wheat_share = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
         p_soybean_share = p_soybean_share - mean(p_soybean_share, na.rm = TRUE)) %>% 
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
            prec = mean(prec, na.rm = TRUE),
            total_a = mean(total_a, na.rm = TRUE),
            corn_w = mean(corn_grain_a, na.rm = TRUE),
            cotton_w = mean(cotton_a, na.rm = TRUE),
            hay_w = mean(hay_a, na.rm = TRUE),
            wheat_w = mean(wheat_a, na.rm = TRUE),
            soybean_w = mean(soybean_a, na.rm = TRUE)) %>% 
      ungroup()
    int.dat$year <- i
    mergdat <- rbind(mergdat, int.dat)
  }
  return(mergdat)
}


decadedat <- decade_merge(cropdat, 1950, 2000, 20)

decadedat$dday0_10 <- decadedat$dday0C - decadedat$dday10C
decadedat$dday10_30 <- decadedat$dday10C - decadedat$dday30C
decadedat$prec <- decadedat$prec
decadedat$prec_sq <- decadedat$prec^2

# Exposure weighted values equal zero
decadedat$dday0_10 <- decadedat$dday0_10 - mean(decadedat$dday0_10, na.rm = TRUE)
decadedat$dday10_30 <- decadedat$dday10_30 - mean(decadedat$dday10_30, na.rm = TRUE)
decadedat$dday30C <- decadedat$dday30C - mean(decadedat$dday30C, na.rm = TRUE)
decadedat$prec <- decadedat$prec - mean(decadedat$prec, na.rm = TRUE)
decadedat$prec_sq <- decadedat$prec^2

decadedat$ln_corn_rrev <- decadedat$ln_corn_rrev - mean(decadedat$ln_corn_rrev, na.rm = TRUE)
decadedat$ln_cotton_rrev <- decadedat$ln_cotton_rrev - mean(decadedat$ln_cotton_rrev, na.rm = TRUE)
decadedat$ln_hay_rrev <- decadedat$ln_hay_rrev - mean(decadedat$ln_hay_rrev, na.rm = TRUE)
decadedat$ln_wheat_rrev <- decadedat$ln_wheat_rrev - mean(decadedat$ln_wheat_rrev, na.rm = TRUE)
decadedat$ln_soybean_rrev <- decadedat$ln_soybean_rrev - mean(decadedat$ln_soybean_rrev, na.rm = TRUE)

decadedat$p_corn_share <- decadedat$p_corn_share - mean(decadedat$p_corn_share)
decadedat$p_cotton_share <- decadedat$p_cotton_share - mean(decadedat$p_cotton_share)
decadedat$p_hay_share <- decadedat$p_hay_share - mean(decadedat$p_hay_share)
decadedat$p_wheat_share <- decadedat$p_wheat_share - mean(decadedat$p_wheat_share)
decadedat$p_soybean_share <- decadedat$p_soybean_share - mean(decadedat$p_soybean_share)


# Setup data sets for regression
corndat <- filter(decadedat, !is.na(ln_corn_rrev))
cottondat <- filter(decadedat, !is.na(ln_cotton_rrev))
haydat <- filter(decadedat, !is.na(ln_hay_rrev))
wheatdat <- filter(decadedat, !is.na(ln_wheat_rrev))
soybeandat <- filter(decadedat, !is.na(ln_soybean_rrev))

# Log Revenue

# Corn
diff.corn.mod1 <- felm(ln_corn_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
                   data = corndat, weights = corndat$corn_w)
summary(diff.corn.mod1)

diff.corn.mod2 <- felm(ln_corn_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = corndat, weights = corndat$corn_w)
summary(diff.corn.mod2)

# Cotton
diff.cotton.mod1 <- felm(ln_cotton_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
                   data = cottondat, weights = cottondat$cotton_w)
summary(diff.cotton.mod1)

diff.cotton.mod2 <- felm(ln_cotton_rrev ~ dday0_10 +dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = cottondat, weights = cottondat$cotton_w)
summary(diff.cotton.mod2)

# Hay
diff.hay.mod1 <- felm(ln_hay_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
                   data = haydat, weights = haydat$hay_w)
summary(diff.hay.mod1)

diff.hay.mod2 <- felm(ln_hay_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = haydat, weights = haydat$hay_w)
summary(diff.hay.mod2)

# Wheat
diff.wheat.mod1 <- felm(ln_wheat_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
                   data = wheatdat, weights = wheatdat$wheat_w)
summary(diff.wheat.mod1)

diff.wheat.mod2 <- felm(ln_wheat_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = wheatdat, weights = wheatdat$wheat_w)
summary(diff.wheat.mod2)

# Soybean
diff.soybean.mod1 <- felm(ln_soybean_rrev ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
                   data = soybeandat, weights = soybeandat$soybean_w)
summary(diff.soybean.mod1)

diff.soybean.mod2 <- felm(ln_soybean_rrev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = soybeandat, weights = soybeandat$soybean_w)
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

# Proportion of acreage regressions

# Corn
diff.corn.mod1 <- felm(p_corn_share ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
                   data = corndat, weights = corndat$total_a)
summary(diff.corn.mod1)

diff.corn.mod2 <- felm(p_corn_share ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = corndat, weights = corndat$total_a)
summary(diff.corn.mod2)

# Cotton
diff.cotton.mod1 <- felm(p_cotton_share ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
                   data = cottondat, weights = cottondat$total_a)
summary(diff.cotton.mod1)

diff.cotton.mod2 <- felm(p_cotton_share ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = cottondat, weights = cottondat$total_a)
summary(diff.cotton.mod2)

# Hay
diff.hay.mod1 <- felm(p_hay_share ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
                   data = haydat, weights = haydat$total_a)
summary(diff.hay.mod1)

diff.hay.mod2 <- felm(p_hay_share ~dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = haydat, weights = haydat$total_a)
summary(diff.hay.mod2)

# Wheat
diff.wheat.mod1 <- felm(p_wheat_share ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
                   data = wheatdat, weights = wheatdat$total_a)
summary(diff.wheat.mod1)

diff.wheat.mod2 <- felm(p_wheat_share ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
                   data = wheatdat, weights = wheatdat$total_a)
summary(diff.wheat.mod2)

# Soybean
diff.soybean.mod1 <- felm(p_soybean_share ~ tavg + I(tavg^2) + prec + I(prec^2) | fips + year | 0 | state, 
                   data = soybeandat, weights = soybeandat$total_a)
summary(diff.soybean.mod1)

diff.soybean.mod2 <- felm(p_soybean_share ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips + year | 0 | state, 
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

