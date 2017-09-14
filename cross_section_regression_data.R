library(tidyverse)

# Baseline for predictions
# Cross-section: Log(Corn Rev) --------------------------------------------

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

# Crop data
cropdat <- readRDS("data/full_ag_data.rds")

# Soil data
#soil <- read_dta("data/soilData.dta")
#soil <- readRDS("data/soilData.rds")
#soil$fips <- as.numeric(soil$fips)

cropdat$prec_sq <- cropdat$prec^2
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

# Log revenue
cropdat$ln_corn_rrev <- log(1 + cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(1 + cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(1 + cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(1 + cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(1 + cropdat$soybean_rrev)

# cropdat$corn_grain_a <- ifelse(is.na(cropdat$corn_grain_a), 0, cropdat$corn_grain_a)
# cropdat$cotton_a <- ifelse(is.na(cropdat$cotton_a), 0, cropdat$cotton_a)
# cropdat$hay_a <- ifelse(is.na(cropdat$hay_a), 0, cropdat$hay_a)
# cropdat$wheat_a <- ifelse(is.na(cropdat$wheat_a), 0, cropdat$wheat_a)
# cropdat$soybean_a <- ifelse(is.na(cropdat$soybean_a), 0, cropdat$soybean_a)

# Crop weights
cropdat$corn_w <- cropdat$corn_grain_a
cropdat$cotton_w <- cropdat$cotton_a
cropdat$hay_w <- cropdat$hay_a
cropdat$wheat_w <- cropdat$wheat_a
cropdat$soybean_w <- cropdat$soybean_a



# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))

cropdat <- cropdat %>%
  group_by(year) %>%
  mutate(dm_ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
         dm_ln_cotton_rrev = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
         dm_ln_hay_rrev = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
         dm_ln_wheat_rrev = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
         dm_ln_soybean_rrev = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE)) %>% 
  group_by(state, fips) %>%
  summarise(ln_corn_rrev = mean(dm_ln_corn_rrev, na.rm = TRUE),
            ln_cotton_rrev = mean(dm_ln_cotton_rrev, na.rm = TRUE),
            ln_hay_rrev = mean(dm_ln_hay_rrev, na.rm = TRUE),
            ln_wheat_rrev = mean(dm_ln_wheat_rrev, na.rm = TRUE),
            ln_soybean_rrev = mean(dm_ln_soybean_rrev, na.rm = TRUE),
            corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
            cotton_a = mean(cotton_a, na.rm = TRUE),
            hay_a = mean(hay_a, na.rm = TRUE),
            wheat_a = mean(wheat_a, na.rm = TRUE),
            soybean_a = mean(soybean_a, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE),
            prec = mean(prec, na.rm = TRUE),
            dday0C = mean(dday0C, na.rm = TRUE),
            dday10C = mean(dday10C, na.rm = TRUE),
            dday30C = mean(dday30C, na.rm = TRUE),
            corn_w = mean(corn_w, na.rm = TRUE),
            cotton_w = mean(cotton_w, na.rm = TRUE),
            hay_w = mean(hay_w, na.rm = TRUE),
            wheat_w = mean(wheat_w, na.rm = TRUE),
            soybean_w = mean(soybean_w, na.rm = TRUE),
            lat = mean(lat, na.rm = TRUE),
            long = mean(long, na.rm = TRUE)) %>% 
  ungroup() 

cropdat$corn_grain_a <- ifelse(is.na(cropdat$corn_grain_a), 0, cropdat$corn_grain_a)
cropdat$cotton_a <- ifelse(is.na(cropdat$cotton_a), 0, cropdat$cotton_a)
cropdat$hay_a <- ifelse(is.na(cropdat$hay_a), 0, cropdat$hay_a)
cropdat$wheat_a <- ifelse(is.na(cropdat$wheat_a), 0, cropdat$wheat_a)
cropdat$soybean_a <- ifelse(is.na(cropdat$soybean_a), 0, cropdat$soybean_a)

# Total acres
cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)

# Get proportion of crop share
cropdat$p_corn_share <- cropdat$corn_grain_a/cropdat$total_a
cropdat$p_cotton_share <- cropdat$cotton_a/cropdat$total_a
cropdat$p_hay_share <- cropdat$hay_a/cropdat$total_a
cropdat$p_wheat_share <- cropdat$wheat_a/cropdat$total_a
cropdat$p_soybean_share <- cropdat$soybean_a/cropdat$total_a

cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

cropdat$prec_sq <- cropdat$prec^2
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$latlong <- cropdat$lat*cropdat$long
cropdat$`(Intercept)` <- 1

cropdat$ln_corn_rrev <- ifelse(is.na(cropdat$ln_corn_rrev), 0, cropdat$ln_corn_rrev)
cropdat$ln_cotton_rrev <- ifelse(is.na(cropdat$ln_cotton_rrev), 0, cropdat$ln_cotton_rrev)
cropdat$ln_hay_rrev <- ifelse(is.na(cropdat$ln_hay_rrev), 0, cropdat$ln_hay_rrev)
cropdat$ln_wheat_rrev <- ifelse(is.na(cropdat$ln_wheat_rrev), 0, cropdat$ln_wheat_rrev)
cropdat$ln_soybean_rrev <- ifelse(is.na(cropdat$ln_soybean_rrev), 0, cropdat$ln_soybean_rrev)


# cropdat <- left_join(cropdat, soil, by = "fips")

cropdat <- ungroup(cropdat)
cropdat <- as.data.frame(cropdat)

saveRDS(cropdat, "data/cross_section_regression_data.rds")

