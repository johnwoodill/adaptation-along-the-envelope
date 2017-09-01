library(tidyverse)


# Baseline for predictions ------------------------------------------------


cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1930 & year <= 2010)


cropdat$prec_sq <- cropdat$prec^2
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

cropdat$corn_rrev <- ifelse(is.na(cropdat$corn_rrev), 0, cropdat$corn_rrev)
cropdat$cotton_rrev <- ifelse(is.na(cropdat$cotton_rrev), 0, cropdat$cotton_rrev)
cropdat$hay_rrev <- ifelse(is.na(cropdat$hay_rrev), 0, cropdat$hay_rrev)
cropdat$wheat_rrev <- ifelse(is.na(cropdat$wheat_rrev), 0, cropdat$wheat_rrev)
cropdat$soybean_rrev <- ifelse(is.na(cropdat$soybean_rrev), 0, cropdat$soybean_rrev)

# Log revenue
cropdat$ln_corn_rrev <- log(1 + cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(1 + cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(1 + cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(1 + cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(1 + cropdat$soybean_rrev)

# NA = 0 for tobit
cropdat$corn_grain_a <- ifelse(is.na(cropdat$corn_grain_a), 0, cropdat$corn_grain_a)
cropdat$cotton_a <- ifelse(is.na(cropdat$cotton_a), 0, cropdat$cotton_a)
cropdat$hay_a <- ifelse(is.na(cropdat$hay_a), 0, cropdat$hay_a)
cropdat$wheat_a <- ifelse(is.na(cropdat$wheat_a), 0, cropdat$wheat_a)
cropdat$soybean_a <- ifelse(is.na(cropdat$soybean_a), 0, cropdat$soybean_a)

# Crop weights
cropdat$corn_w <- cropdat$corn_grain_a
cropdat$cotton_w <- cropdat$cotton_a
cropdat$hay_w <- cropdat$hay_a
cropdat$wheat_w <- cropdat$wheat_a
cropdat$soybean_w <- cropdat$soybean_a

# Total acres
cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)

# Get proportion of crop share
cropdat$p_corn_share <- cropdat$corn_grain_a/cropdat$total_a
cropdat$p_cotton_share <- cropdat$cotton_a/cropdat$total_a
cropdat$p_hay_share <- cropdat$hay_a/cropdat$total_a
cropdat$p_wheat_share <- cropdat$wheat_a/cropdat$total_a
cropdat$p_soybean_share <- cropdat$soybean_a/cropdat$total_a

# Set weights
cropdat <- cropdat %>% 
  group_by(fips) %>% 
  mutate(corn_w = mean(corn_w, na.rm = TRUE),
         cotton_w = mean(cotton_w, na.rm = TRUE),
         hay_w = mean(hay_w, na.rm = TRUE),
         wheat_w = mean(wheat_w, na.rm = TRUE),
         soybean_w = mean(soybean_w, na.rm = TRUE),
         total_w = mean(total_a, na.rm = TRUE))


cropdat$tavg_sq <- cropdat$tavg^2
cropdat$prec_sq <- cropdat$prec^2
cropdat$`lat:long` <- cropdat$lat*cropdat$long
cropdat$`(Intercept)` <- 1


saveRDS(cropdat, "data/panel_regression_data.rds")


