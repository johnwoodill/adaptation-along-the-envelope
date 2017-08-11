library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)
library(lfe)
library(cowplot)
library(plm)
library(arm)
library(lfe)

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1930 & year <= 2010)


cropdat$prec_sq <- cropdat$prec^2
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

# Log dependent variables
cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(cropdat$soybean_rrev)

# Crop weights
cropdat$corn_w <- cropdat$corn_grain_a
cropdat$cotton_w <- cropdat$cotton_a
cropdat$hay_w <- cropdat$hay_a
cropdat$wheat_w <- cropdat$wheat_a
cropdat$soybean_w <- cropdat$soybean_a

# NA = 0
cropdat$corn_grain_a <- ifelse(is.na(cropdat$corn_grain_a), 0, cropdat$corn_grain_a)
cropdat$cotton_a <- ifelse(is.na(cropdat$cotton_a), 0, cropdat$cotton_a)
cropdat$cotton_a <- ifelse(is.na(cropdat$hay_a), 0, cropdat$hay_a)
cropdat$wheat_a <- ifelse(is.na(cropdat$wheat_a), 0, cropdat$wheat_a)
cropdat$soybean_a <- ifelse(is.na(cropdat$soybean_a), 0, cropdat$soybean_a)

# Get proportion of crop share
cropdat$total_a <- rowSums(cropdat[, c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)
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

# # Exposure weighted values equal zero
# cropdat$tavg <- cropdat$tavg - mean(cropdat$tavg, na.rm = TRUE)
# cropdat$dday0_10 <- cropdat$dday0_10 - mean(cropdat$dday0_10, na.rm = TRUE)
# cropdat$dday10_30 <- cropdat$dday10_30 - mean(cropdat$dday10_30, na.rm = TRUE)
# cropdat$dday30C <- cropdat$dday30C - mean(cropdat$dday30C, na.rm = TRUE)
# cropdat$prec <- cropdat$prec - mean(cropdat$prec, na.rm = TRUE)
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$prec_sq <- cropdat$prec^2

# cropdat$ln_corn_rrev <- cropdat$ln_corn_rrev - mean(cropdat$ln_corn_rrev, na.rm = TRUE)
# cropdat$ln_cotton_rrev <- cropdat$ln_cotton_rrev - mean(cropdat$ln_cotton_rrev, na.rm = TRUE)
# cropdat$ln_hay_rrev <- cropdat$ln_hay_rrev - mean(cropdat$ln_hay_rrev, na.rm = TRUE)
# cropdat$ln_wheat_rrev <- cropdat$ln_wheat_rrev - mean(cropdat$ln_wheat_rrev, na.rm = TRUE)
# cropdat$ln_soybean_rrev <- cropdat$ln_soybean_rrev - mean(cropdat$ln_soybean_rrev, na.rm = TRUE)
# 
# cropdat$p_corn_share <- cropdat$p_corn_share - mean(cropdat$p_corn_share, na.rm = TRUE)
# cropdat$p_cotton_share <- cropdat$p_cotton_share - mean(cropdat$p_cotton_share, na.rm = TRUE)
# cropdat$p_hay_share <- cropdat$p_hay_share - mean(cropdat$p_hay_share, na.rm = TRUE)
# cropdat$p_wheat_share <- cropdat$p_wheat_share - mean(cropdat$p_wheat_share, na.rm = TRUE)
# cropdat$p_soybean_share <- cropdat$p_soybean_share - mean(cropdat$p_soybean_share, na.rm = TRUE)


saveRDS(cropdat, "data/panel_regression_data.rds")

