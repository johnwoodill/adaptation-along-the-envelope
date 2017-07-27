library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)
library(lfe)
library(cowplot)
library(plm)
library(lfe)

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1970 & year <= 2010)

cropdat$prec_sq <- cropdat$prec^2
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$fips <- factor(cropdat$fips)
cropdat$state <- factor(cropdat$state)
cropdat$year <- factor(cropdat$year)
cropdat$dday8C_32C <- cropdat$dday8C - cropdat$dday32C
cropdat$dday10C_29C <- cropdat$dday10C - cropdat$dday29C
cropdat$dday10C_30C <- cropdat$dday10C - cropdat$dday30C
cropdat$dday10C_30C_sq <- cropdat$dday10C_30C^2
cropdat$dday8C_32C_sq <- cropdat$dday8C_32C^2
cropdat$dday34C_sqrt <- sqrt(cropdat$dday34C)
cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(cropdat$soybean_rrev)

cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)

cropdat <- cropdat %>% 
  group_by(year) %>% 
  mutate(p_corn_share = corn_grain_a/sum(total_a, na.rm = TRUE),
         p_cotton_share = cotton_a/sum(total_a, na.rm = TRUE),
         p_hay_share = hay_a/sum(total_a, na.rm = TRUE),
         p_wheat_share = wheat_a/sum(total_a, na.rm = TRUE),
         p_soybean_share = soybean_a/sum(total_a, na.rm = TRUE)) %>% 
  ungroup()

saveRDS(cropdat, "data/panel_regression_data.rds")
