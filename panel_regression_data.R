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

cropdat$prec_sq <- cropdat$prec^2
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$fips <- factor(cropdat$fips)
cropdat$state <- factor(cropdat$state)
cropdat$year <- factor(cropdat$year)
cropdat$dday8_32 <- cropdat$dday8C - cropdat$dday32C
cropdat$dday10_29 <- cropdat$dday10C - cropdat$dday29C
cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C
cropdat$dday34_sqrt <- sqrt(cropdat$dday34C)
cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(cropdat$soybean_rrev)



# cropdat <- filter(cropdat, corn_grain_a != 0 | cotton_a != 0 | hay_a != 0 | wheat_a != 0 | soybean_a != 0)
# cropdat <- filter(cropdat, corn_grain_a != 0 & cotton_a != 0 & hay_a != 0 & wheat_a != 0 & soybean_a != 0)

cropdat$total_a <- rowSums(cropdat[, c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE) 
cropdat$p_corn_share <- cropdat$corn_grain_a/cropdat$total_a
cropdat$p_cotton_share <- cropdat$cotton_a/cropdat$total_a
cropdat$p_hay_share <- cropdat$hay_a/cropdat$total_a
cropdat$p_wheat_share <- cropdat$wheat_a/cropdat$total_a
cropdat$p_soybean_share <- cropdat$soybean_a/cropdat$total_a

saveRDS(cropdat, "data/panel_regression_data.rds")


# Share of acres
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
#cropdat <- filter(cropdat, corn_grain_a != 0 & cotton_a != 0 & hay_a != 0 &  soybean_a != 0 & wheat_a != 0)


cropdat$prec_sq <- cropdat$prec^2
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$fips <- factor(cropdat$fips)
cropdat$state <- factor(cropdat$state)
cropdat$year <- factor(cropdat$year)
cropdat$dday8_32 <- cropdat$dday8C - cropdat$dday32C
cropdat$dday10_29 <- cropdat$dday10C - cropdat$dday29C
cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C
cropdat$dday34_sqrt <- sqrt(cropdat$dday34C)
cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(cropdat$soybean_rrev)



# cropdat <- filter(cropdat, corn_grain_a != 0 | cotton_a != 0 | hay_a != 0 | wheat_a != 0 | soybean_a != 0)
# cropdat <- filter(cropdat, corn_grain_a != 0 & cotton_a != 0 & hay_a != 0 & wheat_a != 0 & soybean_a != 0)

cropdat$total_a <- rowSums(cropdat[, c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE) 
cropdat$p_corn_share <- cropdat$corn_grain_a/cropdat$total_a
cropdat$p_cotton_share <- cropdat$cotton_a/cropdat$total_a
cropdat$p_hay_share <- cropdat$hay_a/cropdat$total_a
cropdat$p_wheat_share <- cropdat$wheat_a/cropdat$total_a
cropdat$p_soybean_share <- cropdat$soybean_a/cropdat$total_a

saveRDS(cropdat, "data/panel_p_share_regression_data.rds")
