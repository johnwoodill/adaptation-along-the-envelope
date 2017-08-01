library(plm)
library(tidyverse)
library(stargazer)
library(rms)
library(cowplot)
library(multiwayvcov)
library(lmtest)
library(lfe)

# Cross-section: Log(Corn Rev) --------------------------------------------


setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

# Crop data
cropdat <- readRDS("data/full_ag_data.rds")


# Soil data
#soil <- read_dta("data/soilData.dta")
#soil <- readRDS("data/soilData.rds")
#soil$fips <- as.numeric(soil$fips)


# East of 100th meridian
cropdat <- filter(cropdat, abs(long) <= 100)

cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(cropdat$soybean_rrev)
cropdat$state <- factor(cropdat$state)
cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)
cropdat$p_corn_share <- cropdat$corn_grain_a/cropdat$total_a
cropdat$p_cotton_share <- cropdat$cotton_a/cropdat$total_a
cropdat$p_hay_share <- cropdat$hay_a/cropdat$total_a
cropdat$p_wheat_share <- cropdat$wheat_a/cropdat$total_a
cropdat$p_soybean_share <- cropdat$soybean_a/cropdat$total_a

# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))


cropdat <- cropdat %>%
  group_by(year) %>%
  mutate(dm_ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
         dm_ln_cotton_rrev = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
         dm_ln_hay_rrev = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
         dm_ln_wheat_rrev = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
         dm_ln_soybean_rrev = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE),
         p_corn_share = p_corn_share - mean(p_corn_share, na.rm = TRUE),
         p_cotton_share = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
         p_hay_share = p_hay_share - mean(p_hay_share, na.rm = TRUE),
         p_wheat_share = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
         p_soybean_share = p_soybean_share - mean(p_soybean_share, na.rm = TRUE),
         corn_w = corn_grain_a/mean(corn_grain_a, na.rm = TRUE)) %>% 
    group_by(state, fips) %>%
    summarise(ln_corn_rrev = mean(dm_ln_corn_rrev, na.rm = TRUE),
              ln_cotton_rrev = mean(dm_ln_cotton_rrev, na.rm = TRUE),
              ln_hay_rrev = mean(dm_ln_hay_rrev, na.rm = TRUE),
              ln_wheat_rrev = mean(dm_ln_wheat_rrev, na.rm = TRUE),
              ln_soybean_rrev = mean(dm_ln_soybean_rrev, na.rm = TRUE),
              p_corn_share = mean(p_corn_share, na.rm = TRUE),
              p_cotton_share = mean(p_cotton_share, na.rm = TRUE),
              p_hay_share = mean(p_hay_share, na.rm = TRUE),
              p_wheat_share = mean(p_wheat_share, na.rm = TRUE),
              p_soybean_share = mean(p_soybean_share, na.rm = TRUE),
              tavg = mean(tavg, na.rm = TRUE),
              prec = mean(prec, na.rm = TRUE),
              corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
              cotton_a = mean(cotton_a, na.rm = TRUE),
              hay_a = mean(hay_a, na.rm = TRUE),
              wheat_a = mean(wheat_a, na.rm = TRUE),
              soybean_a = mean(soybean_a, na.rm = TRUE),
              total_a = mean(total_a, na.rm = TRUE),
              lat = mean(lat, na.rm = TRUE),
              dday0C = mean(dday0C, na.rm = TRUE),
              dday10C = mean(dday10C, na.rm = TRUE),
              dday15C = mean(dday15C, na.rm = TRUE),
              dday17C = mean(dday17C, na.rm = TRUE),
              dday29C = mean(dday29C, na.rm = TRUE),
              dday30C = mean(dday30C, na.rm = TRUE),
              dday10C = mean(dday10C, na.rm = TRUE),
              dday32C = mean(dday32C, na.rm = TRUE),
              dday33C = mean(dday33C, na.rm = TRUE),
              dday34C = mean(dday34C, na.rm = TRUE),
              ipc = mean(ipc, na.rm = TRUE),
              pop_dens = mean(pop_dens, na.rm = TRUE)) %>% 
  ungroup()

cropdat$tavg_sq <- cropdat$tavg^2
cropdat$prec_sq <- cropdat$prec^2
cropdat$corn_w <- cropdat$corn_grain_a/mean(cropdat$corn_grain_a, na.rm = TRUE)
cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C
cropdat$dday10_29 <- cropdat$dday10C - cropdat$dday29C
cropdat$dday30_sq <- cropdat$dday30C^2
cropdat$pop_dens_sq <- cropdat$pop_dens^2

cropdat$dday0_10w <- cropdat$dday0_10*cropdat$corn_w
cropdat$dday10_30w <- cropdat$dday10_30*cropdat$corn_w
cropdat$dday10_29w <- cropdat$dday10_29*cropdat$corn_w
cropdat$dday30w <- cropdat$dday30C*cropdat$corn_w
cropdat$dday34w <- cropdat$dday34C*cropdat$corn_w

# cropdat <- left_join(cropdat, soil, by = "fips")

# Remove all NA in explanatory variables
#cropdat <- filter(cropdat, !is.na(dm_ipc) & !is.na(dm_pop_dens) & !is.na(dm_pop_dens_sq) &
                                       # !is.na(waterCapacity) & !is.na(percentClay) & 
                                       # !is.na(minPermeability) & !is.na(kFactor) & !is.na(bestSoil))

cropdat <- ungroup(cropdat)
cropdat <- as.data.frame(cropdat)

saveRDS(cropdat, "data/cross_section_regression_data.rds")



# Crop data
setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
cropdat <- readRDS("data/full_ag_data.rds")
#cropdat <- filter(cropdat, corn_grain_a != 0 & cotton_a != 0 & hay_a != 0 &  soybean_a != 0 & wheat_a != 0)


# East of 100th meridian
cropdat <- filter(cropdat, abs(long) <= 100)

cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(cropdat$soybean_rrev)
cropdat$state <- factor(cropdat$state)
cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)
cropdat$p_corn_share <- cropdat$corn_grain_a/cropdat$total_a
cropdat$p_cotton_share <- cropdat$cotton_a/cropdat$total_a
cropdat$p_hay_share <- cropdat$hay_a/cropdat$total_a
cropdat$p_wheat_share <- cropdat$wheat_a/cropdat$total_a
cropdat$p_soybean_share <- cropdat$soybean_a/cropdat$total_a

# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))


cropdat <- cropdat %>%
  group_by(year) %>%
  mutate(dm_ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
         dm_ln_cotton_rrev = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
         dm_ln_hay_rrev = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
         dm_ln_wheat_rrev = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
         dm_ln_soybean_rrev = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE),
         p_corn_share = p_corn_share - mean(p_corn_share, na.rm = TRUE),
         p_cotton_share = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
         p_hay_share = p_hay_share - mean(p_hay_share, na.rm = TRUE),
         p_wheat_share = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
         p_soybean_share = p_soybean_share - mean(p_soybean_share, na.rm = TRUE),
         corn_w = corn_grain_a/mean(corn_grain_a, na.rm = TRUE)) %>% 
    group_by(state, fips) %>%
    summarise(ln_corn_rrev = mean(dm_ln_corn_rrev, na.rm = TRUE),
              ln_cotton_rrev = mean(dm_ln_cotton_rrev, na.rm = TRUE),
              ln_hay_rrev = mean(dm_ln_hay_rrev, na.rm = TRUE),
              ln_wheat_rrev = mean(dm_ln_wheat_rrev, na.rm = TRUE),
              ln_soybean_rrev = mean(dm_ln_soybean_rrev, na.rm = TRUE),
              p_corn_share = mean(p_corn_share, na.rm = TRUE),
              p_cotton_share = mean(p_cotton_share, na.rm = TRUE),
              p_hay_share = mean(p_hay_share, na.rm = TRUE),
              p_wheat_share = mean(p_wheat_share, na.rm = TRUE),
              p_soybean_share = mean(p_soybean_share, na.rm = TRUE),
              tavg = mean(tavg, na.rm = TRUE),
              prec = mean(prec, na.rm = TRUE),
              corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
              cotton_a = mean(cotton_a, na.rm = TRUE),
              hay_a = mean(hay_a, na.rm = TRUE),
              wheat_a = mean(wheat_a, na.rm = TRUE),
              soybean_a = mean(soybean_a, na.rm = TRUE),
              total_a = mean(total_a, na.rm = TRUE),
              lat = mean(lat, na.rm = TRUE),
              dday0C = mean(dday0C, na.rm = TRUE),
              dday10C = mean(dday10C, na.rm = TRUE),
              dday15C = mean(dday15C, na.rm = TRUE),
              dday17C = mean(dday17C, na.rm = TRUE),
              dday29C = mean(dday29C, na.rm = TRUE),
              dday30C = mean(dday30C, na.rm = TRUE),
              dday10C = mean(dday10C, na.rm = TRUE),
              dday32C = mean(dday32C, na.rm = TRUE),
              dday33C = mean(dday33C, na.rm = TRUE),
              dday34C = mean(dday34C, na.rm = TRUE),
              ipc = mean(ipc, na.rm = TRUE),
              pop_dens = mean(pop_dens, na.rm = TRUE)) %>% 
  ungroup()

cropdat$tavg_sq <- cropdat$tavg^2
cropdat$prec_sq <- cropdat$prec^2
cropdat$corn_w <- cropdat$corn_grain_a/mean(cropdat$corn_grain_a, na.rm = TRUE)
cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C
cropdat$dday10_29 <- cropdat$dday10C - cropdat$dday29C
cropdat$dday30_sq <- cropdat$dday30C^2
cropdat$pop_dens_sq <- cropdat$pop_dens^2

cropdat$dday0_10w <- cropdat$dday0_10*cropdat$corn_w
cropdat$dday10_30w <- cropdat$dday10_30*cropdat$corn_w
cropdat$dday10_29w <- cropdat$dday10_29*cropdat$corn_w
cropdat$dday30w <- cropdat$dday30C*cropdat$corn_w
cropdat$dday34w <- cropdat$dday34C*cropdat$corn_w

# cropdat <- left_join(cropdat, soil, by = "fips")

# Remove all NA in explanatory variables
#cropdat <- filter(cropdat, !is.na(dm_ipc) & !is.na(dm_pop_dens) & !is.na(dm_pop_dens_sq) &
                                       # !is.na(waterCapacity) & !is.na(percentClay) & 
                                       # !is.na(minPermeability) & !is.na(kFactor) & !is.na(bestSoil))

cropdat <- ungroup(cropdat)
cropdat <- as.data.frame(cropdat)

saveRDS(cropdat, "data/cross_section_p_share_regression_data.rds")
