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
soil <- readRDS("data/soilData.rds")
soil$fips <- as.numeric(soil$fips)


# East of 100th meridian
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1970 & year <= 2010)

cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(cropdat$soybean_rrev)
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C
cropdat$dday10_30_sq <- cropdat$dday10_30^2
cropdat$dday8_32 <- cropdat$dday8C - cropdat$dday32C
cropdat$dday8_32_sq <- cropdat$dday8_32^2
cropdat$pop_dens_sq <- cropdat$pop_dens^2
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$prec_sq <- cropdat$prec^2
cropdat$dday34C_sqrt <- sqrt(cropdat$dday34C)
cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")])
cropdat$state <- factor(cropdat$state)

# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))


# Need to remove mean from weather variables too
cropdat <- cropdat %>%
  group_by(year) %>%
  mutate(dm_ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
         dm_ln_cotton_rrev = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
         dm_ln_hay_rrev = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
         dm_ln_wheat_rrev = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
         dm_ln_soybean_rrev = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE),
         p_corn_share = corn_grain_a/sum(total_a, na.rm = TRUE),
         p_cotton_share = cotton_a/sum(total_a, na.rm = TRUE),
         p_hay_share = hay_a/sum(total_a, na.rm = TRUE),
         p_wheat_share = wheat_a/sum(total_a, na.rm = TRUE),
         p_soybean_share = soybean_a/sum(total_a, na.rm = TRUE),
         p_corn_share = p_corn_share - mean(p_corn_share, na.rm = TRUE),
         p_cotton_share = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
         p_hay_share = p_hay_share - mean(p_hay_share, na.rm = TRUE),
         p_wheat_share = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
         p_soybean_share = p_soybean_share - mean(p_soybean_share, na.rm = TRUE)) %>% 
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
              dday30C = mean(dday30C, na.rm = TRUE),
              dday10C = mean(dday10C, na.rm = TRUE),
              dday34C = mean(dday34C, na.rm = TRUE),
              ipc = mean(ipc, na.rm = TRUE),
              pop_dens = mean(pop_dens, na.rm = TRUE)) %>% 
  ungroup()

cropdat$tavg_sq <- cropdat$tavg^2
cropdat$prec_sq <- cropdat$prec^2
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C
cropdat$dday30_sq <- cropdat$dday30C^2
cropdat$pop_dens_sq <- cropdat$pop_dens^2

cropdat <- left_join(cropdat, soil, by = "fips")

# Remove all NA in explanatory variables
#cropdat <- filter(cropdat, !is.na(dm_ipc) & !is.na(dm_pop_dens) & !is.na(dm_pop_dens_sq) &
                                       # !is.na(waterCapacity) & !is.na(percentClay) & 
                                       # !is.na(minPermeability) & !is.na(kFactor) & !is.na(bestSoil))

cropdat <- ungroup(cropdat)
cropdat <- as.data.frame(cropdat)

saveRDS(cropdat, "data/cross_section_regression_data.rds")
