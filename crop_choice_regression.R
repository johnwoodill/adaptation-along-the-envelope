library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)
library(cowplot)
library(pracma)
library(plm)

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1950 & year <= 2010)
soil <- readRDS("data/soilData.rds")
soil$fips <- as.numeric(soil$fips)

cropdat[is.na(cropdat)] <- 0
cropdat <- filter(cropdat, corn_grain_a != 0 & cotton_a != 0 & hay_a != 0 & wheat_a != 0 & soybean_a != 0)
cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")])
cropdat$dday8_32 <- cropdat$dday8C - cropdat$dday32C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C
cropdat$dday8_32sq <- cropdat$dday8_32^2
cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)

cropdat$dday10_30_sq <- cropdat$dday10_30^2
cropdat$pop_dens_sq <- cropdat$pop_dens^2
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$prec_sq <- cropdat$prec^2
cropdat$dday34C_sqrt <- sqrt(cropdat$dday34C)
# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))

# cropdat <- cropdat %>% 
#   group_by(year) %>% 
#   mutate(p_corn_a = corn_grain_a/sum(corn_grain_a, na.rm = TRUE),
#          dm_p_corn_a = p_corn_a - mean(p_corn_a, na.rm = TRUE),
#          dm_ipc = ipc - mean(ipc, na.rm = TRUE),
#          dm_pop_dens = pop_dens - mean(pop_dens, na.rm = TRUE)) %>% 
#   group_by(fips) %>% 
#   summarise(p_corn_a = mean(dm_p_corn_a, na.rm = TRUE),
#         lat = mean(lat, na.rm = TRUE),
#         tavg = mean(tavg, na.rm = TRUE),
#         prec = mean(prec, na.rm = TRUE),
#         dday8_32 = mean(dday8_32, na.rm = TRUE),
#         dday10_30 = mean(dday10_30, na.rm = TRUE),
#         dday34C = mean(dday34C, na.rm = TRUE),
#         dm_ipc = mean(dm_ipc, na.rm = TRUE),
#         dm_pop_dens = mean(dm_pop_dens, na.rm = TRUE))

cropdat <- cropdat %>%
  group_by(year) %>%
  mutate(p_corn_a = corn_grain_a/sum(total_a, na.rm = TRUE),
         dm_p_corn_a = p_corn_a - mean(p_corn_a, na.rm = TRUE),
        dm_ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
        dm_tavg = tavg - mean(tavg, na.rm = TRUE),
        dm_tavg_sq = tavg_sq - mean(tavg_sq, na.rm = TRUE),
        dm_prec = prec - mean(prec, na.rm = TRUE),
        dm_prec_sq = prec_sq - mean(prec_sq, na.rm = TRUE),
        dm_corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
        dm_dday10_30 = dday10_30 - mean(dday10_30, na.rm = TRUE),
        dm_dday30 = dday30C - mean(dday30C, na.rm = TRUE),
        dm_dday10_30_sq = dday10_30_sq - mean(dday10_30_sq, na.rm = TRUE),
        dm_dday34C = dday34C - mean(dday34C, na.rm = TRUE),
        dm_dday34C_sqrt = dday34C_sqrt - mean(dday34C_sqrt, na.rm = TRUE),
        dm_ipc = ipc - mean(ipc, na.rm = TRUE),
        dm_pop_dens = pop_dens - mean(pop_dens, na.rm = TRUE),
        dm_pop_dens_sq = pop_dens_sq - mean(pop_dens_sq, na.rm = TRUE)) %>%
  group_by(state, fips) %>%

    summarise(p_corn_a = mean(p_corn_a, na.rm = TRUE),
              ln_corn_rrev = mean(dm_ln_corn_rrev, na.rm = TRUE),
              tavg = mean(dm_tavg, na.rm = TRUE),
              tavg_sq = mean(dm_tavg_sq, na.rm = TRUE),
              prec = mean(dm_prec, na.rm = TRUE),
              prec_sq = mean(dm_prec_sq, na.rm = TRUE),
              corn_grain_a = mean(dm_corn_grain_a, na.rm = TRUE),
              lat = mean(lat, na.rm = TRUE),
              dday10_30 = mean(dm_dday10_30, na.rm = TRUE),
              dday30 = mean(dm_dday30, na.rm = TRUE),
              dday10_30_sq = mean(dm_dday10_30_sq, na.rm = TRUE),
              dday34C = mean(dm_dday34C, na.rm = TRUE),
              dday34C_sqrt = mean(dm_dday34C_sqrt, na.rm = TRUE),
              ipc = mean(dm_ipc, na.rm = TRUE),
              pop_dens = mean(dm_pop_dens, na.rm = TRUE),
              pop_dens_sq = mean(dm_pop_dens_sq, na.rm = TRUE))

cropdat <- left_join(cropdat, soil, by = "fips")

cc.corn.mod1 <- lm(p_corn_a ~ tavg + I(tavg^2) + prec + I(prec^2) + 
                     lat + ipc + pop_dens + I(pop_dens^2) + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil, data = cropdat)
summary(cc.corn.mod1)

# l.corn.mod1 <- glm(p_corn_a ~ tavg + tavgsq + prec + precsq, data = cropdat, family = "quasibinomial")
# summary(l.corn.mod1)

cc.corn.mod2 <- lm(p_corn_a ~ dday10_30  + I(dday10_30^2) + dday30 + prec + I(prec^2) + 
                    lat + ipc + pop_dens + I(pop_dens^2) + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil, data = cropdat)
summary(cc.corn.mod2)

# l.corn.mod2 <- glm(p_corn_rrev ~ dday8_32 + I(dday8_32^2) + I(sqrt(dday34C)) + prec + precsq, data = cropdat, family = "quasibinomial")
# summary(l.corn.mod2)

saveRDS(cc.corn.mod1, "models/cc.corn.mod1")
saveRDS(cc.corn.mod2, "models/cc.corn.mod2")


