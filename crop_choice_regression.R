library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)
library(cowplot)
library(pracma)
library(plm)


# # Remove trend
# cropdat <- cropdat %>% 
#   group_by(year) %>% 
#   mutate(dm_ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
#         dm_tavg = tavg - mean(tavg, na.rm = TRUE),
#         dm_prec = prec - mean(prec, na.rm = TRUE),
#         dm_corn_grain_a = corn_grain_a - mean(corn_grain_a, na.rm = TRUE),
#         dm_dday8_32 = dday8_32 - mean(dday8_32, na.rm = TRUE),
#         dm_dday34C = dday34C - mean(dday32C, na.rm = TRUE),
#         dm_ipc = ipc - mean(ipc, na.rm = TRUE),
#         dm_pop_dens = pop_dens - mean(pop_dens, na.rm = TRUE))



cropdat <- cropdat %>% 
  group_by(year) %>% 
  mutate(p_corn_a = corn_grain_a/sum(corn_grain_a, na.rm = TRUE),
         dm_p_corn_a = p_corn_a - mean(p_corn_a, na.rm = TRUE),
         dm_ipc = ipc - mean(ipc, na.rm = TRUE),
         dm_pop_dens = pop_dens - mean(pop_dens, na.rm = TRUE)) %>% 
  group_by(fips) %>% 
  summarise(p_corn_a = mean(dm_p_corn_a, na.rm = TRUE),
        lat = mean(lat, na.rm = TRUE),
        tavg = mean(tavg, na.rm = TRUE),
        prec = mean(prec, na.rm = TRUE),
        dday8_32 = mean(dday8_32, na.rm = TRUE),
        dday10_30 = mean(dday10_30, na.rm = TRUE),
        dday34C = mean(dday34C, na.rm = TRUE),
        dm_ipc = mean(dm_ipc, na.rm = TRUE),
        dm_pop_dens = mean(dm_pop_dens, na.rm = TRUE))

cropdat$tavgsq <- cropdat$tavg^2
cropdat$precsq <- cropdat$prec^2

cropdat <- left_join(cropdat, soil, by = "fips")

cc.corn.mod1 <- lm(p_corn_a ~ tavg + tavgsq + prec + precsq, data = cropdat)
summary(cc.corn.mod1)

# l.corn.mod1 <- glm(p_corn_a ~ tavg + tavgsq + prec + precsq, data = cropdat, family = "quasibinomial")
# summary(l.corn.mod1)

cc.corn.mod2 <- lm(p_corn_a ~ dday10_30  + dday34C + prec + precsq, data = cropdat)
summary(cc.corn.mod2)

# l.corn.mod2 <- glm(p_corn_rrev ~ dday8_32 + I(dday8_32^2) + I(sqrt(dday34C)) + prec + precsq, data = cropdat, family = "quasibinomial")
# summary(l.corn.mod2)

saveRDS(cc.corn.mod1, "models/cc.corn.mod1")
saveRDS(cc.corn.mod2, "models/cc.corn.mod2")
