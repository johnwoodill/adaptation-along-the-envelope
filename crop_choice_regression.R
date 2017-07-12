library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)
library(cowplot)
library(pracma)
library(plm)

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)

cropdat$total_a <- cropdat$corn_grain_a + cropdat$cotton_a + cropdat$hay_a + cropdat$wheat_a + cropdat$soybean_a
cropdat$dday8_32 <- cropdat$dday8C - cropdat$dday32C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

cropdat$dday8_32sq <- cropdat$dday8_32^2
cropdat$dday10_30sq <- cropdat$dday10_30^2
cropdat$dday34Csqrt <- sqrt(cropdat$dday34C)

cropdat$tavgsq <- cropdat$tavg^2
cropdat$precsq <- cropdat$prec^2

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


# 10-year interval 1960's and 2000's --------------------------------------


 
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)

# Soil data
#soil <- read_dta("data/soilData.dta")
soil <- readRDS("data/soilData.rds")
soil$fips <- as.numeric(soil$fips)

cropdat$total_a <- cropdat$corn_grain_a + cropdat$cotton_a + cropdat$hay_a + cropdat$wheat_a + cropdat$soybean_a
cropdat$dday8_32 <- cropdat$dday8C - cropdat$dday32C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

cropdat$dday8_32sq <- cropdat$dday8_32^2
cropdat$dday10_30sq <- cropdat$dday10_30^2
cropdat$dday34Csqrt <- sqrt(cropdat$dday34C)

cropdat$tavgsq <- cropdat$tavg^2
cropdat$precsq <- cropdat$prec^2

cropdat <- cropdat %>% 
  group_by(year) %>% 
  mutate(p_corn_share = corn_grain_a/sum(corn_grain_a, na.rm = TRUE))
  
cropdat1960 <- filter(cropdat, year >= 1960 & year < 1970)
cropdat1970 <- filter(cropdat, year >= 1970 & year < 1980)
cropdat1990 <- filter(cropdat, year <= 1990 & year < 2000)
cropdat2000 <- filter(cropdat, year >= 2000 & year < 2010)

cropdat1960 <- cropdat1960 %>% 
  group_by(state, fips) %>% 
  summarise(p_corn_share_1960 = mean(p_corn_share, na.rm = TRUE),
            dday10_30_1960 = mean(dday10_30, na.rm = TRUE),
            dday10_30sq_1960 = mean(dday10_30sq, na.rm = TRUE),
            dday34C_sqrt_1960 = mean(dday34Csqrt, na.rm = TRUE),
            prec_1960 = mean(prec, na.rm = TRUE),
            precsq_1960 = mean(precsq, na.rm = TRUE))

cropdat1970 <- cropdat1970 %>% 
  group_by(state, fips) %>% 
  summarise(p_corn_share_1970 = mean(p_corn_share, na.rm = TRUE),
            dday10_30_1970 = mean(dday10_30, na.rm = TRUE),
            dday10_30sq_1970 = mean(dday10_30sq, na.rm = TRUE),
            dday34C_sqrt_1970 = mean(dday34Csqrt, na.rm = TRUE),
            prec_1970 = mean(prec, na.rm = TRUE),
            precsq_1970 = mean(precsq, na.rm = TRUE))

cropdat1990 <- cropdat1990 %>% 
  group_by(state, fips) %>% 
  summarise(p_corn_share_1990 = mean(p_corn_share, na.rm = TRUE),
            dday10_30_1990 = mean(dday10_30, na.rm = TRUE),
            dday10_30sq_1990 = mean(dday10_30sq, na.rm = TRUE),
            dday34C_sqrt_1990 = mean(dday34Csqrt, na.rm = TRUE),
            prec_1990 = mean(prec, na.rm = TRUE),
            precsq_1990 = mean(precsq, na.rm = TRUE))


cropdat2000 <- cropdat2000 %>% 
  group_by(state, fips) %>% 
  summarise(p_corn_share_2000 = mean(p_corn_share, na.rm = TRUE),
            dday10_30_2000 = mean(dday10_30, na.rm = TRUE),
            dday10_30sq_2000 = mean(dday10_30sq, na.rm = TRUE),
            dday34C_sqrt_2000 = mean(dday34Csqrt, na.rm = TRUE),
            prec_2000 = mean(prec, na.rm = TRUE),
            precsq_2000 = mean(precsq, na.rm = TRUE))



newcropdat1960 <- left_join(cropdat1960, cropdat1970, by = c("state", "fips"))

newcropdat1960$diff_p_corn_share <- newcropdat1960$p_corn_share_1970 - newcropdat1960$p_corn_share_1960
newcropdat1960$diff_dday10_30 <- newcropdat1960$dday10_30_1970 - newcropdat1960$dday10_30_1960
newcropdat1960$diff_dday10_30sq <- newcropdat1960$dday10_30sq_1970 - newcropdat1960$dday10_30sq_1960
newcropdat1960$diff_dday34_sqrt <- newcropdat1960$dday34C_sqrt_1970 - newcropdat1960$dday34C_sqrt_1960
newcropdat1960$diff_prec <- newcropdat1960$prec_1970 - newcropdat1960$prec_1960
newcropdat1960$diff_precsq <- newcropdat1960$precsq_1970 - newcropdat1960$precsq_1960
newcropdat1960$year <- 1960
newcropdat1960 <- select(newcropdat1960, year, state, fips, diff_p_corn_share, diff_dday10_30, diff_dday10_30sq, diff_dday34_sqrt, diff_prec, diff_precsq)


newcropdat1990 <- left_join(cropdat1990, cropdat2000, by = c("state", "fips"))

newcropdat1990$diff_p_corn_share <- newcropdat1990$p_corn_share_2000 - newcropdat1990$p_corn_share_1990
newcropdat1990$diff_dday10_30 <- newcropdat1990$dday10_30_2000 - newcropdat1990$dday10_30_1990
newcropdat1990$diff_dday10_30sq <- newcropdat1990$dday10_30sq_2000 - newcropdat1990$dday10_30sq_1990
newcropdat1990$diff_dday34_sqrt <- newcropdat1990$dday34C_sqrt_2000 - newcropdat1990$dday34C_sqrt_1990
newcropdat1990$diff_prec <- newcropdat1990$prec_2000 - newcropdat1990$prec_1990
newcropdat1990$diff_precsq <- newcropdat1990$precsq_2000 - newcropdat1990$precsq_1990
newcropdat1990 <- select(newcropdat1990, year, state, fips, diff_p_corn_share, diff_dday10_30, diff_dday10_30sq, diff_dday34_sqrt, diff_prec, diff_precsq)
newcropdat1990$year <- 1990

newcropdat <- rbind(newcropdat1960, newcropdat1990)


cc.corn.mod1 <- lm(diff_p_corn_share ~ factor(state) + factor(year) + diff_dday10_30 + diff_dday34_sqrt + diff_prec + diff_precsq, 
                   data = newcropdat)
summary(cc.corn.mod1)



# Calculate share in each year
group_by(year) %>%
   mutate(s_corn_rrev = dt_corn_rrev/sum(dt_corn_rrev, na.rm = TRUE))  %>%

   # Get cross-section averages
 group_by(fips) %>%
   summarise(s_dt_corn_rrev1960 = mean(s_corn_rrev, na.rm = TRUE),
           tavg1960 = mean(tavg, na.rm = TRUE),
           prec1960 = mean(prec, na.rm = TRUE),
         dday8_32_1960 = mean(dday8_32, na.rm = TRUE),
           dday34_1960 = mean(dday34C, na.rm = TRUE))

 #cropdat1960$precsq <- cropdat1960$prec^2
 #cropdat1960$tavgsq <- cropdat1960$tavg^2
# cropdat2000<- cropdat2000 %>%

   group_by(year) %>%
   mutate(s_corn_rrev = dt_corn_rrev/sum(dt_corn_rrev, na.rm = TRUE))  %>%

 # Get cross-section averages
#   group_by(fips) %>%
#   summarise(s_dt_corn_rrev2000 = mean(s_corn_rrev, na.rm = TRUE),
#           tavg2000 = mean(tavg, na.rm = TRUE),
#           prec2000 = mean(prec, na.rm = TRUE),
#           dday8_32_2000 = mean(dday8_32, na.rm = TRUE),
#           dday34_2000 = mean(dday34C, na.rm = TRUE))
#
# #cropdat2000$precsq <- cropdat2000$prec^2
# #cropdat2000$tavgsq <- cropdat2000$tavg^2
#
# logitdat <- left_join(cropdat1960, cropdat2000, by = "fips")
# logitdat$corn_rrev <- logitdat$s_dt_corn_rrev2000 - logitdat$s_dt_corn_rrev1960
# logitdat$tavg <- logitdat$tavg2000 - logitdat$tavg1960
# logitdat$prec <- logitdat$prec2000 - logitdat$prec1960
# logitdat$dday8_32 <- logitdat$dday8_32_2000 - logitdat$dday8_32_1960
# logitdat$dday34 <- logitdat$dday34_2000 - logitdat$dday34_1960
#
# logitdat$tavgsq <- logitdat$tavg^2
# logitdat$precsq <- logitdat$prec^2
# logitdat$dday8_32sq <- logitdat$dday8_32^2
# logitdat$dday34sr <- sqrt(logitdat$dday34)
#
# l.corn.mod3 <- lm(corn_rrev ~ tavg + tavgsq + prec + precsq, data = logitdat)
# summary(l.corn.mod3)
#
# l.corn.mod4 <- lm(corn_rrev ~ dday8_32 + dday8_32sq + dday34sr + prec + precsq, data = logitdat)
# summary(l.corn.mod4)
#






#
# cropdat1960 <- cropdat1960 %>%
#   group_by(year) %>%
#   mutate(p_corn_rrev = corn_rrev/sum(corn_rrev, na.rm = TRUE)) %>%
#   group_by(fips) %>%
#   summarise(p_corn_rrev1960 = mean(p_corn_rrev,na.rm = TRUE),
#             tavg1960 = mean(tavg, na.rm = TRUE),
#             prec1960 = mean(prec, na.rm = TRUE),
#             dday8_32_1960 = mean(dday8_32, na.rm = TRUE),
#             dday34C_1960 = mean(dday34C, na.rm = TRUE))
#
#
# cropdat2000 <- cropdat2000 %>%
#   group_by(year) %>%
#   mutate(p_corn_rrev = corn_rrev/sum(corn_rrev, na.rm = TRUE)) %>%
#   group_by(fips) %>%
#   summarise(p_corn_rrev2000 = mean(p_corn_rrev,na.rm = TRUE),
#             tavg2000 = mean(tavg, na.rm = TRUE),
#             prec2000 = mean(prec, na.rm = TRUE),
#             dday8_32_2000 = mean(dday8_32, na.rm = TRUE),
#             dday34C_2000 = mean(dday34C, na.rm = TRUE))
#
#
# logitdat <- left_join(cropdat1960, cropdat2000, by = "fips")
#
# # Difference
# logitdat$diff_corn_rrev <- logitdat$p_corn_rrev2000 - logitdat$p_corn_rrev1960
# logitdat$diff_tavg <- logitdat$tavg2000 - logitdat$tavg1960
# logitdat$diff_prec <- logitdat$prec2000 - logitdat$prec1960
# logitdat$diff_dday8_32 <- logitdat$dday8_32_2000 - logitdat$dday8_32_1960
# logitdat$diff_dday34 <- logitdat$dday34C_2000 - logitdat$dday34C_1960
#
# logitdat$diff_tavgsq <- logitdat$diff_tavg^2
# logitdat$diff_precsq <- logitdat$diff_prec^2
#
# l.corn.mod1 <- glm(diff_corn_rrev ~ diff_tavg + diff_tavgsq + diff_prec + diff_precsq, data = logitdat, family = "quasibinomial")
# summary(l.corn.mod1)
#
# l.corn.mod2 <- glm(p_corn_rrev ~ dday8_32 + I(dday8_32^2) + I(sqrt(dday34C)) + prec + precsq, data = cropdat, family = "quasibinomial")
# summary(l.corn.mod2)