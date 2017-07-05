library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)
library(cowplot)
library(pracma)

# Function to extract legend from ggplot object
g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
}



cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1960 & year <= 2010)

cropdat$total_rev <- cropdat$corn_rrev + cropdat$cotton_rrev + cropdat$hay_rrev + cropdat$wheat_rrev + cropdat$soybean_rrev
cropdat$total_a <- cropdat$corn_grain_a + cropdat$cotton_a + cropdat$hay_a + cropdat$wheat_a + cropdat$soybean_a
cropdat$dday8_32 <- cropdat$dday8C - cropdat$dday32C

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
  mutate(p_corn_rrev = corn_rrev/sum(corn_rrev, na.rm = TRUE)) %>% 
  group_by(fips) %>% 
  summarise(p_corn_rrev = mean(p_corn_rrev, na.rm = TRUE),
        tavg = mean(tavg, na.rm = TRUE),
        prec = mean(prec, na.rm = TRUE),
        dday8_32 = mean(dday8_32, na.rm = TRUE),
        dday34C = mean(dday34C, na.rm = TRUE))

# 
#         dm_tavg = tavg - mean(tavg, na.rm = TRUE),
#         dm_prec = prec - mean(prec, na.rm = TRUE),
#         dm_corn_grain_a = corn_grain_a - mean(corn_grain_a, na.rm = TRUE),
#         dm_dday8_32 = dday8_32 - mean(dday8_32, na.rm = TRUE),
#         dm_dday34C = dday34C - mean(dday32C, na.rm = TRUE),
#         dm_ipc = ipc - mean(ipc, na.rm = TRUE),
#         dm_pop_dens = pop_dens - mean(pop_dens, na.rm = TRUE)) %>% 
#   group_by(state, fips) %>% 
# 
#     summarise(dm_ln_corn_rrev = mean(dm_ln_corn_rrev, na.rm = TRUE),
#         dm_tavg = mean(dm_tavg, na.rm = TRUE),
#         dm_prec = mean(dm_prec, na.rm = TRUE),
#         dm_corn_grain_a = mean(dm_corn_grain_a, na.rm = TRUE),
#         lat = mean(lat, na.rm = TRUE),
#         dm_dday8_32 = mean(dday8_32, na.rm = TRUE),
#         dm_dday34C = mean(dday34C, na.rm = TRUE),
#         dm_ipc = mean(ipc, na.rm = TRUE),
#         dm_pop_dens = mean(pop_dens, na.rm = TRUE)) 

cropdat$tavgsq <- cropdat$tavg^2
cropdat$precsq <- cropdat$prec^2

l.corn.mod1 <- glm(p_corn_rrev ~ tavg + tavgsq + prec + precsq, data = cropdat, family = "quasibinomial")
summary(l.corn.mod1)

l.corn.mod2 <- glm(p_corn_rrev ~ dday8_32 + I(dday8_32^2) + I(sqrt(dday34C)) + prec + precsq, data = cropdat, family = "quasibinomial")
summary(l.corn.mod2)


# 10-year interval 1960's and 2000's

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat$dday8_32 <- cropdat$dday8C - cropdat$dday32C

# cropdat <- cropdat %>% 
#   group_by(year) %>% 
#   mutate(p_corn_rrev = corn_rrev/sum(corn_rrev, na.rm = TRUE))

cropdat1960 <- filter(cropdat, year >= 1960 & year < 1970)
#cropdat1960 <- filter(cropdat, corn_rrev > 0)
cropdat2000 <- filter(cropdat, year <= 2000 & year < 2010)

# Remove linear trend
cropdat1960$dt_corn_rrev <- cropdat1960$corn_rrev - coefficients(lm(corn_rrev ~ I(year - 1959), data = cropdat1960))[2]
cropdat2000$dt_corn_rrev <- cropdat2000$corn_rrev - coefficients(lm(corn_rrev ~ I(year - 1999), data = cropdat2000))[2]

cropdat1960 <- cropdat1960 %>% 

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

cropdat2000<- cropdat2000 %>% 
  
  group_by(year) %>% 
  mutate(s_corn_rrev = dt_corn_rrev/sum(dt_corn_rrev, na.rm = TRUE))  %>% 
  
  # Get cross-section averages
  group_by(fips) %>% 
  summarise(s_dt_corn_rrev2000 = mean(s_corn_rrev, na.rm = TRUE),
          tavg2000 = mean(tavg, na.rm = TRUE),
          prec2000 = mean(prec, na.rm = TRUE),
          dday8_32_2000 = mean(dday8_32, na.rm = TRUE),
          dday34_2000 = mean(dday34C, na.rm = TRUE))  

#cropdat2000$precsq <- cropdat2000$prec^2
#cropdat2000$tavgsq <- cropdat2000$tavg^2

logitdat <- left_join(cropdat1960, cropdat2000, by = "fips")
logitdat$corn_rrev <- logitdat$s_dt_corn_rrev2000 - logitdat$s_dt_corn_rrev1960
logitdat$tavg <- logitdat$tavg2000 - logitdat$tavg1960
logitdat$prec <- logitdat$prec2000 - logitdat$prec1960
logitdat$dday8_32 <- logitdat$dday8_32_2000 - logitdat$dday8_32_1960
logitdat$dday34 <- logitdat$dday34_2000 - logitdat$dday34_1960

logitdat$tavgsq <- logitdat$tavg^2
logitdat$precsq <- logitdat$prec^2
logitdat$dday8_32sq <- logitdat$dday8_32^2
logitdat$dday34sr <- sqrt(logitdat$dday34)

l.corn.mod3 <- lm(corn_rrev ~ tavg + tavgsq + prec + precsq, data = logitdat)
summary(l.corn.mod3)

l.corn.mod4 <- lm(corn_rrev ~ dday8_32 + dday8_32sq + dday34sr + prec + precsq, data = logitdat)
summary(l.corn.mod4)







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