library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)
library(lfe)
library(cowplot)
library(plm)


# Panel for Corn Revenue per Acre -----------------------------------------


cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1950 & year <= 2010)
# cropdat <- filter(cropdat, corn_rrev > 0)
#cropdat <- filter(cropdat, fips == 18175)

cropdat$trend <- cropdat$year - 1959
cropdat$trendsq <- cropdat$trend^2
cropdat$precsq <- cropdat$prec^2
cropdat$tavgsq <- cropdat$tavg^2
cropdat$ffips <- as.factor(cropdat$fips)
cropdat$fstate <- as.factor(cropdat$state)
cropdat$dday8C_32C <- cropdat$dday8C - cropdat$dday32C
cropdat$dday10C_30C <- cropdat$dday10C - cropdat$dday30C
cropdat$dday10C_30C_sq <- cropdat$dday10C_30C^2
cropdat$dday8C_32C_sq <- cropdat$dday8C_32C^2
cropdat$dday34C_sqrt <- sqrt(cropdat$dday34C)
cropdat$ln_corn_rrev <- log(1 + cropdat$corn_rrev)

cropdat <- filter(cropdat, !is.na(ln_corn_rrev))

# cropdat <- cropdat %>%
#    group_by(fips) %>%
#    mutate(ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
#              tavg = tavg - mean(tavg, na.rm = TRUE),
#              prec = prec - mean(prec, na.rm = TRUE))
# 
cropdat$precsq <- cropdat$prec^2
cropdat$tavgsq <- cropdat$tavg^2

p.cropdat <- plm.data(cropdat, index = c("fips", "year"))


 p.corn.mod1 <- plm(ln_corn_rrev ~ factor(year) + tavg + tavgsq + prec + precsq, 
                    data = p.cropdat, model = "within")
 summary(p.corn.mod1)
 
 p.corn.mod2 <- plm(ln_corn_rrev ~ factor(year) + dday10C_30C  + dday10C_30C_sq + dday34C_sqrt +
               prec + precsq, data = p.cropdat, model = "within")
 summary(p.corn.mod2)

# p.corn.mod1 <- lm(ln_corn_rrev ~ factor(year) + factor(fips) + tavg + tavgsq + prec + precsq,
#                    data = cropdat)
# summary(p.corn.mod1)
# 
# p.corn.mod2 <- lm(ln_corn_rrev ~ factor(year) + factor(fips) + dday10C_30C + dday10C_30C_sq + dday34C_sqrt +
#               prec + precsq, data = cropdat)
# summary(p.corn.mod2)

saveRDS(p.corn.mod1, "models/p.corn.mod1")
saveRDS(p.corn.mod2, "models/p.corn.mod2")



# Panel Corn Acres --------------------------------------------------------

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1950 & year <= 2010)

cropdat$total_rev <- cropdat$corn_rrev + cropdat$cotton_rrev + cropdat$hay_rrev + cropdat$wheat_rrev + cropdat$soybean_rrev
cropdat$total_a <- cropdat$corn_grain_a + cropdat$cotton_a + cropdat$hay_a + cropdat$wheat_a + cropdat$soybean_a
cropdat$dday8_32 <- cropdat$dday8C - cropdat$dday32C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

cropdat <- cropdat %>% 
  group_by(year) %>% 
  mutate(p_corn_a = corn_grain_a/sum(corn_grain_a, na.rm = TRUE))

cropdat$trend <- cropdat$year - 1959
cropdat$trendsq <- cropdat$trend^2
cropdat$precsq <- cropdat$prec^2
cropdat$tavgsq <- cropdat$tavg^2
cropdat$ffips <- as.factor(cropdat$fips)
cropdat$fstate <- as.factor(cropdat$state)
cropdat$dday8C_32C <- cropdat$dday8C - cropdat$dday32C
cropdat$dday10C_30C <- cropdat$dday10C - cropdat$dday30C
cropdat$dday10C_30C_sq <- cropdat$dday10C_30C^2
cropdat$dday8C_32C_sq <- cropdat$dday8C_32C^2
cropdat$dday34C_sqrt <- sqrt(cropdat$dday34C)
cropdat$ln_corn_rrev <- log(1 + cropdat$corn_rrev)

cropdat <- filter(cropdat, !is.na(ln_corn_rrev))

p.cropdat <- plm.data(cropdat, index = c("fips", "year"))

p.corn.mod3 <- plm(p_corn_a ~ factor(year) + tavg + tavgsq + prec + precsq, 
                    data = p.cropdat, model = "within")
summary(p.corn.mod3)

p.corn.mod4 <- plm(p_corn_a ~ factor(year) + dday10C_30C + dday10C_30C_sq + dday34C_sqrt +
               prec + precsq, data = p.cropdat, model = "within")
summary(p.corn.mod4)

saveRDS(p.corn.mod3, "models/p.corn.mod3")
saveRDS(p.corn.mod4, "models/p.corn.mod4")


