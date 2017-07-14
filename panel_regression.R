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

cropdat$trend <- cropdat$year - 1959
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


 p.corn.mod1 <- plm(ln_corn_rrev ~ factor(year) + tavg + I(tavg^2) + prec + I(prec^2), 
                    data = p.cropdat, model = "within")
 summary(p.corn.mod1)
 
 p.corn.mod2 <- plm(ln_corn_rrev ~ factor(year) + dday10C_30C  + I(dday10C_30C^2) + dday30C +
               prec + I(prec^2), data = p.cropdat, model = "within")
 summary(p.corn.mod2)

saveRDS(p.corn.mod1, "models/p.corn.mod1")
saveRDS(p.corn.mod2, "models/p.corn.mod2")



# Panel Corn Acres --------------------------------------------------------

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1950 & year <= 2010)

cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)
cropdat$dday8_32 <- cropdat$dday8C - cropdat$dday32C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

cropdat <- cropdat %>% 
  group_by(year) %>% 
  mutate(p_corn_a = corn_grain_a/sum(total_a, na.rm = TRUE))

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
cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)

cropdat <- filter(cropdat, !is.na(ln_corn_rrev))

p.cropdat <- plm.data(cropdat, index = c("fips", "year"))

p.corn.mod3 <- plm(p_corn_a ~ factor(year) + tavg + I(tavg^2) + prec + I(prec^2), 
                    data = p.cropdat, model = "within")
summary(p.corn.mod3)

p.corn.mod4 <- plm(p_corn_a ~ factor(year) + dday10C_30C + I(dday10C_30C^2) + dday30C +
               prec + I(prec^2), data = p.cropdat, model = "within")
summary(p.corn.mod4)

saveRDS(p.corn.mod3, "models/p.corn.mod3")
saveRDS(p.corn.mod4, "models/p.corn.mod4")


