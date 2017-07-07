library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)
library(cowplot)
library(plm)

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1970 & year <= 2010)
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

p.cropdat <- plm.data(cropdat, index = c("fips", "year"))

# With Trend
p.corn.mod1 <- plm(ln_corn_rrev ~ factor(year) + tavg + tavgsq + prec + precsq, 
                   data = p.cropdat, index = c("fips", "year"), model = "within")
summary(p.corn.mod1)

p.corn.mod2 <- plm(ln_corn_rrev ~ factor(year) + dday10C_30C + dday10C_30C_sq + dday34C_sqrt +
              prec + precsq + trend + trendsq, data = p.cropdat, model = "within")
summary(p.corn.mod2)



# 
# 
# test <- mod1$model
# plot(mod1)
# 
# mod2 <- plm(log(1 + cotton_rrev) ~ dday8C_32C + dday8C_32C_sq + 
#              dday34C_sqrt + trend + trendsq + prec + precsq, data = dat, index = c("ffips"))
# summary(mod2)
# 
# mod3 <- plm(log(1 + hay_rrev) ~ dday8C_32C + dday8C_32C_sq + 
#              dday34C_sqrt + trend + trendsq + prec + precsq, data = dat, index = "ffips")
# summary(mod3)
# 
# mod4 <- plm(log(1 + wheat_rrev) ~ ~ dday8C_32C + dday8C_32C_sq + 
#              dday34C_sqrt + trend + trendsq + prec + precsq, data = dat, index = "ffips")
# summary(mod4)
# 
# mod5 <- plm(log(1 + soybean_rrev) ~ ~ dday8C_32C + dday8C_32C_sq + 
#              dday34C_sqrt + trend + trendsq + prec + precsq, data = dat = "ffips")
# summary(mod5)


