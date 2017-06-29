library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)
library(cowplot)
library(plm)

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat <- filter(cropdat, year >= 1960 & year <= 2010)
#cropdat <- filter(cropdat, fips == 18175)



cropdat$trend <- cropdat$year - 1959
cropdat$trendsq <- cropdat$trend^2
cropdat$precsq <- cropdat$prec^2
cropdat$tavgsq <- cropdat$tavg^2
cropdat$ffips <- as.factor(cropdat$fips)
cropdat$fstate <- as.factor(cropdat$state)
cropdat$dday8C_32C <- cropdat$dday8C - cropdat$dday32C
cropdat$dday8C_32C_sq <- cropdat$dday8C_32C^2
cropdat$dday34C_sqrt <- sqrt(cropdat$dday32C)

dat <- cropdat
dbdist <- datadist(dat)
options("datadist" = "dbdist")

#mod1 <- ols(log(corn_yield) ~ statefe + corn_dday10w + corn_dday29w,
#            data=gp, x = TRUE, y = TRUE)

mod1 <- plm(log(1 + corn_rrev) ~ dday8C_32C + dday8C_32C_sq + 
             dday34C_sqrt + trend + trendsq + prec + precsq, data = dat, index = "state")
mod1
summary(mod1)
test <- mod1$model
plot(mod1)

mod2 <- plm(log(1 + cotton_rrev) ~ dday8C_32C + dday8C_32C_sq + 
             dday34C_sqrt + trend + trendsq + prec + precsq, data = dat, index = c("ffips"))
summary(mod2)

mod3 <- plm(log(1 + hay_rrev) ~ dday8C_32C + dday8C_32C_sq + 
             dday34C_sqrt + trend + trendsq + prec + precsq, data = dat, index = "ffips")
summary(mod3)

mod4 <- plm(log(1 + wheat_rrev) ~ ~ dday8C_32C + dday8C_32C_sq + 
             dday34C_sqrt + trend + trendsq + prec + precsq, data = dat, index = "ffips")
summary(mod4)

mod5 <- plm(log(1 + soybean_rrev) ~ ~ dday8C_32C + dday8C_32C_sq + 
             dday34C_sqrt + trend + trendsq + prec + precsq, data = dat = "ffips")
summary(mod5)


