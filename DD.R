library(lfe)
library(tidyverse)
library(ggthemes)
library(rms)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, year < 2010)
cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

dummyCreator <- function(invec, prefix = NULL) {
     L <- length(invec)
     ColNames <- sort(unique(invec))
     M <- matrix(0L, ncol = length(ColNames), nrow = L,
                 dimnames = list(NULL, ColNames))
     M[cbind(seq_len(L), match(invec, ColNames))] <- 1L
     if (!is.null(prefix)) colnames(M) <- paste(prefix, colnames(M), sep = "_")
     M
} 

#cropdat <- filter(cropdat, state == "wi")
#cropdat
# # Constant prices
cropdat$corn_rprice <- mean(cropdat$corn_rprice, na.rm = TRUE)
cropdat$cotton_rprice <- mean(cropdat$cotton_rprice, na.rm = TRUE)
cropdat$hay_rprice <- mean(cropdat$hay_rprice, na.rm = TRUE)
cropdat$wheat_rprice <- mean(cropdat$wheat_rprice, na.rm = TRUE)
cropdat$soybean_rprice <- mean(cropdat$soybean_rprice, na.rm = TRUE)

cropdat <- cropdat %>% 
   group_by(fips) %>% 
   mutate(avg_corn_a = mean(corn_grain_a, na.rm = TRUE),
          avg_cotton_a = mean(cotton_a, na.rm = TRUE),
          avg_hay_a = mean(hay_a, na.rm = TRUE),
          avg_soybean_a = mean(soybean_a, na.rm = TRUE),
          avg_wheat_a = mean(wheat_a, na.rm = TRUE))

cropdat$corn <- (cropdat$corn_grain_p/cropdat$avg_corn_a)*cropdat$corn_rprice
cropdat$cotton <- (cropdat$cotton_p/cropdat$avg_cotton_a)*cropdat$cotton_rprice
cropdat$hay <- (cropdat$hay_p/cropdat$avg_hay_a)*cropdat$hay_rprice
cropdat$soybean <- (cropdat$soybean_p/cropdat$avg_soybean_a)*cropdat$soybean_rprice
cropdat$wheat <- (cropdat$wheat_p/cropdat$avg_wheat_a)*cropdat$wheat_rprice

 
# Total Activity
# cropdat$corn <- cropdat$corn_yield*cropdat$corn_rprice
# cropdat$cotton <- cropdat$cotton_yield*cropdat$cotton_rprice
# cropdat$hay <- cropdat$hay_yield*cropdat$hay_rprice
# cropdat$wheat <- cropdat$wheat_yield*cropdat$wheat_rprice
# cropdat$soybean <- cropdat$soybean_yield*cropdat$soybean_rprice

cropdat$rev <- rowSums(cropdat[, c("corn", "cotton", "hay", "soybean", "wheat")], na.rm = TRUE)
cropdat$acres <- rowSums(cropdat[, c("corn_grain_a", "cotton_a", "hay_a", "soybean_a", "wheat_a")], na.rm = TRUE)
cropdat$yield <- rowSums(cropdat[, c("corn_yield", "cotton_yield", "hay_yield", "soybean_yield", "wheat_yield")], na.rm = TRUE)
cropdat$trend <- cropdat$year - 1949
head(cropdat$acres)

cropdat$ln_rev <- log(1 + cropdat$rev)
cropdat$ln_acres <- log(1 + cropdat$acres)
cropdat$prec_sq <- cropdat$prec^2

# Proportion of crop acres as total of harvested_farmland_a
cropdat$p_corn_a <- cropdat$corn_grain_a/cropdat$cropland_a
cropdat$p_cotton_a <- cropdat$cotton_a/cropdat$cropland_a
cropdat$p_hay_a <- cropdat$hay_a/cropdat$cropland_a
cropdat$p_soybean_a <- cropdat$soybean_a/cropdat$cropland_a
cropdat$p_wheat_a <- cropdat$wheat_a/cropdat$cropland_a

cropdat$p_corn_a <- ifelse(is.infinite(cropdat$p_corn_a), NA, cropdat$p_corn_a)
cropdat$p_cotton_a <- ifelse(is.infinite(cropdat$p_cotton_a), NA, cropdat$p_cotton_a)
cropdat$p_hay_a <- ifelse(is.infinite(cropdat$p_hay_a), NA, cropdat$p_hay_a)
cropdat$p_soybean_a <- ifelse(is.infinite(cropdat$p_soybean_a), NA, cropdat$p_soybean_a)
cropdat$p_wheat_a <- ifelse(is.infinite(cropdat$p_wheat_a), NA, cropdat$p_wheat_a)

# Find warmest counties
dat1950 <- filter(cropdat, year >= 1950 & year <= 1979)
dat1950 <- dat1950 %>% 
  group_by(state, fips) %>% 
  summarise(dday30C_1950 = mean(dday30C, na.rm = TRUE))

dat2000 <- filter(cropdat, year >= 1980 & year <= 2009)
dat2000 <- dat2000 %>% 
  group_by(state, fips) %>% 
  summarise(dday30C_2000 = mean(dday30C, na.rm = TRUE))

dat <- left_join(dat1950, dat2000, by = c("state", "fips"))
dat$tdiff <- dat$dday30C_2000 - dat$dday30C_1950

diff <- arrange(dat, -tdiff)
head(diff)

# Split into thirds by state
diff <- diff %>% 
   group_by(state) %>% 
   mutate(thirds = dplyr::ntile(tdiff, 3))

spdiff <- filter(diff, thirds == 3) # Warmest
wfips <- spdiff$fips

tpdiff <- filter(diff, thirds == 1) # Coolest
cfips <- tpdiff$fips

moddat1 <- filter(cropdat, fips %in% wfips)
moddat1$type <- "Counties that warmed the most"
moddat2 <- filter(cropdat, fips %in% cfips)
moddat2$type <- "Counties that cooled the most"

moddat1$omega <- 1
moddat2$omega <- 0

moddat1 <- moddat1 %>% 
  mutate(trend_dday30C = dday30C - mean(dday30C, na.rm = TRUE))
mod1 <- lm(dday30C ~ trend, data = moddat1)
moddat1$lm_dday30C <- mod1$coefficients[2]*moddat1$trend

moddat2 <- moddat2 %>% 
  mutate(trend_dday30C = dday30C - mean(dday30C, na.rm = TRUE))
mod2 <- lm(dday30C ~ trend, data = moddat2)
moddat2$lm_dday30C <- mod2$coefficients[2]*moddat2$trend

moddat <- rbind(moddat1, moddat2)
head(moddat)

# moddat <- moddat %>% 
#   group_by(state) %>% 
#   mutate(state_trend = year - 1949)
head(moddat)
state_trend <- dummyCreator(moddat$state, prefix = "state")
state_trend <- state_trend*moddat$trend
cbind(moddat, state_trend)

moddat$tau <- ifelse(moddat$year >= 1980, 1, 0)

moddat$did <- moddat$tau*moddat$omega
#moddat$state_trend <- as.numeric(factor(moddat$state, levels = unique(moddat$state)))*moddat$trend

# Hand computer data
a = sapply(subset(moddat, tau == 0 & omega == 0, select = rev), mean)
b = sapply(subset(moddat, tau == 0 & omega == 1, select = rev), mean)
c = sapply(subset(moddat, tau == 1 & omega == 0, select = rev), mean)
d = sapply(subset(moddat, tau == 1 & omega == 1, select = rev), mean)

# average difference
(d - c) - (b - a)
# -10.88443

# percentage difference
((d - c) / c)* 100 - ((b - a) / a)*100
#-3.037302

moddat$type <- factor(moddat$type, levels = c("Counties that warmed the most", "Counties that cooled the most"),
                      labels = c("Counties that warmed the most", "Counties that cooled the most"))

moddat$omega <- factor(moddat$omega, levels = c(1, 0),
                      labels = c("Counties that warmed the most", "Counties that cooled the most"))

moddat$pre <- ifelse(moddat$year >= 1980, moddat$rev, NA)

pdat <- moddat %>% 
  group_by(year, omega) %>% 
  summarise(rev = mean(rev, na.rm = TRUE),
            yield = mean(yield, na.rm =))

ggplot(pdat, aes(year, rev, color = type, group = omega)) + 
  geom_line(pdat = moddat, aes(year, rev, color = omega)) +
  #geom_smooth(method='lm',formula=y~rcs(x, 5)) + 
  theme_tufte(base_size = 12) +
  geom_hline(yintercept = 0, color = "grey") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlab(NULL) + ylab("Crop Revenue per Acre") +
  theme(legend.position = c(0,1), 
        legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.key = element_blank(),
        legend.title = element_blank()) 
  
ggplot(moddat, aes(year, yield, color = factor(type))) + 
  geom_smooth(method='lm',formula=y~rcs(x, 5)) + 
  theme_tufte(base_size = 12) +
  geom_hline(yintercept = 0, color = "grey") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlab(NULL) + ylab("Crop Revenue per Acre") +
  theme(legend.position = c(0,1), 
        legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.key = element_blank(),
        legend.title = element_blank()) 

mod0 <- felm(ln_rev ~ tau + omega + tau + did, data = moddat)
summary(mod0)

mod1 <- felm(ln_rev ~ tau + omega + tau + did | fips | 0 | state, data = moddat)
summary(mod1)

mod2 <- felm(ln_rev ~ state_trend + omega + tau + did  | fips | 0 | state, data = moddat)
summary(mod2)

mod3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | 0 | 0 | 0, data = moddat)
summary(mod3)

mod4 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | state, data = moddat)
summary(mod4)

mod5 <- felm(ln_rev ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | state, data = moddat)
 
saveRDS(mod0, "models/dd_mod0.rds")
saveRDS(mod1, "models/dd_mod1.rds")
saveRDS(mod2, "models/dd_mod2.rds")
saveRDS(mod3, "models/dd_mod3.rds")
saveRDS(mod4, "models/dd_mod4.rds")
saveRDS(mod5, "models/dd_mod5.rds")

# Trend treatment effect
mod <- lm(dday30C ~ trend, data = moddat)
moddat$lm_dday30C <- moddat$trend*mod$coefficients[2]

moddat$omega <- moddat$omega
moddat$tau <- moddat$lm_dday30C
moddat$did <- moddat$omega*moddat$tau

moda <- felm(ln_rev ~ omega + tau + did  | 0 | 0 | 0, data = moddat)

modb <- felm(ln_rev ~ omega + tau + did  | fips | 0 | state, data = moddat)

modc <- felm(ln_rev ~ state_trend + omega + tau + did  | fips | 0 | state, data = moddat)

modd <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | 0 | 0 | 0, data = moddat)

mode <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | state, data = moddat)

modf <- felm(ln_rev ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | state, data = moddat)

saveRDS(moda, "models/dd_moda.rds")
saveRDS(modb, "models/dd_modb.rds")
saveRDS(modc, "models/dd_modc.rds")
saveRDS(modd, "models/dd_modd.rds")
saveRDS(mode, "models/dd_mode.rds")
saveRDS(modf, "models/dd_modf.rds")

summary(mod)

source("R/predictFelm.R")
 
graphplot1 <- data.frame(year = c("1950-1980", "1950-1980", "1950-1980", "1980-2010", "1980-2010", "1980-2010"),
           temp = c("C", "W", "T", "C", "W", "T"),
           rev = c(143.39, 137.59, 137.59, 235.61, 218, 229.81),
           se = c(0.543, 0.54, 0, 0.853, 0.873, 0))
graphplot1$temp <- factor(graphplot1$temp, labels = c("Cooled the most", "Counterfactual", "Warmed the most"))

ggplot(graphplot1, aes(year, rev, color = temp, group = temp, linetype = temp)) + 
  theme_tufte(base_size = 14) +
  #geom_line(aes(x = year, y = rev + 1.96*se, color = temp, group = temp), linetype = "dashed", alpha = 0.3) +
  #geom_line(aes(x = year, y = rev - 1.96*se, color = temp, group = temp), linetype = "dashed", alpha = 0.3) +
  geom_point() + 
  geom_line() + 
  ylim(100,250) + 
  xlab(NULL) +
  ylab("Revenue per Acre") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = c(0,1), legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"),
        legend.title = element_blank()) + 
  scale_color_manual(values = c("blue", "grey", "red"),
                     guide = guide_legend(override.aes = list(
                       shape = NA,
                     size = 2))) +
  scale_linetype_manual(values = c("solid", "dashed", "solid"))
  
graphplot2 <- data.frame(year = c("1950-1980", "1950-1980", "1950-1980", "1980-2010", "1980-2010", "1980-2010"),
           temp = c("C", "W", "T", "C", "W", "T"),
           yield = c(69.36, 67.28, 67.28, 116.46, 108.60, 114.38),
           se = c(0.543, 0.54, 0, 0.853, 0.873, 0))
graphplot2$temp <- factor(graphplot2$temp, labels = c("Cooled the most", "Counterfactual", "Warmed the most"))

ggplot(graphplot2, aes(year, yield, color = temp, group = temp, linetype = temp)) + 
  theme_tufte(base_size = 14) +
  #geom_line(aes(x = year, y = rev + 1.96*se, color = temp, group = temp), linetype = "dashed", alpha = 0.3) +
  #geom_line(aes(x = year, y = rev - 1.96*se, color = temp, group = temp), linetype = "dashed", alpha = 0.3) +
  geom_point() + 
  geom_line() + 
  ylim(50,150) + 
  xlab(NULL) +
  ylab("Crop Acres") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = c(0,1), legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"),
        legend.title = element_blank()) + 
  scale_color_manual(values = c("blue", "grey", "red"),
                     guide = guide_legend(override.aes = list(
                       shape = NA,
                     size = 2))) +
  scale_linetype_manual(values = c("solid", "dashed", "solid"))

graphplot3 <- 


#moddat$did <- moddat$tau*moddat$trend

#moddat$state <- factor(moddat$state) 

moddat <- cbind(moddat, rtomega)
dat <- moddat
dbdist <- datadist(dat)
options("datadist" = "dbdist")

mod3 <- ols(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
                 rcs(trend, 7) + omega + rtomega, data = dat, x = TRUE, y = TRUE)
mod3
pa <- Predict(mod3, "omega")
plot(pa)
plot(y = pa$yhat, x = pa$trend)

pa$yhat
plot(pa)

summary(mod3) 
predict(mod3, rcs(trend))

pa <- predict.felm(mod3, newdata = moddat)

a <- predict.lfe(mod3)
a$pred_data$
plot(y = a$fit, x = moddat$tau_trend)

# Crop Acre Regressions  

# Corn
modc1 <- felm(log(corn_grain_a) ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
                state_trend + tau + omega + did | state | 0 | state, data = moddat)
summary(modc1) 

modc2 <- felm(p_corn_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               state_trend + tau + omega + did, data = moddat)
summary(modc2)

modco1 <- felm(log(cotton_a) ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               state_trend + tau + omega + did | state | 0 | state, data = moddat)
summary(moda2) 

modco2 <- felm(p_cotton_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               state_trend + tau + omega + did | state | 0 | state, data = moddat)

modh1 <- felm(log(hay_a) ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               state_trend + tau + omega + did | state | 0 | state, data = moddat)
summary(moda2) 

modh2 <- felm(p_hay_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               state_trend + tau + omega + did | state | 0 | state, data = moddat)

mods1 <- felm(log(soybean_a) ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               state_trend + tau + omega + did | state | 0 | state, data = moddat)
summary(moda2) 

mods2 <- felm(p_soybean_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               state_trend + tau + omega + did | state | 0 | state, data = moddat)

modw1 <- felm(log(wheat_a) ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               state_trend + tau + omega + did | state | 0 | state, data = moddat)
summary(moda2) 

modw2 <- felm(p_wheat_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               state_trend + tau + omega + did | state | 0 | state, data = moddat)
 
 
 
saveRDS(modc1, "models/dd_modc1.rds")
saveRDS(modc2, "models/dd_modc2.rds")
saveRDS(modco1, "models/dd_modco1.rds")
saveRDS(modco2, "models/dd_modco2.rds")
saveRDS(modh1, "models/dd_modh1.rds")
saveRDS(modh2, "models/dd_modh2.rds")
saveRDS(mods1, "models/dd_mods1.rds")
saveRDS(mods2, "models/dd_mods2.rds")
saveRDS(modw1, "models/dd_modw1.rds")
saveRDS(modw2, "models/dd_modw2.rds")
 
 
mod1 <- p_corn_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               state_trend + tau + omega + did
mod2 <- p_cotton_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               state_trend + tau + omega + did
mod3 <- p_hay_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               state_trend + tau + omega + did
mod4 <- p_soybean_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               state_trend + tau + omega + did
mod5 <- p_wheat_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               state_trend + tau + omega + did
 
modlist <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5)
modlist <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5)
sysfit <- systemfit(modlist, data = moddat) 
print(sysfit) 
summary(sysfit)
 
 
 
 
 
 
 

##################

mod0 <- felm(ln_acres ~ tau + omega + tau + did, data = moddat)
summary(mod0)

mod1 <- felm(ln_rev ~ tau + omega + tau + did + state_trend | fips, data = moddat)

#mod2 <- felm(ln_rev ~ tau + omega + tau + did | fips + year, data = moddat)

mod2 <- felm(ln_acres ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + omega + tau + did | 0 | 0 | state, data = moddat)
summary(mod2)

mod3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + omega + tau + did + state_trend | fips, data = moddat)

mod4 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
              tau + omega + tau + did | fips + year, data = moddat)

# summary(mod1)
# summary(mod2)
# summary(mod3)
# summary(mod4)



 
#------------------------------------
# Without adaptation for warmest counties
setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, year < 2010)
cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

cropdat <- filter(cropdat, fips %in% wfips)

#cropdat <- filter(cropdat, state == "wi")
#cropdat

# # Constant prices
cropdat$corn_rprice <- mean(cropdat$corn_rprice, na.rm = TRUE)
cropdat$cotton_rprice <- mean(cropdat$cotton_rprice, na.rm = TRUE)
cropdat$hay_rprice <- mean(cropdat$hay_rprice, na.rm = TRUE)
cropdat$wheat_rprice <- mean(cropdat$wheat_rprice, na.rm = TRUE)
cropdat$soybean_rprice <- mean(cropdat$soybean_rprice, na.rm = TRUE)

cropdat <- cropdat %>% 
   group_by(fips) %>% 
   mutate(avg_corn_a = mean(corn_grain_a, na.rm = TRUE),
          avg_cotton_a = mean(cotton_a, na.rm = TRUE),
          avg_hay_a = mean(hay_a, na.rm = TRUE),
          avg_soybean_a = mean(soybean_a, na.rm = TRUE),
          avg_wheat_a = mean(wheat_a, na.rm = TRUE))
 
# Total Activity
cropdat$corn <- cropdat$corn_yield*cropdat$corn_rprice
cropdat$cotton <- cropdat$cotton_yield*cropdat$cotton_rprice
cropdat$hay <- cropdat$hay_yield*cropdat$hay_rprice
cropdat$wheat <- cropdat$wheat_yield*cropdat$wheat_rprice
cropdat$soybean <- cropdat$soybean_yield*cropdat$soybean_rprice

set.seed(234)
cropdat$rand <- 1

rfips <- sample(unique(cropdat$fips), size = length(unique(cropdat$fips))/2)

cropdat$rand <- ifelse(cropdat$fips %in% rfips, 0, 1)

cropdat$corn <- ifelse(cropdat$rand == 0, (cropdat$corn_grain_p/cropdat$avg_corn_a)*cropdat$corn_rprice, cropdat$corn)
cropdat$cotton <- ifelse(cropdat$rand == 0, (cropdat$cotton_p/cropdat$avg_cotton_a)*cropdat$cotton_rprice, cropdat$cotton)
cropdat$hay <- ifelse(cropdat$rand == 0, (cropdat$hay_p/cropdat$avg_hay_a)*cropdat$hay_rprice, cropdat$hay)
cropdat$soybean <- ifelse(cropdat$rand == 0, (cropdat$soybean_p/cropdat$avg_soybean_a)*cropdat$soybean_rprice, cropdat$soybean)
cropdat$wheat <- ifelse(cropdat$rand == 0, (cropdat$wheat_p/cropdat$avg_wheat_a)*cropdat$wheat_rprice, cropdat$wheat)

cropdat$rev <- rowSums(cropdat[, c("corn", "cotton", "hay", "soybean", "wheat")], na.rm = TRUE)
cropdat$ln_rev <- log(1 + cropdat$rev)
cropdat$prec_sq <- cropdat$prec^2

moddat <- cropdat

moddat$tau <- ifelse(moddat$year >= 1980, 1, 0)
moddat$omega <- moddat$rand

moddat$did <- moddat$tau*moddat$omega
moddat$trend <- moddat$year - 1949
moddat$state_trend <- as.numeric(factor(moddat$state, levels = unique(moddat$state)))*moddat$trend

# Hand computer data
a = sapply(subset(moddat, tau == 0 & omega == 0, select = rev), mean)
b = sapply(subset(moddat, tau == 0 & omega == 1, select = rev), mean)
c = sapply(subset(moddat, tau == 1 & omega == 0, select = rev), mean)
d = sapply(subset(moddat, tau == 1 & omega == 1, select = rev), mean)

# average difference
(d - c) - (b - a)
# -11.61372 

# percentage difference
((d - c) / c)* 100 - ((b - a) / a)*100
#-7.990657 

moddat$type <- ifelse(moddat$rand == 0, "No Adaptation", "Adaptation")
#moddat$type <- factor(moddat$type, levels = c("No Adaptation", "Adaptation"),
#                      labels = c("No Adaptation", "Adaptation"))

moddat$pre <- ifelse(moddat$year >= 1980, moddat$rev, NA)

ggplot(moddat, aes(year, rev, color = factor(type))) + 
  geom_smooth(method='lm',formula=y~x) + 
  theme_tufte(base_size = 12) +
  geom_hline(yintercept = 0, color = "grey") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlab(NULL) + ylab("Crop Revenue per Acre") +
  theme(legend.position = c(0,1), 
        legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.key = element_blank(),
        legend.title = element_blank()) 
  

f <- function (dat, model) {
   cropdat <- dat
   cropdat$rand <- 1
   
   rfips <- sample(unique(cropdat$fips), size = length(unique(cropdat$fips))/2)
   
   cropdat$rand <- ifelse(cropdat$fips %in% rfips, 0, 1)
   
  cropdat$corn <- ifelse(cropdat$rand == 0, (cropdat$corn_grain_p/cropdat$avg_corn_a)*cropdat$corn_rprice, cropdat$corn)
  cropdat$cotton <- ifelse(cropdat$rand == 0, (cropdat$cotton_p/cropdat$avg_cotton_a)*cropdat$cotton_rprice, cropdat$cotton)
  cropdat$hay <- ifelse(cropdat$rand == 0, (cropdat$hay_p/cropdat$avg_hay_a)*cropdat$hay_rprice, cropdat$hay)
  cropdat$soybean <- ifelse(cropdat$rand == 0, (cropdat$soybean_p/cropdat$avg_soybean_a)*cropdat$soybean_rprice, cropdat$soybean)
  cropdat$wheat <- ifelse(cropdat$rand == 0, (cropdat$wheat_p/cropdat$avg_wheat_a)*cropdat$wheat_rprice, cropdat$wheat)

  cropdat$rev <- rowSums(cropdat[, c("corn", "cotton", "hay", "soybean", "wheat")], na.rm = TRUE)
  moddat <- cropdat
  
  moddat$tau <- ifelse(moddat$year >= 1980, 1, 0)
  moddat$omega <- moddat$rand
  
  moddat$did <- moddat$tau*moddat$omega
  moddat$trend <- moddat$year - 1949
  moddat$state_trend <- as.numeric(factor(moddat$state, levels = unique(moddat$state)))*moddat$trend
  # fit <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
  #              omega + tau + did | state | 0 | state, data = moddat, subset = sample(nrow(moddat), 
  #                                                                                    nrow(moddat)/2, 
  #                                                                                    replace = TRUE))
  if(model == 1){
  fit <- felm(ln_rev ~ omega + tau + did, data = moddat)
  return(coef(fit))
  }
  
  if(model == 2){
  fit <- felm(ln_rev ~ omega + tau + did | state | 0 | state, data = moddat)
  return(coef(fit))
  }
  
  if(model == 3){
  fit <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did, data = moddat)
  return(coef(fit))
  }
  
  if(model == 4){
  fit <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did | state | 0 | state, data = moddat)
  return(coef(fit))
  }
  
}

# Basic Model
z1 <- t(replicate(1000, f(cropdat, model = 1)))

bs_mod1 <- lm(ln_rev ~ omega + tau + did, data = moddat)
for (i in 1:length(bs_mod1$coefficients)){
  bs_mod1$coefficients[i] <- mean(z1[, i])
  bs_mod1$se[i] <- sd(z1[, i])
}

# Basic Model w/ Fixed Effect and cluster s.e.
z2 <- t(replicate(1000, f(cropdat, model = 2)))

bs_mod2 <- lm(ln_rev ~ omega + tau + did - 1, data = moddat)

for (i in 1:length(bs_mod2$coefficients)){
  bs_mod2$coefficients[i] <- mean(z2[, i])
  bs_mod2$se[i] <- sd(z2[, i])
}

# Climate Model
z3 <- t(replicate(1000, f(cropdat, model = 3)))

bs_mod3 <- lm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did, data = moddat)
for (i in 1:length(bs_mod3$coefficients)){
  bs_mod3$coefficients[i] <- mean(z3[, i])
  bs_mod3$se[i] <- sd(z3[, i])
}

# Climate Model with fixed effect and clustered s.e.
z4 <- t(replicate(1000, f(cropdat, model = 4)))

bs_mod4 <- lm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did - 1, data = moddat)
for (i in 1:length(bs_mod4$coefficients)){
  bs_mod4$coefficients[i] <- mean(z4[, i])
  bs_mod4$se[i] <- sd(z4[, i])
}


bs_mod1
bs_mod2
bs_mod3
bs_mod4

saveRDS(bs_mod1, "models/bs_mod1.rds")
saveRDS(bs_mod2, "models/bs_mod2.rds")
saveRDS(bs_mod3, "models/bs_mod3.rds")
saveRDS(bs_mod4, "models/bs_mod4.rds")
 
summary(bs_mod1)
coef(bs_mod1)
stargazer(bs_mod1)
mod0 <- felm(ln_rev ~ omega + tau + did, data = moddat)
summary(mod0)

mod1 <- felm(ln_rev ~ omega + tau + did | state | 0 | state | fips, data = moddat)
summary(mod1)

mod2 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did | state | 0 | state, data = moddat)
summary(mod2)

mod3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + omega + tau + did + state_trend | state | 0 | state, data = moddat)
summary(mod3)

mod4 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
              tau + omega + tau + did | fips + year, data = moddat)

summary(mod0)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)


saveRDS(mod0, "models/dd_mod0.rds")
saveRDS(mod1, "models/dd_mod1.rds")
saveRDS(mod2, "models/dd_mod2.rds")
saveRDS(mod3, "models/dd_mod3.rds")
saveRDS(mod4, "models/dd_mod4.rds")
 
 