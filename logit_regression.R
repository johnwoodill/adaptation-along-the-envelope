library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)
library(cowplot)

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

l.mod1 <- glm(p_corn_rrev ~ tavg + tavgsq + prec + precsq, data = cropdat, family = "quasibinomial")
summary(l.mod1)
l.mod1$coefficients*100*100

l.mod1 <- glm(p_corn_rrev ~ dday8_32 + I(dday8_32^2) + I(sqrt(dday34C)) + prec + precsq, data = cropdat, family = "quasibinomial")
summary(l.mod1)
l.mod1$coefficients*100




logitdat <- cropdat %>% 
  group_by(year) %>% 
  mutate(prob_corn = corn_rev/sum(corn_rev,na.rm = TRUE))

l8 <- glm(prob_corn ~ tavg, data = logitdat, family = "quasibinomial")

ggplot(cropdat, aes(year, prob_corn)) + geom_smooth()

stargazer(l1, l6, l7, align = TRUE, no.space = TRUE, digits = 2,  report = "vc*", 
          omit = "ffips", omit.stat = c("ser", "f"), title = "Regression Models explaining Crop Revenue (weighted by crop acreage)", dep.var.labels = c("Log(Corn Rev)", "Log(Cotton Rev)", "Log(Hay Rev)", "Log(Wheat Rev)", "Log(Soybean Rev)")
          )
