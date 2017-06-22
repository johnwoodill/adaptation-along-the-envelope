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
cropdat$corn_rev <- (cropdat$corn_grain_p*cropdat$corn_rprice)
cropdat$cotton_rev <- (cropdat$cotton_p*cropdat$cotton_rprice)
cropdat$hay_rev <- (cropdat$hay_p*cropdat$hay_rprice)
cropdat$wheat_rev <- (cropdat$wheat_p*cropdat$wheat_rprice)
cropdat$soybean_rev <- (cropdat$soybean_p*cropdat$soybean_rprice)

cropdat$total_rev <- cropdat$corn_rev + cropdat$cotton_rev + cropdat$hay_rev + cropdat$wheat_rev + cropdat$soybean_rev
cropdat$total_a <- cropdat$corn_grain_a + cropdat$cotton_a + cropdat$hay_a + cropdat$wheat_a + cropdat$soybean_a

cropdat$prob_corn <- cropdat$corn_rev/sum(cropdat$corn_rev, na.rm = TRUE)

l7 <- glm(prob_corn ~ tavg + prec, data = cropdat, family = "quasibinomial")

logitdat <- cropdat %>% 
  group_by(year) %>% 
  mutate(prob_corn = corn_rev/sum(corn_rev,na.rm = TRUE))

l8 <- glm(prob_corn ~ tavg, data = logitdat, family = "quasibinomial")

ggplot(cropdat, aes(year, prob_corn)) + geom_smooth()

stargazer(l1, l6, l7, align = TRUE, no.space = TRUE, digits = 2,  report = "vc*", 
          omit = "ffips", omit.stat = c("ser", "f"), title = "Regression Models explaining Crop Revenue (weighted by crop acreage)", dep.var.labels = c("Log(Corn Rev)", "Log(Cotton Rev)", "Log(Hay Rev)", "Log(Wheat Rev)", "Log(Soybean Rev)")
          )
