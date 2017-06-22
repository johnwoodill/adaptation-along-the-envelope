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

cropdat$trend <- cropdat$year - 1959
cropdat$trendsq <- cropdat$trend^2
cropdat$precsq <- cropdat$prec^2
cropdat$tavgsq <- cropdat$tavg^2
cropdat$ffips <- as.factor(cropdat$fips)
cropdat$fstate <- as.factor(cropdat$state)

l6 <- lm(log(1 + corn_rev) ~ ffips + tavg + tavgsq + trend + trendsq + prec + precsq, data = cropdat, weights = cropdat$corn_grain_a)
#l6s <- lm(log(1 + corn_rev) ~ fstate + tavg + tavgsq + trend + trendsq + prec + precsq, data = cropdat, weights = cropdat$corn_grain_a)
