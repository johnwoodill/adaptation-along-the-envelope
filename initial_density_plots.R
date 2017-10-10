library(tidyverse)
library(ggthemes)
library(cowplot)
library(choroplethr)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

source("R/densityShare.R")

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

cropdat <- readRDS("data/full_ag_data.rds")
cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

# Total Activity
cropdat$corn <- cropdat$corn_yield*cropdat$corn_grain_a
cropdat$cotton <- cropdat$cotton_yield*cropdat$cotton_a
cropdat$hay <- cropdat$hay_yield*cropdat$hay_a
cropdat$wheat <- cropdat$wheat_yield*cropdat$wheat_a
cropdat$soybean <- cropdat$soybean_yield*cropdat$soybean_a

# Crop Acres
# cropdat$corn <- cropdat$corn_grain_a
# cropdat$cotton <- cropdat$cotton_a
# cropdat$hay <- cropdat$hay_a
# cropdat$wheat <- cropdat$wheat_a
# cropdat$soybean <- cropdat$soybean_a


# Yield per acre
# cropdat$corn <- cropdat$corn_yield
# cropdat$cotton <- cropdat$cotton_yield
# cropdat$hay <- cropdat$hay_yield
# cropdat$wheat <- cropdat$wheat_yield
# cropdat$soybean <- cropdat$soybean_yield

# cropdat <- filter(cropdat, !is.na(corn) | !is.na(cotton) | !is.na(hay) | !is.na(wheat) | !is.na(soybean) |
#                     !is.na(tavg))

dat1 <- filter(cropdat, year >= 1950 & year <= 1979)
dat1 <- dat1 %>% 
  group_by(fips) %>% 
  summarise(dday30C = mean(dday30C, na.rm = TRUE),
            dday10_30 = mean(dday10_30, na.rm = TRUE),
            Corn = mean(corn, na.rm = TRUE),
            Cotton = mean(cotton, na.rm = TRUE),
            Hay = mean(hay, na.rm = TRUE),
            Wheat = mean(wheat, na.rm = TRUE),
            Soybean = mean(soybean, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE))

dat2 <- filter(cropdat, year >= 1980 & year <= 2009)
dat2 <- dat2 %>% 
  filter(fips %in% unique(dat1$fips)) %>% 
  group_by(fips) %>% 
  summarise(dday30C = mean(dday30C, na.rm = TRUE),
            dday10_30 = mean(dday10_30, na.rm = TRUE),
            Corn = mean(corn, na.rm = TRUE),
            Cotton = mean(cotton, na.rm = TRUE),
            Hay = mean(hay, na.rm = TRUE),
            Wheat = mean(wheat, na.rm = TRUE),
            Soybean = mean(soybean, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE))

dat1 <- arrange(dat1, fips)
dat2 <- arrange(dat2, fips)

diff <- data.frame(fips = dat1$fips, 
                   diff30 = dat2$dday30C - dat1$dday30C,
                   diff10_30 = dat2$dday10_30 - dat1$dday10_30,
                   difftavg = dat2$tavg - dat1$tavg)

#diff <- arrange(diff, -diff30)
diff <- arrange(diff, -difftavg)
head(diff)


# Split into thirds
#diff$thirds <- ntile(diff$diff30, 3)
diff$thirds <- ntile(diff$difftavg, 3)
spdiff1 <- filter(diff, thirds == 1) # Coolest
spdiff2 <- filter(diff, thirds == 3) # Warmest

fips1 <- spdiff1$fips
fips2 <- spdiff2$fips

# Subset new data warmest counties
wdat1 <- filter(dat1, fips %in% fips2)
wdat1 <- select(wdat1, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
wdat1 <- gather(wdat1, key = crop, value = value, -tavg, -fips)
wdat1 <- filter(wdat1, !is.na(tavg) & !is.na(value))

wdat2 <- filter(dat2, fips %in% fips2)
wdat2 <- select(wdat2, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
wdat2 <- gather(wdat2, key = crop, value = value, -tavg, -fips)
wdat2 <- filter(wdat2, !is.na(tavg) & !is.na(value))

wplot1 <- densityShare(wdat1, "tavg", "value") +  theme(legend.position = c(0,1), 
        legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.key = element_blank(),
        legend.title = element_blank()) + 
  xlab("Average Temperature (C)") + ylab("Crop Activity \n (Production)") +
  annotate("text", x = 21, y = .2, label = "1950-1980", size = 6) + ylim(0, 0.2) + xlim(8, 25)
wplot1

wplot2 <- densityShare(wdat2, "tavg", "value") + theme(legend.position = "none") +
   xlab("Average Temperature (C)") + annotate("text", x = 21, y = .2, label = "1980-2010", size = 6) +
   ylim(0, 0.2)+ xlim(8, 25)
wplot2

mdat1 <- filter(dat1, fips %in% fips2)
mdat1 <- select(mdat1, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
mdat1 <- gather(mdat1, key = crop, value = value, -tavg, -fips)

mdat2 <- filter(dat2, fips %in% fips2)
mdat2 <- select(mdat2, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
mdat2 <- gather(mdat2, key = crop, value = value, -tavg, -fips)

mergedat <- data.frame(fips = mdat1$fips,
                       crop = mdat1$crop,
                       tavg2 = mdat2$tavg,
                       tavg1 = mdat1$tavg,
                       value1 = mdat1$value,
                       value2 = mdat2$value)

mergedat$tavgdiff = mergedat$tavg2 - mergedat$tavg1
mergedat$tavgdiff <- ifelse(is.na(mergedat$value1), NA, mergedat$tavgdiff)
mergedat$tavgdiff <- ifelse(is.na(mergedat$value2), NA, mergedat$tavgdiff)

# Remove outliers for boxplot
tuftedata <- mergedat %>% 
  group_by(crop) %>% 
  mutate(tavgdiff = remove_outliers(tavgdiff))

# Get outliers for boxplot
diff <- filter(mergedat, !tavgdiff %in% tuftedata$tavgdiff)
head(diff)

cpercent <- mergedat %>% 
  group_by(crop) %>% 
  summarise(change = (sum(value2, na.rm = TRUE) - sum(value1, na.rm = TRUE))/sum(value1, na.rm = TRUE))
cpercent

#plot(density(mergdat$tdiff, na.rm = TRUE))
 # scale_fill_brewer(palette = "Dark2") +
  
p1 <- ggplot(mergedat, aes(y = tavgdiff, x = crop, color = crop)) +
  ggthemes::geom_tufteboxplot(data = tuftedata, median.type = "line", 
                              whisker.type = 'line', hoffset = 0, width = 3, size = 1, show.legend = FALSE) +
  geom_point(data = diff) +
  theme_tufte(base_size = 14) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab(NULL) + ylab("Change in \n Temperature (C)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = c(0, 1, 2), labels = c("0", "+1C", "+2C"))




p2 <- ggplot(cpercent, aes(y = change, x = crop)) + 
  geom_point(data = cpercent, aes(crop, change), color = c(ggplotColours(n = 5)), size = 1.5) +
  theme_tufte(base_size = 14) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlab(NULL) + 
  ylab("% Change in \n Production") + ylim(0, 2)
 

dplot <-plot_grid(p1, p2, ncol = 1)

plot_grid(wplot1, wplot2, dplot, ncol = 3) 

###################

# Subset new data coldest counties
cdat1 <- filter(dat1, fips %in% fips1)
cdat1 <- select(cdat1, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
cdat1 <- gather(cdat1, key = crop, value = value, -tavg, -fips)
cdat1 <- filter(cdat1, !is.na(tavg) & !is.na(value))

cdat2 <- filter(dat2, fips %in% fips1)
cdat2 <- select(cdat2, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
cdat2 <- gather(cdat2, key = crop, value = value, -tavg, -fips)
cdat2 <- filter(cdat2, !is.na(tavg) & !is.na(value))

wplot1 <- densityShare(cdat1, "tavg", "value") +  theme(legend.position = c(0,1), 
        legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.key = element_blank(),
        legend.title = element_blank()) + 
  xlab("Average Temperature (C)") + ylab("Crop Activity \n (Production)") +
  annotate("text", x = 21, y = .28, label = "1950-1980", size = 6) + ylim(0, 0.28) + xlim(10, 25)
wplot1

wplot2 <- densityShare(cdat2, "tavg", "value") + theme(legend.position = "none") +
   xlab("Average Temperature (C)") + annotate("text", x = 21, y = .28, label = "1980-2010", size = 6) +
   ylim(0, 0.28)+ xlim(10, 25)
wplot2

mdat1 <- filter(dat1, fips %in% fips1)
mdat1 <- select(mdat1, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
mdat1 <- gather(mdat1, key = crop, value = value, -tavg, -fips)

mdat2 <- filter(dat2, fips %in% fips1)
mdat2 <- select(mdat2, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
mdat2 <- gather(mdat2, key = crop, value = value, -tavg, -fips)

mergedat <- data.frame(fips = mdat1$fips,
                       crop = mdat1$crop,
                       tavg2 = mdat2$tavg,
                       tavg1 = mdat1$tavg,
                       value1 = mdat1$value,
                       value2 = mdat2$value)

mergedat$tavgdiff = mergedat$tavg2 - mergedat$tavg1
mergedat$tavgdiff <- ifelse(is.na(mergedat$value1), NA, mergedat$tavgdiff)
mergedat$tavgdiff <- ifelse(is.na(mergedat$value2), NA, mergedat$tavgdiff)

# Remove outliers for boxplot
tuftedata <- mergedat %>% 
  group_by(crop) %>% 
  mutate(tavgdiff = remove_outliers(tavgdiff))

# Get outliers for boxplot
diff <- filter(mergedat, !tavgdiff %in% tuftedata$tavgdiff)
head(diff)

cpercent <- mergedat %>% 
  group_by(crop) %>% 
  summarise(change = (sum(value2, na.rm = TRUE) - sum(value1, na.rm = TRUE))/sum(value1, na.rm = TRUE))
cpercent

#plot(density(mergdat$tdiff, na.rm = TRUE))
 # scale_fill_brewer(palette = "Dark2") +
  
p1 <- ggplot(mergedat, aes(y = tavgdiff, x = crop, color = crop)) +
  ggthemes::geom_tufteboxplot(data = tuftedata, median.type = "line", 
                              whisker.type = 'line', hoffset = 0, width = 3, size = 1, show.legend = FALSE) +
  geom_point(data = diff) +
  theme_tufte(base_size = 14) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab(NULL) + ylab("Change in \n Temperature (C)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = c(0, -0.5, -1), labels = c("0", "-0.5C", "-1C"))
p1



p2 <- ggplot(cpercent, aes(y = change, x = crop)) + 
  geom_point(data = cpercent, aes(crop, change), color = c(ggplotColours(n = 5)), size = 1.5) +
  theme_tufte(base_size = 14) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlab(NULL) + 
  ylab("% Change in \n Production") + ylim(-0.5, 2)
p2 

dplot <-plot_grid(p1, p2, ncol = 1)

plot_grid(wplot1, wplot2, dplot, ncol = 3)

###########################
###########################
# Crop Acres
cropdat$corn <- cropdat$corn_grain_a
cropdat$cotton <- cropdat$cotton_a
cropdat$hay <- cropdat$hay_a
cropdat$wheat <- cropdat$wheat_a
cropdat$soybean <- cropdat$soybean_a

dat1 <- filter(cropdat, year >= 1950 & year <= 1979)
dat1 <- dat1 %>% 
  group_by(fips) %>% 
  summarise(dday30C = mean(dday30C, na.rm = TRUE),
            dday10_30 = mean(dday10_30, na.rm = TRUE),
            Corn = mean(corn, na.rm = TRUE),
            Cotton = mean(cotton, na.rm = TRUE),
            Hay = mean(hay, na.rm = TRUE),
            Wheat = mean(wheat, na.rm = TRUE),
            Soybean = mean(soybean, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE))

dat2 <- filter(cropdat, year >= 1980 & year <= 2009)
dat2 <- dat2 %>% 
  filter(fips %in% unique(dat1$fips)) %>% 
  group_by(fips) %>% 
  summarise(dday30C = mean(dday30C, na.rm = TRUE),
            dday10_30 = mean(dday10_30, na.rm = TRUE),
            Corn = mean(corn, na.rm = TRUE),
            Cotton = mean(cotton, na.rm = TRUE),
            Hay = mean(hay, na.rm = TRUE),
            Wheat = mean(wheat, na.rm = TRUE),
            Soybean = mean(soybean, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE))

dat1 <- arrange(dat1, fips)
dat2 <- arrange(dat2, fips)

diff <- data.frame(fips = dat1$fips, 
                   diff30 = dat2$dday30C - dat1$dday30C,
                   diff10_30 = dat2$dday10_30 - dat1$dday10_30,
                   difftavg = dat2$tavg - dat1$tavg)

#diff <- arrange(diff, -diff30)
diff <- arrange(diff, -difftavg)
head(diff)


# Split into thirds
#diff$thirds <- ntile(diff$diff30, 3)
diff$thirds <- ntile(diff$difftavg, 3)
spdiff1 <- filter(diff, thirds == 1) # Coolest
spdiff2 <- filter(diff, thirds == 3) # Warmest

fips1 <- spdiff1$fips
fips2 <- spdiff2$fips

# Subset new data warmest counties
wdat1 <- filter(dat1, fips %in% fips2)
wdat1 <- select(wdat1, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
wdat1 <- gather(wdat1, key = crop, value = value, -tavg, -fips)
wdat1 <- filter(wdat1, !is.na(tavg) & !is.na(value))

wdat2 <- filter(dat2, fips %in% fips2)
wdat2 <- select(wdat2, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
wdat2 <- gather(wdat2, key = crop, value = value, -tavg, -fips)
wdat2 <- filter(wdat2, !is.na(tavg) & !is.na(value))

wplot1 <- densityShare(wdat1, "tavg", "value") +  theme(legend.position = c(0,1), 
        legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.key = element_blank(),
        legend.title = element_blank()) + 
  xlab("Average Temperature (C)") + ylab("Crop Acres") +
  annotate("text", x = 21, y = .28, label = "1950-1980", size = 6) + ylim(0, 0.28) + xlim(8, 25)
wplot1

wplot2 <- densityShare(wdat2, "tavg", "value") + theme(legend.position = "none") +
   xlab("Average Temperature (C)") + annotate("text", x = 21, y = .28, label = "1980-2010", size = 6) +
   ylim(0, 0.28)+ xlim(8, 25)
wplot2

mdat1 <- filter(dat1, fips %in% fips2)
mdat1 <- select(mdat1, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
mdat1 <- gather(mdat1, key = crop, value = value, -tavg, -fips)

mdat2 <- filter(dat2, fips %in% fips2)
mdat2 <- select(mdat2, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
mdat2 <- gather(mdat2, key = crop, value = value, -tavg, -fips)

mergedat <- data.frame(fips = mdat1$fips,
                       crop = mdat1$crop,
                       tavg2 = mdat2$tavg,
                       tavg1 = mdat1$tavg,
                       value1 = mdat1$value,
                       value2 = mdat2$value)

mergedat$tavgdiff = mergedat$tavg2 - mergedat$tavg1
mergedat$tavgdiff <- ifelse(is.na(mergedat$value1), NA, mergedat$tavgdiff)
mergedat$tavgdiff <- ifelse(is.na(mergedat$value2), NA, mergedat$tavgdiff)

# Remove outliers for boxplot
tuftedata <- mergedat %>% 
  group_by(crop) %>% 
  mutate(tavgdiff = remove_outliers(tavgdiff))

# Get outliers for boxplot
diff <- filter(mergedat, !tavgdiff %in% tuftedata$tavgdiff)
head(diff)

cpercent <- mergedat %>% 
  group_by(crop) %>% 
  summarise(change = (sum(value2, na.rm = TRUE) - sum(value1, na.rm = TRUE))/sum(value1, na.rm = TRUE))
cpercent

#plot(density(mergdat$tdiff, na.rm = TRUE))
 # scale_fill_brewer(palette = "Dark2") +
  
p1 <- ggplot(mergedat, aes(y = tavgdiff, x = crop, color = crop)) +
  ggthemes::geom_tufteboxplot(data = tuftedata, median.type = "line", 
                              whisker.type = 'line', hoffset = 0, width = 3, size = 1, show.legend = FALSE) +
  geom_point(data = diff) +
  theme_tufte(base_size = 14) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab(NULL) + ylab("Change in \n Temperature (C)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = c(0, 1, 2), labels = c("0", "+1C", "+2C"))




p2 <- ggplot(cpercent, aes(y = change, x = crop)) + 
  geom_point(data = cpercent, aes(crop, change), color = c(ggplotColours(n = 5)), size = 1.5) +
  theme_tufte(base_size = 14) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlab(NULL) + 
  ylab("% Change in \n Acres") + ylim(-0.5, 2)
 

dplot <-plot_grid(p1, p2, ncol = 1)

plot_grid(wplot1, wplot2, dplot, ncol = 3) + ggtitle("asdf")

###################

# Subset new data coldest counties
cdat1 <- filter(dat1, fips %in% fips1)
cdat1 <- select(cdat1, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
cdat1 <- gather(cdat1, key = crop, value = value, -tavg, -fips)
cdat1 <- filter(cdat1, !is.na(tavg) & !is.na(value))

cdat2 <- filter(dat2, fips %in% fips1)
cdat2 <- select(cdat2, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
cdat2 <- gather(cdat2, key = crop, value = value, -tavg, -fips)
cdat2 <- filter(cdat2, !is.na(tavg) & !is.na(value))

wplot1 <- densityShare(cdat1, "tavg", "value") +  theme(legend.position = c(0,1), 
        legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.key = element_blank(),
        legend.title = element_blank()) + 
  xlab("Average Temperature (C)") + ylab("Crop Acres") +
  annotate("text", x = 22, y = .2, label = "1950-1980", size = 6) + ylim(0, 0.2) + xlim(10, 25)
wplot1

wplot2 <- densityShare(cdat2, "tavg", "value") + theme(legend.position = "none") +
   xlab("Average Temperature (C)") + annotate("text", x = 22, y = .20, label = "1980-2010", size = 6) +
   ylim(0, 0.2)+ xlim(10, 25)
wplot2

mdat1 <- filter(dat1, fips %in% fips1)
mdat1 <- select(mdat1, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
mdat1 <- gather(mdat1, key = crop, value = value, -tavg, -fips)

mdat2 <- filter(dat2, fips %in% fips1)
mdat2 <- select(mdat2, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
mdat2 <- gather(mdat2, key = crop, value = value, -tavg, -fips)

mergedat <- data.frame(fips = mdat1$fips,
                       crop = mdat1$crop,
                       tavg2 = mdat2$tavg,
                       tavg1 = mdat1$tavg,
                       value1 = mdat1$value,
                       value2 = mdat2$value)

mergedat$tavgdiff = mergedat$tavg2 - mergedat$tavg1
mergedat$tavgdiff <- ifelse(is.na(mergedat$value1), NA, mergedat$tavgdiff)
mergedat$tavgdiff <- ifelse(is.na(mergedat$value2), NA, mergedat$tavgdiff)

# Remove outliers for boxplot
tuftedata <- mergedat %>% 
  group_by(crop) %>% 
  mutate(tavgdiff = remove_outliers(tavgdiff))

# Get outliers for boxplot
diff <- filter(mergedat, !tavgdiff %in% tuftedata$tavgdiff)
head(diff)

cpercent <- mergedat %>% 
  group_by(crop) %>% 
  summarise(change = (sum(value2, na.rm = TRUE) - sum(value1, na.rm = TRUE))/sum(value1, na.rm = TRUE))
cpercent

#plot(density(mergdat$tdiff, na.rm = TRUE))
 # scale_fill_brewer(palette = "Dark2") +
  
p1 <- ggplot(mergedat, aes(y = tavgdiff, x = crop, color = crop)) +
  ggthemes::geom_tufteboxplot(data = tuftedata, median.type = "line", 
                              whisker.type = 'line', hoffset = 0, width = 3, size = 1, show.legend = FALSE) +
  geom_point(data = diff) +
  theme_tufte(base_size = 14) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  xlab(NULL) + ylab("Change in \n Temperature (C)") +
  theme(legend.position = "none") +
    scale_y_continuous(breaks = c(0, -0.5, -1), labels = c("0", "-0.5C", "-1C"))
p1



p2 <- ggplot(cpercent, aes(y = change, x = crop)) + 
  geom_point(data = cpercent, aes(crop, change), color = c(ggplotColours(n = 5)), size = 1.5) +
  theme_tufte(base_size = 14) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlab(NULL) + 
  ylab("% Change in \n Acres") + ylim(-0.5, 2)
p2 

dplot <-plot_grid(p1, p2, ncol = 1)

plot_grid(wplot1, wplot2, dplot, ncol = 3) + ggtitle("asdf")

# mapdat <- data.frame(region = unique(wdat2$fips), value = 1)
# mapdat <- rbind(mapdat, data.frame(region = unique(wdat2$fips), value = 2))
# 
# map <- county_choropleth(mapdat,
#                  title      = NULL)
# map
