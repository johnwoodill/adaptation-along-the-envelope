library(tidyverse)
library(ggthemes)
library(cowplot)
library(choroplethr)
library(gridExtra)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

source("R/densityShare.R")

g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
}

million <- function(x) x/1000000

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

# Constant prices
cropdat$corn_rprice <- mean(cropdat$corn_rprice, na.rm = TRUE)
cropdat$cotton_rprice <- mean(cropdat$cotton_rprice, na.rm = TRUE)
cropdat$hay_rprice <- mean(cropdat$hay_rprice, na.rm = TRUE)
cropdat$wheat_rprice <- mean(cropdat$wheat_rprice, na.rm = TRUE)
cropdat$soybean_rprice <- mean(cropdat$soybean_rprice, na.rm = TRUE)

# Total Activity
cropdat$corn <- cropdat$corn_yield*cropdat$corn_grain_a*cropdat$corn_rprice
cropdat$cotton <- cropdat$cotton_yield*cropdat$cotton_a*cropdat$cotton_rprice
cropdat$hay <- cropdat$hay_yield*cropdat$hay_a*cropdat$hay_rprice
cropdat$wheat <- cropdat$wheat_yield*cropdat$wheat_a*cropdat$wheat_rprice
cropdat$soybean <- cropdat$soybean_yield*cropdat$soybean_a*cropdat$soybean_rprice



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
  group_by(state, fips) %>% 
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
  group_by(state, fips) %>% 
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
                   state = dat1$state,
                   diff30 = dat2$dday30C - dat1$dday30C,
                   diff10_30 = dat2$dday10_30 - dat1$dday10_30,
                   difftavg = dat2$tavg - dat1$tavg)

#diff <- arrange(diff, -diff30)
diff <- arrange(diff, -difftavg)
head(diff)


# Split into thirds
#diff$thirds <- ntile(diff$difftavg, 3)

# Split into thirds by state
 diff <- diff %>% 
   group_by(state) %>% 
   mutate(thirds = dplyr::ntile(difftavg, 3))


spdiff1 <- filter(diff, thirds == 1) # Coolest
spdiff2 <- filter(diff, thirds == 3) # Warmest

fips1 <- spdiff1$fips
fips2 <- spdiff2$fips

# Map of counties
mapdat <- data.frame(region = c(fips1, fips2), value = c(rep("Coolest", length(fips1)), rep("Warmest", length(fips2))))

states <- tolower(unique(state.name[match(cropdat$state, tolower(state.abb))]))

map <- county_choropleth(mapdat,
                  title      = NULL,
                  state_zoom = states)
map+ scale_fill_manual(values=c("#9ecae1", "#de2d26", "black"), breaks = c("Warmest", "Coolest"))  +
 #scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Sample Data") + ylab(NULL) + theme(legend.position = c(0,0),
                                    legend.justification = c("left", "bottom"),
                                    legend.title = element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     axis.ticks.y = element_blank(),
                                     panel.border = element_rect(fill = NA)) 

# Subset new data warmest counties
wdat1 <- filter(dat1, fips %in% fips2)
wdat1 <- select(wdat1, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
wdat1 <- gather(wdat1, key = crop, value = value, -tavg, -fips, -state)
wdat1 <- filter(wdat1, !is.na(tavg) & !is.na(value))
wdat1$value <- wdat1$value/1000000

wdat2 <- filter(dat2, fips %in% fips2)
wdat2 <- select(wdat2, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
wdat2 <- gather(wdat2, key = crop, value = value, -tavg, -fips, -state)
wdat2 <- filter(wdat2, !is.na(tavg) & !is.na(value))
wdat2$value <- wdat2$value/1000000

 # x = wdat1
 # variable = "tavg"
 # weight = "value"


wplot1 <- densityShare(wdat1, "tavg", "value")$plot + theme(legend.position = "none") +
  xlab("") + ylab("Value of Activity \n ($1 Million)") +
  xlim(8, 25) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
wplot1

# Legend
wplot1_legend <- densityShare(wdat1, "tavg", "value")$plot + theme(legend.position = c(0.45,1.1), 
        legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.key = element_blank(),
        legend.title = element_blank()) + 
  xlab("") + ylab("Crop Value of Activity \n ($1 Million)") +
  xlim(8, 25) 
legend <- g_legend(wplot1_legend)
plot(legend)


wplot2 <- densityShare(wdat2, "tavg", "value")$plot + theme(legend.position = "none") +
   xlab("Average Temperature (C)") +
   xlim(8, 25) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
wplot2

mdat1 <- filter(dat1, fips %in% fips2)
mdat1 <- select(mdat1, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
mdat1 <- gather(mdat1, key = crop, value = value, -tavg, -fips, -state)

mdat2 <- filter(dat2, fips %in% fips2)
mdat2 <- select(mdat2, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
mdat2 <- gather(mdat2, key = crop, value = value, -tavg, -fips, -state)

mergedat <- data.frame(fips = mdat1$fips,
                       crop = mdat1$crop,
                       tavg2 = mdat2$tavg,
                       tavg1 = mdat1$tavg,
                       value1 = mdat1$value,
                       value2 = mdat2$value,
                       diffvalue = mdat2$value - mdat1$value,
                       mtavg = (mdat1$tavg + mdat2$tavg)/2)

mergedat$tavgdiff = mergedat$tavg2 - mergedat$tavg1
mergedat$tavgdiff <- ifelse(is.na(mergedat$value1), NA, mergedat$tavgdiff)
mergedat$tavgdiff <- ifelse(is.na(mergedat$value2), NA, mergedat$tavgdiff)

# Difference distribution
wdens1 <- densityShare(wdat1, "tavg", "value")$densdata
wdens1 <- arrange(wdens1, crop, y)

wdens2 <- densityShare(wdat2, "tavg", "value")$densdata
wdens2 <- arrange(wdens2, crop, y)
wdens <- wdens1
wdens$x2 <- wdens2$x
wdens$y2 <- wdens2$y

wdens$ydiff <- wdens$y2 - wdens$y
wdens$dtavg <- (wdens$x + wdens$x2)/2

wdens_crop <- wdens %>% 
  group_by(crop) %>% 
  summarise(total = sum(ydiff)) %>% 
  arrange(-total)
wdens_crop    

wdensplot <- ggplot(NULL) + 
      geom_area(data = filter(wdens, crop == wdens_crop$crop[1]), aes(x2, ydiff, fill = wdens_crop$crop[1])) +
      geom_area(data = filter(wdens, crop == wdens_crop$crop[2]), aes(x2, ydiff, fill = wdens_crop$crop[2])) +
      geom_area(data = filter(wdens, crop == wdens_crop$crop[3]), aes(x2, ydiff, fill = wdens_crop$crop[3])) +
      geom_area(data = filter(wdens, crop == wdens_crop$crop[4]), aes(x2, ydiff, fill = wdens_crop$crop[4])) +
      geom_area(data = filter(wdens, crop == wdens_crop$crop[5]), aes(x2, ydiff, fill = wdens_crop$crop[5])) +
      xlab("") + ylab(NULL)+ theme_tufte(base_size = 14) +
      #scale_fill_discrete(breaks = c("Corn", "soybean", "hay", "wheat", "cotton")) + 
      theme(legend.position="none") + 
      theme(legend.title=element_blank()) +
        annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
        annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlim(8, 25)+ geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
wdensplot

# Remove outliers for boxplot
tuftedata <- mergedat %>% 
  group_by(crop) %>% 
  mutate(tavgdiff = remove_outliers(tavgdiff))

# Get outliers for boxplot
diff <- filter(mergedat, !tavgdiff %in% tuftedata$tavgdiff)
head(diff)

cpercent <- mergedat %>% 
  group_by(crop) %>% 
  summarise(change = (sum(value2, na.rm = TRUE) - sum(value1, na.rm = TRUE))/sum(value1, na.rm = TRUE)*100)
cpercent

p1 <- ggplot(mergedat, aes(y = tavgdiff, x = crop, color = crop)) +
  ggthemes::geom_tufteboxplot(data = tuftedata, size = 1) +
  geom_point(data = diff, size = 1) +
  theme_tufte(base_size = 12) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  # theme(axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank()) +
  xlab(NULL) + ylab("Change in \n Temperature (C)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = c(0, 1, 2), labels = c("0", "+1C", "+2C"))
p1

p2 <- ggplot(cpercent, aes(y = change, x = crop)) + 
  geom_point(data = cpercent, aes(crop, change), color = c(ggplotColours(n = 5)), size = 1.5) +
  theme_tufte(base_size = 12) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlab(NULL) + 
  ylab("% Change in \n Value of Activity") + ylim(0, 200)

wplot1_ymax <- ggplot_build(wplot1)$layout$panel_ranges[[1]]$y.range[2]
wplot2_ymax <- ggplot_build(wplot2)$layout$panel_ranges[[1]]$y.range[2]

wplot_ymax <- max(wplot1_ymax, wplot2_ymax)

wplot1 <- wplot1 +  ylim(0, wplot_ymax) + annotate("text", x = 17, y = wplot_ymax, label = "1950-1980", size = 4)
wplot2 <- wplot2 +  ylim(0, wplot_ymax)  + annotate("text", x = 17, y = wplot_ymax, label = "1980-2010", size = 4)
wdensplot <- wdensplot +  ylim(-50, wplot_ymax)  + annotate("text", x = 17, y = wplot_ymax, label = "Difference", size = 4)
wplot1
dplot <-plot_grid(p1, p2, ncol = 2)

plot_grid(wplot1, wplot2, wdensplot, p1, p2, legend, ncol = 3, rel_heights = c(1, .5))

#--------------------------------------------------------------
# Coldest counties

cdat1 <- filter(dat1, fips %in% fips1)
cdat1 <- select(cdat1, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
cdat1 <- gather(cdat1, key = crop, value = value, -tavg, -fips, -state)
cdat1 <- filter(cdat1, !is.na(tavg) & !is.na(value))
cdat1$value <- cdat1$value/1000000

cdat2 <- filter(dat2, fips %in% fips1)
cdat2 <- select(cdat2, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
cdat2 <- gather(cdat2, key = crop, value = value, -tavg, -fips, -state)
cdat2 <- filter(cdat2, !is.na(tavg) & !is.na(value))
cdat2$value <- cdat2$value/1000000

 # x = cdat1
 # variable = "tavg"
 # weight = "value"


wplot1 <- densityShare(cdat1, "tavg", "value")$plot + theme(legend.position = "none") +
  xlab("") + ylab("Value of Activity \n ($1 Million)") +
  xlim(10, 25) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
#wplot1

# Legend
wplot1_legend <- densityShare(cdat1, "tavg", "value")$plot + theme(legend.position = c(0.45,1.1), 
        legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.key = element_blank(),
        legend.title = element_blank()) + 
  xlab("") + ylab("Crop Value of Activity \n ($1 Million)") +
  xlim(10, 25) 
legend <- g_legend(wplot1_legend)
#plot(legend)


wplot2 <- densityShare(cdat2, "tavg", "value")$plot + theme(legend.position = "none") +
   xlab("Average Temperature (C)") +
   xlim(10, 25) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
#wplot2

mdat1 <- filter(dat1, fips %in% fips1)
mdat1 <- select(mdat1, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
mdat1 <- gather(mdat1, key = crop, value = value, -tavg, -fips, -state)

mdat2 <- filter(dat2, fips %in% fips1)
mdat2 <- select(mdat2, fips, Corn, Cotton, Hay, Wheat, Soybean, tavg)
mdat2 <- gather(mdat2, key = crop, value = value, -tavg, -fips, -state)

mergedat <- data.frame(fips = mdat1$fips,
                       crop = mdat1$crop,
                       tavg2 = mdat2$tavg,
                       tavg1 = mdat1$tavg,
                       value1 = mdat1$value,
                       value2 = mdat2$value,
                       diffvalue = mdat2$value - mdat1$value)

mergedat$tavgdiff = mergedat$tavg2 - mergedat$tavg1
mergedat$tavgdiff <- ifelse(is.na(mergedat$value1), NA, mergedat$tavgdiff)
mergedat$tavgdiff <- ifelse(is.na(mergedat$value2), NA, mergedat$tavgdiff)

# Difference distribution
cdens1 <- densityShare(cdat1, "tavg", "value")$densdata
cdens1 <- arrange(cdens1, crop, y)

cdens2 <- densityShare(cdat2, "tavg", "value")$densdata
cdens2 <- arrange(cdens2, crop, y)
cdens <- cdens1
cdens$x2 <- cdens2$x
cdens$y2 <- cdens2$y

cdens$ydiff <- cdens$y2 - cdens$y
cdens$dtavg <- (cdens$x + cdens$x2)/2

cdens_crop <- cdens %>% 
  group_by(crop) %>% 
  summarise(total = sum(ydiff)) %>% 
  arrange(-total)
cdens_crop    

cdensplot <- ggplot(NULL) + 
      geom_area(data = filter(cdens, crop == cdens_crop$crop[1]), aes(x2, ydiff, fill = cdens_crop$crop[1])) +
      geom_area(data = filter(cdens, crop == cdens_crop$crop[2]), aes(x2, ydiff, fill = cdens_crop$crop[2])) +
      geom_area(data = filter(cdens, crop == cdens_crop$crop[3]), aes(x2, ydiff, fill = cdens_crop$crop[3])) +
      geom_area(data = filter(cdens, crop == cdens_crop$crop[4]), aes(x2, ydiff, fill = cdens_crop$crop[4])) +
      geom_area(data = filter(cdens, crop == cdens_crop$crop[5]), aes(x2, ydiff, fill = cdens_crop$crop[5])) +
      xlab("") + ylab(NULL)+ theme_tufte(base_size = 14) +
      #scale_fill_discrete(breaks = c("Corn", "soybean", "hay", "wheat", "cotton")) + 
      theme(legend.position="none") + 
      theme(legend.title=element_blank()) +
        annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
        annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlim(8, 25)+ geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
#cdensplot

# Remove outliers for boxplot
tuftedata <- mergedat %>% 
  group_by(crop) %>% 
  mutate(tavgdiff = remove_outliers(tavgdiff))

# Get outliers for boxplot
diff <- filter(mergedat, !tavgdiff %in% tuftedata$tavgdiff)
head(diff)

cpercent <- mergedat %>% 
  group_by(crop) %>% 
  summarise(change = (sum(value2, na.rm = TRUE) - sum(value1, na.rm = TRUE))/sum(value1, na.rm = TRUE)*100)
cpercent

p1 <- ggplot(mergedat, aes(y = tavgdiff, x = crop, color = crop)) +
  ggthemes::geom_tufteboxplot(data = tuftedata, size = 1) +
  geom_point(data = diff, size = 1) +
  theme_tufte(base_size = 12) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  # theme(axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank()) +
  xlab(NULL) + ylab("Change in \n Temperature (C)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = c(0, 1, 2), labels = c("0", "+1C", "+2C"))
#p1

p2 <- ggplot(cpercent, aes(y = change, x = crop)) + 
  geom_point(data = cpercent, aes(crop, change), color = c(ggplotColours(n = 5)), size = 1.5) +
  theme_tufte(base_size = 12) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlab(NULL) + 
  ylab("% Change in \n Production") + ylim(0, 200)

wplot1_ymax <- ggplot_build(wplot1)$layout$panel_ranges[[1]]$y.range[2]
wplot2_ymax <- ggplot_build(wplot2)$layout$panel_ranges[[1]]$y.range[2]

wplot_ymax <- max(wplot1_ymax, wplot2_ymax)

wplot1 <- wplot1 +  ylim(0, wplot_ymax) + annotate("text", x = 17, y = wplot_ymax, label = "1950-1980", size = 4)
wplot2 <- wplot2 +  ylim(0, wplot_ymax)  + annotate("text", x = 17, y = wplot_ymax, label = "1980-2010", size = 4)
cdensplot <- cdensplot +  ylim(-50, wplot_ymax)  + annotate("text", x = 17, y = wplot_ymax, label = "Difference", size = 4)
#wplot1
dplot <-plot_grid(p1, p2, ncol = 2)

plot_grid(wplot1, wplot2, cdensplot, p1, p2, legend, ncol = 3, rel_heights = c(1, .5))


#### Differences between warm and cold

wdens <- arrange(wdens, crop, ydiff)
cdens <- arrange(cdens, crop, ydiff)

diffdens <- data.frame(wd_ydiff = wdens$ydiff,
                      cd_ydiff = cdens$ydiff,
                      crop = wdens$crop, 
                      x2 = wdens$x2)

diffdens$ydiff <- diffdens$wd_ydiff - diffdens$cd_ydiff

diffdens_crop <- diffdens %>% 
  group_by(crop) %>% 
  summarise(total = sum(ydiff),
            wd_ydiff = sum(wd_ydiff),
            cd_ydiff = sum(cd_ydiff)) %>% 
  arrange(-total)

diffdens_crop  

totalwd <- wdat2 %>% 
  group_by(crop) %>% 
  summarise(total_wp = sum(value))
totalwd

totalcd <- cdat2 %>% 
  group_by(crop) %>% 
  summarise(total_cp = sum(value))
totalcd

diffdens_crop <- left_join(diffdens_crop, totalwd, by = "crop")
diffdens_crop <- left_join(diffdens_crop, totalcd, by = "crop")

# total <- data.frame(crop = "All Crops",
#                     total = sum(diffdens_crop$total),
#                     wd_ydiff = 0,
#                     cd_ydiff = 0,
#                     total_wp = 0,
#                     total_cp = 0)
# 
# diffdens_crop <- rbind(diffdens_crop, total)                    
diffdens_crop$wperc <- ifelse(diffdens_crop$total < 0, diffdens_crop$total/diffdens_crop$total_cp, 
                              diffdens_crop$total/diffdens_crop$total_wp)
diffdens_crop$wperc[[6]] <- NA

diffdens_crop$wperc <- diffdens_crop$wperc*100
diffdens_crop

diffdensplot <- ggplot(NULL) + 
      geom_area(data = filter(diffdens, crop == diffdens_crop$crop[1]), aes(x2, ydiff, fill = diffdens_crop$crop[1])) +
      geom_area(data = filter(diffdens, crop == diffdens_crop$crop[2]), aes(x2, ydiff, fill = diffdens_crop$crop[2])) +
      geom_area(data = filter(diffdens, crop == diffdens_crop$crop[3]), aes(x2, ydiff, fill = diffdens_crop$crop[3])) +
      geom_area(data = filter(diffdens, crop == diffdens_crop$crop[4]), aes(x2, ydiff, fill = diffdens_crop$crop[4])) +
      geom_area(data = filter(diffdens, crop == diffdens_crop$crop[5]), aes(x2, ydiff, fill = diffdens_crop$crop[5])) +
      theme_tufte(base_size = 12) + xlab(NULL) +
      #scale_fill_discrete(breaks = c("Corn", "soybean", "hay", "wheat", "cotton")) + 
      theme(legend.title=element_blank()) +
      theme(legend.position = "none") +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
      annotate("text", x = 21, y = 47, label = "Increase activity \n in warmest \n counties", fontface = 2) +
      annotate("text", x = 21, y = -37, label = "Increase activity \n in coolest \n counties", fontface = 2) +
      xlim(8, 25)+ geom_hline(yintercept = 0, linetype = "dashed", color = "grey") + ylim(-60,60) 
diffdensplot <- diffdensplot + ylab("Difference of Change") 
diffdensplot

diffdens_crop$crop <- factor(diffdens_crop$crop, levels = c("Corn", "Cotton", "Hay", "Wheat", "Soybean", "All Crops"))

diffp1 <- ggplot(diffdens_crop, aes(crop, wperc, color = crop)) + geom_point(size=2) +
  theme_tufte(base_size = 12) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlab("")  + theme(legend.position = "none") +
  ylab("Proportion of \n Total Activity (%)") + ylim(-40, 40) 
  #annotate("text", x = 4.8, y = 37, label = "Increase activity \n in warmest \n counties") +
  #annotate("text", x = 4.8, y = -27, label = "Increase activity \n in coolest \n counties") 
diffp1

wdensplot2 <- ggplot(NULL) + 
      geom_area(data = filter(wdens, crop == wdens_crop$crop[1]), aes(x2, ydiff, fill = wdens_crop$crop[1])) +
      geom_area(data = filter(wdens, crop == wdens_crop$crop[2]), aes(x2, ydiff, fill = wdens_crop$crop[2])) +
      geom_area(data = filter(wdens, crop == wdens_crop$crop[3]), aes(x2, ydiff, fill = wdens_crop$crop[3])) +
      geom_area(data = filter(wdens, crop == wdens_crop$crop[4]), aes(x2, ydiff, fill = wdens_crop$crop[4])) +
      geom_area(data = filter(wdens, crop == wdens_crop$crop[5]), aes(x2, ydiff, fill = wdens_crop$crop[5])) +
      xlab("") + ylab(NULL)+ theme_tufte(base_size = 12) +
      #scale_fill_discrete(breaks = c("Corn", "soybean", "hay", "wheat", "cotton")) + 
      theme(legend.position="none") + 
      theme(legend.title=element_blank()) +
        annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
        annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlim(8, 25)+ geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  ylim(-50, 400) + annotate("text", x = 17, y = 350, label = "Warmest \n Change") +
  ylab("Value of Activity \n ($1 Million)")
wdensplot2

cdensplot2 <- ggplot(NULL) + 
      geom_area(data = filter(cdens, crop == cdens_crop$crop[1]), aes(x2, ydiff, fill = cdens_crop$crop[1])) +
      geom_area(data = filter(cdens, crop == cdens_crop$crop[2]), aes(x2, ydiff, fill = cdens_crop$crop[2])) +
      geom_area(data = filter(cdens, crop == cdens_crop$crop[3]), aes(x2, ydiff, fill = cdens_crop$crop[3])) +
      geom_area(data = filter(cdens, crop == cdens_crop$crop[4]), aes(x2, ydiff, fill = cdens_crop$crop[4])) +
      geom_area(data = filter(cdens, crop == cdens_crop$crop[5]), aes(x2, ydiff, fill = cdens_crop$crop[5])) +
      xlab("Average Temperature (C)") + ylab(NULL)+ theme_tufte(base_size = 12) +
      #scale_fill_discrete(breaks = c("Corn", "soybean", "hay", "wheat", "cotton")) + 
      theme(legend.position="none") + 
      theme(legend.title=element_blank()) +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlim(8, 25)+ geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  ylim(-50, 400) + annotate("text", x = 17, y = 350, label = "Coolest \n Change")
cdensplot2

wplot1_legend <- densityShare(wdat1, "tavg", "value")$plot + theme(legend.position = c(0.45,1), 
        legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.key = element_blank(),
        legend.title = element_blank()) + 
  xlab("") + ylab("Crop Value of Activity \n ($1 Million)") +
  xlim(8, 25) 
legend <- g_legend(wplot1_legend)
plot(legend)

#wdensplot <- wdensplot + theme(legend.position = "none") + annotate("text", x = 17, y = 450, label = "(Warmest)")
#cdensplot <- cdensplot + ylim(0, 500) + annotate("text", x = 17, y = 450, label = "(Coolest)")

plot_grid(wdensplot2, cdensplot2, diffdensplot, diffp1, legend, ncol = 3, rel_heights =  c(1, .5))

####################################################################################################
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
