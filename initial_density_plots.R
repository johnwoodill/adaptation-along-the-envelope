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

# # Constant prices
# cropdat$corn_rprice <- mean(cropdat$corn_rprice, na.rm = TRUE)
# cropdat$cotton_rprice <- mean(cropdat$cotton_rprice, na.rm = TRUE)
# cropdat$hay_rprice <- mean(cropdat$hay_rprice, na.rm = TRUE)
# cropdat$wheat_rprice <- mean(cropdat$wheat_rprice, na.rm = TRUE)
# cropdat$soybean_rprice <- mean(cropdat$soybean_rprice, na.rm = TRUE)
# 
# Total Activity
cropdat$corn <- cropdat$corn_yield*cropdat$corn_grain_a*cropdat$corn_rprice
cropdat$cotton <- cropdat$cotton_yield*cropdat$cotton_a*cropdat$cotton_rprice
cropdat$hay <- cropdat$hay_yield*cropdat$hay_a*cropdat$hay_rprice
cropdat$wheat <- cropdat$wheat_yield*cropdat$wheat_a*cropdat$wheat_rprice
cropdat$soybean <- cropdat$soybean_yield*cropdat$soybean_a*cropdat$soybean_rprice
# 
# # Crop Acres
cropdat$corn <- cropdat$corn_grain_a
cropdat$cotton <- cropdat$cotton_a
cropdat$hay <- cropdat$hay_a
cropdat$wheat <- cropdat$wheat_a
cropdat$soybean <- cropdat$soybean_a


# Yield per acre
# cropdat$corn <- cropdat$corn_yield
# cropdat$cotton <- cropdat$cotton_yield
# cropdat$hay <- cropdat$hay_yield
# cropdat$wheat <- cropdat$wheat_yield
# cropdat$soybean <- cropdat$soybean_yield

# cropdat <- filter(cropdat, !is.na(corn) | !is.na(cotton) | !is.na(hay) | !is.na(wheat) | !is.na(soybean) |
#                     !is.na(tavg))



# Aggregate total value of activity
dat1 <- filter(cropdat, year >= 1950 & year <= 1979)
dat1 <- dat1 %>% 
  group_by(year) %>% 
  mutate(Corn = corn - mean(corn, na.rm = TRUE),
            Cotton = cotton - mean(cotton, na.rm = TRUE),
            Hay = hay - mean(hay, na.rm = TRUE),
            Wheat = wheat - mean(wheat, na.rm = TRUE),
            Soybean = soybean - mean(soybean, na.rm = TRUE)) %>%
  group_by(state, fips) %>% 
  summarise(dday30C = mean(dday30C, na.rm = TRUE),
            dday10_30 = mean(dday10_30, na.rm = TRUE),
            Corn = mean(Corn, na.rm = TRUE),
            Cotton = mean(Cotton, na.rm = TRUE),
            Hay = mean(Hay, na.rm = TRUE),
            Wheat = mean(Wheat, na.rm = TRUE),
            Soybean = mean(Soybean, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE))

dat2 <- filter(cropdat, year >= 1980 & year <= 2009)
dat2 <- dat2 %>% 
  group_by(year) %>% 
  mutate(Corn = corn - mean(corn, na.rm = TRUE),
            Cotton = cotton - mean(cotton, na.rm = TRUE),
            Hay = hay - mean(hay, na.rm = TRUE),
            Wheat = wheat - mean(wheat, na.rm = TRUE),
            Soybean = soybean - mean(soybean, na.rm = TRUE)) %>% 
  group_by(state, fips) %>% 
  summarise(dday30C = mean(dday30C, na.rm = TRUE),
            dday10_30 = mean(dday10_30, na.rm = TRUE),
            Corn = mean(Corn, na.rm = TRUE),
            Cotton = mean(Cotton, na.rm = TRUE),
            Hay = mean(Hay, na.rm = TRUE),
            Wheat = mean(Wheat, na.rm = TRUE),
            Soybean = mean(Soybean, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE))

# Crop acres
dat1 <- cropdat %>% 
  mutate(dday30C = dday30C,
            dday10_30 = dday10_30,
            Corn = corn,
            Cotton = cotton,
            Hay = hay,
            Wheat = wheat,
            Soybean = soybean,
            tavg = tavg)

dat2 <- cropdat %>% 
  group_by(state, fips) %>% 
  summarise(dday30C = mean(dday30C, na.rm = TRUE),
            dday10_30 = mean(dday10_30, na.rm = TRUE),
            Corn = mean(corn, na.rm = TRUE),
            Cotton = mean(cotton, na.rm = TRUE),
            Hay = mean(hay, na.rm = TRUE),
            Wheat = mean(wheat, na.rm = TRUE),
            Soybean = mean(soybean, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE)) %>% 
  ungroup()


# Aggregate total acres
#dat1 <- filter(cropdat, year >= 1950 & year <= 1979)
# dat1 <- cropdat %>% 
#   group_by(state, fips) %>% 
#   summarise(dday30C = mean(dday30C, na.rm = TRUE),
#             dday10_30 = mean(dday10_30, na.rm = TRUE),
#             Corn = mean(corn, na.rm = TRUE),
#             Cotton = mean(cotton, na.rm = TRUE),
#             Hay = mean(hay, na.rm = TRUE),
#             Wheat = mean(wheat, na.rm = TRUE),
#             Soybean = mean(soybean, na.rm = TRUE),
#             tavg = mean(tavg, na.rm = TRUE)) %>% 
#   ungroup()

#dat2 <- filter(cropdat, year >= 1980 & year <= 2009)


# dat2 <- filter(dat2, fips %in% unique(dat1$fips))
# dat1 <- filter(dat1, fips %in% unique(dat2$fips))

dat1950 <- filter(cropdat, year >= 1950 & year <= 1979)
dat1950 <- dat1950 %>% 
  group_by(state, fips) %>% 
  summarise(tavg = mean(tavg, na.rm = TRUE))

dat2000 <- filter(cropdat, year >= 1980 & year <= 2009)
dat2000 <- dat2000 %>% 
  group_by(state, fips) %>% 
  summarise(tavg = mean(tavg, na.rm = TRUE))

dat1950 <- filter(dat1950, fips %in% unique(dat1950$fips))
dat2000 <- filter(dat2000, fips %in% unique(dat2000$fips))

dat <- data.frame(state = dat1950$state,
                  fips = dat1950$fips,
                  tavg1 = dat1950$tavg,
                  tavg2 = dat2000$tavg)

dat$difftavg = dat$tavg2 - dat$tavg1
diff <- arrange(dat, -difftavg)
head(diff)

# Split into thirds
#diff$thirds <- ntile(diff$difftavg, 3)

# Split into thirds by state
diff <- diff %>% 
   group_by(state) %>% 
   mutate(thirds = dplyr::ntile(difftavg, 3),
          difftavg = difftavg)


spdiff <- filter(diff, thirds == 3) # Warmest
fipss <- spdiff$fips

# Get data for change in temp graph
dat1950 <- filter(cropdat, year >= 1950 & year <= 1959 & fips %in% fipss)
dat1950 <- dat1950 %>% 
  group_by(state, fips) %>% 
  summarise(dday30C = mean(dday30C, na.rm = TRUE),
            dday10_30 = mean(dday10_30, na.rm = TRUE),
            Corn = mean(corn, na.rm = TRUE),
            Cotton = mean(cotton, na.rm = TRUE),
            Hay = mean(hay, na.rm = TRUE),
            Wheat = mean(wheat, na.rm = TRUE),
            Soybean = mean(soybean, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE))

dat1950 <- select(dat1950, state, fips, tavg, Corn, Cotton, Hay, Wheat, Soybean)
dat1950 <- gather(dat1950, key = crop, value = value, -tavg, -fips, -state)

dat2000 <- filter(cropdat, year >= 2000 & year <= 2009 & fips %in% fipss)
dat2000 <- dat2000 %>% 
  group_by(state, fips) %>% 
  summarise(dday30C = mean(dday30C, na.rm = TRUE),
            dday10_30 = mean(dday10_30, na.rm = TRUE),
            Corn = mean(corn, na.rm = TRUE),
            Cotton = mean(cotton, na.rm = TRUE),
            Hay = mean(hay, na.rm = TRUE),
            Wheat = mean(wheat, na.rm = TRUE),
            Soybean = mean(soybean, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE))
dat2000 <- select(dat2000, state, fips, tavg, Corn, Cotton, Hay, Wheat, Soybean)
dat2000 <- gather(dat2000, key = crop, value = value, -tavg, -fips, -state)
head(dat1950)
head(dat2000)

tempdat <- data.frame(fips = dat1950$fips,
                       crop = dat1950$crop,
                       tavg2 = dat2000$tavg,
                       tavg1 = dat1950$tavg,
                       value1 = dat1950$value,
                       value2 = dat2000$value,
                       diffvalue = dat2000$value - dat1950$value)

tempdat$tavgdiff = tempdat$tavg2 - tempdat$tavg1
tempdat$tavgdiff <- ifelse(is.na(tempdat$value1), NA, tempdat$tavgdiff)
tempdat$tavgdiff <- ifelse(is.na(tempdat$value2), NA, tempdat$tavgdiff)

# Map of counties
mapdat <- data.frame(region = fipss, value = rep("Warmest", length(fipss)))

states <- tolower(unique(state.name[match(cropdat$state, tolower(state.abb))]))

map <- county_choropleth(mapdat,
                  title      = NULL,
                  state_zoom = states)
map <- map +
 #scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Sample Data") + ylab(NULL) + theme(legend.position = c(0,0),
                                    legend.justification = c("left", "bottom"),
                                    legend.title = element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     axis.ticks.y = element_blank(),
                                     panel.border = element_rect(fill = NA)) +
  scale_fill_manual(values=c("#de2d26"), breaks = c("Warmest")) 
map

# Subset new data warmest counties
dat1  <- dplyr::filter(dat1, fips %in% fipss)
wdat1 <- select(dat1, fips, state, Corn, Cotton, Hay, Wheat, Soybean, tavg)
wdat1 <- gather(wdat1, key = crop, value = value, -tavg, -state, -fips)
wdat1 <- filter(wdat1, !is.na(tavg) & !is.na(value))
wdat1$value <- wdat1$value/1000000

# wdat1$value <- wdat1$value^2
# wdat1$value <- wdat1$value/(sqrt(wdat1$value))
#wdat1$value <- ifelse(is.na(wdat1$value), 0, wdat1$value)

dat2 <- filter(dat2, fips %in% fipss)
wdat2 <- select(dat2, fips, state, Corn, Cotton, Hay, Wheat, Soybean, tavg)
wdat2 <- gather(wdat2, key = crop, value = value, -tavg, -state, -fips)
wdat2 <- filter(wdat2, !is.na(tavg) & !is.na(value))
wdat2$value <- wdat2$value/1000000

 # wdat2$value <- wdat2$value^2
 # wdat2$value <- wdat2$value/(sqrt(wdat2$value))


 # x = wdat1
 # variable = "tavg"
 # weight = "value"

# dens data for densityShare
newdens <- wdat2 %>% 
  group_by(crop) %>% 
  summarise(value = sum(value, na.rm = TRUE))
newdens

wplot1 <- densityShare(wdat1, "tavg", "value", dens = newdens)$plot + theme(legend.position = "none") +
  xlab("") + ylab("Value of Activity \n ($1 Million)") +
  xlim(8, 25) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
wplot1

# Legend
wplot1_legend <- densityShare(wdat1, "tavg", "value", dens = newdens)$plot + theme(legend.position = c(0.45,1.1), 
        legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.key = element_blank(),
        legend.title = element_blank()) + 
  xlab("") + ylab("Crop Value of Activity \n ($1 Million)") +
  xlim(8, 25) 
legend <- g_legend(wplot1_legend)
plot(legend)


wplot2 <- densityShare(wdat2, "tavg", "value", dens = newdens)$plot + theme(legend.position = "none") +
   xlab("Average Temperature (C)") +
   xlim(8, 25) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
wplot2

#mdat1 <- filter(dat1, fips %in% fips2)
mdat1 <- select(dat1, fips, state, Corn, Cotton, Hay, Wheat, Soybean, tavg)
mdat1 <- gather(mdat1, key = crop, value = value, -tavg, -fips, -state)

#mdat2 <- filter(dat2, fips %in% fips2)
mdat2 <- select(dat2, fips, state, Corn, Cotton, Hay, Wheat, Soybean, tavg)
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
#mergedat$tavgdiff <- ifelse(is.na(mergedat$value1), NA, mergedat$tavgdiff)
#mergedat$tavgdiff <- ifelse(is.na(mergedat$value2), NA, mergedat$tavgdiff)

# Difference distribution
wdens1 <- densityShare(wdat1, "tavg", "value", dens = newdens)$densdata
wdens1 <- arrange(wdens1, crop, y)

wdens2 <- densityShare(wdat2, "tavg", "value", dens = newdens)$densdata
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
tuftedata <- tempdat %>% 
  group_by(crop) %>% 
  mutate(tavgdiff = remove_outliers(tavgdiff))

# Get outliers for boxplot
diff <- filter(tempdat, !tavgdiff %in% tuftedata$tavgdiff)
head(diff)

cpercent <- tempdat %>% 
  group_by(crop) %>% 
  summarise(change = ((sum(value2, na.rm = TRUE) - sum(value1, na.rm = TRUE))/sum(value1, na.rm = TRUE))*100,
            total = sum(value2, na.rm = TRUE) - sum(value1, na.rm = TRUE))
cpercent

p1 <- ggplot(tempdat, aes(y = tavgdiff, x = crop, color = crop)) +
  ggthemes::geom_tufteboxplot(data = tuftedata, size = 1) +
  geom_point(data = diff, size = 1) +
  theme_tufte(base_size = 12) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  # theme(axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank()) +
  xlab(NULL) + 
  ylab("Change in \n Temperature (C)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), 
                     labels = c("-2C", "-1C", "0", "+1C", "+2C"), 
                     limits = c(-2, 2))
p1

p2 <- ggplot(cpercent, aes(y = change, x = crop)) + 
  geom_point(data = cpercent, aes(crop, change), color = c(ggplotColours(n = 5)), size = 1.5) +
  theme_tufte(base_size = 12) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlab(NULL) + 
  ylab("% Change in \n Value of Activity") + ylim(-200, 200)
p2

wplot1_ymax <- ggplot_build(wplot1)$layout$panel_ranges[[1]]$y.range[2]
wplot2_ymax <- ggplot_build(wplot2)$layout$panel_ranges[[1]]$y.range[2]

wplot_ymax <- max(wplot1_ymax, wplot2_ymax)

wplot1 <- wplot1 +  ylim(0, wplot_ymax) + annotate("text", x = 17, y = wplot_ymax, label = "Short-run", size = 4)
wplot2 <- wplot2 +  ylim(0, wplot_ymax)  + annotate("text", x = 17, y = wplot_ymax, label = "Long-run", size = 4)
wdensplot <- wdensplot +  ylim(-1, 1)  + annotate("text", x = 17, y = wplot_ymax, label = "Difference", size = 4)
wplot1
dplot <-plot_grid(p1, p2, ncol = 2)

plot_grid(wplot1, wplot2, wdensplot, p1, p2, legend, ncol = 3, rel_heights = c(1, .5))

