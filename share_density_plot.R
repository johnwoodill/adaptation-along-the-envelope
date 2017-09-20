library(ggplot2)
library(dplyr)
library(cowplot)



# # Function for plotting differences in density
# crop.density <- function(x, start, end, var){ # 1 - crop rev per acre, 2 - crop rev, 3 - acres
#   cropdat <- x
#   if (var == 1){
#     # Crop rev per acre
#     cropdat$corn_var <- (cropdat$corn_grain_p*cropdat$corn_rprice)/cropdat$corn_grain_a
#     cropdat$cotton_var <- (cropdat$cotton_p*cropdat$cotton_rprice)/cropdat$cotton_a
#     cropdat$hay_var <- (cropdat$hay_p*cropdat$hay_rprice)/cropdat$hay_a
#     cropdat$wheat_var <- (cropdat$wheat_p*cropdat$wheat_rprice)/cropdat$wheat_a
#     cropdat$soybean_var <- (cropdat$soybean_p*cropdat$soybean_rprice)/cropdat$soybean_a
#     gt <- paste0("Revenue per Acre Crop Share")
#     title <- paste0("revenue_per_acre_crop_share")
#   } else if (var == 2){
#     # Crop rev
#     cropdat$corn_var <- (cropdat$corn_grain_p*cropdat$corn_rprice)
#     cropdat$cotton_var <- (cropdat$cotton_p*cropdat$cotton_rprice)
#     cropdat$hay_var <- (cropdat$hay_p*cropdat$hay_rprice)
#     cropdat$wheat_var <- (cropdat$wheat_p*cropdat$wheat_rprice)
#     cropdat$soybean_var <- (cropdat$soybean_p*cropdat$soybean_rprice)
#     gt <- paste0("Revenue Crop Share")
#     title <- paste0("revenue_crop_share")
#   } else if (var == 3){
#     # Crop acres
#     cropdat$corn_var <- cropdat$corn_grain_a
#     cropdat$cotton_var <- cropdat$cotton_a
#     cropdat$hay_var <- cropdat$hay_a
#     cropdat$wheat_var <- cropdat$wheat_a
#     cropdat$soybean_var <- cropdat$soybean_a
#     gt <- paste0("Acre Crop Share")
#     title <- paste0("acre_crop_share")
#   }
# 
#     # Start year
#     cropdat1 <- filter(cropdat, year >= start & year < (start + 10))
#     
#     # Remove inf to na
#     is.na(cropdat1) <- do.call(cbind, lapply(cropdat1, is.infinite))
#     
#     corn_dat1 <- filter(cropdat1, !is.na(corn_var) & !is.na(tavg))
#     cotton_dat1 <- filter(cropdat1, !is.na(cotton_var) & !is.na(tavg))
#     hay_dat1 <- filter(cropdat1, !is.na(hay_var) & !is.na(tavg))
#     wheat_dat1 <- filter(cropdat1, !is.na(wheat_var) & !is.na(tavg))
#     soybean_dat1 <- filter(cropdat1, !is.na(soybean_var) & !is.na(tavg))
#     
#     sum.corn1 <- sum(corn_dat1$corn_var)
#     sum.cotton1 <- sum(cotton_dat1$cotton_var)
#     sum.hay1 <- sum(hay_dat1$hay_var)
#     sum.wheat1 <- sum(wheat_dat1$wheat_var)
#     sum.soybean1 <- sum(soybean_dat1$soybean_var)
#     sum.all1 <- sum.corn1 + sum.cotton1 + sum.hay1 + sum.wheat1 + sum.soybean1
#     
#     dens.corn1 <- density(corn_dat1$tavg, weight = corn_dat1$corn_var/sum.all1, from = 0, to = 30, n = 30)
#     dens.cotton1 <- density(cotton_dat1$tavg, weight = cotton_dat1$cotton_var/sum.all1, from = 0, to = 30, n = 30)
#     dens.hay1 <- density(hay_dat1$tavg, weight = hay_dat1$hay_var/sum.all1, from = 0, to = 30, n = 30)
#     dens.wheat1 <- density(wheat_dat1$tavg, weight = wheat_dat1$wheat_var/sum.all1, from = 0, to = 30, n = 30)
#     dens.soybean1 <- density(soybean_dat1$tavg, weight = soybean_dat1$soybean_var/sum.all1, from = 0, to = 30, n = 30)
#     
#     dens.wheat1$y <- dens.wheat1$y + dens.cotton1$y
#     dens.hay1$y <- dens.hay1$y + dens.wheat1$y
#     dens.soybean1$y <- dens.hay1$y + dens.soybean1$y
#     dens.corn1$y <- dens.corn1$y + dens.soybean1$y
#     
#     plot(dens.corn1)
#     lines(dens.cotton1)
#     lines(dens.hay1)
#     lines(dens.soybean1)
#     lines(dens.wheat1)
#     
#     plot(y= (dens.corn1$y + dens.cotton1$y + dens.hay1$y + dens.wheat1$y + dens.soybean1$y), x = 1:30, type = "l")
#     
#     #----------------------
#     # End year
#     
#     cropdat2 <- filter(cropdat, year >= end & year < (end + 10))
#     
#     # Remove inf to na
#     is.na(cropdat2) <- do.call(cbind, lapply(cropdat2, is.infinite))
#     
#     corn_dat2 <- filter(cropdat2, !is.na(corn_var) & !is.na(tavg))
#     cotton_dat2 <- filter(cropdat2, !is.na(cotton_var) & !is.na(tavg))
#     hay_dat2 <- filter(cropdat2, !is.na(hay_var) & !is.na(tavg))
#     wheat_dat2 <- filter(cropdat2, !is.na(wheat_var) & !is.na(tavg))
#     soybean_dat2 <- filter(cropdat2, !is.na(soybean_var) & !is.na(tavg))
#     
#     sum.corn2 <- sum(corn_dat2$corn_var)
#     sum.cotton2 <- sum(cotton_dat2$cotton_var)
#     sum.hay2 <- sum(hay_dat2$hay_var)
#     sum.wheat2 <- sum(wheat_dat2$wheat_var)
#     sum.soybean2 <- sum(soybean_dat2$soybean_var)
#     sum.all2 <- sum.corn2 + sum.cotton2 + sum.hay2 + sum.wheat2 + sum.soybean2
#     
#     dens.corn2 <- density(corn_dat2$tavg, weight = corn_dat2$corn_var/sum.all2, from = 0, to = 30, n = 30)
#     dens.cotton2 <- density(cotton_dat2$tavg, weight = cotton_dat2$cotton_var/sum.all2, from = 0, to = 30, n = 30)
#     dens.hay2 <- density(hay_dat2$tavg, weight = hay_dat2$hay_var/sum.all2, from = 0, to = 30, n = 30)
#     dens.wheat2 <- density(wheat_dat2$tavg, weight = wheat_dat2$wheat_var/sum.all2, from = 0, to = 30, n = 30)
#     dens.soybean2 <- density(soybean_dat2$tavg, weight = soybean_dat2$soybean_var/sum.all2, from = 0, to = 30, n = 30)
#     
#     dens.wheat2$y <- dens.wheat2$y + dens.cotton2$y
#     dens.hay2$y <- dens.hay2$y + dens.wheat2$y
#     dens.soybean2$y <- dens.hay2$y + dens.soybean2$y
#     dens.corn2$y <- dens.corn2$y + dens.soybean2$y
#     
#     ymax <- max(dens.corn1$y, dens.corn2$y) + .05
#     
#     plot1 <- ggplot(NULL, aes(x = dens.corn1$x, y = dens.corn1$y)) + 
#       geom_polygon(aes(x = dens.corn1$x, y = dens.corn1$y, fill = "corn")) +
#       geom_polygon(aes(x = dens.soybean1$x, y = dens.soybean1$y, fill = "soybean")) + 
#       geom_polygon(aes(x = dens.hay1$x, y = dens.hay1$y, fill = "hay")) + 
#       geom_polygon(aes(x = dens.wheat1$x, y = dens.wheat1$y, fill = "wheat")) +
#       geom_polygon(aes(x = dens.cotton1$x, y = dens.cotton1$y, fill = "cotton")) + 
#       xlab(NULL) + ylab(NULL)+ ylim(0, ymax) +
#       scale_fill_discrete(breaks = c("corn", "soybean", "hay", "wheat", "cotton")) + theme(legend.position="top") + 
#       theme(legend.title=element_blank()) + ggtitle(gt)
#     plot1
#     
#     plot2 <- ggplot(NULL, aes(x = dens.corn2$x, y = dens.corn2$y)) + 
#       geom_polygon(aes(x = dens.corn2$x, y = dens.corn2$y, fill = "corn")) +
#       geom_polygon(aes(x = dens.soybean2$x, y = dens.soybean2$y, fill = "soybean")) + 
#       geom_polygon(aes(x = dens.hay2$x, y = dens.hay2$y, fill = "hay")) + 
#       geom_polygon(aes(x = dens.wheat2$x, y = dens.wheat2$y, fill = "wheat")) +
#       geom_polygon(aes(x = dens.cotton2$x, y = dens.cotton2$y, fill = "cotton")) + 
#       xlab("Average Temp (C)") + ylab(NULL) + ylim(0, ymax) +
#       scale_fill_discrete(breaks = c("corn", "soybean", "hay", "wheat", "cotton")) + theme(legend.position="none") + 
#       theme(legend.title=element_blank())
#     plot2
#     
#     plott <- plot_grid(plot1, plot2, ncol = 1)
#     plott
#     save_plot(paste0("figures/", title, ".png"), plott, base_height = 10)
#     return(plott)
# }
# 
# # Revenue per acre share
# crop.density(cropdat, 1950, 2000, 1)
# 
# # Revenue share
# crop.density(cropdat, 1960, 2000, 2)
# 
# # Acreage share
# crop.density(cropdat, 1940, 2000, 3)
# 
source("R/densityShare.R")
cropdat <- readRDS("data/full_ag_data.rds")
cropdat$corn_rrev <- cropdat$corn_rrev*cropdat$corn_grain_a
cropdat$cotton_rrev <- cropdat$cotton_rrev*cropdat$cotton_a
cropdat$hay_rrev <- cropdat$hay_rrev*cropdat$hay_a
cropdat$wheat_rrev <- cropdat$wheat_rrev*cropdat$wheat_a
cropdat$soybean_rrev <- cropdat$soybean_rrev*cropdat$soybean_a

ddat <- select(cropdat, fips, year, tavg, corn_rrev, cotton_rrev, hay_rrev, wheat_rrev, soybean_rrev)
head(ddat)
names(ddat) <- c("fips", "year", "tavg", "corn", "cotton", "hay", "wheat", "soybean")
ddat65 <- filter(ddat, year >= 1950 & year <= 1960)
ddat65 <- ddat65 %>% 
  group_by(fips) %>% 
  summarise(Corn = mean(corn, na.rm = TRUE),
            Cotton = mean(cotton, na.rm = TRUE),
            Hay = mean(hay, na.rm = TRUE),
            Wheat = mean(wheat, na.rm = TRUE),
            Soybean = mean(soybean, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE))
head(ddat65)


ddat65 <- gather(ddat65, key = crop, value = value, -tavg, -fips)
ddat65 <- filter(ddat65, !is.na(value) & !is.na(tavg))

ddat10 <- filter(ddat, year >= 2000 & year <= 2010)
ddat10 <- ddat10 %>% 
  group_by(fips) %>% 
  summarise(Corn = mean(corn, na.rm = TRUE),
            Cotton = mean(cotton, na.rm = TRUE),
            Hay = mean(hay, na.rm = TRUE),
            Wheat = mean(wheat, na.rm = TRUE),
            Soybean = mean(soybean, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE))

ddat10 <- gather(ddat10, key = crop, value = value, -tavg, -fips)
ddat10 <- filter(ddat10, !is.na(value) & !is.na(tavg))

head(ddat65)
head(ddat10)

ddat65$crop <- as.character(ddat65$crop)
ddat65$crop <- tools::toTitleCase(ddat65$crop)

ddat10$crop <- as.character(ddat10$crop)
ddat10$crop <- tools::toTitleCase(ddat10$crop)


# x = ddat65
# variable = "tavg"
# weight = "value"
dev.off()
p1 <- densityShare(ddat65, "tavg", "value") +
  annotate("text", x = 22,  y = 0.20, label = "1950-1960", size = 6, alpha = 0.8) + 
  theme_tufte(base_size = 12)+
  theme(legend.position = c(0,1), legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.title = element_blank(), legend.key = element_blank()
        )  +
  ylab("Value of Activity \n (Total Revenue)") + xlim(10, 25) + ylim(0, 0.2)+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "black") +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "black")
  
p1 

p2 <- densityShare(ddat10, "tavg", "value") + 
  ylab("Value of Activity \n (Total Revenue)") + theme_tufte(base_size = 12)+
  annotate("text", x = 22, y = 0.2, label = "2000-2010", size = 6, alpha = 0.8) + 
  theme(legend.position = "none")+ xlab("Average Temperature (C)")+ xlim(10, 25) + ylim(0, 0.2) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "black") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "black")
  

p2
dev.off()
pdf("/home/john/Dropbox/Research/Adaptation Along the Envelope/figures/initial_density.pdf", 
    width = 6, height = 5)
plot_grid(p1,p2,ncol = 1)
dev.off()
