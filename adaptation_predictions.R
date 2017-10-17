library(tidyverse)
library(cowplot)
library(ggthemes)
library(ggrepel)
library(scales)

source("R/densityShare.R")

# New Degree Day Data

cs.0C <- readRDS("data/degree_day_changes/cross_section_regression_data_0C")
cs.1C <- readRDS("data/degree_day_changes/cross_section_regression_data_1C")
cs.2C <- readRDS("data/degree_day_changes/cross_section_regression_data_2C")
cs.3C <- readRDS("data/degree_day_changes/cross_section_regression_data_3C")
cs.4C <- readRDS("data/degree_day_changes/cross_section_regression_data_4C")
cs.5C <- readRDS("data/degree_day_changes/cross_section_regression_data_5C")

diff.0C <- readRDS("data/degree_day_changes/diff_regression_data_0C")
diff.1C <- readRDS("data/degree_day_changes/diff_regression_data_1C")
diff.2C <- readRDS("data/degree_day_changes/diff_regression_data_2C")
diff.3C <- readRDS("data/degree_day_changes/diff_regression_data_3C")
diff.4C <- readRDS("data/degree_day_changes/diff_regression_data_4C")
diff.5C <- readRDS("data/degree_day_changes/diff_regression_data_5C")

# Load rev crop predictions
cs.pred_rev <- readRDS("data/cs.pred_revenue.rds")
p.pred_rev <- readRDS("data/p.pred_revenue.rds")
diff.pred_rev <- readRDS("data/diff.pred_revenue.rds")

# Load Predicted acreage
cs.pred_acres <- readRDS("data/cs.predicted_acres.rds")
diff.pred_acres <- readRDS("data/diff.predicted_acres.rds")

# Load panel to get average fips
p.dat <- readRDS("data/panel_regression_data.rds")

# ----------- Without Adaptation (Constant Acres)
# Get average from last 20-years
base.acres <- p.dat %>% 
  filter(year >= 1990) %>% 
  group_by(fips) %>% 
  summarise(corn_w = mean(corn_w, na.rm = TRUE),
            cotton_w = mean(cotton_w, na.rm = TRUE),
            hay_w = mean(hay_w, na.rm = TRUE),
            wheat_w = mean(wheat_w, na.rm = TRUE),
            soybean_w = mean(soybean_w, na.rm = TRUE))

head(base.acres)
which(is.na(base.acres$corn_w))
length(base.acres$fips)
base.acres_g <- gather(base.acres, key = crop, value = acres, -fips)

# Cross-section with and without adaptation
cs.rev <- data.frame(temp = cs.pred_rev$temp, 
                     crop = cs.pred_rev$crop,
                     rev_a = cs.pred_rev$rev, 
                     acres = cs.pred_acres$acres)
head(cs.rev)                     

# Get base acres
cs.rev$b_acres <-rep(base.acres_g$acres, 6)

# Get revenue without adaptation
cs.rev$b_rev_acre <- cs.rev$rev_a*cs.rev$b_acres

# Get revenue with adaptation
cs.rev$c_rev_acre <- cs.rev$rev_a*cs.rev$acres

# Keep base acres for temp = 0
baset <- which(cs.rev$temp == 0)
cs.rev$c_rev_acre[baset] <- cs.rev$b_rev_acre[baset]

# Average temps
cs.rev$tavg <- rep(cs.0C$tavg, 6)
cs.rev$ctavg <- c(cs.0C$tavg, cs.1C$tavg, cs.2C$tavg, cs.3C$tavg, cs.4C$tavg, cs.5C$tavg)
# 
# p0 <- densityShare(filter(cs.rev, temp == 0), "tavg", "b_rev_acre") + theme(legend.position = "none") + ylab("Value of Activity \n Total Revenue") +
#     theme(legend.position = c(0,1), legend.justification = c("left", "top"), legend.box.background = element_rect(colour = "grey"), 
#         legend.title = element_blank(), legend.key = element_blank())  
# p1 <- densityShare(filter(cs.rev, temp == 1), "tavg", "b_rev_acre") + theme(legend.position = "none")
#   #ggtitle("Change in Value of Activity without Adaptation \n (Cross-section estimates)") 
# p2 <- densityShare(filter(cs.rev, temp == 2), "tavg", "b_rev_acre") + theme(legend.position = "none") 
# p3 <- densityShare(filter(cs.rev, temp == 3), "tavg", "b_rev_acre") + theme(legend.position = "none")+ xlab("Average Temperature (C)")+ ylab("Value of Activity \n Total Revenue")
# p4 <- densityShare(filter(cs.rev, temp == 4), "tavg", "b_rev_acre") + theme(legend.position = "none") + xlab("Average Temperature (C)")
# p5 <- densityShare(filter(cs.rev, temp == 5), "tavg", "b_rev_acre") + theme(legend.position = "none")+ xlab("Average Temperature (C)")
# 
# plot_grid(p0, p1, p2, p3, p4, p5, ncol = 3, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C"),label_x = .75, label_y = .75) 
# 
# p00 <- densityShare(filter(cs.rev, temp == 0), "ctavg", "c_rev_acre") + theme(legend.position = "none") + ylab("Value of Activity \n Total Revenue") +
#     theme(legend.position = c(0,1), legend.justification = c("left", "top"), legend.box.background = element_rect(colour = "grey"), 
#         legend.title = element_blank(), legend.key = element_blank())  
# p11 <- densityShare(filter(cs.rev, temp == 1), "ctavg", "c_rev_acre") + theme(legend.position = "none")
#   #ggtitle("Change in Value of Activity with Adaptation \n (Cross-section estimates)") 
# p22 <- densityShare(filter(cs.rev, temp == 2), "ctavg", "c_rev_acre") + theme(legend.position = "none") 
# p33 <- densityShare(filter(cs.rev, temp == 3), "ctavg", "c_rev_acre") + theme(legend.position = "none")+ xlab("Average Temperature (C)")+ ylab("Value of Activity \n Total Revenue")
# p44 <- densityShare(filter(cs.rev, temp == 4), "ctavg", "c_rev_acre") + theme(legend.position = "none") + xlab("Average Temperature (C)")
# p55 <- densityShare(filter(cs.rev, temp == 5), "ctavg", "c_rev_acre") + theme(legend.position = "none")+ xlab("Average Temperature (C)")
# 
# plot_grid(p00, p11, p22, p33, p44, p55, ncol = 3, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C"),label_x = .75, label_y = .75) 

# Differences in revenue with and without adaptation
#cs.rev$diff <- cs.rev$c_rev_acre - cs.rev$b_rev_acre
#ggplot(cs.rev, aes(c_rev_acre, color = crop)) + geom_bar()

# change <- cs.rev %>% 
#   group_by(temp) %>% 
#   summarise(c_rev_acre = sum(c_rev_acre),
#             b_rev_acre = sum(b_rev_acre),
#             diff = sum(c_rev_acre) - sum(b_rev_acre)) %>% 
#   select(temp, b_rev_acre, c_rev_acre,  diff)
# 
# change
# change
#  change <- cs.rev %>% 
#    group_by(temp) %>% 
#    summarise(c_rev_acre = mean(c_rev_acre),
#              b_rev_acre = mean(b_rev_acre),
#              diff = mean(c_rev_acre) - mean(b_rev_acre)) %>% 
#    select(temp, b_rev_acre, c_rev_acre, diff)

#change$nr <- 1:nrow(change)
#gather(change, key = crop, value = b_rev_acre)
#gather(change, key = crop, value = b_rev_acre)
# change
# names(change) <- c("temp", "Rev w/o Adaptation", "Rev w/ Adaptation", "Difference")
# change <- gather(change, key = var, value = value, -temp)
# change
# change$var <- factor(change$var, levels = c("Rev w/ Adaptation", "Rev w/o Adaptation", "Difference")) 

# ggplot(change, aes(x = temp, y = value/1000000, fill = factor(var))) + 
#   geom_bar(stat = "identity", position = "dodge") + ylab("Total Revenue \n (Million $)") +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(breaks = 0:5, labels = c("0", "+1", "+2", "+3", "+4", "+5")) 
# 

# pdf("/home/john/Dropbox/Research/Adaptation Along the Envelope/figures/cs_rev_w_wo_adapt.pdf", 
#     width = 8, height = 10)

# ggplot(filter(change, var != "Difference"), aes(x = temp, y = value/1000000, color = factor(var))) + 
#   geom_line() + geom_point() + ylab("Total Revenue \n ($ Million)") + theme_tufte(base_size = 14) +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(breaks = 0:5, labels = c("0", "+1", "+2", "+3", "+4", "+5")) + ylim(0, 200) +
#   xlab("Change in Temperature (C)") +
#     theme(legend.position="top") + theme_tufte(base_size = 14) +
#       theme(legend.position = c(.9,.8), legend.justification = c("right", "bottom"), 
#             legend.box.background = element_rect(colour = "grey"), 
#         legend.key = element_blank()) + labs(color = "Cross-section")
# dev.off()  
# 
# cs_rev_w_wo_adapt

###############################################
#-----------------------------------------------
# Difference

# Cross-section with and without adaptation
# diff.rev <- data.frame(temp = diff.pred_rev$temp, 
#                      crop = diff.pred_rev$crop,
#                      rev_a = diff.pred_rev$rev, 
#                      acres = diff.pred_acres$acres
#                      )
# head(diff.rev)                     
# 
# # Get base acres
# diff.rev$b_acres <- rep(base.acres_g$acres, 36)
# 
# # Get revenue without adaptation
# diff.rev$b_rev_acre <- diff.rev$rev_a*diff.rev$b_acres
# 
# # Get revenue with adaptation
# diff.rev$c_rev_acre <- diff.rev$rev_a*diff.rev$acres
# 
# # Keep base acres for temp = 0
# diff.baset <- which(diff.rev$temp == 0)
# diff.rev$c_rev_acre[diff.baset] <- diff.rev$b_rev_acre[diff.baset]
# diff.rev
# 
# # Average temps
# diff.rev$tavg <- rep(diff.0C$tavg, 6)
# diff.rev$ctavg <- c(diff.0C$tavg, diff.1C$tavg, diff.2C$tavg, diff.3C$tavg, diff.4C$tavg, diff.5C$tavg)

# # Difference with and without adaptation
# diff.rev <- data.frame(temp = diff.pred_rev$temp, 
#                      crop = diff.pred_rev$crop,
#                      rev_a = diff.pred_rev$rev, 
#                      acres = diff.pred_acres$acres
#                      )
#                      
# base_acres <- filter(diff.rev, temp == 0)
# base_acres <- base_acres$acres
# diff.rev$b_acres <- base_acres
# names(diff.rev)
# diff.rev$b_rev_acre <- diff.rev$rev_a*diff.rev$b_acres
# diff.rev$c_rev_acre <- diff.rev$rev_a*diff.rev$acres
# diff.rev$tavg <- diff.0C$tavg

# p0 <- densityShare(filter(diff.rev, temp == 0), "tavg", "b_rev_acre") + theme(legend.position = "none") + ylab("Value of Activity \n Total Revenue") +
#     theme(legend.position = c(0,1), legend.justification = c("left", "top"), legend.box.background = element_rect(colour = "grey"), 
#         legend.title = element_blank(), legend.key = element_blank())  
# p1 <- densityShare(filter(diff.rev, temp == 1), "tavg", "b_rev_acre") + theme(legend.position = "none") + ggtitle("Change in Value of Activity without Adaptation \n (Long difference estimates)") 
# p2 <- densityShare(filter(diff.rev, temp == 2), "tavg", "b_rev_acre") + theme(legend.position = "none") 
# p3 <- densityShare(filter(diff.rev, temp == 3), "tavg", "b_rev_acre") + theme(legend.position = "none")+ xlab("Average Temperature (C)")+ ylab("Value of Activity \n Total Revenue")
# p4 <- densityShare(filter(diff.rev, temp == 4), "tavg", "b_rev_acre") + theme(legend.position = "none") + xlab("Average Temperature (C)")
# p5 <- densityShare(filter(diff.rev, temp == 5), "tavg", "b_rev_acre") + theme(legend.position = "none")+ xlab("Average Temperature (C)")
# 
# plot_grid(p0, p1, p2, p3, p4, p5, ncol = 3, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C"),label_x = .75, label_y = .75) 
# 
# p00 <- densityShare(filter(diff.rev, temp == 0), "tavg", "c_rev_acre") + theme(legend.position = "none") + ylab("Value of Activity \n Total Revenue") +
#     theme(legend.position = c(0,1), legend.justification = c("left", "top"), legend.box.background = element_rect(colour = "grey"), 
#         legend.title = element_blank(), legend.key = element_blank())  
# p11 <- densityShare(filter(diff.rev, temp == 1), "tavg", "c_rev_acre") + theme(legend.position = "none") + ggtitle("Change in Value of Activity with Adaptation \n (Long difference estimates)") 
# p22 <- densityShare(filter(diff.rev, temp == 2), "tavg", "c_rev_acre") + theme(legend.position = "none") 
# p33 <- densityShare(filter(diff.rev, temp == 3), "tavg", "c_rev_acre") + theme(legend.position = "none")+ xlab("Average Temperature (C)")+ ylab("Value of Activity \n Total Revenue")
# p44 <- densityShare(filter(diff.rev, temp == 4), "tavg", "c_rev_acre") + theme(legend.position = "none") + xlab("Average Temperature (C)")
# p55 <- densityShare(filter(diff.rev, temp == 5), "tavg", "c_rev_acre") + theme(legend.position = "none")+ xlab("Average Temperature (C)")
# 
# plot_grid(p00, p11, p22, p33, p44, p55, ncol = 3, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C"),label_x = .75, label_y = .75) 



# Differences in revenue with and without adaptation
# diff.rev$diff <- diff.rev$c_rev_acre - diff.rev$b_rev_acre
# #ggplot(diff.rev, aes(c_rev_acre, color = crop)) + geom_bar()
# 
# var(sampdat)/sqrt(length(sampdat))
# 
# change <- diff.rev %>% 
#   group_by(temp) %>% 
#   summarise(c_rev_acre = sum(c_rev_acre),
#             b_rev_acre = sum(b_rev_acre),
#             diff = sum(c_rev_acre) - sum(b_rev_acre)) %>% 
#   select(temp, b_rev_acre, c_rev_acre,  diff)
# 
# change
# # change
# #  change <- diff.rev %>% 
# #    group_by(temp) %>% 
# #    summarise(c_rev_acre = mean(c_rev_acre),
# #              b_rev_acre = mean(b_rev_acre),
# #              diff = mean(c_rev_acre) - mean(b_rev_acre)) %>% 
# #    select(temp, b_rev_acre, c_rev_acre, diff)
# 
# #change$nr <- 1:nrow(change)
# #gather(change, key = crop, value = b_rev_acre)
# #gather(change, key = crop, value = b_rev_acre)
# change
# names(change) <- c("temp", "Rev w/o Adaptation", "Rev w/ Adaptation", "Difference")
# change <- gather(change, key = var, value = value, -temp)
# change
# change$var <- factor(change$var, levels = c("Rev w/ Adaptation", "Rev w/o Adaptation", "Difference")) 
# 
# ggplot(change, aes(x = temp, y = value/1000000, fill = factor(var))) + 
#   geom_bar(stat = "identity", position = "dodge") + ylab("Total Revenue \n (Million $)") +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(breaks = 0:5, labels = c("0", "+1", "+2", "+3", "+4", "+5")) 
# 
# 
# pdf("/home/john/Dropbox/Research/Adaptation Along the Envelope/figures/diff_rev_w_wo_adapt.pdf", 
#     width = 8, height = 10)
# 
# ggplot(filter(change, var != "Difference"), aes(x = temp, y = value/1000000, color = factor(var))) + 
#   geom_line() + geom_point() + ylab("Total Revenue \n ($ Million)") + theme_tufte(base_size = 14) +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(breaks = 0:5, labels = c("0", "+1", "+2", "+3", "+4", "+5")) + ylim(0, 2000) +
#   xlab("Change in Temperature (C)") +
#     theme(legend.position="top") + theme_tufte(base_size = 14) +
#       theme(legend.position = c(.9,.8), legend.justification = c("right", "bottom"), 
#             legend.box.background = element_rect(colour = "grey"), 
#         legend.key = element_blank()) + labs(color = "Decade")
# dev.off()


# Panel using cross-section and difference acres
# Load panel data
p.dat <- readRDS("data/panel_regression_data.rds")
p.pred_rev <- readRDS("data/p.pred_revenue.rds")

# Get average from last 20-years
base.acres <- p.dat %>% 
  filter(year >= 1990) %>% 
  group_by(fips) %>% 
  summarise(Corn = mean(corn_w, na.rm = TRUE),
            Cotton = mean(cotton_w, na.rm = TRUE),
            Hay = mean(hay_w, na.rm = TRUE),
            Wheat = mean(wheat_w, na.rm = TRUE),
            Soybean = mean(soybean_w, na.rm = TRUE))

# To long format
base.acres_g <- gather(base.acres, key = crop, value = base_acres, -fips)

# Assign fips
p.pred_rev$fips <- rep(p.dat$fips, 30)

# Aggregate predicated rev to average temp in county for crop
p.pred_rev <- p.pred_rev %>% 
  group_by(temp, fips, crop) %>% 
  summarise(rev = mean(rev))
head(p.pred_rev)

# Merge base acres
p.pred_rev$crop <- tolower(p.pred_rev$crop)
base.acres_g$crop <- tolower(base.acres_g$crop)
p.pred_rev <- left_join(p.pred_rev, base.acres_g, by = c("fips", "crop")  )
head(p.pred_rev)

# Merge cross-section acres
# Load Predicted acreage
cs.pred_acres <- readRDS("data/cs.predicted_acres.rds")

cs.pred_acres$fips <- rep(cs.0C$fips, 30)
head(cs.pred_acres)
names(cs.pred_acres)[3] <- "cs_acres"
p.pred_rev <- left_join(p.pred_rev, cs.pred_acres, by = c("crop", "fips", "temp"))
head(p.pred_rev)

# Merge difference acres
diff.pred_acres <- readRDS("data/diff.predicted_acres.rds")
diff.pred_acres$fips <- rep(diff.0C$fips, 30)
diff.pred_acres <- diff.pred_acres %>% 
  group_by(temp, fips, crop) %>% 
  summarise(acres = mean(acres))
head(diff.pred_acres)                 
names(diff.pred_acres)[4] <- "diff_acres"
p.pred_rev <- left_join(p.pred_rev, diff.pred_acres, by = c("crop", "fips", "temp"))
head(p.pred_rev)

p.pred_rev$base_rev <- p.pred_rev$rev*p.pred_rev$base_acres

p.pred_rev$cs_rev <- p.pred_rev$rev*p.pred_rev$cs_acres

p.pred_rev$diff_rev <- p.pred_rev$rev*p.pred_rev$diff_acres

head(p.pred_rev)

# Bootstrap s.e.
# .e. of the sum
bse <- p.pred_rev %>% 
  group_by(crop, temp) %>% 
  summarise(base_rev_bse = boot_strap_rev(base_rev),
            cs_rev_bse = boot_strap_rev(cs_rev),
            diff_rev_bse = boot_strap_rev(diff_rev))

bse <- bse %>% 
  group_by(temp) %>% 
  summarise(base_rev_bse = sum(base_rev_bse)/1000000,
            cs_rev_bse = sum(cs_rev_bse)/1000000,
            diff_rev_bse = sum(diff_rev_bse)/1000000)
bse
names(bse) <- c("temp", "w/o Adaptation", "w/ Adaptation (CS)", "w/ Adaptation (DIFF)")
bse <- gather(bse, key = rev, value = se, -temp)

#bse <- boot_strap_rev(x = p.pred_rev$base_rev)
#bse$rev <- "w/o Adaptation"
# bse2 <- p.pred_rev %>% 
#   group_by(crop, temp) %>% 
#   summarise(bse = boot.strap(rev)[[1]])

p.plotdat <- p.pred_rev %>% 
  group_by(temp) %>% 
  summarise(base_rev = sum(base_rev),
            cs_rev = sum(cs_rev),
            diff_rev = sum(diff_rev))
head(p.plotdat)

# Set baseline 0C
p.plotdat$cs_rev[1] <- p.plotdat$base_rev[1]
p.plotdat$diff_rev[1] <- p.plotdat$base_rev[1]

names(p.plotdat) <- c("temp", "w/o Adaptation", "w/ Adaptation (CS)", "w/ Adaptation (DIFF)")
p.plotdat <- gather(p.plotdat, key = rev, value = value, -temp)

p.plotdat$rev <- factor(p.plotdat$rev, levels = c("w/o Adaptation", "w/ Adaptation (CS)", "w/ Adaptation (DIFF)"))
bse$rev <- factor(bse$rev, levels = c("w/o Adaptation", "w/ Adaptation (CS)", "w/ Adaptation (DIFF)"))

p.plotdat <- left_join(p.plotdat, bse, by = c("temp", "rev"))

#pdf("/home/john/Dropbox/Research/Adaptation Along the Envelope/figures/panel_rev_w_wo_adapt.pdf", 
#    width = 8, height = 10)

p.plotdat <- as.data.frame(p.plotdat)

#p.plotdat$base_rev_bse*1.96 + p.plotdat$value
ggplot(p.plotdat, aes(x = temp, y = value/1000000, color = factor(rev))) + 
  geom_errorbar(aes(ymin = ((value/1000000)-(1.96*(se))), ymax = ((value/1000000)+1.96*(se))), width = .1) +
  #geom_line(aes(x = temp, y = ((value/1000000)+(1.96*(se))) , color = factor(rev)), linetype = "dashed") +
  #geom_line(aes(x = temp, y = ((value/1000000)-(1.96*(se))) , color = factor(rev)), linetype = "dashed") +
  theme_tufte(base_size = 14) +
  geom_line() + geom_point() + ylab("Total Revenue \n ($ Million)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = 0:5, labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
  scale_y_continuous(label = comma, limits = c(0, 15000)) +
  xlab("Change in Temperature (C)") +
  theme(legend.position = c(.9,.8), 
        legend.justification = c("right", "bottom"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.key = element_blank(), 
        legend.title = element_blank()) 
  

#dev.off()
