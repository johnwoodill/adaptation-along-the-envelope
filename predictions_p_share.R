library(tidyverse)
library(lfe)
library(AER)
library(cowplot)
library(ggthemes)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
source("R/predictFelm.R")
source("R/predictShare.R")
source("R/fitWeight.R")

tobit.ey <- function(mu, sigma){
  p0 <- pnorm(mu/sigma)
  lambda <- function(x) dnorm(x)/pnorm(x)
  ey0 <- mu + sigma * lambda(mu/sigma)
  ey <- p0 * ey0
  return(ey)
}

# New Degree Day Data
cs.0C <- readRDS("data/degree_day_changes/cross_section_regression_data_0C")
cs.1C <- readRDS("data/degree_day_changes/cross_section_regression_data_1C")
cs.2C <- readRDS("data/degree_day_changes/cross_section_regression_data_2C")
cs.3C <- readRDS("data/degree_day_changes/cross_section_regression_data_3C")
cs.4C <- readRDS("data/degree_day_changes/cross_section_regression_data_4C")
cs.5C <- readRDS("data/degree_day_changes/cross_section_regression_data_5C")

p.0C <- readRDS("data/degree_day_changes/panel_regression_data_0C")
p.1C <- readRDS("data/degree_day_changes/panel_regression_data_1C")
p.2C <- readRDS("data/degree_day_changes/panel_regression_data_2C")
p.3C <- readRDS("data/degree_day_changes/panel_regression_data_3C")
p.4C <- readRDS("data/degree_day_changes/panel_regression_data_4C")
p.5C <- readRDS("data/degree_day_changes/panel_regression_data_5C")

diff.0C <- readRDS("data/degree_day_changes/diff_regression_data_0C")
diff.1C <- readRDS("data/degree_day_changes/diff_regression_data_1C")
diff.2C <- readRDS("data/degree_day_changes/diff_regression_data_2C")
diff.3C <- readRDS("data/degree_day_changes/diff_regression_data_3C")
diff.4C <- readRDS("data/degree_day_changes/diff_regression_data_4C")
diff.5C <- readRDS("data/degree_day_changes/diff_regression_data_5C")


#############################
# Get models
cs.p_corn_share <- readRDS("models/cs.dd.p_corn_share")
p.p_corn_share <- readRDS("models/p.dd.p_corn_share")
diff.p_corn_share <- readRDS("models/diff.dd.p_corn_share")

cs.p_cotton_share <- readRDS("models/cs.dd.p_cotton_share")
p.p_cotton_share <- readRDS("models/p.dd.p_cotton_share")
diff.p_cotton_share <- readRDS("models/diff.dd.p_cotton_share")

cs.p_hay_share <- readRDS("models/cs.dd.p_hay_share")
p.p_hay_share <- readRDS("models/p.dd.p_hay_share")
diff.p_hay_share <- readRDS("models/diff.dd.p_hay_share")

cs.p_wheat_share <- readRDS("models/cs.dd.p_wheat_share")
p.p_wheat_share <- readRDS("models/p.dd.p_wheat_share")
diff.p_wheat_share <- readRDS("models/diff.dd.p_wheat_share")

cs.p_soybean_share <- readRDS("models/cs.dd.p_soybean_share")
p.p_soybean_share <- readRDS("models/p.dd.p_soybean_share")
diff.p_soybean_share <- readRDS("models/diff.dd.p_soybean_share")


# List of models
corn.models <- list(cs.corn = cs.p_corn_share,
                    p.corn = p.p_corn_share,
                    diff.corn = diff.p_corn_share)

cotton.models <- list(cs.cotton = cs.p_cotton_share,
                    p.cotton = p.p_cotton_share,
                    diff.cotton = diff.p_cotton_share)

hay.models <- list(cs.hay = cs.p_hay_share,
                    p.hay = p.p_hay_share,
                    diff.hay = diff.p_hay_share)

wheat.models <- list(cs.wheat = cs.p_wheat_share,
                    p.wheat = p.p_wheat_share,
                    diff.wheat = diff.p_wheat_share)

soybean.models <- list(cs.soybean = cs.p_soybean_share,
                    p.soybean = p.p_soybean_share,
                    diff.soybean = diff.p_soybean_share)

# List of data
model.dat <- list(cs.0C = cs.0C,
                 cs.1C = cs.1C,
                 cs.2C = cs.2C,
                 cs.3C = cs.3C,
                 cs.4C = cs.4C,
                 cs.5C = cs.5C,
                 p.0C = p.0C,
                 p.1C = p.1C,
                 p.2C = p.2C,
                 p.3C = p.3C,
                 p.4C = p.4C,
                 p.5C = p.5C,
                 diff.0C = diff.0C,
                 diff.1C = diff.1C,
                 diff.2C = diff.2C,
                 diff.3C = diff.3C,
                 diff.4C = diff.4C,
                 diff.5C = diff.5C)

# 
# models = corn.models
# data = model.dat
# crop = "Corn"

# Cross-section Predictions
corn.pred <- predictShare(models = corn.models, data = model.dat, crop = "corn")
cotton.pred <- predictShare(models = cotton.models, data = model.dat, crop = "cotton")
hay.pred <- predictShare(models = hay.models, data = model.dat, crop = "hay")
wheat.pred <- predictShare(models = wheat.models, data = model.dat, crop = "wheat")
soybean.pred <- predictShare(models = soybean.models, data = model.dat, crop = "soybean")

# Cross-section weighted proportions
corn.pred_prop <- corn.pred$pred.prop$cs.prop
cotton.pred_prop <- cotton.pred$pred.prop$cs.prop
hay.pred_prop <- hay.pred$pred.prop$cs.prop
wheat.pred_prop <- wheat.pred$pred.prop$cs.prop
soybean.pred_prop <- soybean.pred$pred.prop$cs.prop

# Bind and weight
pred.prop <- rbind(corn.pred_prop, cotton.pred_prop, hay.pred_prop, wheat.pred_prop, soybean.pred_prop)
cs.prop_w <- fitWeight(pred.prop)

cs.prop_w$corn <- cs.prop_w$corn*cs.0C$total_a
cs.prop_w$cotton <- cs.prop_w$cotton*cs.0C$total_a
cs.prop_w$hay <- cs.prop_w$hay*cs.0C$total_a
cs.prop_w$wheat <- cs.prop_w$wheat*cs.0C$total_a
cs.prop_w$soybean <- cs.prop_w$soybean*cs.0C$total_a


cs.prop_w <- gather(cs.prop_w, key = crop, value = acres, -temp)
saveRDS(cs.prop_w, "data/cs.prop_w.rds")
plot.cs_prop <- cs.prop_w %>% 
  group_by(temp, crop) %>% 
  summarise(sum_a = sum(acres))

sum_a <- cs.prop_w %>% 
  group_by(temp) %>% 
  summarise(sum_a = sum(acres))

ggplot(plot.cs_prop, aes(temp, sum_a, color = crop)) + geom_line() + geom_line(data = sum_a, aes(temp, sum_a), linetype = "dashed", color = "grey") + 
  annotate("text", x = 0.5, y = 21764450, label = "Total Acres")


# Panel weighted proportions
corn.pred_prop <- corn.pred$pred.prop$p.prop
cotton.pred_prop <- cotton.pred$pred.prop$p.prop
hay.pred_prop <- hay.pred$pred.prop$p.prop
wheat.pred_prop <- wheat.pred$pred.prop$p.prop
soybean.pred_prop <- soybean.pred$pred.prop$p.prop

# Bind and weight
pred.prop <- rbind(corn.pred_prop, cotton.pred_prop, hay.pred_prop, wheat.pred_prop, soybean.pred_prop)
p.prop_w <- fitWeight(pred.prop)

p.prop_w$corn <- p.prop_w$corn*p.0C$total_a
p.prop_w$cotton <- p.prop_w$cotton*p.0C$total_a
p.prop_w$hay <- p.prop_w$hay*p.0C$total_a
p.prop_w$wheat <- p.prop_w$wheat*p.0C$total_a
p.prop_w$soybean <- p.prop_w$soybean*p.0C$total_a


p.prop_w <- gather(p.prop_w, key = crop, value = acres, -temp)
saveRDS(p.prop_w, "data/p.prop_w.rds")
plot.cs_prop <- p.prop_w %>% 
  group_by(temp, crop) %>% 
  summarise(sum_a = sum(acres))

sum_a <- p.prop_w %>% 
  group_by(temp) %>% 
  summarise(sum_a = sum(acres))

ggplot(plot.cs_prop, aes(temp, sum_a, color = crop)) + geom_line() + geom_line(data = sum_a, aes(temp, sum_a), linetype = "dashed", color = "grey") + 
  annotate("text", x = 0.5, y = 21764450, label = "Total Acres")


# Difference weighted proportions
corn.pred_prop <- corn.pred$pred.prop$diff.prop
cotton.pred_prop <- cotton.pred$pred.prop$diff.prop
hay.pred_prop <- hay.pred$pred.prop$diff.prop
wheat.pred_prop <- wheat.pred$pred.prop$diff.prop
soybean.pred_prop <- soybean.pred$pred.prop$diff.prop

# Bind and weight
pred.prop <- rbind(corn.pred_prop, cotton.pred_prop, hay.pred_prop, wheat.pred_prop, soybean.pred_prop)
diff.prop_w <- fitWeight(pred.prop)

diff.prop_w$corn <- diff.prop_w$corn*diff.0C$total_a
diff.prop_w$cotton <- diff.prop_w$cotton*diff.0C$total_a
diff.prop_w$hay <- diff.prop_w$hay*diff.0C$total_a
diff.prop_w$wheat <- diff.prop_w$wheat*diff.0C$total_a
diff.prop_w$soybean <- diff.prop_w$soybean*diff.0C$total_a


diff.prop_w <- gather(diff.prop_w, key = crop, value = acres, -temp)
saveRDS(diff.prop_w, "data/diff.prop_w.rds")
plot.cs_prop <- diff.prop_w %>% 
  group_by(temp, crop) %>% 
  summarise(sum_a = sum(acres))

sum_a <- p.prop_w %>% 
  group_by(temp) %>% 
  summarise(sum_a = sum(acres))

ggplot(plot.cs_prop, aes(temp, sum_a, color = crop)) + geom_line() + geom_line(data = sum_a, aes(temp, sum_a), linetype = "dashed", color = "grey") + 
  annotate("text", x = 0.5, y = 21764450, label = "Total Acres")




# # Merge data
# 
# pred_change <- rbind(corn.pred$pred.change, cotton.pred$pred.change, hay.pred$pred.change, wheat.pred$pred.change, soybean.pred$pred.change)
# saveRDS(pred_change, "data/avg_predicted_acreage.rds")
# 
# pacres <- rbind(corn.pred$pred.acres$p.acres, cotton.pred$pred.acres$p.acres, hay.pred$pred.acres$p.acres, wheat.pred$pred.acres$p.acres, soybean.pred$pred.acres$p.acres)
# 
# predacres <- pacres %>% 
#   group_by(temp) %>% 
#   summarise(acres = sum(acres))
# predacres
# 
# 
# predacres <- pacres %>% 
#   group_by(temp, crop) %>% 
#   summarise(acres = sum(acres))
# predacres
# 
# ggplot(predacres, aes(temp, acres, color = crop )) + geom_line() + ggtitle("Sum of Predicted Acres by Crop") + ylab("Total Predicted Acres")+
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
#   theme_tufte(base_size = 14) +  xlab("Change in Temperature (C)") +
#   theme(panel.grid = element_blank(),legend.position = "top",
#         legend.title = element_blank()) 
# 
# 
# 
# pa$share <- round(pa$share)
# 
# p1 <- ggplot(pa, aes(temp, share, color = reg)) + 
#   geom_line(alpha=0.3, size=0.7) + ylab("Crop Acreage Impact (% Change) ") + 
#   xlab("Change in Temperature (C)") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
#   facet_wrap(~crop) + 
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
#   theme_tufte(base_size = 14) +
#   theme(panel.grid = element_blank(),legend.position = "top",
#         legend.title = element_blank()) +
#   guides(colour=guide_legend(override.aes=list(alpha = 1, size=1))) +
#   theme(legend.position = "top",
#         legend.title = element_blank()) + 
#   geom_text_repel(aes(temp, share, label = share), show.legend  = FALSE) 
# p1
# 
# 
# pa$acreage <- round(pa$acreage, 2)
# 
# pa.plot <- ggplot(pa, aes(temp, acreage, color = reg)) + 
#   geom_line(alpha=0.3, size=0.7) + ylab("Crop Acreage Impact (% Change) ") + 
#   xlab(NULL) + 
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
#   facet_wrap(~crop) + 
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
#   theme_tufte(base_size = 14) +
#   theme(panel.grid = element_blank(),legend.position = "top",
#         legend.title = element_blank()) +
#   guides(colour=guide_legend(override.aes=list(alpha = 1, size=1))) +
#   theme(legend.position = "top",
#         legend.title = element_blank()) 
#   #geom_text_repel(aes(temp, acreage, label = acreage), show.legend  = FALSE) 
# pa.plot
# 
# 
# 
# 
# 
# 
# p2 <- ggplot(filter(plotdat, crop %in% c("Wheat", "Cotton")), aes(temp, share, color = reg)) + 
#   geom_line(alpha=0.3, size=0.7) + ylab("Crop Acreage Impact (% Change) ") + 
#   xlab(NULL) + 
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
#   facet_wrap(~crop) + 
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(labels = c("+1", "+2", "+3", "+4", "+5")) +
#   theme_tufte(base_size = 14) +
#   theme(panel.grid = element_blank(),legend.position = "top",
#         legend.title = element_blank()) +
#   guides(colour=guide_legend(override.aes=list(alpha = 1, size=1))) +
#   theme(legend.position = "none",
#         legend.title = element_blank()) + 
#   geom_text_repel(aes(temp, share, label = rev), show.legend  = FALSE) + xlab("Change in Temperature (C)")  
# p2
# 
# plot_grid(p1, p2, ncol = 1)
