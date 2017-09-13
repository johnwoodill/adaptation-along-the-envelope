library(tidyverse)
library(lfe)
library(AER)
library(cowplot)
library(ggthemes)
library(devtools)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

load_all("/run/media/john/1TB/SpiderOak/Projects/fmlogit/")
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

cs.0Ct <- select(cs.0C, dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)
cs.1Ct <- select(cs.1C, dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)
cs.2Ct <- select(cs.2C, dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)
cs.3Ct <- select(cs.3C, dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)
cs.4Ct <- select(cs.4C, dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)
cs.5Ct <- select(cs.5C, dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)

diff.0C <- readRDS("data/degree_day_changes/diff_regression_data_0C")
diff.1C <- readRDS("data/degree_day_changes/diff_regression_data_1C")
diff.2C <- readRDS("data/degree_day_changes/diff_regression_data_2C")
diff.3C <- readRDS("data/degree_day_changes/diff_regression_data_3C")
diff.4C <- readRDS("data/degree_day_changes/diff_regression_data_4C")
diff.5C <- readRDS("data/degree_day_changes/diff_regression_data_5C")

diff.0Ct <- select(diff.0C, dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)
diff.1Ct <- select(diff.1C, dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)
diff.2Ct <- select(diff.2C, dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)
diff.3Ct <- select(diff.3C, dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)
diff.4Ct <- select(diff.4C, dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)
diff.5Ct <- select(diff.5C, dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)

#############################
# Get models
cs.fmlogit <- readRDS("models/cs.fmlogit_test.rds")
cs.fmlogit_effect <- readRDS("models/cs.fmlogit_effects_test.rds")

diff.fmlogit <- readRDS("models/diff.fmlogit_test.rds")
diff.fmlogit_effect <- readRDS("models/diff.fmlogit_effects_test.rds")

# Predictions
cs.pred0.fm <- predict(cs.fmlogit, cs.0Ct)
cs.pred1.fm <- predict(cs.fmlogit, cs.1Ct)
cs.pred2.fm <- predict(cs.fmlogit, cs.2Ct)
cs.pred3.fm <- predict(cs.fmlogit, cs.3Ct)
cs.pred4.fm <- predict(cs.fmlogit, cs.4Ct)
cs.pred5.fm <- predict(cs.fmlogit, cs.5Ct)

cs.pred0.fm_acres <- stack(as.data.frame(apply(cs.pred0.fm, 2, function(x) x*cs.0C$total_a)[,1:5]))
cs.pred1.fm_acres <- stack(as.data.frame(apply(cs.pred1.fm, 2, function(x) x*cs.0C$total_a)[,1:5]))
cs.pred2.fm_acres <- stack(as.data.frame(apply(cs.pred2.fm, 2, function(x) x*cs.0C$total_a)[,1:5]))
cs.pred3.fm_acres <- stack(as.data.frame(apply(cs.pred3.fm, 2, function(x) x*cs.0C$total_a)[,1:5]))
cs.pred4.fm_acres <- stack(as.data.frame(apply(cs.pred4.fm, 2, function(x) x*cs.0C$total_a)[,1:5]))
cs.pred5.fm_acres <- stack(as.data.frame(apply(cs.pred5.fm, 2, function(x) x*cs.0C$total_a)[,1:5]))

diff.pred0.fm <- predict(diff.fmlogit, diff.0Ct)
diff.pred1.fm <- predict(diff.fmlogit, diff.1Ct)
diff.pred2.fm <- predict(diff.fmlogit, diff.2Ct)
diff.pred3.fm <- predict(diff.fmlogit, diff.3Ct)
diff.pred4.fm <- predict(diff.fmlogit, diff.4Ct)
diff.pred5.fm <- predict(diff.fmlogit, diff.5Ct)

diff.pred0.fm_acres <- stack(as.data.frame(apply(diff.pred0.fm, 2, function(x) x*diff.0C$total_a)[,1:5]))
diff.pred1.fm_acres <- stack(as.data.frame(apply(diff.pred1.fm, 2, function(x) x*diff.0C$total_a)[,1:5]))
diff.pred2.fm_acres <- stack(as.data.frame(apply(diff.pred2.fm, 2, function(x) x*diff.0C$total_a)[,1:5]))
diff.pred3.fm_acres <- stack(as.data.frame(apply(diff.pred3.fm, 2, function(x) x*diff.0C$total_a)[,1:5]))
diff.pred4.fm_acres <- stack(as.data.frame(apply(diff.pred4.fm, 2, function(x) x*diff.0C$total_a)[,1:5]))
diff.pred5.fm_acres <- stack(as.data.frame(apply(diff.pred5.fm, 2, function(x) x*diff.0C$total_a)[,1:5]))


cs.pred_acres <- data.frame(temp = rep(c(0, 1, 2, 3, 4, 5), each = length(cs.pred0.fm_acres$values)),
                         crop = rep(c("corn", "cotton", "hay", "wheat", "soybean"), 6, each = length(cs.pred0.fm_acres$ind)/5),
                        acres = c(cs.pred0.fm_acres$values, 
                                   cs.pred1.fm_acres$values,
                                   cs.pred2.fm_acres$values,
                                   cs.pred3.fm_acres$values,
                                   cs.pred4.fm_acres$values,
                                   cs.pred5.fm_acres$values))

diff.pred_acres <- data.frame(temp = rep(c(0, 1, 2, 3, 4, 5), each = length(diff.pred0.fm_acres$values)),
                         crop = rep(c("corn", "cotton", "hay", "wheat", "soybean"), 6, each = length(diff.pred0.fm_acres$ind)/5),
                        acres = c(diff.pred0.fm_acres$values, 
                                   diff.pred1.fm_acres$values,
                                   diff.pred2.fm_acres$values,
                                   diff.pred3.fm_acres$values,
                                   diff.pred4.fm_acres$values,
                                   diff.pred5.fm_acres$values))

head(cs.pred_acres)           
           
saveRDS(cs.pred_acres, "data/cs.prop_w.rds")
saveRDS(diff.pred_acres, "data/diff.prop_w.rds")

plot.cs_prop <- cs.pred_acres %>% 
  group_by(temp, crop) %>% 
  summarise(sum_a = sum(acres))

plot.diff_prop <- diff.pred_acres %>% 
  group_by(temp, crop) %>% 
  summarise(sum_a = sum(acres))

plot.cs_prop$reg <- "Cross-section"
plot.diff_prop$reg <- "Difference"

cs.sum_a <- cs.pred_acres %>% 
  group_by(temp) %>% 
  summarise(sum_a = sum(acres))

diff.sum_a <- diff.pred_acres %>% 
  group_by(temp) %>% 
  summarise(sum_a = sum(acres))


ggplot(plot.cs_prop, aes(temp, sum_a/1000000, color = crop)) + geom_line() + 
  geom_line(data = cs.sum_a, aes(temp, sum_a/1000000), linetype = "dashed", color = "grey") + 
  annotate("text", x = 0.5, y = cs.sum_a$sum_a[1]/1000000 - (cs.sum_a$sum_a[1]*.05)/1000000, label = "Total Crop Acres") + 
  theme_tufte(base_size = 14) + ylab("Total Acres \n (Million)") + xlab("Change in Temperature (C)") +
  theme(legend.position = "top",  legend.title = element_blank()) +
   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
   scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) 
#+ ggtitle("Predicted Crop Acres with Increase in Temperature \n (Cross-section estimates)")

ggplot(plot.diff_prop, aes(temp, sum_a/1000000, color = crop)) + geom_line() + 
  geom_line(data = diff.sum_a, aes(temp, sum_a/1000000), linetype = "dashed", color = "grey") + 
  annotate("text", x = 0.5, y = diff.sum_a$sum_a[1]/1000000 - (diff.sum_a$sum_a[1]*.05)/1000000, label = "Total Crop Acres") + 
  theme_tufte(base_size = 14) + ylab("Total Acres \n (Million)") + xlab("Change in Temperature (C)") +
  theme(legend.position = "top",  legend.title = element_blank()) +
   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
   scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) 
#+ ggtitle("Predicted Crop Acres with Increase in Temperature \n (Cross-section estimates)")

saveRDS(cs.pred_acres, "data/cs.predicted_acres.rds")
saveRDS(diff.pred_acres, "data/diff.predicted_acres.rds")
