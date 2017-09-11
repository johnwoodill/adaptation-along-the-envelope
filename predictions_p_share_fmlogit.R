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

cs.0C$latlong <- cs.0C$lat*cs.0C$long
cs.1C$latlong <- cs.1C$lat*cs.1C$long
cs.2C$latlong <- cs.2C$lat*cs.2C$long
cs.3C$latlong <- cs.3C$lat*cs.3C$long
cs.4C$latlong <- cs.4C$lat*cs.4C$long
cs.5C$latlong <- cs.5C$lat*cs.5C$long

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

#############################
# Get models
cs.fmlogit <- readRDS("models/cs.fmlogit_test.rds")
cs.fmlogit_effect <- readRDS("models/cs.fmlogit_effects_test.rds")


# Predictions
pred0.fm <- predict(cs.fmlogit, cs.0Ct)
pred1.fm <- predict(cs.fmlogit, cs.1Ct)
pred2.fm <- predict(cs.fmlogit, cs.2Ct)
pred3.fm <- predict(cs.fmlogit, cs.3Ct)
pred4.fm <- predict(cs.fmlogit, cs.4Ct)
pred5.fm <- predict(cs.fmlogit, cs.5Ct)

pred0.fm_acres <- stack(as.data.frame(apply(pred0.fm, 2, function(x) x*cs.0C$total_a)[,1:5]))
pred1.fm_acres <- stack(as.data.frame(apply(pred1.fm, 2, function(x) x*cs.1C$total_a)[,1:5]))
pred2.fm_acres <- stack(as.data.frame(apply(pred2.fm, 2, function(x) x*cs.2C$total_a)[,1:5]))
pred3.fm_acres <- stack(as.data.frame(apply(pred3.fm, 2, function(x) x*cs.3C$total_a)[,1:5]))
pred4.fm_acres <- stack(as.data.frame(apply(pred4.fm, 2, function(x) x*cs.4C$total_a)[,1:5]))
pred5.fm_acres <- stack(as.data.frame(apply(pred5.fm, 2, function(x) x*cs.5C$total_a)[,1:5]))


cs.pred_acres <- data.frame(temp = rep(c(0, 1, 2, 3, 4, 5), each = length(pred0.fm_acres$values)),
                         crop = rep(c("corn", "cotton", "hay", "wheat", "soybean"), 6, each = length(pred0.fm_acres$ind)/5),
                        acres = c(pred0.fm_acres$values, 
                                   pred1.fm_acres$values,
                                   pred2.fm_acres$values,
                                   pred3.fm_acres$values,
                                   pred4.fm_acres$values,
                                   pred5.fm_acres$values))
head(cs.pred_acres)           
           
saveRDS(cs.pred_acres, "data/cs.prop_w.rds")

plot.cs_prop <- cs.pred_acres %>% 
  group_by(temp, crop) %>% 
  summarise(sum_a = sum(acres))

plot.cs_prop$reg <- "Cross-section"

sum_a <- cs.prop_w %>% 
  group_by(temp) %>% 
  summarise(sum_a = sum(acres))


ggplot(plot.cs_prop, aes(temp, sum_a/1000000, color = crop)) + geom_line() + #geom_line(data = sum_a, aes(temp, sum_a/1000000), linetype = "dashed", color = "grey") + 
  #annotate("text", x = 0.5, y = sum_a$sum_a[1]/1000000 - (sum_a$sum_a[1]*.05)/1000000, label = "Total Crop Acres") + 
  theme_tufte(base_size = 14) + ylab("Total Acres \n (Million)") + xlab("Change in Temperature (C)") +
  theme(legend.position = "top",  legend.title = element_blank()) +
   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
   scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) + 
  ggtitle("Predicted Crop Acres with Increase in Temperature \n (Cross-section estimates)")

saveRDS(cs.pred_acres, "data/cs.predicted_acres.rds")
