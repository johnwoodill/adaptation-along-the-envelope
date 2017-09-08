library(tidyverse)
library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

source("R/predictFelm.R")
source("R/predictRev.R")

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
cs.ln_corn_rrev <- readRDS("models/cs.dd.ln_corn_rrev")
p.ln_corn_rrev <- readRDS("models/p.dd.ln_corn_rrev")
diff.ln_corn_rrev <- readRDS("models/diff.dd.ln_corn_rrev")

cs.ln_cotton_rrev <- readRDS("models/cs.dd.ln_cotton_rrev")
p.ln_cotton_rrev <- readRDS("models/p.dd.ln_cotton_rrev")
diff.ln_cotton_rrev <- readRDS("models/diff.dd.ln_cotton_rrev")

cs.ln_hay_rrev <- readRDS("models/cs.dd.ln_hay_rrev")
p.ln_hay_rrev <- readRDS("models/p.dd.ln_hay_rrev")
diff.ln_hay_rrev <- readRDS("models/diff.dd.ln_hay_rrev")

cs.ln_wheat_rrev <- readRDS("models/cs.dd.ln_wheat_rrev")
p.ln_wheat_rrev <- readRDS("models/p.dd.ln_wheat_rrev")
diff.ln_wheat_rrev <- readRDS("models/diff.dd.ln_wheat_rrev")

cs.ln_soybean_rrev <- readRDS("models/cs.dd.ln_soybean_rrev")
p.ln_soybean_rrev <- readRDS("models/p.dd.ln_soybean_rrev")
diff.ln_soybean_rrev <- readRDS("models/diff.dd.ln_soybean_rrev")

# List of models
corn.models <- list(cs.corn = cs.ln_corn_rrev,
                    p.corn = p.ln_corn_rrev,
                    diff.corn = diff.ln_corn_rrev)

cotton.models <- list(cs.cotton = cs.ln_cotton_rrev,
                    p.cotton = p.ln_cotton_rrev,
                    diff.cotton = diff.ln_cotton_rrev)

hay.models <- list(cs.hay = cs.ln_hay_rrev,
                    p.hay = p.ln_hay_rrev,
                    diff.hay = diff.ln_hay_rrev)

wheat.models <- list(cs.wheat = cs.ln_wheat_rrev,
                    p.wheat = p.ln_wheat_rrev,
                    diff.wheat = diff.ln_wheat_rrev)

soybean.models <- list(cs.soybean = cs.ln_soybean_rrev,
                    p.soybean = p.ln_soybean_rrev,
                    diff.soybean = diff.ln_soybean_rrev)

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






# Corn predictions
corn.pred <- predictRev(models = corn.models, data = model.dat, crop = "Corn")

ggplot(corn.pred$pred.change, aes(temp, rev, group = reg)) + 
  #geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5") + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")

# Cotton predictions
cotton.pred <- predictRev(models = cotton.models, data = model.dat, crop = "Cotton")
ggplot(cotton.pred$pred.change, aes(temp, rev, group = reg)) + 
  #geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5") + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")

# Hay predictions
hay.pred <- predictRev(models = hay.models, data = model.dat, crop = "hay")
ggplot(hay.pred$pred.change, aes(temp, rev, group = reg)) + 
  #geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5") + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")

# Wheat predictions
wheat.pred <- predictRev(models = wheat.models, data = model.dat, crop = "wheat")
ggplot(wheat.pred$pred.change, aes(temp, rev, group = reg)) + 
  #geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5") + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")

# Soybean predictions
soybean.pred <- predictRev(models = soybean.models, data = model.dat, crop = "soybean")
ggplot(soybean.pred$pred.change, aes(temp, rev, group = reg)) + 
  #geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5") + 
  geom_line(aes(temp, rev, color = reg)) + geom_hline(yintercept = 0, linetype = "dashed")


library(ggthemes)
library(ggrepel)
plot.dat <- rbind(corn.pred$pred.change, cotton.pred$pred.change, hay.pred$pred.change, wheat.pred$pred.change, soybean.pred$pred.change)

cs.pred_dat <- rbind(corn.pred$pred.rev$cs.acres, 
                     cotton.pred$pred.rev$cs.acres, 
                     hay.pred$pred.rev$cs.acres, 
                     wheat.pred$pred.rev$cs.acres, 
                     soybean.pred$pred.rev$cs.acres)

p.pred_dat <- rbind(corn.pred$pred.rev$p.acres, 
                     cotton.pred$pred.rev$p.acres, 
                     hay.pred$pred.rev$p.acres, 
                     wheat.pred$pred.rev$p.acres, 
                     soybean.pred$pred.rev$p.acres)

diff.pred_dat <- rbind(corn.pred$pred.rev$diff.acres, 
                     cotton.pred$pred.rev$diff.acres, 
                     hay.pred$pred.rev$diff.acres, 
                     wheat.pred$pred.rev$diff.acres, 
                     soybean.pred$pred.rev$diff.acres)

saveRDS(cs.pred_dat, "data/cs.pred_revenue.rds")
saveRDS(p.pred_dat, "data/p.pred_revenue.rds")
saveRDS(diff.pred_dat, "data/diff.pred_revenue.rds")


p1 <- ggplot(plot.dat, aes(temp, rev, color = reg)) + 
  #geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5", size = 0) + 
  geom_line() + ylab("Revenue/Acre Impact (% Change) ") + 
  geom_point(size = 0.5) +
  xlab("Change in Temperature (C)") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
  facet_wrap(~crop) + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
  theme_tufte(base_size = 14) +
  theme(legend.position = "top",
        legend.title = element_blank()) + 
  geom_text_repel(aes(temp, rev, label = round(rev)), show.legend  = FALSE, alpha = 0.5) 
p1

#  Full Plot --------------------------------------------------------------
# library(ggrepel)
# library(ggthemes)
# library(ggplot2)
# 
# pred_rev_dat <- rbind(corn.pred_rev, cotton.pred_rev, hay.pred_rev, wheat.pred_rev, soybean.pred_rev)
# 
# 
# # Merge data
# plotdat <- rbind(corn.plotdat, cotton.plotdat, hay.plotdat, wheat.plotdat, soybean.plotdat)
# saveRDS(plotdat, "data/pred_plot_dat.rds")
# plotdat <- readRDS("data/pred_plot_dat.rds")
# plotdat$rev <- round(plotdat$rev, 2)
# plotdat$min <- round(plotdat$min, 2)
# plotdat$max <- round(plotdat$max, 2)
# 
# p1 <- ggplot(plotdat, aes(temp, rev, color = reg)) + 
#   #geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5", size = 0) + 
#   geom_line() + ylab("Revenue/Acre Impact (% Change) ") + 
#   geom_point(size = 0.5) +
#   xlab("Change in Temperature (C)") + 
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
#   facet_wrap(~crop) + 
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
#   theme_tufte(base_size = 14) +
#   theme(legend.position = "top",
#         legend.title = element_blank()) + 
#   geom_text_repel(aes(temp, rev, label = round(rev)), show.legend  = FALSE, alpha = 0.5) 
# p1
# 
# p1 <- ggplot(filter(plotdat, crop %in% c("Corn", "cotton", "hay", "wheat")), aes(temp, rev, color = reg)) + 
#   ylab("Revenue/Acre Impact (% Change) ") + 
#   geom_ribbon(aes(ymin = min, ymax = max), size = 0, fill = "#C0CCD5") + 
#   geom_line() +
#   ylim(-100, 50) +
#   geom_point(size = 0.5) +
#   xlab("Change in Temperature (C)") + 
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
#   facet_wrap(~crop) + 
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
#   theme_tufte(base_size = 14) +
#   theme(legend.position = "top",
#         legend.title = element_blank()) + 
#   geom_text_repel(aes(temp, rev, label = rev), show.legend  = FALSE, alpha = 0.5) 
# p1
# 
# df.labeled <- plotdat %>%
#   ungroup() %>% group_by(crop, rev) %>%
#   mutate(label = c(rev[1], rep(NA, length(rev) - 1)))
# 
# df.labeled = df.labeled %>% 
#   group_by(crop, is.na(label)) %>% 
#   arrange(rev) %>%
#   mutate(xval = rep(c(-0.1,-0.35), ceiling(n()/2))[1:n()])
# 
# 
# ggplot(plotdat, aes(temp, rev, color = reg)) + 
#   ylab("Revenue Impact (% Change) ") + xlab("Change in Temperature (C)") + 
#   geom_text(aes(label = rev), size=3, show.legend=FALSE) + theme_tufte(base_size = 12) +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey")+
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   geom_line(alpha=0.3, size=0.7) + 
#   facet_wrap(~crop) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
#   scale_x_continuous( labels = c("+1", "+2", "+3", "+4", "+5")) +
#   #theme_bw() + 
#   theme(panel.grid = element_blank(),legend.position = "top",
#         legend.title = element_blank()) +
#   guides(colour=guide_legend(override.aes=list(alpha = 1, size=1)))
# 
# 
# 
# ggplot(df.labeled, aes(temp, rev, color = reg)) +
#   geom_segment(aes(xend = 0, yend = rev), linetype = "dashed", color = "grey") +
#   geom_text(aes(label = label, x = -0.1), colour = "black", hjust = 1) +
#   geom_vline(xintercept = 0) +
#   geom_point() + geom_line() + facet_wrap(~crop, ncol = 3) +
#   scale_y_continuous(breaks = NULL) +
#   scale_x_continuous(limits = c(-0.5, NA)) +
#   theme(panel.grid = element_blank()) + theme_base()
# 
# 
# p2 <- ggplot(filter(plotdat, crop %in% c("corn", "cotton", "hay", "soybean")), aes(temp, rev, color = reg)) +
#   geom_line() + ylab("Impact (% Change) ") +
#   xlab("Change in Temperature (C)") + geom_hline(yintercept = 0, linetype = "dashed")+
#   facet_wrap(~crop)
# p2
# 
# plot_grid(p1, p2, ncol = 1)
# 
# c1 <- ggplot(filter(plotdat, reg == "cross-section"), aes(temp, rev, color = reg)) +
#   geom_ribbon(aes(ymin = min, ymax = max), fill = "#C0CCD5", alpha = 0.5) +
#   geom_line() + ylab("Revenue Impact (% Change) ") + geom_point() +
#   xlab("Change in Temperature (C)") + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
#    facet_wrap(~crop, ncol = 5, scale = "free") + scale_y_continuous(breaks = plotdat$rev)
# c1
# 
# c2 <- ggplot(filter(plotdat, reg == "panel"), aes(temp, rev, color = reg)) +
#   geom_line() + ylab("Revenue Impact (% Change) ") + geom_point() +
#   xlab("Change in Temperature (C)") + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
#    facet_wrap(~crop, ncol = 5) + scale_y_continuous(breaks = plotdat$rev)
# c2
# 
# c3 <- ggplot(filter(plotdat, reg == "diff"), aes(temp, rev, color = reg)) +
#   geom_line() + ylab("Revenue Impact (% Change) ") + geom_point() +
#   xlab("Change in Temperature (C)") + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
#    facet_wrap(~crop, ncol = 5) + scale_y_continuous(breaks = plotdat$rev)
# c3
# 
# plot_grid(c1, c2, c3, ncol = 1)
# 
# # Prediction tables
# pred.table <- data.frame(temp = rep(c("+1C", "+2C", "+3C", "+4C", "+5C"), 15),
#                          crop = rep(c("Corn", "Cotton", "Hay", "Wheat", "Soybean"),3, each = 5),
#                          reg = rep(c("Cross-Section", "Panel", "Difference"), each = 25),
#                          impact = c(cs.corn.rev1, cs.corn.rev2, cs.corn.rev3, cs.corn.rev4, cs.corn.rev5,
#                                     cs.cotton.rev1, cs.cotton.rev2, cs.cotton.rev3, cs.cotton.rev4, cs.cotton.rev5,
#                                     cs.hay.rev1, cs.hay.rev2, cs.hay.rev3, cs.hay.rev4, cs.hay.rev5,
#                                     cs.wheat.rev1, cs.wheat.rev2, cs.wheat.rev3, cs.wheat.rev4, cs.wheat.rev5,
#                                     cs.soybean.rev1, cs.soybean.rev2, cs.soybean.rev3, cs.soybean.rev4, cs.soybean.rev5,
#                                     p.corn.rev1, p.corn.rev2, p.corn.rev3, p.corn.rev4, p.corn.rev5,
#                                     p.cotton.rev1, p.cotton.rev2, p.cotton.rev3, p.cotton.rev4, p.cotton.rev5,
#                                     p.hay.rev1, p.hay.rev2, p.hay.rev3, p.hay.rev4, p.hay.rev5,
#                                     p.wheat.rev1, p.wheat.rev2, p.wheat.rev3, p.wheat.rev4, p.wheat.rev5,
#                                     p.soybean.rev1, p.soybean.rev2, p.soybean.rev3, p.soybean.rev4, p.soybean.rev5,
#                                     diff.corn.rev1, diff.corn.rev2, diff.corn.rev3, diff.corn.rev4, diff.corn.rev5,
#                                     diff.cotton.rev1, diff.cotton.rev2, diff.cotton.rev3, diff.cotton.rev4, diff.cotton.rev5,
#                                     diff.hay.rev1, diff.hay.rev2, diff.hay.rev3, diff.hay.rev4, diff.hay.rev5,
#                                     diff.wheat.rev1, diff.wheat.rev2, diff.wheat.rev3, diff.wheat.rev4, diff.wheat.rev5,
#                                     diff.soybean.rev1, diff.soybean.rev2, diff.soybean.rev3, diff.soybean.rev4, diff.soybean.rev5),
#                          se = c(cs1C.pred_ln_corn_rrev_se, cs2C.pred_ln_corn_rrev_se, cs3C.pred_ln_corn_rrev_se, cs4C.pred_ln_corn_rrev_se, cs5C.pred_ln_corn_rrev_se,
#                                   cs1C.pred_ln_cotton_rrev_se, cs2C.pred_ln_cotton_rrev_se, cs3C.pred_ln_cotton_rrev_se, cs4C.pred_ln_cotton_rrev_se, cs5C.pred_ln_cotton_rrev_se,
#                                   cs1C.pred_ln_hay_rrev_se, cs2C.pred_ln_hay_rrev_se, cs3C.pred_ln_hay_rrev_se, cs4C.pred_ln_hay_rrev_se, cs5C.pred_ln_hay_rrev_se,
#                                   cs1C.pred_ln_wheat_rrev_se, cs2C.pred_ln_wheat_rrev_se, cs3C.pred_ln_wheat_rrev_se, cs4C.pred_ln_wheat_rrev_se, cs5C.pred_ln_wheat_rrev_se,
#                                   cs1C.pred_ln_soybean_rrev_se, cs2C.pred_ln_soybean_rrev_se, cs3C.pred_ln_soybean_rrev_se, cs4C.pred_ln_soybean_rrev_se, cs5C.pred_ln_soybean_rrev_se,
#                                   p1C.pred_ln_corn_rrev_se, p2C.pred_ln_corn_rrev_se, p3C.pred_ln_corn_rrev_se, p4C.pred_ln_corn_rrev_se, p5C.pred_ln_corn_rrev_se,
#                                   p1C.pred_ln_cotton_rrev_se, p2C.pred_ln_cotton_rrev_se, p3C.pred_ln_cotton_rrev_se, p4C.pred_ln_cotton_rrev_se, p5C.pred_ln_cotton_rrev_se,
#                                   p1C.pred_ln_hay_rrev_se, p2C.pred_ln_hay_rrev_se, p3C.pred_ln_hay_rrev_se, p4C.pred_ln_hay_rrev_se, p5C.pred_ln_hay_rrev_se,
#                                   p1C.pred_ln_wheat_rrev_se, p2C.pred_ln_wheat_rrev_se, p3C.pred_ln_wheat_rrev_se, p4C.pred_ln_wheat_rrev_se, p5C.pred_ln_wheat_rrev_se,
#                                   p1C.pred_ln_soybean_rrev_se, p2C.pred_ln_soybean_rrev_se, p3C.pred_ln_soybean_rrev_se, p4C.pred_ln_soybean_rrev_se, p5C.pred_ln_soybean_rrev_se,
#                                   diff1C.pred_ln_corn_rrev_se, diff2C.pred_ln_corn_rrev_se, diff3C.pred_ln_corn_rrev_se, diff4C.pred_ln_corn_rrev_se, diff5C.pred_ln_corn_rrev_se,
#                                   diff1C.pred_ln_cotton_rrev_se, diff2C.pred_ln_cotton_rrev_se, diff3C.pred_ln_cotton_rrev_se, diff4C.pred_ln_cotton_rrev_se, diff5C.pred_ln_cotton_rrev_se,
#                                   diff1C.pred_ln_hay_rrev_se, diff2C.pred_ln_hay_rrev_se, diff3C.pred_ln_hay_rrev_se, diff4C.pred_ln_hay_rrev_se, diff5C.pred_ln_hay_rrev_se,
#                                   diff1C.pred_ln_wheat_rrev_se, diff2C.pred_ln_wheat_rrev_se, diff3C.pred_ln_wheat_rrev_se, diff4C.pred_ln_wheat_rrev_se, diff5C.pred_ln_wheat_rrev_se,
#                                   diff1C.pred_ln_soybean_rrev_se, diff2C.pred_ln_soybean_rrev_se, diff3C.pred_ln_soybean_rrev_se, diff4C.pred_ln_soybean_rrev_se, diff5C.pred_ln_soybean_rrev_se))
# head(pred.table)
# pred.table
# setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
# saveRDS(pred.table, "tables/predictions.rds")
# 
# head(pred.table)
# library()
# tab <- tabular
# 
# 
# 
# 
# 
# 
# 
# 
