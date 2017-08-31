library(tidyverse)
library(cowplot)
library(ggthemes)
library(ggrepel)

# Load rev crop predictions
pred_rev <- readRDS("data/pred_rev_dat.rds")

# Load Predicted acreage
pred_acreage <- readRDS("data/avg_predicted_acreage.rds")
adapt <- left_join(pred_rev, pred_acreage, by = c("crop", "reg", "temp"))


# Predictions under model predictions -------------------------------------

adapt <- adapt %>% 
  group_by(reg, crop) %>% 
  mutate(adapt_rev = rev*acreage,
         adapt_rev_min = min*acreage,
         adapt_rev_max = max*acreage,
         wo_adapt_rev = rev*first(acreage),
         wo_adapt_rev_min = min*first(acreage),
         wo_adapt_rev_max = max*first(acreage),
         change_w_adapt = ((adapt_rev - first(adapt_rev))/first(adapt_rev))*100,
         change_wo_adapt = ((wo_adapt_rev - first(wo_adapt_rev))/first(wo_adapt_rev))*100,
         change_wo_adapt_min = ((wo_adapt_rev_min - first(wo_adapt_rev_min))/first(wo_adapt_rev_min))*100,
         change_wo_adapt_max = ((wo_adapt_rev_max - first(wo_adapt_rev_max))/first(wo_adapt_rev_max))*100,
         change_w_adapt_min = ((adapt_rev_min - first(adapt_rev_min))/first(adapt_rev_min))*100,
         change_w_adapt_max = ((adapt_rev_max - first(adapt_rev_max))/first(adapt_rev_max))*100)

adapt
ggplot(adapt, aes(y = change_w_adapt, x = temp, color = reg)) + 
  geom_ribbon(aes(ymin = change_w_adapt_min, ymax = change_w_adapt_max), fill = "#C0CCD5", size = 0) + 
  geom_line() + facet_wrap(~crop) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")

ggplot(adapt, aes(y = change_wo_adapt, x = temp, color = reg)) + 
  geom_ribbon(aes(ymin = change_wo_adapt_min, ymax = change_wo_adapt_max), fill = "#C0CCD5", size = 0) + 
  facet_wrap(~crop) + geom_line() 
                   
total_rev <- adapt %>% 
  group_by(temp, reg) %>% 
  summarise(total_rev_w_adapt = sum(adapt_rev),
            total_rev_w_adapt_min = sum(adapt_rev_min),
            total_rev_w_adapt_max = sum(adapt_rev_max),
            total_rev_wo_adapt = sum(wo_adapt_rev),
            total_rev_wo_adapt_min = sum(wo_adapt_rev_min),
            total_rev_wo_adapt_max = sum(wo_adapt_rev_max)) %>% 
  group_by(reg) %>% 
  mutate(change_adapt = ((total_rev_w_adapt - first(total_rev_w_adapt))/first(total_rev_w_adapt))*100,
         change_adapt_min = ((total_rev_w_adapt_min - first(total_rev_w_adapt_min))/first(total_rev_w_adapt_min))*100,
         change_adapt_max = ((total_rev_w_adapt_max - first(total_rev_w_adapt_max))/first(total_rev_w_adapt_max))*100,
         change_wo_adapt = ((total_rev_wo_adapt - first(total_rev_wo_adapt))/first(total_rev_wo_adapt))*100,
         change_wo_adapt_min = ((total_rev_wo_adapt_min - first(total_rev_wo_adapt_min))/first(total_rev_wo_adapt_min))*100,
         change_wo_adapt_max = ((total_rev_wo_adapt_max - first(total_rev_wo_adapt_max))/first(total_rev_wo_adapt_max))*100)

total_rev
total_rev$change_adapt <- round(total_rev$change_adapt)
total_rev$change_wo_adapt <- round(total_rev$change_wo_adapt)

pdat1 <- total_rev

# With adapation
p1 <- ggplot(pdat1, aes(temp, change_adapt, color = reg)) + 
  geom_ribbon(aes(ymin = change_adapt_min, ymax = change_adapt_max), fill = "#C0CCD5", size = 0) +
  geom_line() + guides(color=guide_legend(override.aes=list(fill=NA))) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  ylab("Revenue Impact with Adapation \n (% Change) ") + 
  geom_point(size = 0.5) +
  xlab(NULL) + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
  theme_tufte(base_size = 14) +
  theme(legend.position = c(0,1),
        legend.justification = c("left", "top"),
        legend.box.background = element_rect(colour = "grey"), 
        legend.title = element_blank(),
        legend.key = element_blank()) + 
  geom_text_repel(aes(temp, change_adapt, label = change_adapt), show.legend  = FALSE, alpha = 0.5, nudge_y = 15) 
p1
# Without adaptation
p11 <- ggplot(filter(pdat1, reg %in% c("Cross-section", "Difference")), aes(temp, change_wo_adapt, color = reg)) + 
  geom_ribbon(aes(ymin = change_wo_adapt_min, ymax = change_wo_adapt_max), fill = "#C0CCD5", size = 0) +
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  ylab("Revenue Impact Without Adapation \n (% Change) ") + 
  geom_point(size = 0.5) +
  xlab("Change in Temperature (C)") + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
  theme_tufte(base_size = 14) +
  theme(legend.position = "top",
        legend.title = element_blank()) + 
  geom_text_repel(aes(temp, change_wo_adapt, label = change_wo_adapt), show.legend  = FALSE, alpha = 0.5) 
p11

# Proportion of acreage change
ggplot(pred_acreage, aes(x = temp, y = acreage, fill = crop)) + 
  geom_bar(position = "fill",stat = "identity") + facet_wrap(~reg) +
  theme_tufte(base_size = 14) +
  xlab("Change in Temperature (C)") + 
  ylab("Crop Acreage Proportion (%)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5"), breaks = c(0, 1, 2, 3, 4, 5)) +
    theme(legend.position = "top",
        legend.title = element_blank()) 
  



# Predictions under cross-section acreage predictions -------------------------------------

# Load rev crop predictions
pred_rev <- readRDS("data/pred_rev_dat.rds")

# Load Predicted acreage
pred_acreage <- readRDS("data/avg_predicted_acreage.rds")
pred_acreage <- filter(pred_acreage, reg == "Cross-section")
pred_acreage$reg <- NULL
adapt <- left_join(pred_rev, pred_acreage, by = c("crop", "temp"))

adapt <- adapt %>% 
  group_by(reg, crop) %>% 
  mutate(adapt_rev = rev*acreage,
         adapt_rev_min = min*acreage,
         adapt_rev_max = max*acreage,
         wo_adapt_rev = rev*first(acreage),
         wo_adapt_rev_min = min*first(acreage),
         wo_adapt_rev_max = max*first(acreage),
         change_w_adapt = ((adapt_rev - first(adapt_rev))/first(adapt_rev))*100,
         change_wo_adapt = ((wo_adapt_rev - first(wo_adapt_rev))/first(wo_adapt_rev))*100,
         change_wo_adapt_min = ((wo_adapt_rev_min - first(wo_adapt_rev_min))/first(wo_adapt_rev_min))*100,
         change_wo_adapt_max = ((wo_adapt_rev_max - first(wo_adapt_rev_max))/first(wo_adapt_rev_max))*100,
         change_w_adapt_min = ((adapt_rev_min - first(adapt_rev_min))/first(adapt_rev_min))*100,
         change_w_adapt_max = ((adapt_rev_max - first(adapt_rev_max))/first(adapt_rev_max))*100)

# Change with adaptation
ggplot(adapt, aes(y = change_w_adapt, x = temp, color = reg)) + 
  geom_ribbon(aes(ymin = change_w_adapt_min, ymax = change_w_adapt_max), fill = "#C0CCD5", size = 0) + 
  geom_line() + facet_wrap(~crop) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")

# Change without adaptation
ggplot(adapt, aes(y = change_wo_adapt, x = temp, color = reg)) + 
  geom_ribbon(aes(ymin = change_wo_adapt_min, ymax = change_wo_adapt_max), fill = "#C0CCD5", size = 0) + 
  facet_wrap(~crop) + geom_line() 
                   
total_rev <- adapt %>% 
  group_by(temp, reg) %>% 
  summarise(total_rev_w_adapt = sum(adapt_rev),
            total_rev_w_adapt_min = sum(adapt_rev_min),
            total_rev_w_adapt_max = sum(adapt_rev_max),
            total_rev_wo_adapt = sum(wo_adapt_rev),
            total_rev_wo_adapt_min = sum(wo_adapt_rev_min),
            total_rev_wo_adapt_max = sum(wo_adapt_rev_max)) %>% 
  group_by(reg) %>% 
  mutate(change_adapt = ((total_rev_w_adapt - first(total_rev_w_adapt))/first(total_rev_w_adapt))*100,
         change_adapt_min = ((total_rev_w_adapt_min - first(total_rev_w_adapt_min))/first(total_rev_w_adapt_min))*100,
         change_adapt_max = ((total_rev_w_adapt_max - first(total_rev_w_adapt_max))/first(total_rev_w_adapt_max))*100,
         change_wo_adapt = ((total_rev_wo_adapt - first(total_rev_wo_adapt))/first(total_rev_wo_adapt))*100,
         change_wo_adapt_min = ((total_rev_wo_adapt_min - first(total_rev_wo_adapt_min))/first(total_rev_wo_adapt_min))*100,
         change_wo_adapt_max = ((total_rev_wo_adapt_max - first(total_rev_wo_adapt_max))/first(total_rev_wo_adapt_max))*100)

total_rev
total_rev$change_adapt <- round(total_rev$change_adapt)
total_rev$change_wo_adapt <- round(total_rev$change_wo_adapt)

pdat2 <- total_rev

# With adapation
p2 <- ggplot(pdat2, aes(temp, change_adapt, color = reg)) + 
  geom_ribbon(aes(ymin = change_adapt_min, ymax = change_adapt_max), fill = "#C0CCD5", size = 0) +
  geom_line() + guides(color=guide_legend(override.aes=list(fill=NA))) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  ylab(NULL) + ggtitle("Predicted Cross-section Acreage") +
  geom_point(size = 0.5) +
  xlab(NULL) + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
  theme_tufte(base_size = 14) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.key = element_blank()) + 
  geom_text_repel(aes(temp, change_adapt, label = change_adapt), show.legend  = FALSE, alpha = 0.5, nudge_y = 15) 
p2

# Without adaptation
p22 <- ggplot(pdat2, aes(temp, change_wo_adapt, color = reg)) + 
  geom_ribbon(aes(ymin = change_wo_adapt_min, ymax = change_wo_adapt_max), fill = "#C0CCD5", size = 0) +
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  ylab("Revenue Impact Without Adapation \n (% Change) ") + 
  geom_point(size = 0.5) +
  xlab("Change in Temperature (C)") + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
  theme_tufte(base_size = 14) +
  theme(legend.position = "top",
        legend.title = element_blank()) + 
  geom_text_repel(aes(temp, change_wo_adapt, label = change_wo_adapt), show.legend  = FALSE, alpha = 0.5) 


  
# Predictions under panel acreage predictions -------------------------------------

# Load rev crop predictions
pred_rev <- readRDS("data/pred_rev_dat.rds")

# Load Predicted acreage
pred_acreage <- readRDS("data/avg_predicted_acreage.rds")
pred_acreage <- filter(pred_acreage, reg == "Panel")
pred_acreage$reg <- NULL
adapt <- left_join(pred_rev, pred_acreage, by = c("crop", "temp"))

adapt <- adapt %>% 
  group_by(reg, crop) %>% 
  mutate(adapt_rev = rev*acreage,
         adapt_rev_min = min*acreage,
         adapt_rev_max = max*acreage,
         wo_adapt_rev = rev*first(acreage),
         wo_adapt_rev_min = min*first(acreage),
         wo_adapt_rev_max = max*first(acreage),
         change_w_adapt = ((adapt_rev - first(adapt_rev))/first(adapt_rev))*100,
         change_wo_adapt = ((wo_adapt_rev - first(wo_adapt_rev))/first(wo_adapt_rev))*100,
         change_wo_adapt_min = ((wo_adapt_rev_min - first(wo_adapt_rev_min))/first(wo_adapt_rev_min))*100,
         change_wo_adapt_max = ((wo_adapt_rev_max - first(wo_adapt_rev_max))/first(wo_adapt_rev_max))*100,
         change_w_adapt_min = ((adapt_rev_min - first(adapt_rev_min))/first(adapt_rev_min))*100,
         change_w_adapt_max = ((adapt_rev_max - first(adapt_rev_max))/first(adapt_rev_max))*100)

# Change with adaptation
ggplot(adapt, aes(y = change_w_adapt, x = temp, color = reg)) + 
  geom_ribbon(aes(ymin = change_w_adapt_min, ymax = change_w_adapt_max), fill = "#C0CCD5", size = 0) + 
  geom_line() + facet_wrap(~crop) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")

# Change without adaptation
ggplot(adapt, aes(y = change_wo_adapt, x = temp, color = reg)) + 
  geom_ribbon(aes(ymin = change_wo_adapt_min, ymax = change_wo_adapt_max), fill = "#C0CCD5", size = 0) + 
  facet_wrap(~crop) + geom_line() 
                   
total_rev <- adapt %>% 
  group_by(temp, reg) %>% 
  summarise(total_rev_w_adapt = sum(adapt_rev),
            total_rev_w_adapt_min = sum(adapt_rev_min),
            total_rev_w_adapt_max = sum(adapt_rev_max),
            total_rev_wo_adapt = sum(wo_adapt_rev),
            total_rev_wo_adapt_min = sum(wo_adapt_rev_min),
            total_rev_wo_adapt_max = sum(wo_adapt_rev_max)) %>% 
  group_by(reg) %>% 
  mutate(change_adapt = ((total_rev_w_adapt - first(total_rev_w_adapt))/first(total_rev_w_adapt))*100,
         change_adapt_min = ((total_rev_w_adapt_min - first(total_rev_w_adapt_min))/first(total_rev_w_adapt_min))*100,
         change_adapt_max = ((total_rev_w_adapt_max - first(total_rev_w_adapt_max))/first(total_rev_w_adapt_max))*100,
         change_wo_adapt = ((total_rev_wo_adapt - first(total_rev_wo_adapt))/first(total_rev_wo_adapt))*100,
         change_wo_adapt_min = ((total_rev_wo_adapt_min - first(total_rev_wo_adapt_min))/first(total_rev_wo_adapt_min))*100,
         change_wo_adapt_max = ((total_rev_wo_adapt_max - first(total_rev_wo_adapt_max))/first(total_rev_wo_adapt_max))*100)

total_rev
total_rev$change_adapt <- round(total_rev$change_adapt)
total_rev$change_wo_adapt <- round(total_rev$change_wo_adapt)
pdat3 <- total_rev

# With adapation
p3 <- ggplot(pdat3, aes(temp, change_adapt, color = reg)) + 
  geom_ribbon(aes(ymin = change_adapt_min, ymax = change_adapt_max), fill = "#C0CCD5", size = 0) +
  geom_line() + guides(color=guide_legend(override.aes=list(fill=NA))) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  ylab("Revenue Impact with Adapation \n (% Change) ") + 
  geom_point(size = 0.5) + ggtitle("Predicted Panel Acreage") +
  xlab("Change in Temperature (C)") + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
  theme_tufte(base_size = 14) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.key = element_blank()) + 
  geom_text_repel(aes(temp, change_adapt, label = change_adapt), show.legend  = FALSE, alpha = 0.5, nudge_y = 15) 
p3

# Without adaptation
p33 <- ggplot(pdat3, aes(temp, change_wo_adapt, color = reg)) + 
  geom_ribbon(aes(ymin = change_wo_adapt_min, ymax = change_wo_adapt_max), fill = "#C0CCD5", size = 0) +
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  ylab("Revenue Impact Without Adapation \n (% Change) ") + 
  geom_point(size = 0.5) +
  xlab("Change in Temperature (C)") + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
  theme_tufte(base_size = 14) +
  theme(legend.position = "top",
        legend.title = element_blank()) + 
  geom_text_repel(aes(temp, change_wo_adapt, label = change_wo_adapt), show.legend  = FALSE, alpha = 0.5) 


# Predictions under difference acreage predictions -------------------------------------

# Load rev crop predictions
pred_rev <- readRDS("data/pred_rev_dat.rds")

# Load Predicted acreage
pred_acreage <- readRDS("data/avg_predicted_acreage.rds")
pred_acreage <- filter(pred_acreage, reg == "Difference")
pred_acreage$reg <- NULL
adapt <- left_join(pred_rev, pred_acreage, by = c("crop", "temp"))

adapt <- adapt %>% 
  group_by(reg, crop) %>% 
  mutate(adapt_rev = rev*acreage,
         adapt_rev_min = min*acreage,
         adapt_rev_max = max*acreage,
         wo_adapt_rev = rev*first(acreage),
         wo_adapt_rev_min = min*first(acreage),
         wo_adapt_rev_max = max*first(acreage),
         change_w_adapt = ((adapt_rev - first(adapt_rev))/first(adapt_rev))*100,
         change_wo_adapt = ((wo_adapt_rev - first(wo_adapt_rev))/first(wo_adapt_rev))*100,
         change_wo_adapt_min = ((wo_adapt_rev_min - first(wo_adapt_rev_min))/first(wo_adapt_rev_min))*100,
         change_wo_adapt_max = ((wo_adapt_rev_max - first(wo_adapt_rev_max))/first(wo_adapt_rev_max))*100,
         change_w_adapt_min = ((adapt_rev_min - first(adapt_rev_min))/first(adapt_rev_min))*100,
         change_w_adapt_max = ((adapt_rev_max - first(adapt_rev_max))/first(adapt_rev_max))*100)

# Change with adaptation
ggplot(adapt, aes(y = change_w_adapt, x = temp, color = reg)) + 
  geom_ribbon(aes(ymin = change_w_adapt_min, ymax = change_w_adapt_max), fill = "#C0CCD5", size = 0) + 
  geom_line() + facet_wrap(~crop) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey")

# Change without adaptation
ggplot(adapt, aes(y = change_wo_adapt, x = temp, color = reg)) + 
  geom_ribbon(aes(ymin = change_wo_adapt_min, ymax = change_wo_adapt_max), fill = "#C0CCD5", size = 0) + 
  facet_wrap(~crop) + geom_line() 
                   
total_rev <- adapt %>% 
  group_by(temp, reg) %>% 
  summarise(total_rev_w_adapt = sum(adapt_rev),
            total_rev_w_adapt_min = sum(adapt_rev_min),
            total_rev_w_adapt_max = sum(adapt_rev_max),
            total_rev_wo_adapt = sum(wo_adapt_rev),
            total_rev_wo_adapt_min = sum(wo_adapt_rev_min),
            total_rev_wo_adapt_max = sum(wo_adapt_rev_max)) %>% 
  group_by(reg) %>% 
  mutate(change_adapt = ((total_rev_w_adapt - first(total_rev_w_adapt))/first(total_rev_w_adapt))*100,
         change_adapt_min = ((total_rev_w_adapt_min - first(total_rev_w_adapt_min))/first(total_rev_w_adapt_min))*100,
         change_adapt_max = ((total_rev_w_adapt_max - first(total_rev_w_adapt_max))/first(total_rev_w_adapt_max))*100,
         change_wo_adapt = ((total_rev_wo_adapt - first(total_rev_wo_adapt))/first(total_rev_wo_adapt))*100,
         change_wo_adapt_min = ((total_rev_wo_adapt_min - first(total_rev_wo_adapt_min))/first(total_rev_wo_adapt_min))*100,
         change_wo_adapt_max = ((total_rev_wo_adapt_max - first(total_rev_wo_adapt_max))/first(total_rev_wo_adapt_max))*100)

total_rev
total_rev$change_adapt <- round(total_rev$change_adapt)
total_rev$change_wo_adapt <- round(total_rev$change_wo_adapt)
pdat4 <- total_rev

# With adapation
p4 <- ggplot(pdat4, aes(temp, change_adapt, color = reg)) + 
  geom_ribbon(aes(ymin = change_adapt_min, ymax = change_adapt_max), fill = "#C0CCD5", size = 0) +
  geom_line() + guides(color=guide_legend(override.aes=list(fill=NA))) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  ylab(NULL) + ggtitle("Predicted Difference Acreage") +
  geom_point(size = 0.5) +
  xlab("Change in Temperature (C)") + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
  theme_tufte(base_size = 14) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.key = element_blank()) + 
  geom_text_repel(aes(temp, change_adapt, label = change_adapt), show.legend  = FALSE, alpha = 0.5, nudge_y = 15) 
p4

# Without adaptation
p44 <- ggplot(pdat4, aes(temp, change_wo_adapt, color = reg)) + 
  geom_ribbon(aes(ymin = change_wo_adapt_min, ymax = change_wo_adapt_max), fill = "#C0CCD5", size = 0) +
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  ylab("Revenue Impact Without Adapation \n (% Change) ") + 
  geom_point(size = 0.5) +
  xlab("Change in Temperature (C)") + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(labels = c("0", "+1", "+2", "+3", "+4", "+5")) +
  theme_tufte(base_size = 14) +
  theme(legend.position = "none",
        legend.title = element_blank()) + 
  geom_text_repel(aes(temp, change_wo_adapt, label = change_wo_adapt), show.legend  = FALSE, alpha = 0.5) 



# Combine plots
mainplot <- plot_grid(p1, p2, p3, p4, ncol = 2)
mainplot
