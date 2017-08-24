library(tidyverse)

# Load rev crop predictions
pred_rev <- readRDS("data/pred_rev_dat.rds")
pred_rev$min <- pred_rev$rev - (1.96*2.5
pred_rev$max <- pred_rev$rev + 1.96*2.5

# Load Predicted acreage
pred_acreage <- readRDS("data/avg_predicted_acreage.rds")

# Predictions under Panel predictions

# Corn
adapt <- left_join(pred_rev, pred_acreage, by = c("crop", "reg", "temp"))

adapt <- adapt %>% 
  group_by(reg, crop) %>% 
  mutate(adapt_rev = rev*acreage,
         adapt_rev_min = min*acreage,
         adapt_rev_max = max*acreage,
         wo_adapt_rev = rev*first(acreage),
         change_w_adapt = ((adapt_rev - first(adapt_rev))/first(adapt_rev))*100,
         change_wo_adapt = ((wo_adapt_rev - first(wo_adapt_rev))/first(wo_adapt_rev))*100,
         change_w_adapt_min = ((adapt_rev_min - first(adapt_rev_min))/first(adapt_rev_min))*100,
         change_w_adapt_max = ((adapt_rev_max - first(adapt_rev_min))/first(adapt_rev_max))*100)

adapt
ggplot(adapt, aes(y = change_w_adapt, x = temp, color = reg)) + facet_wrap(~crop) + geom_line()
ggplot(adapt, aes(y = change_wo_adapt, x = temp, color = reg)) + facet_wrap(~crop) + geom_line() 
                   
total_rev <- adapt %>% 
  group_by(temp, reg) %>% 
  summarise(total_rev_w_adapt = sum(adapt_rev),
            total_rev_w_adapt_min = sum(adapt_rev_min),
            total_rev_w_adapt_max = sum(adapt_rev_max),
            total_rev_wo_adapt = sum(wo_adapt_rev)) %>% 
  group_by(reg) %>% 
  mutate(change_adapt = ((total_rev_w_adapt - first(total_rev_w_adapt))/first(total_rev_w_adapt))*100,
         change_adapt_min = ((total_rev_w_adapt_min - first(total_rev_w_adapt_min))/first(total_rev_w_adapt_min))*100,
         change_adapt_max = ((total_rev_w_adapt_max - first(total_rev_w_adapt_max))/first(total_rev_w_adapt_max))*100,
         change_wo_adapt = ((total_rev_wo_adapt - first(total_rev_wo_adapt))/first(total_rev_wo_adapt))*100)

total_rev
ggplot(total_rev, aes(temp, change_adapt, color = reg)) + geom_line() + geom_line(aes(temp, change_adapt_min), linetype = "dashed")+ geom_line(aes(temp, change_adapt_max), linetype = "dashed")
ggplot(filter(total_rev, reg == "Cross-section"), aes(temp, change_adapt, color = reg)) + geom_line() + geom_line(aes(temp, change_adapt_min), linetype = "dashed")+ geom_line(aes(temp, change_adapt_max), linetype = "dashed")
ggplot(total_rev, aes(temp, change_wo_adapt, color = reg)) + geom_line()

