library(dotwhisker)
library(broom)
library(dplyr)
library(cowplot)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

# Load models

# Revenue Per Acre

cs.dd.ln_corn_rrev <- readRDS("models/cs.dd.ln_corn_rrev")
cs.dd.ln_cotton_rrev <- readRDS("models/cs.dd.ln_cotton_rrev")
cs.dd.ln_hay_rrev <- readRDS("models/cs.dd.ln_hay_rrev")
cs.dd.ln_wheat_rrev <- readRDS("models/cs.dd.ln_wheat_rrev")
cs.dd.ln_soybean_rrev <- readRDS("models/cs.dd.ln_soybean_rrev")

p.dd.ln_corn_rrev <- readRDS("models/p.dd.ln_corn_rrev")
p.dd.ln_cotton_rrev <- readRDS("models/p.dd.ln_cotton_rrev")
p.dd.ln_hay_rrev <- readRDS("models/p.dd.ln_hay_rrev")
p.dd.ln_wheat_rrev <- readRDS("models/p.dd.ln_wheat_rrev")
p.dd.ln_soybean_rrev <- readRDS("models/p.dd.ln_soybean_rrev")

diff.dd.ln_corn_rrev <- readRDS("models/diff.dd.ln_corn_rrev")
diff.dd.ln_cotton_rrev <- readRDS("models/diff.dd.ln_cotton_rrev")
diff.dd.ln_hay_rrev <- readRDS("models/diff.dd.ln_hay_rrev")
diff.dd.ln_wheat_rrev <- readRDS("models/diff.dd.ln_wheat_rrev")
diff.dd.ln_soybean_rrev <- readRDS("models/diff.dd.ln_soybean_rrev")

# Crop Share

cs.dd.p_corn_share <- readRDS("models/cs.dd.p_corn_share")
cs.dd.p_cotton_share <- readRDS("models/cs.dd.p_cotton_share")
cs.dd.p_hay_share <- readRDS("models/cs.dd.p_hay_share")
cs.dd.p_wheat_share <- readRDS("models/cs.dd.p_wheat_share")
cs.dd.p_soybean_share <- readRDS("models/cs.dd.p_soybean_share")

p.dd.p_corn_share <- readRDS("models/p.dd.p_corn_share")
p.dd.p_cotton_share <- readRDS("models/p.dd.p_cotton_share")
p.dd.p_hay_share <- readRDS("models/p.dd.p_hay_share")
p.dd.p_wheat_share <- readRDS("models/p.dd.p_wheat_share")
p.dd.p_soybean_share <- readRDS("models/p.dd.p_soybean_share")

diff.dd.p_corn_share <- readRDS("models/diff.dd.p_corn_share")
diff.dd.p_cotton_share <- readRDS("models/diff.dd.p_cotton_share")
diff.dd.p_hay_share <- readRDS("models/diff.dd.p_hay_share")
diff.dd.p_wheat_share <- readRDS("models/diff.dd.p_wheat_share")
diff.dd.p_soybean_share <- readRDS("models/diff.dd.p_soybean_share")

# Cross-section Temperature Coefficients
xsection.rev_temp <- rbind(tidy(cs.dd.ln_corn_rrev) %>% mutate(model = "Corn"),
                      tidy(cs.dd.ln_cotton_rrev) %>% mutate(model = "Cotton"),
                      tidy(cs.dd.ln_hay_rrev) %>% mutate(model = "Hay"),
                      tidy(cs.dd.ln_wheat_rrev) %>% mutate(model = "Wheat"),
                      tidy(cs.dd.ln_soybean_rrev) %>% mutate(model = "Soybean")) %>% 
  filter(term %in% c("dday0_10", "dday10_30", "dday30C")) %>% 
  group_by(model) %>% 
  relabel_predictors(c(dday0_10 = "Degree Days (0-10C)",
                       dday10_30 = "Degree Days (10-30C)",
                       dday30C = "Degree Days (30C)"))

xsection1 <- dwplot(xsection.rev_temp) + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + 
  scale_fill_brewer(palette = "YlGnBu") +
  theme_tufte() + ggtitle("Cross-section Temperature Coefficients") +
  theme(plot.title = element_text(face="bold"),
        legend.position= "top",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5,
        legend.title = element_blank())
xsection1

# Panel Temperature Coefficients
panel.rev_temp <- rbind(tidy(p.dd.ln_corn_rrev) %>% mutate(model = "Corn"),
                           tidy(p.dd.ln_cotton_rrev) %>% mutate(model = "Cotton"),
                           tidy(p.dd.ln_hay_rrev) %>% mutate(model = "Hay"),
                           tidy(p.dd.ln_wheat_rrev) %>% mutate(model = "Wheat"),
                           tidy(p.dd.ln_soybean_rrev) %>% mutate(model = "Soybean")) %>% 
  filter(term %in% c("dday0_10", "dday10_30", "dday30C")) %>% 
  group_by(model) %>% 
  relabel_predictors(c(dday0_10 = "Degree Days (0-10C)",
                       dday10_30 = "Degree Days (10-30C)",
                       dday30C = "Degree Days (30C)"))

panel1 <- dwplot(panel.rev_temp) + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + 
  scale_fill_brewer(palette = "YlGnBu") +
  theme_tufte() + ggtitle("Panel Temperature Coefficients") +
  theme(plot.title = element_text(face="bold"),
        legend.position= "top",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5,
        legend.title = element_blank())
panel1

# Diff Temperature Coefficients
diff.rev_temp <- rbind(tidy(diff.dd.ln_corn_rrev) %>% mutate(model = "Corn"),
                        tidy(diff.dd.ln_cotton_rrev) %>% mutate(model = "Cotton"),
                        tidy(diff.dd.ln_hay_rrev) %>% mutate(model = "Hay"),
                        tidy(diff.dd.ln_wheat_rrev) %>% mutate(model = "Wheat"),
                        tidy(diff.dd.ln_soybean_rrev) %>% mutate(model = "Soybean")) %>% 
  filter(term %in% c("dday0_10", "dday10_30", "dday30C")) %>% 
  group_by(model) %>% 
  relabel_predictors(c(dday0_10 = "Degree Days (0-10C)",
                       dday10_30 = "Degree Days (10-30C)",
                       dday30C = "Degree Days (30C)"))

diff1 <- dwplot(diff.rev_temp) + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + 
  scale_fill_brewer(palette = "YlGnBu") +
  theme_tufte() + ggtitle("Long Difference Temperature Coefficients") +
  theme(plot.title = element_text(face="bold"),
        legend.position= "top",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5,
        legend.title = element_blank())
diff1


plot_grid(xsection1, panel1, diff1)


# Share Regression Coeffients ---------------------------------------------

# Cross-section Temperature Coefficients
xsection.share <- rbind(tidy(cs.dd.p_corn_share) %>% mutate(model = "Corn"),
                           tidy(cs.dd.p_cotton_share) %>% mutate(model = "Cotton"),
                           tidy(cs.dd.p_hay_share) %>% mutate(model = "Hay"),
                           tidy(cs.dd.p_wheat_share) %>% mutate(model = "Wheat"),
                           tidy(cs.dd.p_soybean_share) %>% mutate(model = "Soybean")) %>% 
  filter(term %in% c("dday0_10", "dday10_30", "dday30C")) %>% 
  group_by(model) %>% 
  relabel_predictors(c(dday0_10 = "Degree Days (0-10C)",
                       dday10_30 = "Degree Days (10-30C)",
                       dday30C = "Degree Days (30C)"))

xsection1 <- dwplot(xsection.share) + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + 
  scale_fill_brewer(palette = "YlGnBu") +
  theme_tufte() + ggtitle("Cross-section Temperature Coefficients") +
  theme(plot.title = element_text(face="bold"),
        legend.position= "top",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5,
        legend.title = element_blank())
xsection1

# Panel Temperature Coefficients
panel.share <- rbind(tidy(p.dd.p_corn_share) %>% mutate(model = "Corn"),
                        tidy(p.dd.p_cotton_share) %>% mutate(model = "Cotton"),
                        tidy(p.dd.p_hay_share) %>% mutate(model = "Hay"),
                        tidy(p.dd.p_wheat_share) %>% mutate(model = "Wheat"),
                        tidy(p.dd.p_soybean_share) %>% mutate(model = "Soybean")) %>% 
  filter(term %in% c("dday0_10", "dday10_30", "dday30C")) %>% 
  group_by(model) %>% 
  relabel_predictors(c(dday0_10 = "Degree Days (0-10C)",
                       dday10_30 = "Degree Days (10-30C)",
                       dday30C = "Degree Days (30C)"))

panel1 <- dwplot(panel.share) + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + 
  scale_fill_brewer(palette = "YlGnBu") +
  theme_tufte() + ggtitle("Panel Temperature Coefficients") +
  theme(plot.title = element_text(face="bold"),
        legend.position= "top",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5,
        legend.title = element_blank())
panel1

# Diff Temperature Coefficients
diff.share <- rbind(tidy(diff.dd.p_corn_share) %>% mutate(model = "Corn"),
                       tidy(diff.dd.p_cotton_share) %>% mutate(model = "Cotton"),
                       tidy(diff.dd.p_hay_share) %>% mutate(model = "Hay"),
                       tidy(diff.dd.p_wheat_share) %>% mutate(model = "Wheat"),
                       tidy(diff.dd.ln_soybean_rrev) %>% mutate(model = "Soybean")) %>% 
  filter(term %in% c("dday0_10", "dday10_30", "dday30C")) %>% 
  group_by(model) %>% 
  relabel_predictors(c(dday0_10 = "Degree Days (0-10C)",
                       dday10_30 = "Degree Days (10-30C)",
                       dday30C = "Degree Days (30C)"))

diff1 <- dwplot(diff.share) + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + 
  scale_fill_brewer(palette = "YlGnBu") +
  theme_tufte() + ggtitle("Long Difference Temperature Coefficients") +
  theme(plot.title = element_text(face="bold"),
        legend.position= "top",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5,
        legend.title = element_blank())
diff1


plot_grid(xsection1, panel1, diff1)

