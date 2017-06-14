library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)
library(cowplot)

# Function to extract legend from ggplot object
g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
}



cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)
cropdat$corn_rev <- cropdat$corn_grain_p*cropdat$corn_rprice
cropdat$cotton_rev <- cropdat$cotton_p*cropdat$cotton_rprice
cropdat$hay_rev <- cropdat$hay_p*cropdat$hay_rprice
cropdat$wheat_rev <- cropdat$wheat_p*cropdat$wheat_rprice
cropdat$soybean_rev <- cropdat$soybean_p*cropdat$soybean_rprice

cropdat$total_rev <- cropdat$corn_rev + cropdat$cotton_rev + cropdat$hay_rev + cropdat$wheat_rev + cropdat$soybean_rev
cropdat$total_a <- cropdat$corn_grain_a + cropdat$cotton_a + cropdat$hay_a + cropdat$wheat_a + cropdat$soybean_a

# Loop through regression to find best predictor (no interesting results produced)
# # Find single degree day that best predicts crop revenue ------------------
# 
# pred_dat <- data.frame()
# 
# for (i in 24:64){
#   mod <- lm(corn_rev ~ cropdat[,i], data = cropdat)
#   newdat <- data.frame(coef = colnames(cropdat)[i],
#                        est = mod$coefficients[2],
#                        tstat = summary(mod)$coefficients[,3][2])
#   pred_dat <- rbind(pred_dat, newdat)
# }
# 
# # Find interval of degree day that best predicts crop revenue ------------------
# 
# mpred_dat <- data.frame()
# gridd <- data.frame(var1 = 0:35, var2 = 0:35)
# gridd <- expand.grid(gridd$var1, gridd$var2)
# gridd <- filter(gridd, Var1 < Var2)
# gridd$diff <- gridd$Var2 - gridd$Var1
# gridd <- filter(gridd, diff >= 5)
# 
# for (i in 1:630){
#   for (j in gridd$Var2[i]:35){
#     scoeff <- paste("dday", j, "C", sep = "")
#     coeffs <- paste("dday",gridd$Var1[i], "C", " - ", "dday", gridd$Var2[i], "C", sep = "")
#     form <- formula(paste("log(1 + corn_rev) ~ I(", coeffs , ") +", scoeff, " + prec + I(prec^2)"))
#     mod <- lm(form, data = cropdat)
#     newdat <- data.frame(coef_1 = coeffs,
#                          coeff_2 = scoeff,
#                          coef1_est = mod$coefficients[2],
#                          coef1_tstat = summary(mod)$coefficients[,3][2],
#                          coef2_est = mod$coefficients[3],
#                          coef2_tstat = summary(mod)$coefficients[,3][3],
#                          rsq = summary(mod)$r.squared)
#     mpred_dat <- rbind(mpred_dat, newdat)
#     
#   }
#   print(i)
#   }


l1 <- lm(log(1 + corn_rev) ~ tavg + I(tavg^2) + prec + I(prec^2), data = cropdat, weights = cropdat$corn_grain_a)
l2 <- lm(log(1 + cotton_rev) ~ tavg + I(tavg^2) + prec + I(prec^2), data = cropdat, weights = cropdat$cotton_a)
l3 <- lm(log(1 + hay_rev) ~ tavg + I(tavg^2) + prec + I(prec^2), data = cropdat, weights = cropdat$hay_a)
l4 <- lm(log(1 + wheat_rev) ~ tavg + I(tavg^2) + prec + I(prec^2), data = cropdat, weights = cropdat$wheat_a)
l5 <- lm(log(1 + soybean_rev) ~ tavg + I(tavg^2) + prec + I(prec^2), data = cropdat, weights = cropdat$soybean_a)

cropdat$trend <- cropdat$year - 1899
cropdat$trendsq <- cropdat$trend^2
cropdat$precsq <- cropdat$prec^2
cropdat$tavgsq <- cropdat$tavg^2

l6 <- lm.fit(log(1 + corn_rev) ~ factor(fips) + tavg + tavgsq + trend + trensq + prec + precsq, data = cropdat)

, weights = cropdat$corn_grain_a

stargazer(l1,l2,l3,l4,l5, align = TRUE, no.space = TRUE, digits = 2,  report = "vc*", 
          omit.stat = c("ser", "f"), title = "Regression Models explaining Crop Revenue (weighted by crop acreage)", dep.var.labels = c("Log(Corn Rev)", "Log(Cotton Rev)", "Log(Hay Rev)", "Log(Wheat Rev)", "Log(Soybean Rev)")
          )
stargazer(ll1,ll2,ll3,ll4,ll5, align = TRUE, no.space = TRUE, digits = 2,  report = "vc*", 
          omit.stat = c("ser", "f"), title = "Regression Models explaining Crop Revenue (weighted by crop acreage)", dep.var.labels = c("Log(Corn Rev)", "Log(Cotton Rev)", "Log(Hay Rev)", "Log(Wheat Rev)", "Log(Soybean Rev)")
          )

stargazer(lll1,lll2, align = TRUE, no.space = TRUE, digits = 2,  report = "vc*", 
          omit.stat = c("ser", "f"), title = "Regression Models explaining Crop Revenue (weighted by crop acreage)", dep.var.labels = c("Log(Total Rev)")
          )


# Smoothing Spline Regressions --------------------------------------------

cropdat$dday8_32 <- cropdat$dday8C - cropdat$dday32C

p1 <- ggplot(cropdat, aes(tavg, log(1+corn_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
p2 <- ggplot(cropdat, aes(tavg, log(1+cotton_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5"))+ theme(legend.position="none")
p3 <- ggplot(cropdat, aes(tavg, log(1+hay_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash"))+ scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
p4 <- ggplot(cropdat, aes(tavg, log(1+wheat_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5"))+ theme(legend.position="none")
p5 <- ggplot(cropdat, aes(tavg, log(1+soybean_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5"))+ theme(legend.position="none")
g1legend <- ggplot(cropdat, aes(tavg, log(1+soybean_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash"))+ scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.key.width = unit(1.8, "cm"))

p6 <- ggplot(cropdat, aes(dday8_32, log(1+corn_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
p7 <- ggplot(cropdat, aes(dday8_32, log(1+cotton_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
p8 <- ggplot(cropdat, aes(dday8_32, log(1+hay_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
p9 <- ggplot(cropdat, aes(dday8_32, log(1+wheat_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
p10 <- ggplot(cropdat, aes(dday8_32, log(1+soybean_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
g2legend <- ggplot(cropdat, aes(dday8_32, log(1+soybean_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash"))+ scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.key.width = unit(1.8, "cm"))

p11 <- ggplot(cropdat, aes(dday34C, log(1+corn_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
p12 <- ggplot(cropdat, aes(dday34C, log(1+cotton_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
p13 <- ggplot(cropdat, aes(dday34C, log(1+hay_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
p14 <- ggplot(cropdat, aes(dday34C, log(1+wheat_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
p15 <- ggplot(cropdat, aes(dday34C, log(1+soybean_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
g3legend <- ggplot(cropdat, aes(dday34C, log(1+soybean_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash"))+ scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.key.width = unit(1.8, "cm"))

p16 <- ggplot(cropdat, aes(prec, log(1+corn_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
p17 <- ggplot(cropdat, aes(prec, log(1+cotton_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
p18 <- ggplot(cropdat, aes(prec, log(1+hay_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
p19 <- ggplot(cropdat, aes(prec, log(1+wheat_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
p20 <- ggplot(cropdat, aes(prec, log(1+soybean_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.position="none")
g4legend <- ggplot(cropdat, aes(prec, log(1+soybean_rev))) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 3), aes(linetype = "solid")) + geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 4), aes(linetype = "longdash"))+ geom_smooth(method = "gam", formula = y ~ rms::rcs(x, 5), aes(linetype = "dotdash"))+ scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + scale_linetype_manual(name="Knots", values=c("solid", "longdash", "dotdash"), labels = c("3", "4", "5")) + theme(legend.key.width = unit(1.8, "cm"))


g1legend <- g_legend(g1legend)

g2legend <- g_legend(g2legend)

g3legend <- g_legend(g3legend)

g4legend <- g_legend(g4legend)

cowplot::plot_grid(p1, p2, p3, p4, p5, g1legend, ncol = 3)
cowplot::plot_grid(p6, p7, p8, p9, p10, g2legend, ncol = 3)
cowplot::plot_grid(p11, p12, p13, p14, p15, g3legend, ncol = 3)
cowplot::plot_grid(p16, p17, p18, p19, p20, g4legend, ncol = 3)

