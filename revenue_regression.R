library(ggplot2)
library(dplyr)
library(stargazer)
library(rms)

cropdat <- readRDS("data/full_ag_data.rds")



cropdat$corn_rev <- cropdat$corn_grain_p*cropdat$corn_price
cropdat$cotton_rev <- cropdat$cotton_p*cropdat$cotton_price
cropdat$hay_rev <- cropdat$hay_p*cropdat$hay_price
cropdat$wheat_rev <- cropdat$wheat_p*cropdat$wheat_price
cropdat$soybean_rev <- cropdat$soybean_p*cropdat$soybean_price

cropdat$total_rev <- cropdat$corn_rev + cropdat$cotton_rev + cropdat$hay_rev + cropdat$wheat_rev + cropdat$soybean_rev
cropdat$total_a <- cropdat$corn_grain_a + cropdat$cotton_a + cropdat$hay_a + cropdat$wheat_a + cropdat$soybean_a

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

ll1 <- lm(log(1+corn_rev) ~ I(dday8C - dday32C) + I((dday8C - dday32C)^2) + sqrt(dday34C) + prec + I(prec^2), data = cropdat, weights = cropdat$corn_grain_a)
ll2 <- lm(log(1+cotton_rev) ~ I(dday8C - dday32C) + I((dday8C - dday32C)^2) + sqrt(dday34C) + prec + I(prec^2), data = cropdat, weights = cropdat$cotton_a)
ll3 <- lm(log(1+hay_rev) ~ I(dday8C - dday32C) + I((dday8C - dday32C)^2) + sqrt(dday34C) + prec + I(prec^2), data = cropdat, weights = cropdat$hay_a)
ll4 <- lm(log(1+wheat_rev) ~ I(dday8C - dday32C) + I((dday8C - dday32C)^2) + sqrt(dday34C) + prec + I(prec^2), data = cropdat, weights = cropdat$wheat_a)
ll5 <- lm(log(1+soybean_rev) ~ I(dday8C - dday32C) + I((dday8C - dday32C)^2) + sqrt(dday34C) + prec + I(prec^2), data = cropdat, weights = cropdat$soybean_a)


lll1 <- lm(log(1 + total_rev) ~ tavg + I(tavg^2) + prec + I(prec^2), data = cropdat, weights = cropdat$total_a)
lll2 <- lm(log(1 + total_rev) ~ I(dday8C - dday32C) + I((dday8C - dday32C)^2) + sqrt(dday34C) + prec + I(prec^2), data = cropdat, weights = cropdat$total_a)


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



dat <- cropdat
ndbdist <- datadist(dat)
options("datadist" = "ndbdist")

mod1 <- ols(log(1 + corn_rev) ~ rcs(tavg),
            data=dat, x = TRUE, y = TRUE, weights = cropdat$corn_grain_a)

mod2 <- ols(log(1 + corn_rev) ~ rcs(dday8_32),
            data=dat, x = TRUE, y = TRUE, weights = cropdat$corn_grain_a)

plot(Predict(mod1))
