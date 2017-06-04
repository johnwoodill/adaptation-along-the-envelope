library(ggplot2)
library(dplyr)

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- do.call(data.frame,lapply(cropdat, function(x) replace(x, is.infinite(x),NA)))

cropdat$corn_rev <- cropdat$corn_yield*cropdat$corn_price


# Find single degree day that best predicts crop revenue ------------------

pred_dat <- data.frame()

for (i in 24:64){
  mod <- lm(corn_rev ~ cropdat[,i], data = cropdat)
  newdat <- data.frame(coef = colnames(cropdat)[i],
                       est = mod$coefficients[2],
                       tstat = summary(mod)$coefficients[,3][2])
  pred_dat <- rbind(pred_dat, newdat)
}

# Find interval of degree day that best predicts crop revenue ------------------

mpred_dat <- data.frame()
coln <- colnames(cropdat)[24:59]
vars <- expand.grid(coln, coln)

for (i in 1:796){
  coeffs <- paste(vars$Var2[i], "-", vars$Var1[i])
  form <- formula(paste("corn_rev ~ I(", coeffs , ")"))
  mod <- lm(form, data = cropdat)
  newdat <- data.frame(coef = coeffs,
                       est = mod$coefficients[2],
                       tstat = summary(mod)$coefficients[,3][2])
  mpred_dat <- rbind(mpred_dat, newdat)
  print(i)
}


