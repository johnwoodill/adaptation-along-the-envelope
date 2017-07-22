library(lfe)
library(dplyr)
library(ggplot2)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

predictFelm.baseline <- function(felm.fit, bvar, impact = FALSE){
  felm.formula <- as.character(felm.fit$call[[2]])
  rhs          = felm.formula[3]
  last         = which(strsplit(rhs,"")[[1]]=="|")[1] - 1
  lm.formula   = paste( felm.formula[2], "~", substr(rhs, 1, last))
  dep.var <- felm.formula[2]
  exp.var <- attr(felm.fit$terms,"term.labels")
  w <<- felm.fit$weights
  

  # Get model data
  dat <- as.data.frame(cbind(felm.fit$response, felm.fit$X))
  
  # Get demeaned data from felm object
  dmdat <- as.data.frame(cbind(felm.fit$cY, felm.fit$cX)   )
  
  # No fixed-effects
     if( is.null(getfe( felm.fit )) ) {
       lm.fit    = lm( lm.formula, data=dat )
     }
  
  # W/o weights and fixed-effects
    if (is.null(w) & !is.null(getfe(felm.fit))){
      lm.formula   = paste(lm.formula, "- 1" )
       lm.fit       = lm( lm.formula, data=dmdat)
    }
    
  # With weights and fixed-effects
    if (!is.null(w) & !is.null(getfe(felm.fit))){
      lm.formula   = paste(lm.formula, "- 1" )
       lm.fit       = lm( lm.formula, data=dmdat, weights = w^2  )
    }   
    if (!isTRUE(impact)){
     rangedat = range(dat[, bvar])
     #rangeAveT = c(0, 40)
     newdata <- data.frame(rows = 1:20)
     for (i in exp.var){
       newdata[, i] <- rep(mean(dat[, i], na.rm = TRUE), 20)
     }
     newdata[, bvar] <- seq(rangedat[1], rangedat[2], length = 20)  
     newdata[, paste0(bvar, "_sq")] <- seq(rangedat[1], rangedat[2], length = 20)^2  
     newdata$rows <- NULL

     pred <- predict(lm.fit, newdata = newdata, se.fit = TRUE) 
     pred$tavg <- newdata$tavg
     return(pred)
  }
  
  if (isTRUE(impact)){
    impactdat <- data.frame(degree = rep(0, 11))
    for (i in 1:11){
    rev0 = sum( exp(felm.fit$cY) , na.rm = TRUE)
    newDat = dmdat
  	newDat$tavg = dmdat$tavg + (i - 6)
  	newDat$tavg_sq <- newDat$tavg^2
  	pred = predict(lm.fit, newdata=newDat, se.fit = TRUE)
  	res <- resid(lm.fit)
  	impactdat$degree[i] <- i - 6

  	if( is.null(getfe( felm.fit )) ) {
      impactdat$est[i] <- (sum( exp(pred$fit + res), na.rm = TRUE)/rev0 - 1)*100
    }

    if( !is.null(getfe( felm.fit )) ) {
      fe           = getfe( felm.fit )
      fe.list <- c()
      eff.dat <- data.frame(fit = pred$fit)
      #names(eff.dat) <- paste0("pred_", dep.var)
      eff.dat$res <- res
      #eff.dat[, `dep.var`] <- felm.fit$response

      for (j in names(felm.fit$fe)){
        eff.dat[, `j`] <- felm.fit$fe[[j]]
        fename <- as.character(j)
        mergedat <- filter(fe, fe == j) %>%
                 select(1, 5) %>%
                 setNames(c(paste0(j, "_effect"), paste0(fename)))
        mergedat[, 2] <- factor(mergedat[, 2])
        eff.dat[, `j`] <- factor(eff.dat[, `j`])
        eff.dat <- left_join(eff.dat, mergedat, by = `j`)
        fe.list[length(fe.list) + 1] <- paste0(fename)
      }
        # assign(paste0("fe_",j), filter(fe, fe == j) %>%
        #          select(1, 5) %>%
        #          setNames(c("effect", paste0(fename))) %>% right_join(eff.dat))
        # test <- left_join(eff.dat, )
        # fe.list[length(fe.list) + 1] <- paste0(fename)
      if (length(names(felm.fit$fe)) > 1){
    	eff.dat$effect <- rowSums(eff.dat[, grep("effect", names(eff.dat), value = TRUE)])
      } else {
        nd <- which(names(eff.dat) == grep("effect", names(eff.dat), value = TRUE))
        names(eff.dat)[nd] <- "effect"
      }
      
      impactdat$est[i] <- (sum( exp(eff.dat$fit + eff.dat$res + eff.dat$effect ), na.rm = TRUE)/rev0 - 1)*100
    }}
      return(impactdat)
    }
      
# 
#       fe$FIPS      =  as.numeric( levels(fe$idx) )[fe$idx]
#       fe           =  fe[,c("effect","FIPS")]
#       small.12     =  twelveState[,c("FIPS","lnActual","Actual")]
#       fe.expand    =  merge( small.12 , fe, by="FIPS" )
#       pred.T.level   =  exp( pred.T + fe.expand$effect )
#       pred.Tpp.level =  exp( pred.Tpp + fe.expand$effect )
#       pred.Tpn.level =  exp( pred.Tpn + fe.expand$effect )
#       pred.pp.level  =  exp( pred.pp + fe.expand$effect )
#       pred.pn.level  =  exp( pred.pn + fe.expand$effect )
    
  	
  	
  	
  	#impactdat$stder[i] <- mean(pred$se.fit)
  	#impactdat$stder[i] <- boot(pred, 1000)
  	
  #   impactdat$ymin[i] <- (sum( exp((pred - 1.96*impactdat$stder[i]) + res), na.rm = TRUE)/rev0 - 1)*100
  # 	impactdat$ymax[i] <- (sum( exp((pred + 1.96*impactdat$stder[i]) + res), na.rm = TRUE)/rev0 - 1)*100
   	
  	#impactdat$ymin[i] <- (sum( exp((pred$fit - 1.96*mean(pred$se.fit)) + res), na.rm = TRUE)/rev0 - 1)*100
  	#impactdat$ymax[i] <- (sum( exp((pred$fit + 1.96*mean(pred$se.fit)) + res), na.rm = TRUE)/rev0 - 1)*100
    print(i - 6)

  #   
  #   # fe <- getfe(felm.fit)
  #   # fe.dat <- data.frame(fe$idx, as.numeric(fe$effect))
  #   # names(fe.dat) <- c("state", "effect")
  #   # moddat <- left_join(moddat, fe.dat, by = "state")
  #   # 
  #   # pred$fit <- pred$fit + moddat$effect
  #   # plot(pred$fit)
  #   

}

######################################
######################################
{
# Crop data
cs.cropdat <- readRDS("data/cross_section_regression_data.rds")

# demean data set for prediction 

# vlist = c("ln_corn_rrev", "ln_cotton_rrev", "ln_hay_rrev", "ln_wheat_rrev", "ln_soybean_rrev", "tavg", "tavg_sq", "prec", "prec_sq",
#           "lat", "ipc", "pop_dens", "pop_dens_sq", "waterCapacity",
#           "percentClay", "minPermeability", "kFactor", "bestSoil")

# Corn
corn_cropdat <- filter(cs.cropdat, !is.na(ln_corn_rrev))          
cs.corn.mod1 <- felm(ln_corn_rrev ~ tavg + tavg_sq + prec + prec_sq + 
                     lat + ipc + pop_dens + pop_dens_sq + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, 
                   data = corn_cropdat, keepCX = TRUE, keepX = TRUE)
summary(cs.corn.mod1)

# Cotton
cotton_cropdat <- filter(cs.cropdat, !is.na(ln_cotton_rrev))          
cs.cotton.mod1 <- felm(ln_cotton_rrev ~ tavg + tavg_sq + prec + prec_sq + 
                     lat + ipc + pop_dens + pop_dens_sq + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state  | 0 | state, 
                   data = cotton_cropdat, keepCX = TRUE, keepX = TRUE)
summary(cs.cotton.mod1)

# hay
hay_cropdat <- filter(cs.cropdat, !is.na(ln_hay_rrev))          
cs.hay.mod1 <- felm(ln_hay_rrev ~ tavg + tavg_sq + prec + prec_sq + 
                     lat + ipc + pop_dens + pop_dens_sq + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, 
                   data = hay_cropdat, keepCX = TRUE, keepX = TRUE)
summary(cs.hay.mod1)

# wheat
wheat_cropdat <- filter(cs.cropdat, !is.na(ln_wheat_rrev))          
cs.wheat.mod1 <- felm(ln_wheat_rrev ~ tavg + tavg_sq + prec + prec_sq + 
                     lat + ipc + pop_dens + pop_dens_sq + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, 
                   data = wheat_cropdat, keepCX = TRUE, keepX = TRUE)
summary(cs.wheat.mod1)

# soybean
soybean_cropdat <- filter(cs.cropdat, !is.na(ln_soybean_rrev))          
cs.soybean.mod1 <- felm(ln_soybean_rrev ~ tavg + tavg_sq + prec + prec_sq + 
                     lat + ipc + pop_dens + pop_dens_sq + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, 
                   data = soybean_cropdat, keepCX = TRUE, keepX = TRUE)
summary(cs.soybean.mod1)

# Cross-section Predictions
cs.pred.corn <- predictFelm.baseline(cs.corn.mod1, bvar = "tavg")
cs.pred.cotton <- predictFelm.baseline(cs.cotton.mod1, bvar = "tavg")
cs.pred.hay <- predictFelm.baseline(cs.hay.mod1, bvar = "tavg")
cs.pred.wheat <- predictFelm.baseline(cs.wheat.mod1, bvar = "tavg")
cs.pred.soybean <- predictFelm.baseline(cs.soybean.mod1, bvar = "tavg")

dat <- data.frame(fit = c(cs.pred.corn$fit, cs.pred.cotton$fit, cs.pred.hay$fit, cs.pred.wheat$fit, cs.pred.soybean$fit),
                  se = c(cs.pred.corn$se.fit, cs.pred.cotton$se.fit, cs.pred.hay$se.fit, cs.pred.wheat$se.fit, cs.pred.soybean$se.fit),
                  tavg = c(cs.pred.corn$tavg, cs.pred.cotton$tavg, cs.pred.hay$tavg, cs.pred.wheat$tavg, cs.pred.soybean$tavg),
                  crop = rep(c("corn", "cotton", "hay", "wheat", "soybean"), 1, each = 20))
dat <- dat %>%
  group_by(crop) %>%
  mutate(fit = scale(fit))

dat$crop <- factor(dat$crop)
p1 <- ggplot(dat, aes(y = fit, x = tavg, color = crop)) + ggtitle("Cross-section Change in Temperature") + theme_tufte() +
  ylab("Log(Corn Rev)") + xlab("Average Temperature") + xlim(0, 30) +
  #geom_ribbon(aes(ymin = fit - 1.97*se, ymax = fit + 1.97*se, x = tavg), alpha = 0.3) + 
  geom_line()  
p1
}
######################################
######################################
# Panel data
{
p.cropdat <- readRDS("data/panel_regression_data.rds")
# Corn
corn_cropdat <- filter(p.cropdat, !is.na(ln_corn_rrev))          
p.corn.mod1 <- felm(ln_corn_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = corn_cropdat, keepCX = TRUE, keepX = TRUE)
summary(p.corn.mod1)

# cotton
cotton_cropdat <- filter(p.cropdat, !is.na(ln_cotton_rrev))          
p.cotton.mod1 <- felm(ln_cotton_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = cotton_cropdat, keepCX = TRUE, keepX = TRUE)
summary(p.cotton.mod1)

# hay
hay_cropdat <- filter(p.cropdat, !is.na(ln_hay_rrev))          
p.hay.mod1 <- felm(ln_hay_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = hay_cropdat, keepCX = TRUE, keepX = TRUE)
summary(p.hay.mod1)

# wheat
wheat_cropdat <- filter(p.cropdat, !is.na(ln_wheat_rrev))          
p.wheat.mod1 <- felm(ln_wheat_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = wheat_cropdat, keepCX = TRUE, keepX = TRUE)
summary(p.wheat.mod1)

# soybean
soybean_cropdat <- filter(p.cropdat, !is.na(ln_soybean_rrev))          
p.soybean.mod1 <- felm(ln_soybean_rrev ~ tavg + tavg_sq + prec + prec_sq | fips + year | 0 | state, 
                   data = soybean_cropdat, keepCX = TRUE, keepX = TRUE)
summary(p.soybean.mod1)

# Panel Predictions
p.pred.corn <- predictFelm.baseline(p.corn.mod1, "tavg")
p.pred.cotton <- predictFelm.baseline(p.cotton.mod1, "tavg")
p.pred.hay <- predictFelm.baseline(p.hay.mod1, "tavg")
p.pred.wheat <- predictFelm.baseline(p.wheat.mod1, "tavg")
p.pred.soybean <- predictFelm.baseline(p.soybean.mod1, "tavg")

p.dat <- data.frame(fit = c(p.pred.corn$fit, p.pred.cotton$fit, p.pred.hay$fit, p.pred.wheat$fit, p.pred.soybean$fit),
                  se = c(p.pred.corn$se.fit, p.pred.cotton$se.fit, p.pred.hay$se.fit, p.pred.wheat$se.fit, p.pred.soybean$se.fit),
                  tavg = c(p.pred.corn$tavg, p.pred.cotton$tavg, p.pred.hay$tavg, p.pred.wheat$tavg, p.pred.soybean$tavg),
                  crop = rep(c("corn", "cotton", "hay", "wheat", "soybean"), 1, each = 20))
p.dat <- p.dat %>%
  group_by(crop) %>%
  mutate(fit = scale(fit))

p.dat$crop <- factor(p.dat$crop)
#dat <- filter(dat, crop != "cotton")
p2 <- ggplot(p.dat, aes(y = fit, x = tavg, color = crop)) + 
  ggtitle("Panel Change in Temperature") + ylab("Log(Corn Rev)") + xlab("Average Temperature") + xlim(0, 30) +
  #geom_ribbon(aes(ymin = fit - 1.97*se, ymax = fit + 1.97*se, x = tavg), alpha = 0.3) + 
  geom_line()  + theme_tufte()
p2
}
library(cowplot)
plot_grid(p1, p2, ncol = 1)
# 
# #plotdat <- filter(newdat, crop == "corn" | crop == "corn_p")
# ggplot(plotdat, aes(y = fit, x = tavg, color = crop)) + 
#   #geom_ribbon(aes(ymin = fit - 1.97*se, ymax = fit + 1.97*se, x = tavg), alpha = 0.3) + 
#   geom_line() 




######################################
######################################
# Unused code

    dpred.T    = predict( lm.fit, newdata=twelveState.T )   + res.fit
    pred.Tpp  = predict( lm.fit, newdata=twelveState.Tpp ) + res.fit
    pred.Tpn  = predict( lm.fit, newdata=twelveState.Tpn ) + res.fit
    pred.pp   = predict( lm.fit, newdata=twelveState.pp )  + res.fit
    pred.pn   = predict( lm.fit, newdata=twelveState.pn )  + res.fit
    
    if( is.null(getfe( felm.fit )) ) {
      lm.fit    = lm( lm.formula, data=twelveState )
      pred.T.level   =  exp( pred.T )
      pred.Tpp.level =  exp( pred.Tpp )
      pred.Tpn.level =  exp( pred.Tpn )
      pred.pp.level  =  exp( pred.pp )
      pred.pn.level  =  exp( pred.pn )
    }
    
    if( !is.null(getfe( felm.fit )) ) {
      fe           = getfe( felm.fit )
      fe$FIPS      = as.numeric( levels(fe$idx) )[fe$idx]
      fe           =  fe[,c("effect","FIPS")]
      small.12     =  twelveState[,c("FIPS","lnActual","Actual")]
      fe.expand    =  merge( small.12 , fe, by="FIPS" )
      pred.T.level   =  exp( pred.T + fe.expand$effect )
      pred.Tpp.level =  exp( pred.Tpp + fe.expand$effect )
      pred.Tpn.level =  exp( pred.Tpn + fe.expand$effect )
      pred.pp.level  =  exp( pred.pp + fe.expand$effect )
      pred.pn.level  =  exp( pred.pn + fe.expand$effect )
    }
    baseline.ave  =  weighted.mean( twelveState$Actual, twelveState$w )
    pred.T.ave    =  weighted.mean( pred.T.level, twelveState$w )
    pred.Tpp.ave  =  weighted.mean( pred.Tpp.level, twelveState$w )
    pred.Tpn.ave  =  weighted.mean( pred.Tpn.level, twelveState$w )
    pred.pp.ave   =  weighted.mean( pred.pp.level, twelveState$w )
    pred.pn.ave   =  weighted.mean( pred.pn.level, twelveState$w )

    Xmat.T        =  as.data.frame( felm.fit$X )
    Xmat.Tpp      =  Xmat.T
    Xmat.Tpn      =  Xmat.T
    Xmat.pp       =  Xmat.T
    Xmat.pn       =  Xmat.T
    
    replace.cols  =  colnames(Xmat.T)[ colnames(Xmat.T) %in% colnames(twelveState.T) ]
    
    Xmat.T[, replace.cols]   = twelveState.T[, replace.cols]
    Xmat.Tpp[, replace.cols] = twelveState.Tpp[, replace.cols]
    Xmat.Tpn[, replace.cols] = twelveState.Tpn[, replace.cols]
    Xmat.pp[, replace.cols]  = twelveState.pp[, replace.cols]
    Xmat.pn[, replace.cols]  = twelveState.pn[, replace.cols]
    
    list(
        ave.impact = data.frame(
                      pred    = c( baseline.ave, pred.T.ave, pred.Tpp.ave, pred.Tpn.ave, pred.pp.ave, pred.pn.ave ),
                      pred.se = c( cc.pred.se( felm.fit$X, felm.fit$clustervcv, twelveState$w ),
                                   cc.pred.se( felm.fit$X - Xmat.T, felm.fit$clustervcv, twelveState$w ),
                                   cc.pred.se( felm.fit$X - Xmat.Tpp, felm.fit$clustervcv, twelveState$w ),
                                   cc.pred.se( felm.fit$X - Xmat.Tpn, felm.fit$clustervcv, twelveState$w ),
                                   cc.pred.se( felm.fit$X - Xmat.pp, felm.fit$clustervcv, twelveState$w ),
                                   cc.pred.se( felm.fit$X - Xmat.pn, felm.fit$clustervcv, twelveState$w )
                                  )
        ),
        county.impact = data.frame(
                      county.baseline = tapply(twelveState$Actual, twelveState$FIPS, mean),
                      county.T        = c.means(pred.T.level),
                      county.Tpp      = c.means(pred.Tpp.level),
                      county.Tpn      = c.means(pred.Tpn.level),
                      county.pp       = c.means(pred.pp.level),
                      county.pn       = c.means(pred.pn.level)

predictFelm = function(felm.fit){
    felm.formula = as.character( felm.fit$call[[2]] )
    rhs          = felm.formula[3]
    last         = which(strsplit(rhs,"")[[1]]=="|")[1] - 1
    lm.formula   = paste( felm.formula[2], "~", substr(rhs, 1, last))

    res.fit      = resid( felm.fit )
    
    if( is.null(getfe( felm.fit )) ) {
      lm.fit    = lm( lm.formula, data=twelveState )
    }
    if( !is.null(getfe( felm.fit )) ) {
      lm.formula   = paste(lm.formula, "- 1" )
      lm.fit       = lm( lm.formula, data=dmTwelveState  )
    }
    pred.T    = predict( lm.fit, newdata=twelveState.T )   + res.fit
    pred.Tpp  = predict( lm.fit, newdata=twelveState.Tpp ) + res.fit
    pred.Tpn  = predict( lm.fit, newdata=twelveState.Tpn ) + res.fit
    pred.pp   = predict( lm.fit, newdata=twelveState.pp )  + res.fit
    pred.pn   = predict( lm.fit, newdata=twelveState.pn )  + res.fit
    
    if( is.null(getfe( felm.fit )) ) {
      lm.fit    = lm( lm.formula, data=twelveState )
      pred.T.level   =  exp( pred.T )
      pred.Tpp.level =  exp( pred.Tpp )
      pred.Tpn.level =  exp( pred.Tpn )
      pred.pp.level  =  exp( pred.pp )
      pred.pn.level  =  exp( pred.pn )
    }
    
    if( !is.null(getfe( felm.fit )) ) {
      fe           = getfe( felm.fit )
      fe$FIPS      = as.numeric( levels(fe$idx) )[fe$idx]
      fe           =  fe[,c("effect","FIPS")]
      small.12     =  twelveState[,c("FIPS","lnActual","Actual")]
      fe.expand    =  merge( small.12 , fe, by="FIPS" )
      pred.T.level   =  exp( pred.T + fe.expand$effect )
      pred.Tpp.level =  exp( pred.Tpp + fe.expand$effect )
      pred.Tpn.level =  exp( pred.Tpn + fe.expand$effect )
      pred.pp.level  =  exp( pred.pp + fe.expand$effect )
      pred.pn.level  =  exp( pred.pn + fe.expand$effect )
    }
    baseline.ave  =  weighted.mean( twelveState$Actual, twelveState$w )
    pred.T.ave    =  weighted.mean( pred.T.level, twelveState$w )
    pred.Tpp.ave  =  weighted.mean( pred.Tpp.level, twelveState$w )
    pred.Tpn.ave  =  weighted.mean( pred.Tpn.level, twelveState$w )
    pred.pp.ave   =  weighted.mean( pred.pp.level, twelveState$w )
    pred.pn.ave   =  weighted.mean( pred.pn.level, twelveState$w )

    Xmat.T        =  as.data.frame( felm.fit$X )
    Xmat.Tpp      =  Xmat.T
    Xmat.Tpn      =  Xmat.T
    Xmat.pp       =  Xmat.T
    Xmat.pn       =  Xmat.T
    
    replace.cols  =  colnames(Xmat.T)[ colnames(Xmat.T) %in% colnames(twelveState.T) ]
    
    Xmat.T[, replace.cols]   = twelveState.T[, replace.cols]
    Xmat.Tpp[, replace.cols] = twelveState.Tpp[, replace.cols]
    Xmat.Tpn[, replace.cols] = twelveState.Tpn[, replace.cols]
    Xmat.pp[, replace.cols]  = twelveState.pp[, replace.cols]
    Xmat.pn[, replace.cols]  = twelveState.pn[, replace.cols]
    
    list(
        ave.impact = data.frame(
                      pred    = c( baseline.ave, pred.T.ave, pred.Tpp.ave, pred.Tpn.ave, pred.pp.ave, pred.pn.ave ),
                      pred.se = c( cc.pred.se( felm.fit$X, felm.fit$clustervcv, twelveState$w ),
                                   cc.pred.se( felm.fit$X - Xmat.T, felm.fit$clustervcv, twelveState$w ),
                                   cc.pred.se( felm.fit$X - Xmat.Tpp, felm.fit$clustervcv, twelveState$w ),
                                   cc.pred.se( felm.fit$X - Xmat.Tpn, felm.fit$clustervcv, twelveState$w ),
                                   cc.pred.se( felm.fit$X - Xmat.pp, felm.fit$clustervcv, twelveState$w ),
                                   cc.pred.se( felm.fit$X - Xmat.pn, felm.fit$clustervcv, twelveState$w )
                                  )
        ),
        county.impact = data.frame(
                      county.baseline = tapply(twelveState$Actual, twelveState$FIPS, mean),
                      county.T        = c.means(pred.T.level),
                      county.Tpp      = c.means(pred.Tpp.level),
                      county.Tpn      = c.means(pred.Tpn.level),
                      county.pp       = c.means(pred.pp.level),
                      county.pn       = c.means(pred.pn.level)
        )
    )
}
    
stat.lm.1.result   = predictFelm( stat.lm.1 )
