library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

# Crop data
cropdat <- readRDS("data/cross_section_regression_data.rds")

# demean data set for prediction 

vlist = c("ln_corn_rrev", "ln_cotton_rrev", "ln_hay_rrev", "ln_wheat_rrev", "ln_soybean_rrev", "tavg", "tavg_sq", "prec", "prec_sq",
          "lat", "ipc", "pop_dens", "pop_dens_sq", "waterCapacity",
          "percentClay", "minPermeability", "kFactor", "bestSoil")

# Corn
corn_cropdat <- filter(cropdat, !is.na(ln_corn_rrev))          
dmcorn_cropdat = demeanlist( corn_cropdat[, vlist], list(corn_cropdat$state), weights = sqrt(corn_cropdat$corn_grain_a) )
dmcorn_cropdat$state = dmcorn_cropdat$state

#corn_cropdat$ok <- ifelse(corn_cropdat$state == "ok", 1, 0)

corn_cropdat$state <- factor(corn_cropdat$state)
cs.corn.mod1 <- felm(ln_corn_rrev ~ tavg + tavg_sq + prec + prec_sq + 
                     lat + ipc + pop_dens + pop_dens_sq + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, 
                   data = corn_cropdat, weights = corn_cropdat$corn_grain_a)
summary(cc.corn.mod1)

# Cotton
cotton_cropdat <- filter(cropdat, !is.na(ln_cotton_rrev))          
dmcotton_cropdat = demeanlist( cotton_cropdat[, vlist], list(cotton_cropdat$state), weights = sqrt(cotton_cropdat$cotton_a) )
dmcotton_cropdat$state = dmcotton_cropdat$state

cotton_cropdat$state <- factor(cotton_cropdat$state)
cs.cotton.mod1 <- felm(ln_cotton_rrev ~ tavg + tavg_sq + prec + prec_sq + 
                     lat + ipc + pop_dens + pop_dens_sq + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, 
                   data = cotton_cropdat, weights = cotton_cropdat$cotton_a)
summary(cs.cotton.mod1)

# hay
hay_cropdat <- filter(cropdat, !is.na(ln_hay_rrev))          
dmhay_cropdat = demeanlist( hay_cropdat[, vlist], list(hay_cropdat$state), weights = sqrt(hay_cropdat$hay_a) )
dmhay_cropdat$state = dmhay_cropdat$state

hay_cropdat$state <- factor(hay_cropdat$state)
cs.hay.mod1 <- felm(ln_hay_rrev ~ tavg + tavg_sq + prec + prec_sq + 
                     lat + ipc + pop_dens + pop_dens_sq + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, 
                   data = hay_cropdat, weights = hay_cropdat$hay_a)
summary(cs.hay.mod1)

# wheat
wheat_cropdat <- filter(cropdat, !is.na(ln_wheat_rrev))          
dmwheat_cropdat = demeanlist( wheat_cropdat[, vlist], list(wheat_cropdat$state), weights = sqrt(wheat_cropdat$wheat_a) )
dmwheat_cropdat$state = dmwheat_cropdat$state

wheat_cropdat$state <- factor(wheat_cropdat$state)
cs.wheat.mod1 <- felm(ln_wheat_rrev ~ tavg + tavg_sq + prec + prec_sq + 
                     lat + ipc + pop_dens + pop_dens_sq + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, 
                   data = wheat_cropdat, weights = wheat_cropdat$wheat_a)
summary(cs.wheat.mod1)

# soybean
soybean_cropdat <- filter(cropdat, !is.na(ln_soybean_rrev))          
dmsoybean_cropdat = demeanlist( soybean_cropdat[, vlist], list(soybean_cropdat$state), weights = sqrt(soybean_cropdat$soybean_a) )
dmsoybean_cropdat$state = dmsoybean_cropdat$state

soybean_cropdat$state <- factor(soybean_cropdat$state)
cs.soybean.mod1 <- felm(ln_soybean_rrev ~ tavg + tavg_sq + prec + prec_sq + 
                     lat + ipc + pop_dens + pop_dens_sq + 
                     waterCapacity + percentClay + minPermeability + kFactor + bestSoil | state | 0 | state, 
                   data = soybean_cropdat, weights = soybean_cropdat$soybean_a)
summary(cs.soybean.mod1)


cs.predictFelm.baseline <- function(felm.fit, moddat, dmdat){
  felm.formula <- as.character(felm.fit$call[[2]])
  rhs          = felm.formula[3]
  last         = which(strsplit(rhs,"")[[1]]=="|")[1] - 1
  lm.formula   = paste( felm.formula[2], "~", substr(rhs, 1, last))

    res.fit      = resid( felm.fit )
    
    if( is.null(getfe( felm.fit )) ) {
      lm.fit    = lm( lm.formula, data=moddat )
    }
    if( !is.null(getfe( felm.fit )) ) {
      lm.formula   = paste(lm.formula, "- 1" )
      lm.fit       = lm( lm.formula, data=dmdat  )
    }
    
    rangeAveT = range(moddat$tavg)
    #rangeAveT = c(0, 40)
    # nob <- nrow(lm.fit$model) 
    pDataAveT = data.frame(tavg = seq(rangeAveT[1], rangeAveT[2], length = 20),
                         tavg_sq = seq(rangeAveT[1], rangeAveT[2], length = 20)^2,
                         prec = rep(mean(moddat$prec, na.rm = TRUE), 20),
                         prec_sq = rep(mean(moddat$prec_sq, na.rm = TRUE), 20),
                         lat = rep(mean(moddat$lat, na.rm = TRUE), 20),
                         pop_dens = rep(mean(moddat$pop_dens, na.rm = TRUE), 20),
                         pop_dens_sq = rep(mean(moddat$pop_dens_sq, na.rm = TRUE), 20),
                         ipc = rep(mean(moddat$ipc, na.rm = TRUE), 20),
                         waterCapacity = rep(mean(moddat$waterCapacity, na.rm = TRUE), 20),
                         percentClay = rep(mean(moddat$percentClay, na.rm = TRUE), 20),
                         minPermeability = rep(mean(moddat$minPermeability, na.rm = TRUE), 20),
                         kFactor = rep(mean(moddat$kFactor, na.rm = TRUE), 20),
                         bestSoil = rep(mean(moddat$bestSoil, na.rm = TRUE), 20))
  
    
    pred <- predict(lm.fit, newdata = pDataAveT, se.fit = TRUE) 
    
    # fe <- getfe(felm.fit)
    # fe.dat <- data.frame(fe$idx, as.numeric(fe$effect))
    # names(fe.dat) <- c("state", "effect")
    # moddat <- left_join(moddat, fe.dat, by = "state")
    # 
    # pred$fit <- pred$fit + moddat$effect
    # plot(pred$fit)
    
    pred$tavg <- pDataAveT$tavg
    return(pred)
}

pred.corn <- cs.predictFelm(cs.corn.mod1, corn_cropdat, dmcorn_cropdat)
plot(pred.corn$fit)

pred.cotton <- cs.predictFelm(cs.cotton.mod1, cotton_cropdat, dmcotton_cropdat)
plot(pred.cotton$fit)

pred.hay <- cs.predictFelm(cs.hay.mod1, hay_cropdat, dmhay_cropdat)
plot(pred.hay$fit)

pred.wheat <- cs.predictFelm(cs.wheat.mod1, wheat_cropdat, dmwheat_cropdat)
plot(pred.wheat$fit)

pred.soybean <- cs.predictFelm(cs.soybean.mod1, soybean_cropdat, dmsoybean_cropdat)
plot(pred.soybean$fit)

dat <- data.frame(fit = c(pred.corn$fit, pred.cotton$fit, pred.hay$fit, pred.wheat$fit, pred.soybean$fit),
                  se = c(pred.corn$se.fit, pred.cotton$se.fit, pred.hay$se.fit, pred.wheat$se.fit, pred.soybean$se.fit),
                  tavg = c(pred.corn$tavg, pred.cotton$tavg, pred.hay$tavg, pred.wheat$tavg, pred.soybean$tavg),
                  crop = rep(c("corn", "cotton", "hay", "wheat", "soybean"), 1, each = 20))
dat <- dat %>% 
  group_by(crop) %>% 
  mutate(fit = scale(fit))

dat$crop <- factor(dat$crop)
dat <- filter(dat, crop != "cotton")
ggplot(dat, aes(y = fit, x = tavg, color = crop)) + 
  #geom_ribbon(aes(ymin = fit - 1.97*se, ymax = fit + 1.97*se, x = tavg), alpha = 0.3) + 
  geom_line()  

plotdat <- filter(newdat, crop == "corn" | crop == "corn_p")
ggplot(plotdat, aes(y = fit, x = tavg, color = crop)) + 
  #geom_ribbon(aes(ymin = fit - 1.97*se, ymax = fit + 1.97*se, x = tavg), alpha = 0.3) + 
  geom_line() 


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