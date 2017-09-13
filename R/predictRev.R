# Get predictions
predictRev <- function(models, data, crop, se = FALSE){
  
  # Cross-section Predictions
  cs0C.pred <- predictFelm(felm.fit = models[[1]], newdata = data[[1]])
  cs1C.pred <- predictFelm(felm.fit = models[[1]], newdata = data[[2]])
  cs2C.pred <- predictFelm(felm.fit = models[[1]], newdata = data[[3]])
  cs3C.pred <- predictFelm(felm.fit = models[[1]], newdata = data[[4]])
  cs4C.pred <- predictFelm(felm.fit = models[[1]], newdata = data[[5]])
  cs5C.pred <- predictFelm(felm.fit = models[[1]], newdata = data[[6]])
  
  # Panel Predictions
  p0C.pred <- predictFelm(felm.fit = models[[2]], newdata = data[[7]])
  p1C.pred <- predictFelm(felm.fit = models[[2]], newdata = data[[8]])
  p2C.pred <- predictFelm(felm.fit = models[[2]], newdata = data[[9]])
  p3C.pred <- predictFelm(felm.fit = models[[2]], newdata = data[[10]])
  p4C.pred <- predictFelm(felm.fit = models[[2]], newdata = data[[11]])
  p5C.pred <- predictFelm(felm.fit = models[[2]], newdata = data[[12]])
  
  # Difference Predictions
  diff0C.pred <- predictFelm(felm.fit = models[[3]], newdata = data[[13]])
  diff1C.pred <- predictFelm(felm.fit = models[[3]], newdata = data[[14]])
  diff2C.pred <- predictFelm(felm.fit = models[[3]], newdata = data[[15]])
  diff3C.pred <- predictFelm(felm.fit = models[[3]], newdata = data[[16]])
  diff4C.pred <- predictFelm(felm.fit = models[[3]], newdata = data[[17]])
  diff5C.pred <- predictFelm(felm.fit = models[[3]], newdata = data[[18]])
  
  # Bootstrapped Standard errors 
  cs0C.pred_se <- boot.strap(exp(cs0C.pred$fit + cs0C.pred$res + cs0C.pred$effects ))
  cs1C.pred_se <- boot.strap(exp(cs1C.pred$fit + cs1C.pred$res + cs1C.pred$effect ))
  cs2C.pred_se <- boot.strap(exp(cs2C.pred$fit + cs2C.pred$res + cs2C.pred$effect ))
  cs3C.pred_se <- boot.strap(exp(cs3C.pred$fit + cs3C.pred$res + cs3C.pred$effect ))
  cs4C.pred_se <- boot.strap(exp(cs4C.pred$fit + cs4C.pred$res + cs4C.pred$effect ))
  cs5C.pred_se <- boot.strap(exp(cs5C.pred$fit + cs5C.pred$res + cs5C.pred$effect ))

  p0C.pred_se <- boot.strap(exp(p0C.pred$fit + p0C.pred$res + p0C.pred$effect), cluster = p0C.pred$pred_data$year, rep = 100 )
  p1C.pred_se <- boot.strap(exp(p1C.pred$fit + p1C.pred$res + p1C.pred$effect), cluster = p1C.pred$pred_data$year, rep = 100 )
  p2C.pred_se <- boot.strap(exp(p2C.pred$fit + p2C.pred$res + p2C.pred$effect), cluster = p2C.pred$pred_data$year, rep = 100 )
  p3C.pred_se <- boot.strap(exp(p3C.pred$fit + p3C.pred$res + p3C.pred$effect), cluster = p3C.pred$pred_data$year, rep = 100 )
  p4C.pred_se <- boot.strap(exp(p4C.pred$fit + p4C.pred$res + p4C.pred$effect), cluster = p4C.pred$pred_data$year, rep = 100 )
  p5C.pred_se <- boot.strap(exp(p5C.pred$fit + p5C.pred$res + p5C.pred$effect), cluster = p5C.pred$pred_data$year, rep = 100 )

  diff0C.pred_se <- boot.strap(exp(diff0C.pred$fit + diff0C.pred$res + diff0C.pred$effect), cluster = diff0C.pred$pred_data$year )
  diff1C.pred_se <- boot.strap(exp(diff1C.pred$fit + diff1C.pred$res + diff1C.pred$effect), cluster = diff1C.pred$pred_data$year )
  diff2C.pred_se <- boot.strap(exp(diff2C.pred$fit + diff2C.pred$res + diff2C.pred$effect), cluster = diff2C.pred$pred_data$year )
  diff3C.pred_se <- boot.strap(exp(diff3C.pred$fit + diff3C.pred$res + diff3C.pred$effect), cluster = diff3C.pred$pred_data$year )
  diff4C.pred_se <- boot.strap(exp(diff4C.pred$fit + diff4C.pred$res + diff4C.pred$effect), cluster = diff4C.pred$pred_data$year )
  diff5C.pred_se <- boot.strap(exp(diff5C.pred$fit + diff5C.pred$res + diff5C.pred$effect), cluster = diff5C.pred$pred_data$year )

  # Predicted revenue per acre
  cs.pred_rev0_a <- exp(cs0C.pred$fit + cs0C.pred$res + ifelse(is.null(cs0C.pred$effect), 0,  cs0C.pred$effect))
  cs.pred_rev1_a <- exp(cs1C.pred$fit + cs1C.pred$res + ifelse(is.null(cs1C.pred$effect), 0,  cs1C.pred$effect))
  cs.pred_rev2_a <- exp(cs2C.pred$fit + cs2C.pred$res + ifelse(is.null(cs2C.pred$effect), 0,  cs2C.pred$effect))
  cs.pred_rev3_a <- exp(cs3C.pred$fit + cs3C.pred$res + ifelse(is.null(cs3C.pred$effect), 0,  cs3C.pred$effect))
  cs.pred_rev4_a <- exp(cs4C.pred$fit + cs4C.pred$res + ifelse(is.null(cs4C.pred$effect), 0,  cs4C.pred$effect))
  cs.pred_rev5_a <- exp(cs5C.pred$fit + cs5C.pred$res + ifelse(is.null(cs5C.pred$effect), 0,  cs5C.pred$effect))

  p.pred_rev0_a <- exp(p0C.pred$fit + p0C.pred$res + p0C.pred$effect)
  p.pred_rev1_a <- exp(p1C.pred$fit + p1C.pred$res + p1C.pred$effect)
  p.pred_rev2_a <- exp(p2C.pred$fit + p2C.pred$res + p2C.pred$effect)
  p.pred_rev3_a <- exp(p3C.pred$fit + p3C.pred$res + p3C.pred$effect)
  p.pred_rev4_a <- exp(p4C.pred$fit + p4C.pred$res + p4C.pred$effect)
  p.pred_rev5_a <- exp(p5C.pred$fit + p5C.pred$res + p5C.pred$effect)

  diff.pred_rev0_a <- exp(diff0C.pred$fit + diff0C.pred$res + diff0C.pred$effect)
  diff.pred_rev1_a <- exp(diff1C.pred$fit + diff1C.pred$res + diff1C.pred$effect)
  diff.pred_rev2_a <- exp(diff2C.pred$fit + diff2C.pred$res + diff2C.pred$effect)
  diff.pred_rev3_a <- exp(diff3C.pred$fit + diff3C.pred$res + diff3C.pred$effect)
  diff.pred_rev4_a <- exp(diff4C.pred$fit + diff4C.pred$res + diff4C.pred$effect)
  diff.pred_rev5_a <- exp(diff5C.pred$fit + diff5C.pred$res + diff5C.pred$effect)
  
  # # Sum of predicted revenue
  cs.pred_rev0 <- sum(exp(cs0C.pred$fit + cs0C.pred$res + ifelse(is.null(cs0C.pred$effect), 0,  cs0C.pred$effect)))
  cs.pred_rev1 <- sum(exp(cs1C.pred$fit + cs1C.pred$res + ifelse(is.null(cs1C.pred$effect), 0,  cs1C.pred$effect)))
  cs.pred_rev2 <- sum(exp(cs2C.pred$fit + cs2C.pred$res + ifelse(is.null(cs2C.pred$effect), 0,  cs2C.pred$effect)))
  cs.pred_rev3 <- sum(exp(cs3C.pred$fit + cs3C.pred$res + ifelse(is.null(cs3C.pred$effect), 0,  cs3C.pred$effect)))
  cs.pred_rev4 <- sum(exp(cs4C.pred$fit + cs4C.pred$res + ifelse(is.null(cs4C.pred$effect), 0,  cs4C.pred$effect)))
  cs.pred_rev5 <- sum(exp(cs5C.pred$fit + cs5C.pred$res + ifelse(is.null(cs5C.pred$effect), 0,  cs5C.pred$effect)))

  p.pred_rev0 <- sum(exp(p0C.pred$fit + p0C.pred$res + p0C.pred$effect))
  p.pred_rev1 <- sum(exp(p1C.pred$fit + p1C.pred$res + p1C.pred$effect))
  p.pred_rev2 <- sum(exp(p2C.pred$fit + p2C.pred$res + p2C.pred$effect))
  p.pred_rev3 <- sum(exp(p3C.pred$fit + p3C.pred$res + p3C.pred$effect))
  p.pred_rev4 <- sum(exp(p4C.pred$fit + p4C.pred$res + p4C.pred$effect))
  p.pred_rev5 <- sum(exp(p5C.pred$fit + p5C.pred$res + p5C.pred$effect))

  diff.pred_rev0 <- sum(exp(diff0C.pred$fit + diff0C.pred$res + diff0C.pred$effect))
  diff.pred_rev1 <- sum(exp(diff1C.pred$fit + diff1C.pred$res + diff1C.pred$effect))
  diff.pred_rev2 <- sum(exp(diff2C.pred$fit + diff2C.pred$res + diff2C.pred$effect))
  diff.pred_rev3 <- sum(exp(diff3C.pred$fit + diff3C.pred$res + diff3C.pred$effect))
  diff.pred_rev4 <- sum(exp(diff4C.pred$fit + diff4C.pred$res + diff4C.pred$effect))
  diff.pred_rev5 <- sum(exp(diff5C.pred$fit + diff5C.pred$res + diff5C.pred$effect))

  # # Predicted change in revenue
  cs.rev0 <- cs.pred_rev0
  cs.rev1 <- ((cs.pred_rev1)/cs.rev0 - 1)*100
  cs.rev2 <- ((cs.pred_rev2)/cs.rev0 - 1)*100
  cs.rev3 <- ((cs.pred_rev3)/cs.rev0 - 1)*100
  cs.rev4 <- ((cs.pred_rev4)/cs.rev0 - 1)*100
  cs.rev5 <- ((cs.pred_rev5)/cs.rev0 - 1)*100

  p.rev0 <- p.pred_rev0
  p.rev1 <- ((p.pred_rev1)/p.rev0 - 1)*100
  p.rev2 <- ((p.pred_rev2)/p.rev0 - 1)*100
  p.rev3 <- ((p.pred_rev3)/p.rev0 - 1)*100
  p.rev4 <- ((p.pred_rev4)/p.rev0 - 1)*100
  p.rev5 <- ((p.pred_rev5)/p.rev0 - 1)*100

  diff.rev0 <- diff.pred_rev0
  diff.rev1 <- ((diff.pred_rev1)/diff.rev0 - 1)*100
  diff.rev2 <- ((diff.pred_rev2)/diff.rev0 - 1)*100
  diff.rev3 <- ((diff.pred_rev3)/diff.rev0 - 1)*100
  diff.rev4 <- ((diff.pred_rev4)/diff.rev0 - 1)*100
  diff.rev5 <- ((diff.pred_rev5)/diff.rev0 - 1)*100
   
  # # Predict change in revenue with C.I.
  cs.rev0_min <- cs.pred_rev0 - cs0C.pred_se$se.sum*1.96
  cs.rev0_max <- cs.pred_rev0 + cs0C.pred_se$se.sum*1.96
  cs.rev1_min <- ((cs.pred_rev1 - cs1C.pred_se$se.sum*1.96)/cs.rev0_min - 1)*100
  cs.rev1_max <- ((cs.pred_rev1 + cs1C.pred_se$se.sum*1.96)/cs.rev0_max - 1)*100
  cs.rev2_min <- ((cs.pred_rev2 - cs2C.pred_se$se.sum*1.96)/cs.rev0_min - 1)*100
  cs.rev2_max <- ((cs.pred_rev2 + cs2C.pred_se$se.sum*1.96)/cs.rev0_max - 1)*100
  cs.rev3_min <- ((cs.pred_rev3 - cs3C.pred_se$se.sum*1.96)/cs.rev0_min - 1)*100
  cs.rev3_max <- ((cs.pred_rev3 + cs3C.pred_se$se.sum*1.96)/cs.rev0_max - 1)*100
  cs.rev4_min <- ((cs.pred_rev4 - cs4C.pred_se$se.sum*1.96)/cs.rev0_min - 1)*100
  cs.rev4_max <- ((cs.pred_rev4 + cs4C.pred_se$se.sum*1.96)/cs.rev0_max - 1)*100
  cs.rev5_min <- ((cs.pred_rev5 - cs5C.pred_se$se.sum*1.96)/cs.rev0_min - 1)*100
  cs.rev5_max <- ((cs.pred_rev5 + cs5C.pred_se$se.sum*1.96)/cs.rev0_max - 1)*100

  p.rev0_min <- p.pred_rev0 - p0C.pred_se$se.sum*1.96
  p.rev0_max <- p.pred_rev0 + p0C.pred_se$se.sum*1.96
  p.rev1_min <- ((p.pred_rev1 - p1C.pred_se$se.sum*1.96)/p.rev0_min - 1)*100
  p.rev1_max <- ((p.pred_rev1 + p1C.pred_se$se.sum*1.96)/p.rev0_max - 1)*100
  p.rev2_min <- ((p.pred_rev2 - p2C.pred_se$se.sum*1.96)/p.rev0_min - 1)*100
  p.rev2_max <- ((p.pred_rev2 + p2C.pred_se$se.sum*1.96)/p.rev0_max - 1)*100
  p.rev3_min <- ((p.pred_rev3 - p3C.pred_se$se.sum*1.96)/p.rev0_min - 1)*100
  p.rev3_max <- ((p.pred_rev3 + p3C.pred_se$se.sum*1.96)/p.rev0_max - 1)*100
  p.rev4_min <- ((p.pred_rev4 - p4C.pred_se$se.sum*1.96)/p.rev0_min - 1)*100
  p.rev4_max <- ((p.pred_rev4 + p4C.pred_se$se.sum*1.96)/p.rev0_max - 1)*100
  p.rev5_min <- ((p.pred_rev5 - p5C.pred_se$se.sum*1.96)/p.rev0_min - 1)*100
  p.rev5_max <- ((p.pred_rev5 + p5C.pred_se$se.sum*1.96)/p.rev0_max - 1)*100

  diff.rev0_min <- diff.pred_rev0 - diff0C.pred_se$se.sum*1.96
  diff.rev0_max <- diff.pred_rev0 + diff0C.pred_se$se.sum*1.96
  diff.rev1_min <- ((diff.pred_rev1 - diff1C.pred_se$se.sum*1.96)/diff.rev0_min - 1)*100
  diff.rev1_max <- ((diff.pred_rev1 + diff1C.pred_se$se.sum*1.96)/diff.rev0_max - 1)*100
  diff.rev2_min <- ((diff.pred_rev2 - diff2C.pred_se$se.sum*1.96)/diff.rev0_min - 1)*100
  diff.rev2_max <- ((diff.pred_rev2 + diff2C.pred_se$se.sum*1.96)/diff.rev0_max - 1)*100
  diff.rev3_min <- ((diff.pred_rev3 - diff3C.pred_se$se.sum*1.96)/diff.rev0_min - 1)*100
  diff.rev3_max <- ((diff.pred_rev3 + diff3C.pred_se$se.sum*1.96)/diff.rev0_max - 1)*100
  diff.rev4_min <- ((diff.pred_rev4 - diff4C.pred_se$se.sum*1.96)/diff.rev0_min - 1)*100
  diff.rev4_max <- ((diff.pred_rev4 + diff4C.pred_se$se.sum*1.96)/diff.rev0_max - 1)*100
  diff.rev5_min <- ((diff.pred_rev5 - diff5C.pred_se$se.sum*1.96)/diff.rev0_min - 1)*100
  diff.rev5_max <- ((diff.pred_rev5 + diff5C.pred_se$se.sum*1.96)/diff.rev0_max - 1)*100

  pred.rev_a <- list(cs.acres = data.frame(temp = rep(c(0,1,2,3,4,5),each = length(cs.pred_rev0_a)),
                                            rev = c(cs.pred_rev0_a, cs.pred_rev1_a, cs.pred_rev2_a, cs.pred_rev3_a, cs.pred_rev4_a, cs.pred_rev5_a),
                                            crop = crop),
                      p.acres = data.frame(temp = rep(c(0,1,2,3,4,5),each = length(p.pred_rev0_a)),
                                            rev = c(p.pred_rev0_a, p.pred_rev1_a, p.pred_rev2_a, p.pred_rev3_a, p.pred_rev4_a, p.pred_rev5_a),
                                            crop = crop),
                      diff.acres = data.frame(temp = rep(c(0,1,2,3,4,5),each = length(diff.pred_rev0_a)),
                                            rev = c(diff.pred_rev0_a, diff.pred_rev1_a, diff.pred_rev2_a, diff.pred_rev3_a, diff.pred_rev4_a, diff.pred_rev5_a),
                                            crop = crop))
   
   
   
  revdat <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           rev = c(0, cs.rev1, cs.rev2, cs.rev3, cs.rev4, cs.rev5,
                                   0, p.rev1, p.rev2, p.rev3, p.rev4, p.rev5,
                                   0, diff.rev1, diff.rev2, diff.rev3, diff.rev4, diff.rev5),
                          min = c(0,cs.rev1_min, cs.rev2_min, cs.rev3_min, cs.rev4_min, cs.rev5_min,
                                   0,p.rev1_min, p.rev2_min, p.rev3_min, p.rev4_min, p.rev5_min,
                                   0,diff.rev1_min, diff.rev2_min, diff.rev3_min, diff.rev4_min, diff.rev5_min),
                          max = c(0,cs.rev1_max, cs.rev2_max, cs.rev3_max, cs.rev4_max, cs.rev5_max,
                                   0,p.rev1_max, p.rev2_max, p.rev3_max, p.rev4_max, p.rev5_max,
                                   0,diff.rev1_max, diff.rev2_max, diff.rev3_max, diff.rev4_max, diff.rev5_max),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = crop)
  return(list(pred.rev = pred.rev_a,
         pred.change = revdat))
}
