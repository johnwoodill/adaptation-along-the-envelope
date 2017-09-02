# Get predictions
predictShare <- function(models, data, crop, se = FALSE){
  
  tobit.ey <- function(mu, sigma){
    p0 <- pnorm(mu/sigma)
    lambda <- function(x) dnorm(x)/pnorm(x)
    ey0 <- mu + sigma * lambda(mu/sigma)
    ey <- p0 * ey0
    return(ey)
  }

  
  # Cross-section Predictions
  cs0C.pred <- predict(models[[1]], newdata = data[[1]])
  cs1C.pred <- predict(models[[1]], newdata = data[[2]])
  cs2C.pred <- predict(models[[1]], newdata = data[[3]])
  cs3C.pred <- predict(models[[1]], newdata = data[[4]])
  cs4C.pred <- predict(models[[1]], newdata = data[[5]])
  cs5C.pred <- predict(models[[1]], newdata = data[[6]])
  
  # Panel Predictions
  p0C.pred <- predict(models[[2]], newdata = data[[7]])
  p1C.pred <- predict(models[[2]], newdata = data[[8]])
  p2C.pred <- predict(models[[2]], newdata = data[[9]])
  p3C.pred <- predict(models[[2]], newdata = data[[10]])
  p4C.pred <- predict(models[[2]], newdata = data[[11]])
  p5C.pred <- predict(models[[2]], newdata = data[[12]])
  
  # Difference Predictions
  diff0C.pred <- predict(models[[3]], newdata = data[[13]])
  diff1C.pred <- predict(models[[3]], newdata = data[[14]])
  diff2C.pred <- predict(models[[3]], newdata = data[[15]])
  diff3C.pred <- predict(models[[3]], newdata = data[[16]])
  diff4C.pred <- predict(models[[3]], newdata = data[[17]])
  diff5C.pred <- predict(models[[3]], newdata = data[[18]])
  
  # Bootstrapped Standard errors (incomplete)
  # cs0C.pred_se <- boot.strap(cs0C.pred)
  # cs1C.pred_se <- boot.strap(cs1C.pred)
  # cs2C.pred_se <- boot.strap(cs2C.pred)
  # cs3C.pred_se <- boot.strap(cs3C.pred)
  # cs4C.pred_se <- boot.strap(cs4C.pred)
  # cs5C.pred_se <- boot.strap(cs5C.pred)
  #  
  # p0C.pred_se <- boot.strap((p0C.pred + p0C.pred$res + p0C.pred$effect), cluster = p0C.pred$pred_data$year )
  # p1C.pred_se <- boot.strap((p1C.pred + p1C.pred$res + p1C.pred$effect), cluster = p1C.pred$pred_data$year, rep = 100 )
  # p2C.pred_se <- boot.strap((p2C.pred + p2C.pred$res + p2C.pred$effect), cluster = p2C.pred$pred_data$year, rep = 100 )
  # p3C.pred_se <- boot.strap((p3C.pred + p3C.pred$res + p3C.pred$effect), cluster = p3C.pred$pred_data$year, rep = 100 )
  # p4C.pred_se <- boot.strap((p4C.pred + p4C.pred$res + p4C.pred$effect), cluster = p4C.pred$pred_data$year, rep = 100 )
  # p5C.pred_se <- boot.strap((p5C.pred + p5C.pred$res + p5C.pred$effect), cluster = p5C.pred$pred_data$year, rep = 100 )
  #  
  # diff0C.pred_se <- boot.strap((diff0C.pred + diff0C.pred$res + diff0C.pred$effect), cluster = diff0C.pred$pred_data$year )
  # diff1C.pred_se <- boot.strap((diff1C.pred + diff1C.pred$res + diff1C.pred$effect), cluster = diff1C.pred$pred_data$year )
  # diff2C.pred_se <- boot.strap((diff2C.pred + diff2C.pred$res + diff2C.pred$effect), cluster = diff2C.pred$pred_data$year )
  # diff3C.pred_se <- boot.strap((diff3C.pred + diff3C.pred$res + diff3C.pred$effect), cluster = diff3C.pred$pred_data$year )
  # diff4C.pred_se <- boot.strap((diff4C.pred + diff4C.pred$res + diff4C.pred$effect), cluster = diff4C.pred$pred_data$year )
  # diff5C.pred_se <- boot.strap((diff5C.pred + diff5C.pred$res + diff5C.pred$effect), cluster = diff5C.pred$pred_data$year )

  # Predicted Acreage
  cs.share0_a <-  tobit.ey(cs0C.pred, models[[1]]$scale)*data[[1]]$total_a
  cs.share1_a <-  tobit.ey(cs1C.pred, models[[1]]$scale)*data[[1]]$total_a
  cs.share2_a <-  tobit.ey(cs2C.pred, models[[1]]$scale)*data[[1]]$total_a
  cs.share3_a <-  tobit.ey(cs3C.pred, models[[1]]$scale)*data[[1]]$total_a
  cs.share4_a <-  tobit.ey(cs4C.pred, models[[1]]$scale)*data[[1]]$total_a
  cs.share5_a <-  tobit.ey(cs5C.pred, models[[1]]$scale)*data[[1]]$total_a

  p.share0_a <-   tobit.ey(p0C.pred, models[[2]]$scale)*data[[2]]$total_a
  p.share1_a <-   tobit.ey(p1C.pred, models[[2]]$scale)*data[[2]]$total_a
  p.share2_a <-   tobit.ey(p2C.pred, models[[2]]$scale)*data[[2]]$total_a
  p.share3_a <-   tobit.ey(p3C.pred, models[[2]]$scale)*data[[2]]$total_a
  p.share4_a <-   tobit.ey(p4C.pred, models[[2]]$scale)*data[[2]]$total_a
  p.share5_a <-   tobit.ey(p5C.pred, models[[2]]$scale)*data[[2]]$total_a

  diff.share0_a <- tobit.ey(diff0C.pred, models[[3]]$scale)*data[[3]]$total_a
  diff.share1_a <- tobit.ey(diff1C.pred, models[[3]]$scale)*data[[3]]$total_a
  diff.share2_a <- tobit.ey(diff2C.pred, models[[3]]$scale)*data[[3]]$total_a
  diff.share3_a <- tobit.ey(diff3C.pred, models[[3]]$scale)*data[[3]]$total_a
  diff.share4_a <- tobit.ey(diff4C.pred, models[[3]]$scale)*data[[3]]$total_a
  diff.share5_a <- tobit.ey(diff5C.pred, models[[3]]$scale)*data[[3]]$total_a
  
  # Sum of redicted Acreage
  cs.share0 <-  sum( tobit.ey(cs0C.pred, models[[1]]$scale)*data[[1]]$total_a)
  cs.share1 <- (sum( tobit.ey(cs1C.pred, models[[1]]$scale)*data[[1]]$total_a)/cs.share0 - 1)*100
  cs.share2 <- (sum( tobit.ey(cs2C.pred, models[[1]]$scale)*data[[1]]$total_a)/cs.share0 - 1)*100
  cs.share3 <- (sum( tobit.ey(cs3C.pred, models[[1]]$scale)*data[[1]]$total_a)/cs.share0 - 1)*100
  cs.share4 <- (sum( tobit.ey(cs4C.pred, models[[1]]$scale)*data[[1]]$total_a)/cs.share0 - 1)*100
  cs.share5 <- (sum( tobit.ey(cs5C.pred, models[[1]]$scale)*data[[1]]$total_a)/cs.share0 - 1)*100

  p.share0 <-  sum( tobit.ey(p0C.pred, models[[2]]$scale)*data[[2]]$total_a)
  p.share1 <- (sum( tobit.ey(p1C.pred, models[[2]]$scale)*data[[2]]$total_a)/p.share0 - 1)*100
  p.share2 <- (sum( tobit.ey(p2C.pred, models[[2]]$scale)*data[[2]]$total_a)/p.share0 - 1)*100
  p.share3 <- (sum( tobit.ey(p3C.pred, models[[2]]$scale)*data[[2]]$total_a)/p.share0 - 1)*100
  p.share4 <- (sum( tobit.ey(p4C.pred, models[[2]]$scale)*data[[2]]$total_a)/p.share0 - 1)*100
  p.share5 <- (sum( tobit.ey(p5C.pred, models[[2]]$scale)*data[[2]]$total_a)/p.share0 - 1)*100

  diff.share0 <-  sum( tobit.ey(diff0C.pred, models[[3]]$scale)*data[[3]]$total_a)
  diff.share1 <- (sum( tobit.ey(diff1C.pred, models[[3]]$scale)*data[[3]]$total_a)/diff.share0 - 1)*100
  diff.share2 <- (sum( tobit.ey(diff2C.pred, models[[3]]$scale)*data[[3]]$total_a)/diff.share0 - 1)*100
  diff.share3 <- (sum( tobit.ey(diff3C.pred, models[[3]]$scale)*data[[3]]$total_a)/diff.share0 - 1)*100
  diff.share4 <- (sum( tobit.ey(diff4C.pred, models[[3]]$scale)*data[[3]]$total_a)/diff.share0 - 1)*100
  diff.share5 <- (sum( tobit.ey(diff5C.pred, models[[3]]$scale)*data[[3]]$total_a)/diff.share0 - 1)*100

  
   pred.acres <- list(cs.acres = data.frame(temp = rep(c(0,1,2,3,4,5),each = length(cs.share0_a)),
                                            acres = c(cs.share0_a, cs.share1_a, cs.share2_a, cs.share3_a, cs.share4_a, cs.share5_a),
                                            crop = crop),
                      p.acres = data.frame(temp = rep(c(0,1,2,3,4,5),each = length(p.share0_a)),
                                            acres = c(p.share0_a, p.share1_a, p.share2_a, p.share3_a, p.share4_a, p.share5_a),
                                            crop = crop),
                      diff.acres = data.frame(temp = rep(c(0,1,2,3,4,5),each = length(diff.share0_a)),
                                            acres = c(diff.share0_a, diff.share1_a, diff.share2_a, diff.share3_a, diff.share4_a, diff.share5_a),
                                            crop = crop))

  sharedat <- data.frame(temp = rep(c(0,1,2,3,4,5), 3),
                           share = c(0, cs.share1, cs.share2, cs.share3, cs.share4, cs.share5,
                                   0, p.share1, p.share2, p.share3, p.share4, p.share5,
                                   0, diff.share1, diff.share2, diff.share3, diff.share4, diff.share5),
                          # min = c(0,cs.share1_min, cs.share2_min, cs.share3_min, cs.share4_min, cs.share5_min,
                          #         0,p.share1_min, p.share2_min, p.share3_min, p.share4_min, p.share5_min,
                          #         0,diff.share1_min, diff.share2_min, diff.share3_min, diff.share4_min, diff.share5_min),
                          # max = c(0,cs.share1_max, cs.share2_max, cs.share3_max, cs.share4_max, cs.share5_max,
                          #         0,p.share1_max, p.share2_max, p.share3_max, p.share4_max, p.share5_max,
                          #         0,diff.share1_max, diff.share2_max, diff.share3_max, diff.share4_max, diff.share5_max),
                           reg = rep(c("Cross-section", "Panel", "Difference"), each = 6),
                           crop = crop)
  return(list(pred.acres = pred.acres, 
              pred.change = sharedat))
}
