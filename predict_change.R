library(dplyr)
library(ggplot2)
library(ggthemes)

boot <- function(x, rep){
  stder <- c()
  indat <- c()
  for (i in 1:rep){
    samp <- sample(x, replace = TRUE)
    stder[i] <- sd(samp)/sqrt(length(samp))
  }
   return(mean(stder))
  
}
cs.temp.ln_corn_rrev <- readRDS("models/cs.temp.ln_corn_rrev")
cs.temp.ln_cotton_rrev <- readRDS("models/cs.temp.ln_cotton_rrev")
cs.temp.ln_hay_rrev <- readRDS("models/cs.temp.ln_hay_rrev")
cs.temp.ln_wheat_rrev <- readRDS("models/cs.temp.ln_wheat_rrev")
cs.temp.ln_soybean_rrev <- readRDS("models/cs.temp.ln_soybean_rrev")

 cs.corn.dat <- readRDS("models/cs.corn.dat")

rangeAveT = range(cs.corn.dat$dm_tavg)
pDataAveT = data.frame(dm_tavg = seq(rangeAveT[1], rangeAveT[2], length = 20),
                       dm_prec = rep(mean(cs.corn.dat$dm_prec, na.rm = TRUE), 20),
                       lat = rep(mean(cs.corn.dat$lat, na.rm = TRUE), 20),
                       dm_pop_dens = rep(mean(cs.corn.dat$dm_pop_dens, na.rm = TRUE), 20),
                       dm_ipc = rep(mean(cs.corn.dat$dm_ipc, na.rm = TRUE), 20),
                       waterCapacity = rep(mean(cs.corn.dat$waterCapacity, na.rm = TRUE), 20),
                       percentClay = rep(mean(cs.corn.dat$percentClay, na.rm = TRUE), 20),
                       minPermeability = rep(mean(cs.corn.dat$minPermeability, na.rm = TRUE), 20),
                       kFactor = rep(mean(cs.corn.dat$kFactor, na.rm = TRUE), 20),
                       bestSoil = rep(mean(cs.corn.dat$bestSoil, na.rm = TRUE), 20))

pred.corn <- predict(cs.temp.ln_corn_rrev, newdata=pDataAveT, se.fit = TRUE)
pred.corn$type <- rep("corn", 20)
pred.cotton <- predict(cs.temp.ln_cotton_rrev, newdata=pDataAveT, se.fit = TRUE)
pred.cotton$type <- rep("cotton", 20)
pred.hay <- predict(cs.temp.ln_hay_rrev, newdata=pDataAveT, se.fit = TRUE)
pred.hay$type <- rep("hay", 20)
pred.wheat <- predict(cs.temp.ln_wheat_rrev, newdata=pDataAveT, se.fit = TRUE)
pred.wheat$type <- rep("wheat", 20)
pred.soybean <- predict(cs.temp.ln_soybean_rrev, newdata=pDataAveT, se.fit = TRUE)
pred.soybean$type <- rep("soybean", 20)
pred.data <- rbind(pred.corn, pred.cotton, pred.hay, pred.wheat, pred.soybean)
pred.data <- data.frame(fit = c(pred.corn$fit, pred.cotton$fit, pred.hay$fit, pred.wheat$fit, pred.soybean$fit),
                        se.fit = c(pred.corn$se.fit, pred.cotton$se.fit, pred.hay$se.fit, pred.wheat$se.fit, pred.soybean$se.fit),
                        type = c(pred.corn$type, pred.cotton$type, pred.hay$type, pred.wheat$type, pred.soybean$type),
                        dm_tavg = rep(pDataAveT$dm_tavg, 20))
pred.data$type <- factor(pred.data$type)
#,  fill = "#C0CCD5") +  
ggplot(pred.data, aes(x = dm_tavg, y = fit, fill = type, colors = type)) + geom_ribbon(aes(ymin = fit + 1.97*se.fit, ymax = fit - 1.97*se.fit, x = dm_tavg), alpha = 0.3) + 
  geom_line() +  xlab("Deviation from Average Temperature") + ylab("Log(Corn Rev)") + theme_tufte()



####### Finish top first
impact <- data.frame(degree = rep(0, 11), est = rep(0, 11), stder = rep(0, 11), ymin = rep(0, 11), ymax = rep(0, 11))
total_impact <- data.frame()



est_impact <- function(x){
  for (i in 1:11){
    rev0 = sum( exp(x$model[1]) , na.rm = TRUE)
    newDat = x$model
  	newDat$dm_tavg = x$model$dm_tavg + (i - 6)
  	pred = predict(x, newdata=newDat)
  	res <- resid(x)
  	impact$degree[i] <- i - 6
  	impact$est[i] <- (sum( exp(pred + res), na.rm = TRUE)/rev0 - 1)*100
  	impact$stder[i] <- boot(pred, 1000)
  	
    impact$ymin[i] <- (sum( exp((pred - 1.96*impact$stder[i]) + res), na.rm = TRUE)/rev0 - 1)*100
  	impact$ymax[i] <- (sum( exp((pred + 1.96*impact$stder[i]) + res), na.rm = TRUE)/rev0 - 1)*100
    print(i - 6)
  }
  return(impact)
}

models <- list(corn = cs.temp.ln_corn_rrev,
               cotton = cs.temp.ln_cotton_rrev,
               hay = cs.temp.ln_hay_rrev,
               wheat = cs.temp.ln_wheat_rrev,
               soybean = cs.temp.ln_soybean_rrev)
crops <- c("corn", "cotton", "hay", "wheat", "soybean")

for (j in 1:5){
  x <- models[[j]]
  mergedat <- est_impact(x)
  mergedat$type <- crops[j]  
  total_impact <- rbind(total_impact, mergedat)
}

ggplot(total_impact, aes(x = degree, y = est, fill = type)) + geom_ribbon(aes(ymin = ymin, ymax = ymax, x = degree), alpha = 0.3) + 
  geom_line() + geom_hline(yintercept = 0, linetype = "dashed") + 
  ylab("Difference of Obs. Revenue and Predicted Change Rev.") + xlab("Change in C") +
  ggtitle("Cross-sectional Change in Revenue from Change in Average Temperature")





