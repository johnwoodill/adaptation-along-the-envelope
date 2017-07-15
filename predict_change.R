library(dplyr)
library(ggplot2)
library(ggthemes)

boot <- function(x, rep){
  indat <- c()
  for (i in 1:rep){
    samp <- sample(x, replace = TRUE)
    stder[i] <- sd(samp)/sqrt(length(samp))
  }
   return(mean(stder))
  
}
cs.corn.mod1 <- readRDS("models/cs.corn.mod1")
cs.corn.dat <- readRDS("models/cs.corn.dat")
cs.corn.dat$state <- factor(cs.corn.dat$state)
cs.corn.dat <- filter(cs.corn.dat, !is.na(ln_corn_rrev))

# Linear Model
cs.corn.mod1  <- lm(ln_corn_rrev ~ dm_tavg + I(dm_tavg^2) + dm_prec + I(dm_prec^2) + 
                       lat + dm_ipc + dm_pop_dens + I(dm_pop_dens^2) + waterCapacity + percentClay + minPermeability + kFactor + 
                     bestSoil,
                     data = cs.corn.dat, weights = cs.corn.dat$corn_grain_a, x = TRUE, y = TRUE)
summary(cs.corn.mod1)

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

a <- predict(cs.corn.mod1, newdata=pDataAveT, se.fit = TRUE)

ggplot(NULL) +   geom_ribbon(aes(ymin = a$fit + 1.97*a$se.fit, ymax = a$fit - 1.97*a$se.fit, x = pDataAveT$dm_tavg), fill = "#C0CCD5") + 
  geom_line(aes(x = pDataAveT$dm_tavg, a$fit)) + 
  xlab("Deviation from Average Temperature") + ylab("Log(Corn Rev)") + theme_tufte()
  
impact <- data.frame(degree = rep(0, 11), est = rep(0, 11), stder = rep(0, 11), ymin = rep(0, 11), ymax = rep(0, 11))
rev0 = sum( exp(cs.corn.mod1$model$ln_corn_rrev) , na.rm = TRUE)
for (i in 1:11){
	newDat = cs.corn.mod1$model
	newDat$dm_tavg = cs.corn.mod1$model$dm_tavg + (i - 6)
	pred = predict(cs.corn.mod1, newdata=newDat)
	res <- resid(cs.corn.mod1)
	impact$degree[i] <- i - 6
	impact$est[i] <- (sum( exp(pred), na.rm = TRUE)/rev0 - 1)*100
	#impact$stder[i] <- boot(pred, 1000)
	
  impact$ymin[i] <- impact$est[i] - 1.96*impact$stder[i]
	impact$ymax[i] <- impact$est[i] + 1.96*impact$stder[i]
  print(i - 6)
}

ggplot(impact, aes(x = degree, y = est)) +   geom_ribbon(aes(ymin = ymin, ymax = ymax, x = degree)) + geom_line()  





