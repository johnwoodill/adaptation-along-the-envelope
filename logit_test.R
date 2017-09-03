library(mlogit)
cropdat <- readRDS("data/cross_section_regression_data.rds")
rowSums(cropdat[,c("p_corn_share", "p_cotton_share", "p_hay_share", "p_wheat_share", "p_soybean_share")], na.rm = TRUE)

#cropdat <- select(cropdat, fips, p_corn_share, dday0_10, dday10_30, dday30C, prec, prec_sq)

# Cross Section: Revenue per acre -----------------------------------------

# Exposure weighted values equal zero
cropdat$dday0_10 <- cropdat$dday0_10 - mean(cropdat$dday0_10, na.rm = TRUE)
cropdat$dday10_30 <- cropdat$dday10_30 - mean(cropdat$dday10_30, na.rm = TRUE)
cropdat$dday30C <- cropdat$dday30C - mean(cropdat$dday30C, na.rm = TRUE)
cropdat$prec <- cropdat$prec - mean(cropdat$prec, na.rm = TRUE)
cropdat$prec_sq <- cropdat$prec^2

# Corn
# ml <- mlogit.data(cropdat, shape = "wide", choice = "p_corn_share")
# mod1 <- mlogit(p_corn_share ~ dday0_10 + dday10_30 | 0, data = ml, probit = TRUE)
# summary(mod1)


mod1 <- glm(p_corn_share ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq + lat + long + lat:long, 
    data = cropdat, family = quasibinomial(logit), weights = total_w)
summary(mod1)

mod2 <- glm(p_cotton_share ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq + lat + long + lat:long, 
    data = cropdat, family = quasibinomial(logit), weights = total_w)
summary(mod2)

mod3 <- glm(p_hay_share ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq + lat + long + lat:long, 
    data = cropdat, family = quasibinomial(logit), weights = total_w)
summary(mod3)

mod4 <- glm(p_wheat_share ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq  + lat + long + lat:long, 
    data = cropdat, family = quasibinomial(logit), weights = total_w)
summary(mod4)

mod5 <- glm(p_soybean_share ~ dday0_10 + dday10_30  + dday30C + prec + prec_sq + lat + long + lat:long, 
    data = cropdat, family = quasibinomial(logit), weights = total_w)
summary(mod5)


model.dat <- list(cs.0C = cs.0C,
                 cs.1C = cs.1C,
                 cs.2C = cs.2C,
                 cs.3C = cs.3C,
                 cs.4C = cs.4C,
                 cs.5C = cs.5C)

models <- list(corn = mod1,
               cotton = mod2,
               hay = mod3,
               wheat = mod4,
               soybean = mod5)

# retpred <- function(model, data, crop){
#   indat <- data.frame(temp = c(0, 1, 2, 3, 4, 5), acres = rep(0, 6), crop = crop)
#   for (i in 1:6){
#     newpred <- predict(model, newdata = data[[i]], type = "response")
#     indat$acres[i] <- sum(newpred*data[[1]]$total_a)
#   }
#   return(indat)
#   
# }
# 
# retpred <- function(model, data, crop){
#   
#   #indat <- data.frame()
#   indat <- data.frame(row = rep(0, NROW(data[[1]])), acres = crop, C0 = 0, C1 = 0, C2 = 0, C3 = 0, C4 = 0, C5 = 0 )
#   for (i in 1:6){
#     newpred <- predict(model, newdata = data[[i]], type = "response")
#     indat[,i+2] <- newpred
#     #mergdat <- rbind(mergdat, indat)
#     
#   }
#   return(indat)
#   
# }

retpred <- function(model, data){
  mergdat <- data.frame()
  for (j in 1:6){ # temp increase
    indat <- data.frame(temp = rep(0, NROW(data[[1]])), corn = 0, cotton = 0, hay = 0, wheat = 0, soybean = 0)
    for (i in 1:5){ # crop
      newpred <- predict(model[[i]], newdata = data[[j]], type = "response")
      indat[,i+1] <- newpred
      indat$temp = j - 1
    }
    mergdat <- rbind(mergdat, indat)
  
  }
  mergdat <- mergdat %>% 
    mutate(corn = corn/rowSums(.[2:6]),
           cotton = cotton/rowSums(.[2:6]),
           hay = hay/rowSums(.[2:6]),
           wheat = wheat/rowSums(.[2:6]),
           soybean = soybean/rowSums(.[2:6])) %>% 
    group_by(temp) %>% 
    mutate_all(funs(.*data[[1]]$total_a))
  
  return(mergdat)

}

test <- retpred(models, model.dat)
rowSums(test[, 2:6])
test <- gather(test, temp, crop)
names(test) <- c("temp", "crop", "acres")
test
pldat <- test %>% group_by(temp, crop) %>% summarise(sum_a = sum(acres))

ggplot(pldat, aes(temp, sum_a, color = factor(crop))) + geom_line()
ggplot(pldat, aes(temp, sum_a)) + geom_line()

pldat <- test %>% group_by(temp) %>% summarise(sum_a = sum(acres))
pdat


p1 <- retpred(model = mod1, data = model.dat, crop = "corn")
p2 <- retpred(model = mod2, data = model.dat, crop = "cotton")
p3 <- retpred(model = mod3, data = model.dat, crop = "hay")
p4 <- retpred(model = mod4, data = model.dat, crop = "wheat")
p5 <- retpred(model = mod5, data = model.dat, crop = "soybean")

newdat <- data.frame(corn = p1$C0, cotton = p2$C0, hay = p3$C0, wheat = p4$C0, soybean = p5$C0)
newdat <- data.frame(corn = p1$C1, cotton = p2$C1, hay = p3$C1, wheat = p4$C1, soybean = p5$C1)

#newdat <- data.frame(corn = p1$C1, cotton = p2$C1, hay = p3$C1, wheat = p4$C4, soybean = p5$C5)
newdat[,1:5] <- newdat[,1:5]/rowSums(newdat[, c(1:5)])
rowSums(newdat[, c(1:5)])
rowSums(newdat[, c(1:4)])

pdat <- rbind(p1, p2, p3, p4, p5)
test <- pdat %>% group_by(temp) %>% summarise(sum_a = sum(acres))
test
ggplot(pdat, aes(temp, acres, color = factor(crop))) + geom_line()
ggplot(test, aes(temp, sum_a)) + geom_line()
