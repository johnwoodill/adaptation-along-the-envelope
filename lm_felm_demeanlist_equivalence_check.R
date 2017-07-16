library(lfe)

iris <- iris

iris$Sepal.Width_sq <- iris$Sepal.Width^2
summary(lm(Sepal.Length ~ Sepal.Width + Sepal.Width_sq + factor(Species), data = iris, weights = iris$Petal.Length))

newdat <- demeanlist(iris[, 1:2], list(iris$Species))
summary(lm(Sepal.Length ~ Sepal.Width, data = newdat, weights = iris$Petal.Length))



summary(felm(Sepal.Length ~ Sepal.Width + Sepal.Width_sq | Species, data = iris, weights = iris$Petal.Length))



test <- cropdat
lm(ln_corn_rrev ~ factor(state) + dm_tavg + dm_tavg_sq, data = test)

test2 <- demeanlist(test[1:32], list(test$state))
lm(ln_corn_rrev ~ dm_tavg + dm_tavg_sq, data = test2)
