library(lfe)

iris <- iris

# Create weights
iris$w <- rnorm(150, 10, 1) 

# Quadratic term
iris$Sepal.Width_sq <- iris$Sepal.Width^2

# Simple lm()
summary(lm(Sepal.Length ~ Sepal.Width + Sepal.Width_sq + factor(Species) , data = iris, weights = iris$w))

# With felm()
summary(felm(Sepal.Length ~ Sepal.Width + Sepal.Width_sq| Species, data = iris, weights = iris$w))

# demean and lm()
newdat <- demeanlist(iris, list(iris$Species))
summary(lm(Sepal.Length ~ Sepal.Width + Sepal.Width_sq, data = newdat, weights = iris$w))







test <- cropdat
lm(ln_corn_rrev ~ factor(state) + dm_tavg + dm_tavg_sq, data = test)


