library(lfe)
set.seed(12345)
iris <- iris

# Create weights
iris$w <- rnorm(150, 10, 1) 

# Quadratic term
iris$Sepal.Width_sq <- iris$Sepal.Width^2

# Simple lm()
summary(lm(Sepal.Length ~ Sepal.Width + Sepal.Width_sq + factor(Species) , data = iris, weights = iris$w))

# With felm()
summary(felm(Sepal.Length ~ Sepal.Width + Sepal.Width_sq| Species, data = iris, weights = iris$w))

mod1 <- lm(Sepal.Length ~ Sepal.Width + Sepal.Width_sq + 
         factor(Species), data = iris, weights = iris$w)

mod2 <- felm(Sepal.Length ~ Sepal.Width + Sepal.Width_sq| Species,
         data = iris, weights = iris$w)

# Also the way you were using demeanlist didn't look right at all
newdat <- demeanlist(
  mtx     = as.matrix(iris[, c("Sepal.Length", "Sepal.Width", "Sepal.Width_sq")]), 
  fl      = list(Species = iris$Species), 
  weights = sqrt(iris$w)) #Don't forget for WLS you are actually working with the square-root of the weight

mod3 <- lm(Sepal.Length ~ Sepal.Width + Sepal.Width_sq, 
         data = as.data.frame(newdat), weights = iris$w)

all.equal(
  coef(mod3)[c("Sepal.Width", "Sepal.Width_sq")], 
  coef(mod2)[c("Sepal.Width", "Sepal.Width_sq")])

all.equal(
  coef(mod3)[c("Sepal.Width", "Sepal.Width_sq")], 
  coef(mod1)[c("Sepal.Width", "Sepal.Width_sq")])
