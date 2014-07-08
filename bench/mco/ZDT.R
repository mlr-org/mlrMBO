library(emoa)

## Set of the 6 DTLZ test functions for the case 2D -> 2D.
## For every function: x is a vector of length x such that 0 < x[i] < 1, i=1,...,n
## Output is a vector of length 2, too.

zdt1 <- function (x) {
  f = numeric(2)
  n = length(x)
  f[1] = x[1]
  g = 1 + 9 * mean(x[2:n])
  f[2] = g * (1 - sqrt(f[1] / g))
  return(f)
}

zdt2 <- function (x) {
  f = numeric(2)
  n = length(x)
  f[1] = x[1]
  g = 1 + 9 * mean(x[2:n])
  f[2] = g * (1 - (f[1] / g)^2)
  return(f)
}


zdt3 <- function (x) {
  f = numeric(2)
  n = length(x)
  f[1] = x[1]
  g = 1 + 9 * mean(x[2:n])
  f[2] = g * (1 - sqrt(f[1]/g) - f[1]/g * sin(10 * pi * f[1]))
  return(f)
}

zdt4 = function(x){
  f = numeric(2)
  n = length(x)
  ## transform x[2:n] to [-5, 5]
  x[2:n] = x[2:n] * 10 - 5
  f[1] = x[1]
  g = 1 + 10*(n-1) + sum(x[2:n]^2 - 10 * cos(4 * pi * x[2:n]))
  f[2] = g * (1 - sqrt(f[1] / g))
  return(f)
}

zdt6 = function(x){
  f = numeric(2)
  n = length(x)
  f[1] = 1 - exp(-4 * x[1]) * (sin (6 * pi * x[1]))^6
  g = 1 + 9 * mean(x[2:n])^0.25
  f[2] = g * (1 - (f[1]/g)^2)
  return(f)
}

## Functions, which return l equally distributed points from the
## paretofront of the corresponding uf-function with in-dimension n

zdt1pf = function(n, l){
  X = matrix(0, ncol = l, nrow = 2)
  X[1,] = seq(0, 1, length.out = l)
  X[2,] = 1 - sqrt(X[1,])
  X
}

zdt2pf = function(n, l){
  X = matrix(0, ncol = l, nrow = 2)
  X[1,] = seq(0, 1, length.out = l)
  X[2,] = 1 - X[1,]^2
  X
}

zdt3pf = function(n, l){
  X = matrix(0, ncol = 10*l, nrow = 2)
  X[1,] = seq(0, 1, length.out = 10*l)
  X[2,] = 1 - sqrt(X[1,]) - X[1,] * sin(10 * pi * X[1,])
  X = nondominated_points(X)
  X[,sort(sample(ncol(X), l, replace = TRUE))]
}

zdt4pf = zdt1pf

zdt6pf = zdt2pf