## Set of the 9 DTLZ test functions for the case 2D -> 2D.
## For every function: x is a vector of length n such that 0 < x[i] < 1, i=1,...,n
## Output is a vector of length 2

dtlz1 = function(x) {
  f = numeric(2)
  n = length(x)
  g = 100 * (n - 1 + sum((x[2:n] - 0.5)^2 - cos(20 * pi * (x[2:n] - 0.5))))
  f[1] = 1/2 * x[1] * (1 + g)
  f[2] = 1/2 * (1 - x[1]) * (1 + g)
  return(f)
}

dtlz2 = function(x) {
  f = numeric(2)
  n = length(x)
  g = sum((x[2:n] - 0.5)^2)
  f[1] = (1 + g) * cos(x[1] * pi / 2)
  f[2] = (1 + g) * sin(x[1] * pi / 2)
  return(f)
}

dtlz3 = function(x){
  f = numeric(2)
  n = length(x)
  g = 100 * (n - 1 + sum((x[2:n] - 0.5)^2 - cos(20 * pi * (x[2:n] - 0.5))))
  f[1] = (1 + g) * cos(x[1] * pi / 2)
  f[2] = (1 + g) * sin(x[1] * pi / 2)
  return(f)
}

dtlz4 = function(x){
  f = numeric(2)
  n = length(x)
  x = x^100
  g = sum((x[2:n] - 0.5)^2)
  f[1] = (1 + g) * cos(x[1] * pi / 2)
  f[2] = (1 + g) * sin(x[1] * pi / 2)
  return(f)
}

dtlz5 <- function(x) {
  f <- numeric(2)
  xm <- x[2:length(x)]
  g <- sum(xm^0.1) 
  t <- pi / (4 * (1 + g))
  theta <- x[1] * pi / 2
  f[2] <- (1 + g) *  sin(theta)
  f[1] <- (1 + g) * cos(theta)
  return(f)
}


dtlz6 = function(x){
  f = numeric(2)
  n = length(x)
  x = x * 2.116426807
  f[1] = x[1]
  g = 1 + 9 * mean(x[2:n])
  h = 2 - f[1] / (1 + g) * (1 + sin(3 * pi  * f[1]))
  f[2] = (1+g)*h
  return(f)
}

## Functions, which return l equally distributed points from the
## paretofront of the corresponding uf-function with in-dimension n

dtlz1pf = function(n, l){
  X = matrix(0, ncol = l, nrow = 2)
  X[1,] = seq(0, 0.5, length.out = l)
  X[2,] = 0.5 - X[1,]
  X
}

dtlz2pf = function(n, l){
  x = seq(0, 1, length.out = l)
  X = matrix(0, ncol = l, nrow = 2)
  X[1,] = sin(x * pi / 2)
  X[2,] = cos(x * pi / 2)
  X
}

dtlz3pf = dtlz2pf

dtlz4pf = dtlz2pf

dtlz5pf = dtlz2pf

dtlz6pf = function(n, l){
  X = matrix(0, ncol = l, nrow = 2)
  X[1,] = c(seq(0, 0.2514118360, length.out = floor(l/4)),
            seq(0.6316265307, 0.8594008566, length.out = ceiling(l/4)),
            seq(1.3596178367, 1.5148392681, length.out = floor(l/4)),
            seq(2.0518383519, 2.116426807, length.out = ceiling(l/4)))
  X[2,] = 4 - X[1,] * (1 + sin(3 * pi * X[1,]))
  X
}