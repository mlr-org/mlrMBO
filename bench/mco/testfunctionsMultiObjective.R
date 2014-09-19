library(mco)

source("bench/mco/testfunctionsSingleObjective.R")


# 2D -> 2M
GOMOP_2D2M = function(x) {
  y = numeric(2)
  y[1] = branin(x)
  y[2] = camel3(x)
  return(y)
}

# 5D -> 2M
GOMOP_5D2M = function(x) {
  y = numeric(2)
  y[1] = hartman(x)
  y[2] = rastrigin(x)
  return(y)
}

# 2D -> 5M
GOMOP_2D5M = function(x) {
  y = numeric(5)
  y[1] = branin(x)
  y[2] = hartman(x)
  y[3] = goldsteinPrice(x)
  y[4] = camel3(x)
  y[5] = camel6(x)
  return(y)
}

# 5D -> 5M
GOMOP_5D5M = function(x) {
  y = numeric(5)
  y[1] = hartman(x)
  y[2] = rosenbrock(x)
  y[3] = rastrigin(x)
  y[4] = zakharov(x)
  y[5] = powell(x)
  return(y)
}


# ZDT - functions
# 5D -> 2M
# functions zdt1(), zdt2() and zdt3() from mco package


# DTLZ - functions

# dtlz1: 5D -> 5M
dtlz1_5D5M = function(x) {
  stopifnot(length(x) >= 5)
  y = numeric(5)
  n = length(x)
  g = 100 * (n - 4 + sum((x[5:n] - 0.5) ^ 2 - cos(20 * pi * (x[5:n] - 0.5))))
  y[1] = 1/2 * x[1] * x[2] * x[3] * x[4] * (1 + g)
  y[2] = 1/2 * x[1] * x[2] * x[3] * (1 - x[4]) * (1 + g)
  y[3] = 1/2 * x[1] * x[2] * (1 - x[3]) * (1 + g)
  y[4] = 1/2 * x[1] * (1 - x[2]) * (1 + g)
  y[5] = 1/2 * (1 - x[1]) * (1 + g)
  return(y)
}

# dtlz2: 5D -> 2M
dtlz2_5D2M = function(x) {
  f = numeric(2)
  n = length(x)
  g = sum((x[2:n] - 0.5)^2)
  f[1] = (1 + g) * cos(x[1] * pi / 2)
  f[2] = (1 + g) * sin(x[1] * pi / 2)
  return(f)
}


# dtlz2: 5D -> 5M
dtlz2_5D5M = function(x) {
  stopifnot(length(x) >= 5)
  y = numeric(5)
  n = length(x)
  g = sum((x[5:n] - 0.5)^2)
  y[1] = (1 + g) * cos(x[1] * pi / 2) * cos(x[2] * pi / 2) * cos(x[3] * pi / 2) * cos(x[4] * pi / 2)
  y[2] = (1 + g) * cos(x[1] * pi / 2) * cos(x[2] * pi / 2) * cos(x[3] * pi / 2) * sin(x[4] * pi / 2)
  y[3] = (1 + g) * cos(x[1] * pi / 2) * cos(x[2] * pi / 2) * sin(x[3] * pi / 2)
  y[4] = (1 + g) * cos(x[1] * pi / 2) * sin(x[2] * pi / 2)
  y[5] = (1 + g) * sin(x[1] * pi / 2)
  return(y)
}

