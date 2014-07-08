## Set of the LZ test functions for the case nD -> 2D.
## For every function: x is a vector of length n such that 0 < x[i] < 1, i=1,...,n
## Output is a vector of length 2


lz1 = function(x){
  f = numeric(2)
  n = length(x)
  J1 = seq(3, n, by = 2)
  J2 = seq(2, n, by = 2)
  f[1] = x[1] + 2 / length(J1) * 
    sum((x[J1] - x[1]^(0.5 * (1 + 3 * (J1 - 2) / (n - 2) )))^2)
  f[2] = 1 - sqrt(x[1]) + 2 / length(J2) *
    sum((x[J2] - x[1]^(0.5 * (1 + 3 * (J2 - 2) / (n - 2) )))^2)
  return(f)
}

lz2 = function(x){
  f = numeric(2)
  n = length(x)
  ## transform x[2:n] to [-1,1]
  x[2:n] = 2*x[2:n] - 1
  J1 = seq(3, n, by = 2)
  J2 = seq(2, n, by = 2)
  f[1] = x[1] + 2 / length(J1) * 
    sum (( x[J1] -  sin(6 * pi * x[1] + J1 * pi / n))^2)
  f[2] = 1 - sqrt(x[1]) + 2 / length(J2) *
    sum (( x[J2] -  sin(6 * pi * x[1] + J2 * pi / n))^2)
  return(f)
}

lz3 = function(x){
  f = numeric(2)
  n = length(x)
  ## transform x[2:n] to [-1,1]
  x[2:n] = 2*x[2:n] - 1
  J1 = seq(3, n, by = 2)
  J2 = seq(2, n, by = 2)
  f[1] = x[1] + 2 / length(J1) * 
    sum (( x[J1] -  0.8 * x[1] * cos(6 * pi * x[1] + J1 * pi / n))^2)
  f[2] = 1 - sqrt(x[1]) + 2 / length(J2) *
    sum (( x[J2] -  0.8 * x[1] * sin(6 * pi * x[1] + J2 * pi / n))^2)
  return(f)
}

lz4 = function(x){
  f = numeric(2)
  n = length(x)
  ## transform x[2:n] to [-1,1]
  x[2:n] = 2*x[2:n] - 1
  J1 = seq(3, n, by = 2)
  J2 = seq(2, n, by = 2)
  f[1] = x[1] + 2 / length(J1) * 
    sum (( x[J1] -  0.8 * x[1] * cos((6 * pi * x[1] + J1 * pi / n) / 3))^2)
  f[2] = 1 - sqrt(x[1]) + 2 / length(J2) *
    sum (( x[J2] -  0.8 * x[1] * sin(6 * pi * x[1] + J2 * pi / n))^2)
  return(f)
}

lz5 = function(x){
  f = numeric(2)
  n = length(x)
  ## transform x[2:n] to [-1,1]
  x[2:n] = 2*x[2:n] - 1
  J1 = seq(3, n, by = 2)
  J2 = seq(2, n, by = 2)
  f[1] = x[1] + 2 / length(J1) * 
    sum ((x[J1] - (0.3 * x[1]^2 * cos(24 * pi * x[1] + 4 * J1 * pi / n) + 0.6*x[1]) *
          cos(6 * pi * x[1] + J1 * pi / n))^2)
  f[2] = 1 - sqrt(x[1]) + 2 / length(J2) *
    sum ((x[J2] - (0.3 * x[1]^2 * cos(24 * pi * x[1] + 4 * J2 * pi / n) + 0.6*x[1]) *
          sin(6 * pi * x[1] + J2 * pi / n))^2)
  return(f)
}

## lz6 has 3 target functions, so it misses here

lz7 = function(x){
  f = numeric(2)
  n = length(x)
  J1 = seq(3, n, by = 2)
  J2 = seq(2, n, by = 2)
  y = numeric(n)
  y[2:n] = x[2:n] - x[1]^(0.5 * (1 + 3 * (2:n - 2) / (n - 2) ))
  f[1] = x[1] + 2 / length(J1) * 
    sum(4 * y[J1]^2 - cos(8 * y[J1] * pi) + 1)
  f[2] = 1 - sqrt(x[1]) + 2 / length(J2) *
    sum(4 * y[J2]^2 - cos(8 * y[J2] * pi) + 1) 
  return(f)
}

lz8 = function(x){
  f = numeric(2)
  n = length(x)
  J1 = seq(3, n, by = 2)
  J2 = seq(2, n, by = 2)
  y = numeric(n)
  y[2:n] = x[-1] - x[1]^(0.5 * (1 + 3 * ((2:n) - 2) / (n - 2) ))
  f[1] = x[1] + 2 / length(J1) * 
    (4 * sum(y[J1]^2) - 2 * prod(cos(20 * y[J1] * pi / sqrt(J1))) + 2)
  f[2] = 1 - sqrt(x[1]) + 2 / length(J2) *
    (4 * sum(y[J2]^2) - 2 * prod(cos(20 * y[J2] * pi / sqrt(J2))) + 2)    
  return(f)
}

lz9 = function(x){
  f = numeric(2)
  n = length(x)
  ## transform x[2:n] to [-1,1]
  x[2:n] = 2*x[2:n] - 1
  J1 = seq(3, n, by = 2)
  J2 = seq(2, n, by = 2)
  f[1] = x[1] + 2 / length(J1) * 
    sum (( x[J1] -  sin(6 * pi * x[1] + J1 * pi / n))^2)
  f[2] = 1 - x[1]^2 + 2 / length(J2) *
    sum (( x[J2] -  sin(6 * pi * x[1] + J2 * pi / n))^2)
  return(f)
}

## Functions, which return l equally distributed points from the
## paretoset of the corresponding lz-function with die in-dimension n

lz1ps = function(n, l){
  X = matrix(0, ncol = l, nrow = n)
  X[1,] = seq(0, 1, length.out = l)
  f = function(x)
    x^(0.5 * (1 + 3 * (2:n - 2) / (n - 2) ))
  X[-1,] = sapply(X[1,], f)
  X
}

lz2ps = function(n, l){
  X = matrix(0, ncol = l, nrow = n)
  X[1,] = seq(0, 1, length.out = l)
  f = function(x)
    sin(6 * pi * x + 2:n * pi / n)
  X[-1,] = sapply(X[1,], f)
  X[-1,] = (X[-1,] + 1) / 2
  X
}

lz3ps = function(n, l){
  X = matrix(0, ncol = l, nrow = n)
  X[1,] = seq(0, 1, length.out = l)
  for (i in 2:n){
    if (i%%2){
      X[i,] = 0.8 * X[1,] * cos(6 * pi * X[1,] + i * pi / n)
    } else{
      X[i,] = 0.8 * X[1,] * sin(6 * pi * X[1,] + i * pi / n) 
    }
  }
  X[-1,] = (X[-1,] + 1) / 2
  X
}

lz4ps = function(n, l){
  X = matrix(0, ncol = l, nrow = n)
  X[1,] = seq(0, 1, length.out = l)
  for (i in 2:n){
    if (i%%2){
      X[i,] = 0.8 * X[1,] * cos((6 * pi * X[1,] + i * pi / n) / 3)
    } else{
      X[i,] = 0.8 * X[1,] * sin(6 * pi * X[1,] + i * pi / n) 
    }
  }
  X[-1,] = (X[-1,] + 1) / 2
  X
}

lz5ps = function(n, l){
  X = matrix(0, ncol = l, nrow = n)
  X[1,] = seq(0, 1, length.out = l)
  for (i in 2:n){
    if (i%%2){
      X[i,] = (0.3 * X[1,]^2 * cos(24 * pi * X[1,] + 4 * i * pi / n) + 0.6 * X[1,]) *
        cos(6 * pi * X[1,] + i * pi / n)
    } else{
      X[i,] = (0.3 * X[1,]^2 * cos(24 * pi * X[1,] + 4 * i * pi / n) + 0.6 * X[1,]) *
        sin(6 * pi * X[1,] + i * pi / n)
    }
  }
  X[-1,] = (X[-1,] + 1) / 2
  X
}

lz7ps = lz1ps

lz8ps = lz1ps

lz9ps = lz2ps

## Functions, which return l equally distributed points from the
## paretofront of the corresponding lz-function with in-dimension n

lz1pf = function(n, l)
  apply(lz1ps(n, l), 2, lz1)

lz2pf = function(n, l)
  apply(lz2ps(n, l), 2, lz2)

lz3pf = function(n, l)
  apply(lz3ps(n, l), 2, lz3)

lz4pf = function(n, l)
  apply(lz4ps(n, l), 2, lz4)

lz5pf = function(n, l)
  apply(lz5ps(n, l), 2, lz5)

lz7pf = function(n, l)
  apply(lz7ps(n, l), 2, lz7)

lz8pf = function(n, l)
  apply(lz8ps(n, l), 2, lz8)

lz9pf = function(n, l)
  apply(lz9ps(n, l), 2, lz9)
