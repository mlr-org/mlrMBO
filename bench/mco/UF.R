## Set of the CEC test functions for the case nD -> 2D.
## Since UF1, ..., UF3 are infrom the LZ-testset and UF8, ... is nD -> 3D, only
## UF4, ..., UF7 are used.
## For every function: x is a vector of length n such that 0 < x[i] < 1, i=1,...,n
## Output is a vector of length 2

uf4 = function(x){
  n = length(x)
  x[2:n] = 4 * x[2:n] - 2
  f = numeric(2)
  J1 = seq(3, n, by = 2)
  J2 = seq(2, n, by = 2)
  y = x - sin(6 * pi * x[1] + (1:n) * pi / n)
  h = abs(y) / (1 + exp(2 * abs(y)))
  f[1] = x[1] + 2 / length(J1) * sum(h[J1])
  f[2] = 1 - x[1]^2 + 2 / length(J2) * sum(h[J2])
  return(f)
}

uf5 = function(x, N = 10, eps = 0.1){
  n = length(x)
  x[2:n] = 2 * x[2:n] - 1
  f = numeric(2)
  J1 = seq(3, n, by = 2)
  J2 = seq(2, n, by = 2)
  y = x - sin(6 * pi * x[1] + (1:n) * pi / n)
  h = 2*y^2 - cos(4 * pi * y) + 1
  f[1] = x[1] + (1 / (2 * N) + eps) * abs(sin(2 * N * pi * x[1])) +
    2 / length(J1) * sum(h[J1])
  f[2] = 1 - x[1] + (1 / (2 * N) + eps) * abs(sin(2 * N * pi * x[1])) +
    2 / length(J2) * sum(h[J2])
  return(f)
}

uf6 = function(x, N = 2, eps = 0.1){
  n = length(x)
  x[2:n] = 2 * x[2:n] - 1
  f = numeric(2)
  J1 = seq(3, n, by = 2)
  J2 = seq(2, n, by = 2)
  y = x - sin(6 * pi * x[1] + (1:n) * pi / n)
  f[1] = x[1] +     max(0, 2 * (1 / (2 * N) + eps) * sin(2 * N * pi * x[1])) +
    2 / length(J1) * (4 * sum(y[J1]^2) - 2 * prod(cos(20 * y[J1] * pi / sqrt(J1))) + 2)
  f[2] = 1 - x[1] + max(0, 2 * (1 / (2 * N) + eps) * sin(2 * N * pi * x[1])) +
    2 / length(J2) * (4 * sum(y[J2]^2) - 2 * prod(cos(20 * y[J2] * pi / sqrt(J2))) + 2)
  return(f)
}

uf7 = function(x){
  n = length(x)
  x[2:n] = 2 * x[2:n] - 1
  f = numeric(2)
  J1 = seq(3, n, by = 2)
  J2 = seq(2, n, by = 2)
  y = numeric(n)
  y[2:n] = x[2:n] - sin(6 * pi * x[1] + (2:n) * pi / n)
  f[1] = (x[1])^(1/5) +  2 / length(J1) * sum(y[J1]^2)
  f[2] = 1 - (x[1])^(1/5) + 2 / length(J2) * sum(y[J2]^2)
  return(f)
}

## Functions, which return l equally distributed points from the
## paretoset of the corresponding uf-function with die in-dimension n

uf4ps = function(n, l){
  X = matrix(0, ncol = l, nrow = n)
  X[1,] = seq(0, 1, length.out = l)
  f = function(x)
    sin(6 * pi * x + 2:n * pi / n)
  X[-1,] = sapply(X[1,], f)
  X[-1,] = (X[-1,] + 2) / 4
  X
}

uf7ps = function(n, l){
  X = matrix(0, ncol = l, nrow = n)
  X[1,] = seq(0, 1, length.out = l)
  f = function(x)
    sin(6 * pi * x + 2:n * pi / n)
  X[-1,] = sapply(X[1,], f)
  X[-1,] = (X[-1,] + 1) / 2
  X
}


## Functions, which return l equally distributed points from the
## paretofront of the corresponding uf-function with in-dimension n

uf4pf = function(n, l)
  apply(uf4ps(n, l), 2, uf4)

uf5pf = function(n, l, N=10){
  inds = rep(0:(2 * N), length.out = l)
  X = matrix(0, nrow = 2, ncol = l)
  X[1,] = inds / 2 / N
  X[2,] = 1 - X[1,]
  X
}

uf6pf = function(n, l, N=2){
  X = matrix(0, nrow = 2, ncol = l)
  a = (l-1) %% N
  b = (l-1) %/% N
  anz = sample(c(rep(b, N - a), rep(b+1, a)))
  X[,1] = c(0, 1)
  c = 1
  for(i in 1:N){
    X[1, (c + 1):(c + anz[i])] = seq( (2*i-1) / 2/ N, 2 * i / 2 / N, length.out = anz[i])
    c = c + anz[i] 
  }
  X[2,-1] = 1 - X[1,-1]
  X
}

uf7pf = function(n, l){
  X = matrix(0, ncol = l, nrow = 2)
  X[1,] = seq(0, 1, length.out = l)
  X[2,] = 1 - X[1,]
  X
}
