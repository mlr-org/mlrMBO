library(emoa)

## The shape functions with M=2
## In: A numeric value x, perhaps some constants alpha, A
## Out: A numeric value
make01 = function(x)
  pmin(1, pmax(0, x))


linear1 = function(x) 
  make01(x)

linear2 = function(x)
  make01(1 - x)

convex1 = function(x)
  make01(1 - cos(x * pi / 2))

convex2 = function(x)
  make01(1 - sin(x * pi / 2))

concave1 = function(x)
  make01(sin(x * pi / 2))

concave2 = function(x)
  make01(cos(x * pi / 2))

mixed2 = function(x, alpha, A){
  tmp = 2 * A * pi
  make01((1 - x - cos(tmp * x + pi / 2) / tmp)^alpha)
}

disc2 = function(x, alpha, beta, A)
  make01(1 - x^alpha * cos(A * x^beta * pi)^2)

## The Tranformation Functions
## In: A numeric value y, perhaps some constants
## Out: A numeric value
bPoly = function(y, alpha)
  make01(y^alpha)


bFlat = function(y, A, B, C){
  tmp1 = pmin(0, floor(y - B)) * A * (B - y) / B
  tmp2 = pmin(0, floor(C - y)) * (1 - A) * (y - C) / (1 - C)
  make01(A + tmp1 - tmp2)
}

bParam = function(y, u, A, B, C){
  v = A - (1 - 2 * u) * abs(floor(0.5 - u) + A);
  make01(y^(B + (C - B)*v))
}

sLinear = function(y, A)
  make01( abs(y - A) / abs(floor(A - y) + A) )


sDecept = function(y, A, B, C){
  tmp1 = floor(y - A + B) * (1 - C + (A - B) / B) / (A - B) 
  tmp2 = floor(A + B - y) * (1 - C + (1 - A - B) / B) / (1 - A - B)
  make01(1 + (abs(y - A) - B) * (tmp1 + tmp2 + 1 / B))
}

sMulti = function(y, A, B, C){
  tmp1 = abs(y - C) / (2 * (floor(C - y) + C))
  tmp2 = (4 * A + 2) * pi * (0.5 - tmp1)
  make01(( 1 + cos(tmp2) + 4 * B * (tmp1)^2) / (B + 2))
}

## For these 2 functions y is a vector, not a single value
rSum = function(y, w)
  make01(sum(w * y) / sum(w))


rNonsep = function(y, A){
 n = length(y)
 if (A == 1) return(rSum(y, rep(1, n)))
 mat = array(dim = c(n, A - 1))
 for (i in 1:(A-1))
   mat[,i] = y[1 + (i : (i + n - 1) %% n)]
 make01 ((sum(y) + sum(abs(y - mat))) / (n / A * ceiling(A / 2) * (1 + 2 * A - 2 * ceiling(A / 2))))
}

## Now the 9 WFG09 testfunctions for M = 2
## In: Vector z of length n, 0 < z[i] < 1, i = 1,...,n
##     k: Integers with n = k + l, l%%2==0
## Out: Vector of length 2

wfg1 = function(z, k=3){
  n = length(z)
  t1 = numeric(n)
  t1[1:k] = z[1:k]
  t1[(k+1):n] = sLinear(z[(k+1):n], 0.35)
  t2 = numeric(n)
  t2[1:k] = t1[1:k]
  t2[(k+1):n] = bFlat(t1[(k+1):n], 0.8, 0.75, 0.85)
  t3 = bPoly(t2, 0.02)
  t4 = numeric(2)
  t4[1] = rSum(t3[1:k], 2*1:k)
  t4[2] = rSum(t3[(k+1):n], 2*(k+1):n)
  x = numeric(2)
  x[1] = max(t4[2], 1)*(t4[1] - 0.5) + 0.5
  x[2] = t4[2]
  f = numeric(2)
  f[1] =  x[2] + 2 * convex1(x[1])
  f[2] =  x[2] + 4 * mixed2(x[1], 1, 5)
  return(f)
}


wfg2 = function(z, k=3){
  n = length(z)
  l = n - k
  t1 = numeric(n)
  t1[1:k] = z[1:k]
  t1[(k+1):n] = sLinear(z[(k+1):n], 0.35)
  t2 = numeric(k + l/2)
  t2[1:k] = t1[1:k]
  t2[(k+1):(k+l/2)] = apply(matrix(t1[(k+1):n], nrow=2), 2, rNonsep, A=2)    
  t3 = numeric(2)
  t3[1] = rSum(t2[1:k], rep(1, k))
  t3[2] = rSum(t2[(k+1):(k+l/2)], rep(1, l/2))
  x = numeric(2)
  x[1] = max(t3[2], 1)*(t3[1] - 0.5) + 0.5
  x[2] = t3[2]
  f = numeric(2)
  f[1] =  x[2] + 2 * convex1(x[1])
  f[2] =  x[2] + 4 * disc2(x[1], 1, 1, 5)
  return(f)
}


wfg3 = function(z, k=3){
  n = length(z)
  l = n - k
  t1 = numeric(n)
  t1[1:k] = z[1:k]
  t1[(k+1):n] = sLinear(z[(k+1):n], 0.35)
  t2 = numeric(k + l/2)
  t2[1:k] = t1[1:k]
  t2[(k+1):(k+l/2)] = apply(matrix(t1[(k+1):n], nrow=2), 2, rNonsep, A=2)    
  t3 = numeric(2)
  t3[1] = rSum(t2[1:k], rep(1, k))
  t3[2] = rSum(t2[(k+1):(k+l/2)], rep(1, l/2))
  x = numeric(2)
  x[1] = max(t3[2], 1)*(t3[1] - 0.5) + 0.5
  x[2] = t3[2]
  f = numeric(2)
  f[1] =  x[2] + 2 * linear1(x[1])
  f[2] =  x[2] + 4 * linear2(x[1])
  return(f)
}


wfg4 = function(z, k=3){
  n = length(z)
  t1 = sMulti(z, 30, 10, 0.35)
  t2 = numeric(2)
  t2[1] = rSum(t1[1:k], rep(1, k))
  t2[2] = rSum(t1[(k+1):n], rep(1, n-k))
  x = numeric(2)
  x[1] = max(t2[2], 1)*(t2[1] - 0.5) + 0.5
  x[2] = t2[2]
  f = numeric(2)
  f[1] =  x[2] + 2 * concave1(x[1])
  f[2] =  x[2] + 4 * concave2(x[1])
  return(f)
}

wfg5 = function(z, k=3){
  n = length(z)
  t1 = sDecept(z, 0.35, 0.001, 0.05)
  t2 = numeric(2)
  t2[1] = rSum(t1[1:k], rep(1, k))
  t2[2] = rSum(t1[(k+1):n], rep(1, n-k))
  x = numeric(2)
  x[1] = max(t2[2], 1)*(t2[1] - 0.5) + 0.5
  x[2] = t2[2]
  f = numeric(2)
  f[1] =  x[2] + 2 * concave1(x[1])
  f[2] =  x[2] + 4 * concave2(x[1])
  return(f)
}

wfg6 = function(z, k=3){
  n = length(z)
  t1 = numeric(n)
  t1[1:k] = z[1:k]
  t1[(k+1):n] = sLinear(z[(k+1):n], 0.35)
  t2 = numeric(2)
  t2[1] = rNonsep(t1[1:k], k)
  t2[2] = rNonsep(t1[(k+1):n], n - k)
  x = numeric(2)
  x[1] = max(t2[2], 1)*(t2[1] - 0.5) + 0.5
  x[2] = t2[2]
  f = numeric(2)
  f[1] =  x[2] + 2 * concave1(x[1])
  f[2] =  x[2] + 4 * concave2(x[1])
  return(f)
}

wfg7 = function(z, k=3){
  n = length(z)
  t1 = numeric(n)
  tmp = sum(z[(k+1):n])
  for(i in k:1){
    t1[i] = bParam(z[i], tmp, 0.98/49.98, 0.02, 50)
    tmp = tmp + z[i]
  }   
  t1[(k+1):n] = z[(k+1):n]
  t2 = numeric(n)
  t2[1:k] = t1[1:k]
  t2[(k+1):n] = sLinear(t1[(k+1):n], 0.35)
  t3 = numeric(2)
  t3[1] = rSum(t2[1:k], rep(1, k))
  t3[2] = rSum(t2[(k+1):n], rep(1, n-k))
  x = numeric(2)
  x[1] = max(t3[2], 1)*(t3[1] - 0.5) + 0.5
  x[2] = t3[2]
  f = numeric(2)
  f[1] =  x[2] + 2 * concave1(x[1])
  f[2] =  x[2] + 4 * concave2(x[1])
  return(f)
}

wfg8 = function(z, k=3){
  n = length(z)
  t1 = numeric(n)
  t1[1:k] = z[1:k]
  tmp = sum(z[1:k])
  for(i in (k+1):n){
    t1[i] = bParam(z[i], tmp, 0.98/49.98, 0.02, 50)
    tmp = tmp + z[i]
  }   
  t1[(k+1):n] = z[(k+1):n]
  t2 = numeric(n)
  t2[1:k] = t1[1:k]
  t2[(k+1):n] = sLinear(t1[(k+1):n], 0.35)
  t3 = numeric(2)
  t3[1] = rSum(t2[1:k], rep(1, k))
  t3[2] = rSum(t2[(k+1):n], rep(1, n-k))
  x = numeric(2)
  x[1] = max(t3[2], 1)*(t3[1] - 0.5) + 0.5
  x[2] = t3[2]
  f = numeric(2)
  f[1] =  x[2] + 2 * concave1(x[1])
  f[2] =  x[2] + 4 * concave2(x[1])
  return(f)
}

wfg9 = function(z, k=3){
  n = length(z)
  t1 = numeric(n)
  t1[n] = z[n]
  tmp = z[n]
  for(i in (n-1):1){
    t1[i] = bParam(z[i], tmp, 0.98/49.98, 0.02, 50)
    tmp = tmp + z[i]
  }   
  t2 = numeric(n)
  t2[1:k] = sDecept(t1[1:k], 0.35, 0.001, 0.05)
  t2[(k+1):n] = sMulti(t2[(k+1):n], 30, 95, 0.35)
  t3 = numeric(2)
  t3[1] = rNonsep(t2[1:k], k)
  t3[2] = rNonsep(t2[(k+1):n], n - k)
  x = numeric(2)
  x[1] = max(t3[2], 1)*(t3[1] - 0.5) + 0.5
  x[2] = t3[2]
  f = numeric(2)
  f[1] =  x[2] + 2 * concave1(x[1])
  f[2] =  x[2] + 4 * concave2(x[1])
  return(f)
}

## Functions, which return l equally distributed points from the
## paretofront of the corresponding uf-function with in-dimension n

wfg1pf = function(n, l){
  x = seq(0, 1, length.out = l)
  X = matrix(0, ncol = l, nrow = 2)
  X[1,] = convex1(x)
  X[2,] = mixed2(x, alpha = 1, A = 5)
  X * c(2,4)
}

wfg2pf = function(n, l){
  x = seq(0, 1, length.out = 10*l)
  X = matrix(0, ncol = 10*l, nrow = 2)
  X[1,] = convex1(x)
  X[2,] = disc2(x, alpha = 1, beta = 1, A = 5)
  X = nondominated_points(X)
  X[,sort(sample(ncol(X), l))] * c(2,4)
}

wfg3pf = function(n, l){
  X = matrix(0, ncol = l, nrow = 2)
  X[1,] = seq(0, 1, length.out = l)
  X[2,] = 1 - X[1,]
  X * c(2,4)
}


wfg4pf = function(n, l){
  x = seq(0, 1, length.out = l)
  X = matrix(0, ncol = l, nrow = 2)
  X[1,] = concave1(x)
  X[2,] = concave2(x)
  X * c(2,4)
}

wfg5pf = wfg4pf

wfg6pf = wfg4pf

wfg7pf = wfg4pf

wfg8pf = wfg4pf

wfg9pf = wfg4pf