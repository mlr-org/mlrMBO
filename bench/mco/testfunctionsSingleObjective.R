library(soobench)

################################################################################

shiftAndRotate = function(x, rotation.angle, shift) {
  dim = length(x)
  
  # shift
  if (length(shift) == 1) {
    for (i in 1:dim) {
      if (i %% 2 == 0) {
        x[i] = x[i] - shift
      } else {
        x[i] = x[i] + shift
      }
    }
  } else {
    x = x + shift
  }
  
  # rotate
  radial = rotation.angle * pi / 180
  if (dim != 1) {
    R = matrix(c(cos(radial), -sin(radial), sin(radial), cos(radial)), nrow = 2, 
      byrow = TRUE)
    for (i in 1:(dim-1)) {
      for (j in (i+1):dim) {
        x[c(i, j)] = x[c(i, j)] %*% R
      }
    }
  }
  return(x)
}


################################################################################





# Branin test function
# --------------------
# Dimension: n = 2.
# Domain: -5 <= x1 <= 10, 0 <= x2 <= 15.
branin = function(x) {
  y = branin_function()(x) - 0.397887
  return(y)
}

#  Goldstein and Price test function
#  ---------------------------------
#  Dimension: n = 2
#  Domain: -2 <= xi <= 2, i = 1, 2
goldsteinprice = function(x) {
  y = log(goldstein_price_function()(x)) - log(3)
  return(f)
}


# Hartman's Dim,q function family
# -------------------------------
# Dimension: n = 1...6
# Domain: 0 <= xi <= 1, i = 1...6
hartman = function(x) {
  dim = length(x)
  
  a = cbind(c(10.00,  0.05,  3.00, 17.00),
    c(3.00, 10.00,  3.50,  8.00),
    c(17.00, 17.00,  1.70,  0.05),
    c(3.50,  0.10, 10.00, 10.00),
    c(1.70,  8.00, 17.00,  0.10),
    c(8.00, 14.00,  8.00, 14.00))
  p = cbind(c(0.1312, 0.2329, 0.2348, 0.4047),
    c(0.1696, 0.4135, 0.1451, 0.8828),
    c(0.5569, 0.8307, 0.3522, 0.8732),
    c(0.0124, 0.3736, 0.2883, 0.5743),
    c(0.8283, 0.1004, 0.3047, 0.1091),
    c(0.5886, 0.9991, 0.6650, 0.0381))
  C = c(1.0, 1.2, 3.0, 3.2)
  q = 4
  offset = c(7.747, 4.806, 3.956, 3.727, 3.473, 3.323)
  
  y = 0
  for (i in 1:q) {
    exponent = 0
    for (j in 1:dim) {
      exponent = exponent + a[i, j] * (x[j] - p[i,j])^2
      
    }
    y = y + C[i] * exp(-exponent)
  }
  
  if (dim == 6) {
    y = log(offset[dim]) - log(y)
  } else {
    y = offset[dim] - y
  }
  
  return(y)
}



# 2nd objective of the multi-objective OKA2 test function
# -------------------------------------------------------
# Dimension: n = 3
# Domain: -pi <= x1 <= pi, -5 <= xi <= 5 i = 2, 3

OKA2Obj2 = function(x) {
  y = 1 - 1/(4 * pi ^ 2) * (x[1] + pi)^2 + abs(x[2] - 5 * cos(x[1])) ^ (1/3) + abs(x[3] - 5 * sin(x[1])) ^ (1/3)
  return(y)
}


# Single-objective testfunction based on the multi-objective OKA2 function
# ------------------------------------------------------------------------
# Dimension: n = 2
# Domain: -5 <= xi <= 5, i = 1, 2
OKA2Single = function(x) {
  if (x[1] < 0 & x[2] <= 0) {
    x1 = mean(c(-acos(x[1]/5), -pi - asin(x[2]/5)))
  } 
  if (x[1] >= 0 & x[2] < 0) {
    x1 = mean(c(-acos(x[1]/5), asin(x[2]/5)))
  }
  if (x[1] > 0 & x[2] >= 0) {
    x1 = mean(c(acos(x[1]/5), asin(x[2]/5)))
  }
  if (x[1] <= 0 & x[2] >= 0) {
    x1 = mean(c(acos(x[1]/5), pi - asin(x[2]/5)))
  }
  y = 1 - 1/(4 * pi ^ 2) * (x1 + pi)^2 + abs(x[1] - 5 * cos(x1)) ^ (1/2) + abs(x[2] - 5 * sin(x1)) ^ (1/2)
  return(y)
}


# Single-objective testfunction based on the multi-objective OKA2 function
# ------------------------------------------------------------------------
# Dimension: n = 2
# Domain: -5 <= xi <= 5, i = 1, 2
OKA2Single2 = function(x) {
  dim = length(x)
  x1 = 0.9 * pi - pi / 100 * sum((x - c(5 * cos(0.9 * pi), 5 * sin(0.9 * pi)))^2)
  y = 1 - 1/(4 * pi ^ 2) * (x1 + pi)^2 + abs(x[1] - 5 * cos(x1)) ^ (1/2) + abs(x[2] - 5 * sin(x1)) ^ (1/2)
  return(y)
}


# Shifted and rotated version of the Rastrigin test function
# ----------------------------------------------------------
# Dimension: scalable in n
# Domain: -5.12 <= xi <= 5.12, i = 1...n

rastrigin = function(x) {
  dim = length(x) 
  step = 5 / dim
  shift = seq(from = step, to = 10 - step, by = 2 * step) - 5
  x = shiftAndRotate(x, 22.5, shift)
  y = rastrigin_function(dim)(x)
  return(y)
}

rastriginRelax = function(x) {
  dim = length(x) 
  step = 1/ (2 * dim)
  shift = seq(from = step, to = 1 - step, by = 2 * step) - 0.5
  x = shiftAndRotate(x, 22.5, shift)
  y = rastrigin_function(dim)(x)
  return(y)
}

#  Rosenbrock test function
#  ------------------------
#  Dimension: scalable in n
#  Domain: -5 <= xi <= 10, i = 1...n

rosenbrock = function(x) {
  dim = length(x)
  y = rosenbrock_function(dim)(x)
  return(y)
}

# Sakata Test function
# --------------------
# One-dimensional test function from Sakata S, Ashida F, Zako M.:
# On applying Kriging-based approximate optimization to inaccurate data.
# Int'l. J. Comp. Meth. in Applied Mech. & Eng. 196 (2007), pp. 2055–2069
# x in [0, 5]

sakata = function(x) {
  y = -x * sin(2*x) + 0.2 * x + 3.16172429148322
  return(y)
}


#  Wagner test function
#  ---------------------
#  Dimension: n = 3
#  Domain: -1 <= xi <= 1, i = 1,2,3
wagner = function(x) {
  y = 14.3388 + (10 * sin(10 * x[1]) + x[1] ^ 2) * (((sin(x[2]) / (x[2] ^ 2 + 1)) + 1) / (2 * x[3] ^ 2 + 1))
  return(y)
}


# Shifted and rotated exponentially weighted sphere function
# ----------------------------------------------------------
# Dimension: scalable in n
# Domain: -5 <= xi <= 5, i = 1...n

weightedSphere = function(x) {
  dim = length(x)
  shift = seq(from = -5, to = 5, length.out = dim)
  x = shiftAndRotate(x, 22.5, shift)
  y = 0
  for (i in 1:dim) {
    y = y + 2 ^ i * x[i] ^ 2
  }
  return(y)
}


# % 2nd objective of the multi-objective ZDT4_Relax test function
# % -------------------------------------------------------------
#   % Dimension: scalable in n (usually 3)
# % Domain: 0 <= x1 <= 1, -1 <= xi <= 1 i = 2...n
ZDT4Obj2 = function(x) {
  dim = length(x)
  step = 1 / (dim - 1)
  shift = seq(from = step, to = (2 - step), by = 2 * step) - 1
  xRot = shiftAndRotate(x[2:dim], 22.5, shift)
  sumInG = 0
  for (i in 1:(dim - 1)) {
    sumInG = sumInG + xRot[i]^2 - 10 * cos(4 * pi * xRot[i])
  }
  g = 1 + 10 * (dim - 1) + sumInG
  y = g * (1 - sqrt(x[1] / g))
  return(y)
}

