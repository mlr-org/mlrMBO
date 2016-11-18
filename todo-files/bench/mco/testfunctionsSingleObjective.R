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
  x[1] = 15 * x[1] - 5
  x[2] = 15 * x[2]
  y = branin_function()(x)
  return(y)
}

#  Goldstein and Price test function
#  ---------------------------------
#  Dimension: n = 2
#  Domain: -2 <= xi <= 2, i = 1, 2
goldsteinPrice = function(x) {
  x[1] = 4 * x[1] - 2
  x[2] = 4 * x[2] - 2
  y = log(goldstein_price_function()(x))
  return(y)
}


# 3-Hump Camel Function
# --------------------
# Dimension: n = 2
# Domain: -2 <= xi <= 2, i = 1,2
camel3 <- function(x) {
  x1 <- 4 * x[1] - 2
  x2 <- 4 * x[2] - 2
  
  term1 <- 2 * x1 ^ 2
  term2 <- -1.05 * x1 ^ 4
  term3 <- x1 ^ 6 / 6
  term4 <- x1 * x2
  term5 <- x2 ^ 2
  
  y <- term1 + term2 + term3 + term4 + term5
  return(y)
}


# 6-Hump Camel Function
# --------------------
# Dimension: n = 2
# Domain: -2 <= x1 <= 2, -1 <= x2 <= 1
camel6 <- function(x) {
  x1 <-  4 * x[1] - 2
  x2 <- 2 * x[2] - 1
  
  term1 <- (4 - 2.1 * x1 ^ 2 + (x1 ^ 4) / 3) * x1 ^ 2
  term2 <- x1 * x2
  term3 <- (-4 + 4 * x2 ^ 2) * x2 ^ 2
  
  y <- term1 + term2 + term3
  return(y)
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
  
  y = 0
  for (i in 1:q) {
    exponent = 0
    for (j in 1:dim) {
      exponent = exponent + a[i, j] * (x[j] - p[i,j])^2
      
    }
    y = y + C[i] * exp(-exponent)
  }
  
  if (dim == 6) {
    y = - log(y)
  } else {
    y = - y
  }
  
  return(y)
}



#  Rosenbrock test function
#  ------------------------
#  Dimension: scalable in n
#  Domain: -5 <= xi <= 10, i = 1...n
rosenbrock = function(x) {
  x = 15 * x - 5
  dim = length(x)
  y = rosenbrock_function(dim)(x)
  return(y)
}


# Shifted and rotated version of the Rastrigin test function
# ----------------------------------------------------------
# Dimension: scalable in n
# Domain: -0.5 <= xi <= 0.5, i = 1...n
rastrigin = function(x) {
  x = 1 * x - 0.5
  dim = length(x) 
  step = 5 / dim
  shift = seq(from = step, to = 10 - step, by = 2 * step) - 5
  x = shiftAndRotate(x, 22.5, shift)
  y = rastrigin_function(dim)(x)
  return(y)
}


# Zakharov Function
# -----------------
# Dimension: scalable in n
# Domain: -1 <= xi <= 1, i = 1...n
zakharov <- function(x) {
  x = 2 * x - 1
  dim = length(x)
  
  ii <- c(1:dim)
  sum1 <- sum(x ^ 2)
  sum2 <- sum(0.5 * ii * x)
  
  y <- sum1 + sum2 ^ 2 + sum2 ^ 4
  return(y)
}


# Powell Function
# ---------------
# Dimension: scalable in n
# Domain: -1 <= xi <= 1, i = 1, ..., n

powell <- function(x) {
  x = 2 * x - 1
  dim <- length(x)
  
  xa <- x[seq(1, dim - 3, 4)]
  xb <- x[seq(2, dim - 2, 4)]
  xc <- x[seq(3, dim - 1, 4)]
  xd <- x[seq(4, dim, 4)]
  
  sumterm1 <- (xa + 10 * xb) ^ 2
  sumterm2 <- 5 * (xc - xd) ^ 2
  sumterm3 <- (xb - 2 * xc) ^ 4
  sumterm4 <- 10 * (xa - xd) ^ 4
  y <- sum(sumterm1 + sumterm2 + sumterm3 + sumterm4)

  return(y)
}

