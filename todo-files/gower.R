#' @title Calculate covariance matrix based on Gower distances.
#'
#  @description
#' Calculates a matrix of covariances between rows of \code{data.x} and rows of \code{data.y}.
#'
#' The function can handle mixed parameter spaces. It first calculates the Gower
#' distances, then scales them with theta, then passes them to a kernel function.
#'
#' @param [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Param set for feature space, containing \code{p} parameters (when counting devectorized scalars).
#' @param data.x [\code{data.frame(n, p)}]\cr
#'   x_i observations for cov matrix calculations cov(x_i, x_j), corresponds to rows of result.
#' @param data.y [\code{data.frame(m, p)}]\cr
#'   x_j observations for cov matrix calculations cov(x_i, x_j), corresponds to cols of result.
#'   Default is \code{data.x} so we get a normal covariance matrix for only the x_i observations.
#' @param theta [\code{numeric}]\cr
#'   Length scales for all features. Distances get scaled by (d / theta) before they enter the
#'   covariance kernel defined by \code{dist2cov}.
#'   Default is all 1s.
#' @param dist2cov [\code{character(1)} | \code{function(d)}]\cr
#'   Selected covariance kernel. Can be one of: \dQuote{gauss}, \dQuote{matern3_2}, \dQuote{matern5_2}.
#'   You can also pass a function. Note that \code{d} is a n x m matrix, containing the
#'   gower distances between \dQuote{data.x} and \code{data.y} for ONLY ONE currently considered
#'   feature. You must return a matrix of the same dimension.
#'   Example for Gaussian: \code{exp{-d^2}}.
#'   Default is \dQuote{gauss}.
#' @param KR.corr [\code{logical(1)}}]\cr
#'   See \code{\link[StatMatch]{gower.dist}.
#'   Default is \code{TRUE}.
#' @return [\code{matrix(n, m)}]. Covariances between \code{data.x} and \code{data.y}.
calcGowerCovMat = function (par.set, data.x, data.y = data.x, theta = NULL, dist2cov = "gauss", KR.corr = TRUE) {
  assertClass(par.set, "ParamSet")
  p = getParamNr(par.set, devectorize = TRUE)
  assertDataFrame(data.x, min.rows = 1L, ncols = p)
  assertDataFrame(data.y, min.rows = 1L, ncols = p)
  if (is.null(theta))
    theta = rep(1, p)
  else
    assertNumeric(theta, any.missing = FALSE, len = p, lower = 0)


  if (is.character(dist2cov)) {
    assertChoice(dist2cov, c("gauss", "matern3_2", "matern5_2"))
    dist2cov = switch(dist2cov,
      gauss = function(d) exp(-d^2),
      matern3_2 = function(d) (1 + sqrt(3)*d) * exp(-sqrt(3) * d),
      matern5_2 = function(d) (1 + sqrt(5)*d + (5/3) * d^2) * exp(-sqrt(5) * d)
    )
  } else {
    assertFunction(dist2cov, "d")
  }

  assertFlag(KR.corr)

  pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  pids1 = getParamIds(filterParams(par.set, type = c("numeric", "numericvector")), repeated = TRUE, with.nr = TRUE)
  pids2 = getParamIds(filterParams(par.set, type = c("discrete", "discretevector")), repeated = TRUE, with.nr = TRUE)
  low = getLower(par.set, with.nr = TRUE)
  upp = getUpper(par.set, with.nr = TRUE)
  rngs = setNames(numeric(p), pids)
  rngs[pids1] = upp - low
  rngs[pids2] = NA_real_

  gower.fcn.test = function(theta.k, x, y, rng = NULL, KR.corr = TRUE) {
    nx = length(x)
    ny = length(y)
    cx = class(x)
    cy = class(y)
    delta = matrix(1, nx, ny)
    if (!identical(cx, cy))
      stop("the x and y object are of different type")
    if (is.logical(x)) {
      dd = abs(outer(X = x, Y = y, FUN = "-"))
      delta[outer(x == FALSE, y == FALSE, FUN = "&")] = 0
      delta[outer(is.na(x), is.na(y), FUN = "|")] = 0
    }
    else if (is.character(x) || (is.factor(x) && !is.ordered(x))) {
      if (is.factor(x) && !identical(levels(x), levels(y)))
        stop("x and y have different levels")
      xx = c(matrix(as.character(x), nx, ny))
      yy = c(matrix(as.character(y), nx, ny, byrow = TRUE))
      dd = 1 - outer(x, y, FUN = "==")
      delta[outer(is.na(x), is.na(y), FUN = "|")] = 0
    }
    else if (is.ordered(x)) {
      if (KR.corr) {
        x = as.numeric(x)
        y = as.numeric(y)
        if (is.null(rng) || is.na(rng))
          rng = max(x, y) - 1
        zx = (x - 1)/rng
        zy = (y - 1)/rng
        dd = abs(outer(X = zx, Y = zy, FUN = "-"))/(max(zx,
                                                         zy) - min(zx, zy))
        delta[outer(is.na(zx), is.na(zy), FUN = "|")] = 0
      }
      else {
        x = as.numeric(x)
        y = as.numeric(y)
        if (is.null(rng) || is.na(rng))
          rng = max(x, y) - 1
        dd = abs(outer(X = x, Y = y, FUN = "-"))/rng
        delta[outer(is.na(x), is.na(y), FUN = "|")] = 0
      }
    }
    else {
      if (is.null(rng) || is.na(rng))
        rng = max(x, y) - min(x, y)
      dd = abs(outer(X = x, Y = y, FUN = "-"))/rng
      delta[outer(is.na(x), is.na(y), FUN = "|")] = 0
    }
    # dd = normal gower dist from i to j, in feature k
    # delta = similar to dd above, but 1 implies both feature k entries are "set"
    dist.scaled = dd / (delta * theta.k)
    prod.kern = dist2cov(dist.scaled)

    list(dist = dd, delta = delta*theta.k, prod.kern = prod.kern)
  }

  ### End gower.fcn.test
  if (is.null(dim(data.x)) && is.null(dim(data.y))) {
    stop("error in gowerMaternCovariance - this should'nt happen 2")
    out.gow = gower.fcn.test(x = data.x, y = data.y, rng = rngs,
                              KR.corr = KR.corr)
    out = (out.gow$dist * out.gow$delta)/out.gow$delta
  }
  else if (is.null(dim(data.x)) && !is.null(dim(data.y))) {
    stop("error in gowerMaternCovariance - this should'nt happen 2")
    p = ncol(data.y)
    if (length(data.x) != p)
      stop("data.x should be of the same length of the no. of cols of data.y")
    num = array(0, c(1, nrow(data.y), p))
    den = array(0, c(1, nrow(data.y), p))
    for (k in 1:p) {
      if (is.null(rngs))
        rng.k = NULL
      else rng.k = rngs[k]
      out.gow = gower.fcn.test(x = data.x[, k], y = data.y[, k], rng = rng.k, KR.corr = KR.corr)
      num[, , k] = out.gow$dist * out.gow$delta
      den[, , k] = out.gow$delta
    }
    out = apply(num, c(1, 2), sum, na.rm = TRUE)/apply(den, c(1, 2), sum, na.rm = TRUE)
  }
  else {
    p = ncol(data.y)
    if (ncol(data.x) != p)
      stop("data.x and data.y must have the same no. of cols")
    num = array(0, c(nrow(data.x), nrow(data.y), p))
    den = array(0, c(nrow(data.x), nrow(data.y), p))
    prod.kern = array(0, c(nrow(data.x), nrow(data.y), p))
    for (k in 1:p) {
      if (is.null(rngs))
        rng.k = NULL
      else rng.k = rngs[k]
      out.gow = gower.fcn.test(theta.k = theta[k], x = data.x[, k], y = data.y[, k],
        rng = rng.k, KR.corr = KR.corr)
      num[, , k] = out.gow$dist * out.gow$delta
      den[, , k] = out.gow$delta
      prod.kern[, , k] = out.gow$prod.kern
    }
    result = apply(prod.kern, c(1, 2), prod, na.rm = TRUE)
  }
  result
}


calcGowerLocalUncertainty = function(traindata, y, newdata, ...) {
  n = nrow(traindata)
  R = calcGowerCovMat(data.x = traindata, ...)
  rs = calcGowerCovMat(data.x = traindata, data.y = newdata, ...) # n x m
  o = rep(1, n)
  print(y)
  a1 = solve(R, y)                 # R^-1 y
  a2 = solve(R, o)                 # R^-1 1
  a3 = crossprod(o, a2)            # 1^T R^-1 1
  mu = crossprod(o, a1) / a3       # 1^T R^-1 y / 1^T R^-1 1
  a4 = y - mu * o                  # y - mu * 1
  a5 = solve(R, a4)                # R^-1 (y - mu * 1)
  sigma = crossprod(a4, a5) / n    # (y - mu * 1)^T R^-1 (y - mu * 1) / n
  bs = solve(R, rs)
  crossprods1 = colSums(rs * bs)   # crossprods between cols rs[,i] and bs[,i]
  crossprods2 = colSums(bs)        # crossprods between bs[,i] and 1 vec

  # (1 - r^T R^-1 r + (1 - 1^T R^-1 r)^2 / 1^T R^-1 1) * sigma
  s = sigma * (1 - crossprods1 + (1 - crossprods2)^2 / a3)
  return(s)
}

# set.seed(1L)
# # td = generateRandomDesign(2, ps)
# # nd = generateRandomDesign(3, ps) # this drops levels, fix now!!!!
# # y = c(0, 1)
# # levels(nd$b) = levels(td$b)
# # print(td)
# # print(nd)
# # s = calcGowerLocalUncertainty(td, y, nd, par.set = ps)
# # print(s)

# f = function(x) x^2

# library(ParamHelpers)
# library(checkmate)
# ps = makeParamSet(
#   # makeNumericParam("a", lower = 1, upper = 3),
#   # makeDiscreteParam("b", values = c("v", "w"))
#   makeNumericParam("x1", lower = -5, upper = 5)
#   # makeDiscreteVectorParam("d", len = 2L, values = c("v", "w"))
# )

# # data.x = generateRandomDesign(2, ps)
# # K = calcGowerCovMat(data.x = data.x, par.set = ps, dist2cov = "matern3_2")
# # print(K)

# x1 = -5; x2 = 5
# xseq.train = seq(x1, x2, length.out = 5)
# xseq.test = seq(x1, x2, length.out = 1000)
# traindata = data.frame(x1 = xseq.train)
# y.train = f(xseq.train)
# y.test = f(xseq.test)
# newdata = data.frame(x1 = xseq.test)

# s = calcGowerLocalUncertainty(traindata, y.train, newdata, par.set = ps, theta = c(0.01))

# library(ggplot2)

# ggdat = data.frame(
#   x = xseq.test,
#   y = f(xseq.test),
#   yse = f(xseq.test) + s
# )
# pl = ggplot(ggdat)
# pl = pl + geom_line(mapping = aes(x = x, y = y))
# pl = pl + geom_line(mapping = aes(x = x, y = yse))
# print(pl)




