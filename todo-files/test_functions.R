# in the following we assume:
# - the signature of all testfunction is (lv, x)
# - x is a real value (dim = 1)
# - lv is the multifid param, from [0, 1] here
# - we pre-specify the levels as a finite subset from [0, 1] later, to define the discrete level points

##### NORMAL UNIVARIATE TESTFUNCTIONS #####
# NB: we already add "lv" to signature, but it has nor effect here!

# Hartman 3 (Hartman 1973)
# https://archive.org/stream/someexperimentsi00hart#page/10/mode/2up
# d : X-dimension
# x* = (0.114, 0.556, 0.852), f* = -3.8627 (global minimum for d = 3)
# region of interest is [0, 15]^d
hartman = function(lv, x, d = 1) {
  x.opt = c(0.114, 0.556, 0.852)
  missing = setdiff(1:3, seq_len(d))
  x[missing] = x.opt[missing]
  a = matrix(c(3, 0.1, 3, 0.1, 10, 10, 10, 10, 30, 35, 30, 35), nrow = 4)
  c = c(1, 1.2, 3, 3.2)
  p = matrix(c(0.3689, 0.4699, 0.1091, 0.03815, 0.1170, 0.4387, 0.8732, 0.5743, 0.2673, 0.7470, 0.5547, 0.8828), nrow = 4)
  res = sapply(seq_along(c), function(i) {
    expon = sapply(seq_along(x), function(j) a[i,j] * (x[j] - p[i,j])^2)
    -1 * c[i] * exp(-1 * sum(expon))
  })
  sum(res)
}

hartman2d = function(lv, x) hartman(lv, x, d=2)

hartman1d = lapply(1:3, function(s) {
  force(s)
  function(lv, x) {
    xs = c(0.114, 0.556, 0.852)
    xs[s] = x
    hartman(lv, xs, d = 3)
  }
})

hartman3d = function(lv, x) hartman(lv, x, d=3)

# Sasena (2002)
# Sasena, M.J. (2002), Flexibility and Efficiency Enhancements for Constrained Global Design Optimization
#   with Kriging Approximations, Ph. D. dissertation, University of Michigan.
# only defined for X-dim = 1
# region of interest is [0, 10]
sasena = function(lv, x) {
  - sin(x) - exp(x / 100) + 10
}


##### COMBINATION OPERATIONS #####

# add 2 testfunctions to get a new one of the same form
# examples: a) add a shifting distortion in Y or b) add Gaussian noise
addDistortion = function(f, g, ...) {
  function(lv, x) f(lv, x) + g(lv, x, ...)
}

# given, f and g, does h(lv, x) = f(g(x, lv), lv)
distortX = function(f, g, ...) {
  function(lv, x) f(lv ,g(lv, x, ...))
}

##### COMBINATION OPERANDS #####

# can be used to add gaussian noise to a testfun
noiseGaussian = function(lv, x, sd.fac = 0.1){
  rnorm(1, mean = (1 - lv), sd = sd.fac * 1/lv)
}

# adds a y-shift to a testfunction
yshift = function(lv, x, fac = 1) {
  lv = (1 - lv) * fac
  lv + lv/10 * (x - lv * 10)^2
}

yupp = function(lv, x, fac = 1) {
  fac * (1 - lv)
}

#FIXME ????
# adds a y-shift to a testfunction
xshift = function(lv, x, direction = 2) {
  x - ((1-lv) * direction)
}

##### PLOT A FUNCTION FAMILY #####
# pass: fun(lv, x), vector of lv-values to define family, [x1, x2} interval to plot
plotTestfunFamily = function(f, lvs, x1, x2) {
  requirePackages("ggplot2", why = "plotTestfunFamily")
  xseq = seq(x1, x2, length.out = 100)
  # dataframe for each lv
  data = lapply(lvs, function(lv) {
    y = vnapply(xseq, f, lv = lv)
    data.frame(x = xseq, y = y, lv = lv)
  })
  data = do.call(rbind, data)
  data$lv = as.factor(data$lv)
  pl = ggplot(data = data, mapping = aes_string(x = "x", y = "y", col = "lv"))
  pl = pl + geom_line()
  return(pl)
}

##### CONVERT TEST FUNCTION TO MBO OBJECTIVE FUNCTION #####
# we need to ensure that the signature is now f(x), where x is a list, which contains the lvl param
makeMBOMultifidFunction = function(f, lvls) {
  force(f)
  force(lvls)
  function(x) {
    #put all other x to one vector
    lv = lvls[x$.multifid.lvl]
    x = unlist(dropNamed(x, ".multifid.lvl"))
    f(lv = lv, x = x)
  }
}

##### EXAMPLE CALLS #####
# should be commented out

# f = addDistortion(sasena, yshift)
# f = distortX(sasena, xshift)
# print(plotTestfunFamily(f, lvs = c(0, 0.1, 0.5, 1), x1 = 0, x2 = 10))
