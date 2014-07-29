hartman = function(x, d = 1){
  # Hartman 3 (Hartman 1973)
  # https://archive.org/stream/someexperimentsi00hart#page/10/mode/2up
  if(is.list(x))
    x = unlist(x)
  assertInt(d, lower = 1L, upper = 3L)
  assertNumeric(x, len = d)
  #define for d=3
  a = matrix(c(3, 0.1, 3, 0.1, 10, 10, 10, 10, 30, 35, 30, 35), nrow = 4)
  c = c(1, 1.2, 3, 3.2)
  p = matrix(c(0.3689, 0.4699, 0.1091, 0.03815, 0.1170, 0.4387, 0.8732, 0.5743, 0.2673, 0.7470, 0.5547, 0.8828), nrow = 4)
  #reduce dimensions
  a = a[, seq_len(d), drop = F]
  c = c[seq_len(d)]
  p = p[seq_len(d), , drop = F]
  res = sapply(seq_along(c), function(i) {
    expon = sapply(seq_along(x), function(j) a[i,j] * (x[j] - p[i,j])^2)
    -1 * c[i] * exp(-1 * sum(expon))
  })
  sum(res)
  # x* = (0.114, 0.556, 0.852), f* = -3.8627
}

sasena = function(x) {
  # Sasena (2002)
  # Sasena, M.J. (2002), Flexibility and Efficiency Enhancements for Constrained Global Design Optimization with Kriging Approximations, Ph. D. dissertation, University of Michigan.
  if(is.list(x))
    x = unlist(x)
  assertNumeric(x, len = 1, lower = 0, upper = 10)
  res = -1 * sin(x) - exp(x/100) + 10
  res
}

bakeFunction = function(fun, lvl.par = "dw.perc", ...){
  force(fun)
  force(lvl.par)
  args = list(...)
  function(lvl.par.val, x) {
    lvl.ind = which(names(x) == lvl.par)
    x2 = x[-lvl.ind]
    if(is.list(x2))
      unlist(x2)
    do.call(fun, c(list(x2), args))
  }
}
  
rnormNoise = function(lvl.par.val, x, sd.fac = 0.1){
  if(is.list(x))
    x = unlist(x)
  rnorm(1, mean = (1 - lvl.par.val), sd = sd.fac * 1/lvl.par.val)
}

uppMove = function(lvl.par.val, x, fac = 1) {
  if(is.list(x))
    x = unlist(x)
  lvl.par.val = (1-lvl.par.val) * fac
  lvl.par.val + lvl.par.val/10 * (x - lvl.par.val * 10)^2
}

makeAddFunction = function(fun, addfun = rnormNoise, ...){
  force(fun)
  force(addfun)
  pars = list(...)
  function(x, lvl.par = "dw.perc", ...){
    lvl.ind = which(names(x) == lvl.par)
    lvl.par.val = x[[lvl.ind]]
    x2 = x[-lvl.ind]
    add = do.call(addfun, c(list(lvl.par.val = lvl.par.val, x = x2), pars))
    fun(x = x, lvl.par = lvl.par, ...) + add
  }
}

linShift = function(lvl.par.val, x, direction = 2) {
  if(is.list(x))
    x = unlist(x)
  x - ((1-lvl.par.val) * direction)
}

makeShiftFunction = function(fun, shiftfun = linShift, ...) {
  force(fun)
  force(shiftfun)
  pars = list(...)
  function(x, lvl.par = "dw.perc", ...) {
    lvl.ind = which(names(x) == lvl.par)
    lvl.par.val = x[[lvl.ind]]
    x2 = x[-lvl.ind]
    shifted.x = do.call(shiftfun, c(list(lvl.par.val = lvl.par.val, x = x2), pars))
    x = c(shifted.x, x[lvl.ind])
    res = fun(x = x, lvl.par = lvl.par, ...)
  }
}

x = seq(0,10,by=0.1)
sp = makeAddFunction(fun=bakeFunction(sasena), addfun=uppMove)
y1 = sapply(x, function(x.this) sp(list(x.this, dw.perc = 1)))
y2 = sapply(x, function(x.this) sp(list(x.this, dw.perc = 0.7)))
plot(x,y1, type="l")
lines(x,y2, lty = 2)
# hartNoise = makeNoiseFunction(bakeFunction(hartman), sd.fac = 0.005)
# hartShift = makeShiftFunction(bakeFunction(hartman), direction = 1)
# hartNS = makeShiftFunction(hartNoise, direction = 1)
# y = sapply(x, function(x.this) hartNoise(list(x.this, dw.perc=0.6)))
# y = sapply(x, function(x.this) hartNS(list(x.this, dw.perc=0.9)))

