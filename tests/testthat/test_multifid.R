context("mutlifid")

test_that("essential mutlifid works", {
  
  # helper functions to generate a test case with multiple levels

  sasena = function(x) {
  # Sasena (2002)
  # Sasena, M.J. (2002), Flexibility and Efficiency Enhancements for Constrained Global Design Optimization with Kriging Approximations, Ph. D. dissertation, University of Michigan.
  if(is.list(x))
    x = unlist(x)
  assertNumeric(x, len = 1, lower = 0, upper = 10)
  res = -1 * sin(x) - exp(x/100) + 10
  res
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

  # generate the test case

  objfun = makeAddFunction(fun=bakeFunction(sasena), addfun=uppMove, fac = 1)

  par.set = makeParamSet(
    makeNumericParam("x", lower = 0, upper = 10),
    makeNumericParam("dw.perc", lower=0, upper=1)
  )

  control = makeMBOControl(
    init.design.points = 9L, #distributed over the different levels, seems not to work for <5 each
    init.design.fun = maximinLHS,
    iters = 3L,
    on.learner.error = "stop",
    show.learner.output = FALSE,
  )
  control = setMBOControlInfill(control = control, 
                                crit = "multiFid", 
                                opt = "focussearch", 
                                opt.restarts = 1L, 
                                opt.focussearch.maxit = 1L, 
                                opt.focussearch.points = 20L)
  control = setMBOControlMultiFid(control = control, 
                                  param = "dw.perc", 
                                  lvls = c(0.1, 0.5, 1),
                                  costs = function(cur, last) (last / cur)^0.5,
                                  cor.grid.points = 40L)
  surrogat.learner = makeLearner("regr.km", predict.type="se", nugget.estim = TRUE, jitter = TRUE)
  result = mbo(fun = objfun, par.set = par.set, learner = surrogat.learner, control = control, show.info = FALSE)
  expect_true(inherits(result, "MultiFidResult"))
})

