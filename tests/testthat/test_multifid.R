context("multifid")

test_that("basic multifid works", {
  set.seed(1)
  objfun = function(x) {
    lvl.par.val = x$.multifid.lvl
    x = x$x
    assertNumeric(x, len = 1, lower = 0, upper = 10)
    3 - lvl.par.val + 0.5*x 
  }

  par.set = makeParamSet(
    makeNumericParam("x", lower = 0, upper = 10)
  )

  control = makeMBOControl(
    init.design.points = 9L,
    init.design.fun = maximinLHS,
    iters = 5L,
    on.learner.error = "stop",
    show.learner.output = FALSE,
  )
  control = setMBOControlInfill(control = control, 
                                crit = "ei",
                                opt = "focussearch",
                                opt.restarts = 1L,
                                opt.focussearch.maxit = 1L,
                                opt.focussearch.points = 10L,
                                filter.proposed.points = TRUE,
                                filter.proposed.points.tol = 0.01
  )
  control = setMBOControlMultiFid(control = control,
                                  param = "dw.perc",
                                  costs = 1:3,
                                  lvls = c(0.1, 0.5, 1),
                                  cor.grid.points = 40L)

  surrogat.learner = makeLearner("regr.lm", predict.type = "se")
  result = mbo(fun = objfun, par.set = par.set, learner = surrogat.learner, control = control)
  expect_true(result$y < 0.5)
  op.df = as.data.frame(result$opt.path)

  ### remove costs so time.model will be estimated
  control$multifid.costs = NULL
  objfun.delay = function(x) {
    Sys.sleep(0.1 + x$.multifid.lvl/3)
    objfun(x)
  }
  set.seed(1)
  result.time = mbo(fun = objfun.delay, par.set = par.set, learner = surrogat.learner, control = control)
  
  #this is pretty hard and migh fail?
  op.df.time = as.data.frame(result.time$opt.path)
  expect_equal(op.df.time$.multifid.lvl, op.df$.multifid.lvl)
})
