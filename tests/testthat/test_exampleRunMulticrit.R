context("exampleRunMulticrit")

test_that("exampleRunMulticrit", {

  doRun = function(fun, par.set, method, crit, prop.points, indicator = "sms") {
    # set nugget effect to small value for num stability in this unit test
    learner = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", nugget = 0.001)
    control = makeMBOControl(init.design.points = 10, iters = 1L, propose.points = prop.points,
      number.of.targets = 2L)
    if (method == "mspot")
      control = setMBOControlInfill(control, crit = crit, opt = "nsga2", opt.nsga2.popsize = 4L,
        opt.nsga2.generations = 1L)
    else
      control = setMBOControlInfill(control, crit = crit, opt = "focussearch", opt.restarts = 1L,
        opt.focussearch.points = 10L, opt.focussearch.maxit = 1L)
    control = setMBOControlMultiCrit(control, method = method, dib.indicator = indicator)

    run = exampleRunMultiCrit(fun, par.set, learner, control, points.per.dim = 4L,
      nsga2.args = list(popsize = 4L, generations = 2L), ref.point = c(11, 11))
    res = renderExampleRunPlot(run, iter = 1L)
  }

  fun1 = makeMBOFunction(mco::zdt1)
  par.set1 = makeNumericParamSet("x", lower = 0, upper = 1, len = 2L)

  doRun(fun1, par.set1, method = "parego", crit = "ei", prop.points = 1L)
  doRun(fun1, par.set1, method = "parego", crit = "ei", prop.points = 2L)
  doRun(fun1, par.set1, method = "parego", crit = "lcb", prop.points = 1L)
  doRun(fun1, par.set1, method = "parego", crit = "lcb", prop.points = 2L)
  doRun(fun1, par.set1, method = "mspot", crit = "ei", prop.points = 1L)
  doRun(fun1, par.set1, method = "mspot", crit = "ei", prop.points = 2L)
  doRun(fun1, par.set1, method = "mspot", crit = "lcb", prop.points = 1L)
  doRun(fun1, par.set1, method = "mspot", crit = "lcb", prop.points = 2L)
  doRun(fun1, par.set1, method = "dib", crit = "dib", prop.points = 1L)
  doRun(fun1, par.set1, method = "dib", crit = "dib", prop.points = 2L)
  doRun(fun1, par.set1, method = "dib", crit = "dib", prop.points = 1L, indicator = "eps")
  doRun(fun1, par.set1, method = "dib", crit = "dib", prop.points = 2L, indicator = "eps")
})
