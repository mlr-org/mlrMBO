context("exampleRunMulticrit")

test_that("exampleRunMulticrit", {

  doRun = function(method, crit, prop.points, indicator = "sms") {
    # set nugget effect to small value for num stability in this unit test
    learner = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", nugget = 0.001)
    control = makeMBOControl(n.objectives = 2L,
      propose.points = prop.points)
    control = setMBOControlTermination(control, iters = 1L)
    if (method == "mspot")
      control = setMBOControlInfill(control, crit = crit, opt = "nsga2", opt.nsga2.popsize = 4L,
        opt.nsga2.generations = 1L)
    else
      control = setMBOControlInfill(control, crit = crit, opt = "focussearch", opt.restarts = 1L,
        opt.focussearch.points = 10L, opt.focussearch.maxit = 1L)
    control = setMBOControlMultiCrit(control, method = method, dib.indicator = indicator)

    run = exampleRunMultiCrit(testf.zdt1.2d, testd.zdt1.2d, learner = learner, control = control, points.per.dim = 4L,
      nsga2.args = list(popsize = 4L, generations = 2L))
    res = renderExampleRunPlot(run, iter = 1L)
  }

  doRun(method = "parego", crit = "ei", prop.points = 1L)
  doRun(method = "parego", crit = "ei", prop.points = 2L)
  doRun(method = "parego", crit = "cb", prop.points = 1L)
  doRun(method = "parego", crit = "cb", prop.points = 2L)
  doRun(method = "mspot", crit = "ei", prop.points = 1L)
  doRun(method = "mspot", crit = "ei", prop.points = 2L)
  doRun(method = "mspot", crit = "cb", prop.points = 1L)
  doRun(method = "mspot", crit = "cb", prop.points = 2L)
  doRun(method = "dib", crit = "dib", prop.points = 1L)
  doRun(method = "dib", crit = "dib", prop.points = 2L)
  doRun(method = "dib", crit = "dib", prop.points = 1L, indicator = "eps")
  doRun(method = "dib", crit = "dib", prop.points = 2L, indicator = "eps")
})
