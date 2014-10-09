context("exampleRunMulticrit")

test_that("exampleRunMulticrit", {
  
  doRun = function(fun, par.set, method, crit, prop.points, indicator = "sms", learner = "regr.km") {
    learner = makeLearner(learner, predict.type = "se")
    control = makeMBOControl(init.design.points = 10, iters = 2L, propose.points = prop.points,
      number.of.targets = 2L)
    if (method == "mspot")
      control = setMBOControlInfill(control, crit = crit, opt = "nsga2", opt.nsga2.popsize = 20L,
        opt.nsga2.generations = 2L)
    else
      control = setMBOControlInfill(control, crit = crit, opt = "focussearch", opt.focussearch.points = 10L)
    control = setMBOControlMultiCrit(control, method = method, dib.indicator = indicator)
    
    run = exampleRunMultiCrit(fun, par.set, learner, control, points.per.dim = 4L,
      nsga2.args = list(popsize = 12L, generations = 10L), ref.point = c(11, 11))
    return(autoplot.MBOExampleRunMultiCrit(run, pause = FALSE))
  }
  
  
  fun1 = makeMBOFunction(mco::zdt1)
  par.set1 = makeNumericParamSet("x", lower = 0, upper = 1, len = 2L)
  
  p1 = doRun(fun1, par.set1, method = "parego", crit = "ei", prop.points = 1L)
  # FIXME: Fix p2 an dp4
  #p2 = doRun(fun1, par.set1, method = "parego", crit = "ei", prop.points = 5L)
  p3 = doRun(fun1, par.set1, method = "parego", crit = "lcb", prop.points = 1L)
  #p4 = doRun(fun1, par.set1, method = "parego", crit = "lcb", prop.points = 5L)
  p5 = doRun(fun1, par.set1, method = "mspot", crit = "ei", prop.points = 1L)
  p6 = doRun(fun1, par.set1, method = "mspot", crit = "ei", prop.points = 5L)
  p7 = doRun(fun1, par.set1, method = "mspot", crit = "lcb", prop.points = 1L)
  p8 = doRun(fun1, par.set1, method = "mspot", crit = "lcb", prop.points = 5L)
  p9 = doRun(fun1, par.set1, method = "dib", crit = "dib", prop.points = 1L)
  # FIXME: not supported yet
#  p10 = doRun(fun1, par.set1, method = "dib", crit = "dib", prop.points = 5L)
  p11 = doRun(fun1, par.set1, method = "dib", crit = "dib", prop.points = 1L, indicator = "eps")
#  p12 = doRun(fun1, par.set1, method = "dib", crit = "dib", prop.points = 5L, indicator = "eps)
})