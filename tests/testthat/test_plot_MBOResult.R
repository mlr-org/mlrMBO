context("plot MBO Result")

test_that("plot single-objective", {
  f = makeSingleObjectiveFunction(
    fn = function(x) crossprod(x),
    par.set = makeNumericParamSet(len = 5L, lower = -1, upper = 1)
  )
  learner = makeLearner("regr.km", predict.type = "se")
  des = generateTestDesign(8L, getParamSet(f))
  ctrl = makeMBOControl(propose.points = 1L)
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.ei, opt.focussearch.points = 100L,
    opt.focussearch.maxit = 3L)
  or = mbo(f, des, learner = learner, control = ctrl)

  plot(or, iters = 0:2, pause = FALSE)
})

test_that("plot multi-objective", {
  f = smoof::makeZDT1Function(2L)
  learner = makeLearner("regr.km", predict.type = "se")
  des = generateTestDesign(8L, getParamSet(f))
  ctrl = makeMBOControl(propose.points = 2L, n.objectives = 2L)
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.ei, opt.focussearch.points = 100L,
    opt.focussearch.maxit = 3L, opt = "nsga2")
  ctrl = setMBOControlMultiObj(ctrl, method = "mspot")
  or = mbo(f, des, learner = learner, control = ctrl)

  plot(or, iters = 0:2, pause = FALSE)
})
