context("plot MBO Result")

test_that("plot single crit", {
  f = makeSingleObjectiveFunction(
    fn = function(x) crossprod(x),
    par.set = makeNumericParamSet(len = 5L, lower = -1, upper = 1)
  )
  learner = makeLearner("regr.km", predict.type = "se")
  ctrl = makeMBOControl(iters = 2L, init.design.points = 8L, propose.points = 1L)
  ctrl = setMBOControlInfill(ctrl, crit = "ei", opt.focussearch.points = 100L,
    opt.focussearch.maxit = 3L)
  or = mbo(f, learner = learner, control = ctrl)

  plot(or, iters = 0:2, pause = FALSE)
})

test_that("plot multi crit", {
  f = smoof::makeZDT1Function(2L)
  learner = makeLearner("regr.km", predict.type = "se")
  ctrl = makeMBOControl(iters = 2L, init.design.points = 8L, propose.points = 2L,
    number.of.targets = 2L)
  ctrl = setMBOControlInfill(ctrl, crit = "ei", opt.focussearch.points = 100L,
    opt.focussearch.maxit = 3L, opt = "nsga2")
  ctrl = setMBOControlMultiCrit(ctrl, method = "mspot")
  or = mbo(f, learner = learner, control = ctrl)

  plot(or, iters = 0:2, pause  = FALSE)
})
