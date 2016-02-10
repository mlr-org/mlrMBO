context("multicrit: dib")

test_that("multicrit dib works", {
  par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1)
  f = makeMultiObjectiveFunction(
    fn = function(x) x^2,
    par.set = par.set,
    n.objectives = 2L
  )
  f2 = makeMultiObjectiveFunction(
    fn = function(x) c(1, -1) * x^2,
    par.set = par.set,
    n.objectives = 2L
  )

  # Test normal run
  learner = makeLearner("regr.km", nugget.estim = TRUE, predict.type = "se")
  ctrl = makeMBOControl(init.design.points = 5L, number.of.targets = 2L)
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = "dib", opt.focussearch.points = 10L,
    opt.focussearch.maxit = 5L)
  ctrl = setMBOControlMultiCrit(ctrl, method = "dib")
  or = mbo(f, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))

  # reference point:
  expect_error(setMBOControlMultiCrit(ctrl, method = "dib", ref.point.method = "const"))
  ctrl = setMBOControlMultiCrit(ctrl, method = "dib", ref.point.method = "const", ref.point.val = c(11, 11))
  or = mbo(f, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))
  ctrl = setMBOControlMultiCrit(ctrl, method = "dib", ref.point.method = "all", ref.point.offset = 2)
  or = mbo(f, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))
  ctrl = setMBOControlMultiCrit(ctrl, method = "dib", ref.point.method = "front")
  or = mbo(f, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))


  ctrl = setMBOControlMultiCrit(ctrl, method = "dib", dib.indicator = "eps")
  ctrl = setMBOControlInfill(ctrl, crit = "dib", opt.focussearch.points = 10)
  or = mbo(f, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))
})

