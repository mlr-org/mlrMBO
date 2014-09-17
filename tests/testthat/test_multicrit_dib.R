context("multicrit: dib")

test_that("multicrit dib works", {
  f = makeMBOFunction(function(x) x^2)
  f2 = makeMBOFunction(function(x) c(1, -1) * x^2)
  ps = makeNumericParamSet(len = 2L, lower = -2, upper = 1)

  # Test normal run
  learner = makeLearner("regr.km", nugget.estim = TRUE, predict.type = "se")
  ctrl = makeMBOControl(iters = 5L, number.of.targets = 2L, init.design.points = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = "dib", opt.focussearch.points = 10L,
    opt.focussearch.maxit = 5L)
  ctrl = setMBOControlMultiCrit(ctrl, method = "dib")
  or = mbo(f, ps, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))

  # reference point:
  expect_error(setMBOControlMultiCrit(ctrl, method = "dib", ref.point.method = "const"))
  ctrl = setMBOControlMultiCrit(ctrl, method = "dib", ref.point.method = "const", ref.point.val = c(11, 11))
  or = mbo(f, ps, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))
  ctrl = setMBOControlMultiCrit(ctrl, method = "dib", ref.point.method = "all", ref.point.offset = 2)
  or = mbo(f, ps, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))
  ctrl = setMBOControlMultiCrit(ctrl, method = "dib", ref.point.method = "front")
  or = mbo(f, ps, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))


  ctrl = setMBOControlMultiCrit(ctrl, method = "dib", dib.indicator = "eps")
  ctrl = setMBOControlInfill(ctrl, crit = "dib", opt.focussearch.points = 10)
  or = mbo(f, ps, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))
})

