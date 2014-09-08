context("multicrit: sms")

test_that("multicrit sms works", {
  f = makeMBOFunction(function(x) x^2)
  f2 = makeMBOFunction(function(x) c(1, -1) * x^2)
  ps = makeNumericParamSet(len = 2L, lower = -2, upper = 1)

  # Test normal run
  learner = makeLearner("regr.km", nugget.estim = TRUE, predict.type = "se")
  ctrl = makeMBOControl(iters = 5, number.of.targets = 2L, init.design.points = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = "sms", opt.focussearch.points = 10)
  ctrl = setMBOControlMultiCrit(ctrl, method = "sms")
  or = mbo(f, ps, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))
})

