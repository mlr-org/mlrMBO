context("multicrit: dib")

test_that("multicrit dib works", {
  # Test normal run
  learner = makeLearner("regr.km", nugget.estim = TRUE, predict.type = "se")
  ctrl = makeMBOControl(n.objectives = 2L)
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = "dib", opt.focussearch.points = 10L, opt.focussearch.maxit = 5L)
  ctrl = setMBOControlMultiCrit(ctrl, method = "dib")
  or = mbo(testfmco1, testdesmco1, learner = learner, control = ctrl)
  expect_matrix(or$pareto.front, mode = "numeric", any.missing = FALSE)
  expect_output(print(or), "Optimization path")

  # reference point:
  expect_error(setMBOControlMultiCrit(ctrl, method = "dib", ref.point.method = "const"))
  ctrl = setMBOControlMultiCrit(ctrl, method = "dib", ref.point.method = "const", ref.point.val = c(11, 11))
  or = mbo(testfmco1, testdesmco1, learner = learner, control = ctrl)
  expect_matrix(or$pareto.front, mode = "numeric", any.missing = FALSE)
  ctrl = setMBOControlMultiCrit(ctrl, method = "dib", ref.point.method = "all", ref.point.offset = 2)
  or = mbo(testfmco1, testdesmco1, learner = learner, control = ctrl)
  expect_matrix(or$pareto.front, mode = "numeric", any.missing = FALSE)
  ctrl = setMBOControlMultiCrit(ctrl, method = "dib", ref.point.method = "front")
  or = mbo(testfmco1, testdesmco1, learner = learner, control = ctrl)
  expect_matrix(or$pareto.front, mode = "numeric", any.missing = FALSE)

  ctrl = setMBOControlMultiCrit(ctrl, method = "dib", dib.indicator = "eps")
  ctrl = setMBOControlInfill(ctrl, crit = "dib", opt.focussearch.points = 10)
  or = mbo(testfmco1, testdesmco1, learner = learner, control = ctrl)
  expect_matrix(or$pareto.front, mode = "numeric", any.missing = FALSE)
})
