context("multi-objective: dib")

test_that("multi-objective dib works", {
  # Test normal run
  learner = makeLearner("regr.km", nugget.estim = TRUE, predict.type = "se")
  ctrl = makeMBOControl(n.objectives = 2L)
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.dib1,
    opt.focussearch.points = 10L, opt.focussearch.maxit = 5L)
  ctrl = setMBOControlMultiObj(ctrl, method = "dib")
  or = mbo(testfmco1, testdesmco1, learner = learner, control = ctrl)
  expect_matrix(or$pareto.front, mode = "numeric", any.missing = FALSE)
  expect_output(print(or), "Optimization path")

  # reference point:
  expect_error(setMBOControlMultiObj(ctrl, method = "dib", ref.point.method = "const"))
  ctrl = setMBOControlMultiObj(ctrl, method = "dib", ref.point.method = "const", ref.point.val = c(11, 11))
  or = mbo(testfmco1, testdesmco1, learner = learner, control = ctrl)
  expect_matrix(or$pareto.front, mode = "numeric", any.missing = FALSE)
  ctrl = setMBOControlMultiObj(ctrl, method = "dib", ref.point.method = "all", ref.point.offset = 2)
  or = mbo(testfmco1, testdesmco1, learner = learner, control = ctrl)
  expect_matrix(or$pareto.front, mode = "numeric", any.missing = FALSE)
  ctrl = setMBOControlMultiObj(ctrl, method = "dib", ref.point.method = "front")
  or = mbo(testfmco1, testdesmco1, learner = learner, control = ctrl)
  expect_matrix(or$pareto.front, mode = "numeric", any.missing = FALSE)

  ctrl = setMBOControlMultiObj(ctrl, method = "dib", dib.indicator = "eps")
  ctrl = setMBOControlInfill(ctrl, crit = crit.dib1, opt.focussearch.points = 10)
  or = mbo(testfmco1, testdesmco1, learner = learner, control = ctrl)
  expect_matrix(or$pareto.front, mode = "numeric", any.missing = FALSE)
})
