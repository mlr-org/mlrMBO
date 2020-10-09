context("multi-objective: mspot")

test_that("multi-objective mspot works", {
  # Test normal run
  learner = makeLearner("regr.km", nugget.estim = TRUE, predict.type = "se")
  ctrl = makeMBOControl(n.objectives = 2L)
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.ei, opt = "nsga2", opt.nsga2.generations = 1L, opt.nsga2.popsize = 12L)
  ctrl = setMBOControlMultiObj(ctrl, method = "mspot")
  or = mbo(testf.zdt1.2d, testd.zdt1.2d, learner = learner, control = ctrl)
  expect_output(print(or), "Optimization path")
  op = as.data.frame(or$opt.path)
  k = seq_row(testd.zdt1.2d)
  expect_true(all(is.na(op$ei.y_1[k])))
  expect_true(all(is.na(op$ei.y_2[k])))
  expect_numeric(op$ei.y_1[-k], any.missing = FALSE)
  expect_numeric(op$ei.y_2[-k], any.missing = FALSE)
  expect_matrix(or$pareto.front, mode = "numeric", any.missing = FALSE)
})
