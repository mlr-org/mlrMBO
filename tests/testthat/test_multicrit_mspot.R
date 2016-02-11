context("multicrit: mspot")

test_that("multicrit mspot works", {
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
  ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "nsga2", opt.nsga2.generations = 1L, opt.nsga2.popsize = 12L)
  ctrl = setMBOControlMultiCrit(ctrl, method = "mspot")
  or = mbo(f, learner = learner, control = ctrl)
  op = as.data.frame(or$opt.path)
  expect_true(all(is.na(op$ei.y_1[1:5])))
  expect_true(all(is.na(op$ei.y_2[1:5])))
  expect_true(all(!is.na(op$ei.y_1[6:10])))
  expect_true(all(!is.na(op$ei.y_2[6:10])))
  expect_true(!any(is.na(or$pareto.front)))
})
