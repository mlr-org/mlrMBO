context("multicrit: parego")

test_that("mbo parego works", {
  f = makeMultiObjectiveFunction(
    fn = function(x) x^2,
    par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1),
    n.objectives = 2L
  )
  f2 = makeMultiObjectiveFunction(
    fn = function(x) c(1, -1) * x^2,
    par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1),
    n.objectives = 2L,
    minimize = c(TRUE, FALSE)
  )

  # Test normal run
  learner = makeLearner("regr.km", nugget.estim = TRUE)
  ctrl = makeMBOControl(init.design.points = 5L, number.of.targets = 2L)
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10)
  ctrl = setMBOControlMultiCrit(ctrl, method = "parego", parego.s = 100)
  or = mbo(f, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))

  # Test with initial design
  des = generateDesign(10, smoof::getParamSet(f))
  des = cbind(des, t(apply(des, 1, f)))
  names(des)[3:4] = c("y_1", "y_2")
  or = mbo(f, design = des, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))

  # Test margin points
  ctrl = makeMBOControl(number.of.targets = 2, propose.points = 2L, init.design.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 5)
  ctrl = setMBOControlMultiCrit(ctrl, method = "parego", parego.s = 100, parego.use.margin.points = c(TRUE, TRUE))
  or = mbo(f, learner = learner, control = ctrl)
  w = as.data.frame(or$opt.path)[-(1:5), c(".weight1", ".weight2")]
  expect_true(all(w == 0 | w == 1))
  expect_true(all(1 - w[, 1] == w[, 2]))

  # Test margin points
  ctrl = makeMBOControl(number.of.targets = 2, propose.points = 2L, init.design.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 5)
  ctrl = setMBOControlMultiCrit(ctrl, method = "parego", parego.s = 100, parego.use.margin.points = c(TRUE, TRUE))
  or = mbo(f, learner = learner, control = ctrl)
  w = as.data.frame(or$opt.path)[-(1:5), c(".weight1", ".weight2")]
  expect_true(all(w == 0 | w == 1))
  expect_true(all(1 - w[, 1] == w[, 2]))

  # Test wrong dimension
  ctrl = makeMBOControl(number.of.targets = 3, init.design.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10L)
  ctrl = setMBOControlMultiCrit(ctrl, method = "parego", parego.s = 100)
  expect_error(mbo(f, learner = learner, control = ctrl), "Objective function has")

  # Test multippoint
  ctrl = makeMBOControl(number.of.targets = 2, propose.points = 5L, init.design.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10L)
  ctrl = setMBOControlMultiCrit(ctrl, method = "parego", parego.s = 100)
  or = mbo(f, learner = learner, control = ctrl)

  # check used weights
  w = as.data.frame(or$opt.path)[, c(".weight1", ".weight2")]
  expect_true(all(rowSums(w[-(1:5),]) == 1))
  expect_false(any(duplicated(w[-(1:5), 1])))
  expect_false(any(duplicated(w[-c(1:5), 2])))

  # Multipoint with minimization and maximization
  ctrl = makeMBOControl(number.of.targets = 2, propose.points = 5L, init.design.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10L)
  ctrl = setMBOControlMultiCrit(ctrl, method = "parego", parego.s = 100)
  or = mbo(f2, learner = learner, control = ctrl)
  # make sure the pareto.front is a matrix
  # FIXME: make sure to properly test here. minimum and max reached
  # expect_true(all(or$pareto.front)[1,]^2) < 1e-4)

})
