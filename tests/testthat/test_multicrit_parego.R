context("multicrit: parego")

test_that("mbo parego works", {

  # Test normal run
  learner = makeLearner("regr.km", nugget.estim = TRUE)
  ctrl = makeMBOControl(n.objectives = 2L)
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10)
  ctrl = setMBOControlMultiCrit(ctrl, method = "parego", parego.s = 100)
  or = mbo(testfmco1, testdesmco1, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))

  # Test margin points
  ctrl = makeMBOControl(n.objectives = 2, propose.points = 2L)
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 5)
  ctrl = setMBOControlMultiCrit(ctrl, method = "parego", parego.s = 100, parego.use.margin.points = c(TRUE, TRUE))
  or = mbo(testfmco1, testdesmco1, learner = learner, control = ctrl)
  w = as.data.frame(or$opt.path)[-(1:10), c("parego.weight.1", "parego.weight.2")]
  expect_true(all(w == 0 | w == 1))
  expect_true(all(1 - w[, 1] == w[, 2]))

  # Test margin points
  ctrl = makeMBOControl(n.objectives = 2, propose.points = 2L)
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 5)
  ctrl = setMBOControlMultiCrit(ctrl, method = "parego", parego.s = 100, parego.use.margin.points = c(TRUE, TRUE))
  or = mbo(testfmco1, testdesmco1, learner = learner, control = ctrl)
  w = as.data.frame(or$opt.path)[-(1:10), c("parego.weight.1", "parego.weight.2")]
  expect_true(all(w == 0 | w == 1))
  expect_true(all(1 - w[, 1] == w[, 2]))

  # Test wrong dimension
  ctrl = makeMBOControl(n.objectives = 3)
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10L)
  ctrl = setMBOControlMultiCrit(ctrl, method = "parego", parego.s = 100)
  expect_error(mbo(testfmco1, testdesmco1, learner = learner, control = ctrl), "Objective function has")

  # Test multippoint
  ctrl = makeMBOControl(n.objectives = 2, propose.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10L)
  ctrl = setMBOControlMultiCrit(ctrl, method = "parego", parego.s = 100)
  or = mbo(testfmco1, testdesmco1, learner = learner, control = ctrl)

  # check used weights
  w = as.data.frame(or$opt.path)[, c("parego.weight.1", "parego.weight.2")]
  expect_true(all(rowSums(w[-(1:10),]) == 1))
  expect_false(any(duplicated(w[-(1:10), 1])))
  expect_false(any(duplicated(w[-c(1:10), 2])))

  # Multipoint with minimization and maximization
  ctrl = makeMBOControl(n.objectives = 2, propose.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10L)
  ctrl = setMBOControlMultiCrit(ctrl, method = "parego", parego.s = 100)
  or = mbo(testfmco2, testdesmco2, learner = learner, control = ctrl)
  # make sure the pareto.front is a matrix
  # FIXME: make sure to properly test here. minimum and max reached
  # expect_true(all(or$pareto.front)[1,]^2) < 1e-4)

})
