context("parego")

test_that("mbo parego works", {
  f = makeMBOFunction(function(x) x^2)
  ps = makeParamSet(
    makeNumericParam("x1", lower=-2, upper=1),
    makeNumericParam("x2", lower=-1, upper=2)
  )

  # Test normal run
  learner = makeLearner("regr.km", nugget.estim = TRUE)
  ctrl = makeMBOControl(iters = 5, infill.opt.focussearch.points = 10,
    number.of.targets = 2)
  or = mbo(f, ps, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))
  
  # Test with initial design
  des = generateDesign(10, ps)
  des = cbind(des, t(apply(des, 1, f)))
  names(des)[3:4] = c("y_1", "y_2")
  or = mbo(f, ps, design = des, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))
  
  # Test wrong dimension
  ctrl = makeMBOControl(iters=5, infill.opt.focussearch.points=10,
    number.of.targets = 3)
  expect_error(mbo(f, ps, learner = learner, control = ctrl),
    "wrong length: 2. Should be 3")
  
  # Test multippoint
  ctrl = makeMBOControl(iters=1, infill.opt.focussearch.points=10,
    number.of.targets = 2, parego.propose.points = 50L)
  or = mbo(f, ps, learner = learner, control = ctrl)
  # check used weights
  expect_true(all(apply(or$weight.path[, 1:2], 1, sum) == 1))
  expect_false(any(duplicated(or$weight.path[, 1])))
  expect_false(any(duplicated(or$weight.path[, 2])))
  # Test margin points
  ctrl = makeMBOControl(iters=10, infill.opt.focussearch.points=10,
    number.of.targets = 2, parego.propose.points = 2L, parego.use.margin.points = c(TRUE, TRUE))
  or = mbo(f, ps, learner = learner, control = ctrl)
  expect_true(all(t(or$weight.path[, 1:2]) %in% 0:1))
  expect_true(all(1 - or$weight.path[, 1] == or$weight.path[, 2]))
  
  # Test impute
  f1 = makeMBOFunction(function(x) {
    y = x^2
    ifelse(y < 2, NA, y)
  })
  ps = makeParamSet(
    makeNumericVectorParam("x", len=2, lower=0, upper=3)
  )
  ctrl = makeMBOControl(iters=5, infill.opt.focussearch.points=10,
    number.of.targets = 2, impute=function(x, y, opt.path) 0)
  or = mbo(f1, ps, learner = learner, control = ctrl)
  
  ctrl = makeMBOControl(iters=5, infill.opt.focussearch.points=10,
    number.of.targets = 2, impute=function(x, y, opt.path) 0, parego.propose.points = 2L)
  or = mbo(f1, ps, learner = learner, control = ctrl)
  
})
