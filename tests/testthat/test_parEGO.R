context("parEGO")

test_that("mbo parEGO works", {
  f = makeMBOFunction(function(x) x^2)
  ps = makeParamSet(
    makeNumericParam("x1", lower=-2, upper=1),
    makeNumericParam("x2", lower=-1, upper=2)
  )

  ## Test normal run
  learner = makeLearner("regr.km", nugget.estim=TRUE)
  ctrl = makeMBOControl(iters=5, infill.opt.focussearch.points=10,
    number.of.targets = 2)
  or = mboParEGO(f, ps, learner = learner, control = ctrl)
  expect_true(!any(is.na(or$pareto.front)))
  
  # Test wrong dimension
  ctrl = makeMBOControl(iters=5, infill.opt.focussearch.points=10,
    number.of.targets = 3)
  expect_error(mboParEGO(f, ps, learner = learner, control = ctrl),
    "output has wrong length")
  
  # Test multippoint
  ctrl = makeMBOControl(iters=5, infill.opt.focussearch.points=10,
    number.of.targets = 2, parEGO.propose.points = 2L)
  or = mboParEGO(f, ps, learner = learner, control = ctrl)
  
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
  or = mboParEGO(f1, ps, learner = learner, control = ctrl)
  
})
