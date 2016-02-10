context("stopping criteria")

test_that("stopping criteria works", {
  iters = 30L
  time.budget = 3L # seconds
  target.fun.value = 0.005

  f = makeSphereFunction(1L)
  x.grid = seq(-2, 2, length.out = 10L)
  design = data.frame(x = x.grid, y = sapply(x.grid, f))

  learner = makeLearner("regr.randomForest")
  ctrl = makeMBOControl(iters = iters, time.budget = time.budget)
  or = mbo(f, design = design, learner = learner, control = ctrl)
  expect_equal(or$final.state, "time.exceeded")

  ctrl = makeMBOControl(iters = iters, target.fun.value = target.fun.value)
  or = mbo(f, design = design, learner = learner, control = ctrl)

  expect_equal(or$final.state, "target.fun.value.reached")
  expect_less_than(or$y, target.fun.value)
})
