context("stopping criteria")

test_that("stopping criteria works", {
  iters = 30L
  time.budget = 3L # seconds
  target.fun.value = 0.005
  max.evals = 13L

  f = makeSphereFunction(1L)
  x.grid = seq(-2, 2, length.out = 10L)
  design = data.frame(x = x.grid, y = sapply(x.grid, f))

  learner = makeLearner("regr.randomForest")

  # time budget
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, time.budget = time.budget)
  or = mbo(f, design = design, learner = learner, control = ctrl)

  expect_equal(or$final.state, "term.time")

  # target fun value
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = iters, target.fun.value = target.fun.value)
  or = mbo(f, design = design, learner = learner, control = ctrl)

  expect_equal(or$final.state, "term.yval")
  expect_lt(or$y, target.fun.value)

  # maximal number of target function evaluations
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, max.evals = max.evals)
  or = mbo(f, design = design, learner = learner, control = ctrl)

  expect_equal(or$final.state, "term.feval")
  expect_equal(getOptPathLength(or$opt.path), max.evals)
})
