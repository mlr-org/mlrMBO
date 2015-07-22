context("stopping criteria")

test_that("stopping criteria works", {
  iters = 30L
  time.budget = 3L # seconds
  target.fun.value = 0.005

  obj.fun = function(x) {
    return(x^2)
  }
  par.set = makeParamSet(
    makeNumericParam("x", lower = -2, upper = 2)
    )
  x.grid = seq(-2, 2, length.out = 10L)
  design = data.frame(x = x.grid, y = obj.fun(x.grid))

  learner = makeLearner("regr.randomForest")
  ctrl = makeMBOControl(iters = iters, time.budget = time.budget)
  or = mbo(makeMBOFunction(obj.fun), design = design, par.set = par.set, learner = learner, control = ctrl)
  expect_equal(or$final.state, "time.exceeded")
  
  ctrl = makeMBOControl(iters = iters, target.fun.value = target.fun.value)
  or = mbo(makeMBOFunction(obj.fun), design = design, par.set = par.set, learner = learner, control = ctrl)
  
  expect_equal(or$final.state, "target.fun.value.reached")
  expect_less_than(or$y, target.fun.value)
})
