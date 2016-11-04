context("stopping criteria")

test_that("stopping criteria works", {
  iters = 30L
  time.budget = 3L # seconds
  exec.time.budget = 20L #seconds
  target.fun.value = 0.005
  max.evals = 13L
  tolerance = 0.2
  fake.exec.time = 5L

  f.inner = makeSphereFunction(1L)
  f = makeSingleObjectiveFunction(
    name = "stoppingtest",
    fn = function(...) {
      res = f.inner(...)
      setAttribute(res, "exec.time", fake.exec.time)},
    par.set = getParamSet(f.inner), 
    global.opt.params = getGlobalOptimum(f.inner)$param, 
    global.opt.value = getGlobalOptimum(f.inner)$value
    )
  
  x.grid = seq(-2, 2, length.out = 10L)
  design = data.frame(x = x.grid, y = sapply(x.grid, f))

  learner = makeLearner("regr.randomForest")

  # time budget
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, time.budget = time.budget)
  or = mbo(f, design = design, learner = learner, control = ctrl)

  expect_equal(or$final.state, "term.time")
  expect_gte(diff(range(getOptPathCol(or$opt.path, "exec.timestamp"), na.rm = TRUE)),  time.budget)

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
  
  # time budget for function evaluation
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, exec.time.budget = exec.time.budget)
  or = mbo(f, design = design, learner = learner, control = ctrl)
  
  expect_equal(or$final.state, "term.exectime")
  expect_equal(sum(getOptPathCol(or$opt.path, "exec.time"), na.rm = TRUE), exec.time.budget + fake.exec.time)

  # target input value
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, more.stop.conds = list(makeMBOTerminationTargetInputValue(tolerance = tolerance)))
  or = mbo(f, design = design, learner = learner, control = ctrl)

  expect_equal(or$final.state, "term.custom")
  expect_lt(or$y, f(getGlobalOptimum(f)$param[1,,drop=FALSE] + tolerance)) #works because symmetry and convexity of f
  
  # target fake realtime
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, more.stop.conds = list(makeMBOTerminationFakeRealtime(time.budget = exec.time.budget)))
  or = mbo(f, design = design, learner = learner, control = ctrl)
  op.df = as.data.frame(or$opt.path)
  expect_true(sum(c(op.df$exec.time, op.df$train.time, op.df$propose.time), na.rm = TRUE) %btwn% (exec.time.budget + c(-5, 0)))
  expect_equal(or$final.state, "term.custom")
})
