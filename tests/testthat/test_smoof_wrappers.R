context("smoof wrappers")

test_that("wrapped smoof function work", {
  f = makeSphereFunction(2L)
  fc = addCountingWrapper(f)
  fl = addLoggingWrapper(f)
  fcl = addCountingWrapper(addLoggingWrapper(f))

  learner = makeLearner("regr.rpart")

  ctrl = makeMBOControl()
  ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritMeanResponse())
  ctrl = setMBOControlTermination(ctrl, max.evals = 5)
  or.c = mbo(fc, learner = learner, control = ctrl)
  or.l = mbo(fl, learner = learner, control = ctrl)
  or.cl = mbo(fcl, learner = learner, control = ctrl)

  expect_equal(getOptPathLength(or.c$opt.path), getNumberOfEvaluations(fc))
  expect_equal(getOptPathY(or.l$opt.path), getLoggedValues(fl)$obj.vals)
  expect_equal(getOptPathY(or.cl$opt.path), getLoggedValues(fcl)$obj.val)
  expect_equal(getOptPathLength(or.cl$opt.path), getNumberOfEvaluations(fcl))
})
