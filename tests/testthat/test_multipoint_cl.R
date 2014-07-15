context("multipoint constant liar")

test_that("multipoint constant liar", {
  objfun = branin
  ps = makeNumericParamSet(len = 2L, lower = 0, upper = 1)
  lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

  ctrl = makeMBOControl(init.design.points = 30L, iters = 1L, propose.points = 5L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cl")

  res = mbo(makeMBOFunction(objfun), par.set = ps, learner = lrn, control = ctrl)
  expect_is(res, "MBOResult")
  gap = res$y - 0.3979
  #expect_true(gap < 0.2)
})
