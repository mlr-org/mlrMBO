context("multipoint constant liar")

test_that("multipoint constant liar", {
  objfun = function(x) {
    y = sum(x^2)
  }
  ps = makeNumericParamSet(len = 1L, lower = -1, upper = 1)
  lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

  ctrl = makeMBOControl(init.design.points = 30L, iters = 1L, propose.points = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = "ei")
  ctrl = setMBOControlMultiPoint(ctrl, method = "cl")

  res = mbo(makeMBOFunction(objfun), par.set = ps, learner = lrn, control = ctrl)
  expect_is(res, "MBOResult")
  expect_true(res$y < 0.1)
})
