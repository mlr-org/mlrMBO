context("multipoint lcb")

test_that("multipoint lcb", {
  objfun = function(x) {
    y = sum(x^2)
  }
  ps = makeNumericParamSet(len = 1L, lower = -1, upper = 1)
  lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

  ctrl = makeMBOControl(init.design.points = 30L, iters = 1L, propose.points = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = "lcb", opt = "focussearch", opt.focussearch.points = 100L,
    opt.focussearch.maxit = 2L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "lcb")

  res = mbo(makeMBOFunction(objfun), par.set = ps, learner = lrn, control = ctrl)
  op = as.data.frame(res$opt.path)
  expect_true(all(is.na(op$multipoint.lcb.lambda[1:30])))
  expect_true(all(!is.na(op$multipoint.lcb.lambda[31:35])))
  expect_is(res, "MBOResult")
  expect_true(res$y < 0.1)

  # FIXME: this test must be generalized
  # now check min dist, set to "inf" so we can only propose 1 new point, not 5
  ctrl = makeMBOControl(init.design.points = 30L, iters = 1L, propose.points = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = "lcb", opt = "focussearch", opt.focussearch.points = 100L,
    opt.focussearch.maxit = 2L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "lcb")
  ctrl$lcb.min.dist = 10000

  res = mbo(makeMBOFunction(objfun), par.set = ps, learner = lrn, control = ctrl)
  expect_equal(getOptPathLength(res$opt.path), 35L)
})




