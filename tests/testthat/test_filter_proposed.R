context("filter proposed points")

test_that("filter proposed points", {
  objfun = function(x) {
    y = sum(x^2)
  }
  ps = makeNumericParamSet(len = 1L, lower = -1, upper = 1)
  lrn = makeLearner("regr.km", predict.type = "se")

  # check that flag is not in optpath if not enabled
  ctrl = makeMBOControl(init.design.points = 4, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, filter.proposed.points = FALSE, opt = "focussearch",
    opt.focussearch.points = 5L, opt.focussearch.maxit = 1L, opt.restarts = 1L)
  res = mbo(makeMBOFunction(objfun), par.set = ps, learner = lrn, control = ctrl)
  op = as.data.frame(res$opt.path)
  expect_true("filter.replace" %nin% colnames(op))

  # now check min dist, set to "inf" so we always replace
  ctrl = makeMBOControl(init.design.points = 30L, iters = 1L, propose.points = 2L)
  ctrl = setMBOControlInfill(ctrl, crit = "lcb",
    filter.proposed.points = TRUE, filter.proposed.points.tol = 1000,
    opt = "focussearch", opt.focussearch.points = 100L, opt.focussearch.maxit = 1L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "lcb")

  res = mbo(makeMBOFunction(objfun), par.set = ps, learner = lrn, control = ctrl)
  op = as.data.frame(res$opt.path)
  expect_true(all(is.na(op$filter.replace[1:30])))
  expect_true(all(op$filter.replace[31:32]))
})




