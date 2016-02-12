context("filter proposed points")

test_that("filter proposed points", {
  lrn = makeLearner("regr.km", predict.type = "se")

  # check that flag is not in optpath if not enabled
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, filter.proposed.points = FALSE, opt = "focussearch",
    opt.focussearch.points = 5L, opt.focussearch.maxit = 1L, opt.restarts = 1L)
  res = mbo(testf.fsphere.1d, testd.fsphere.1d, learner = lrn, control = ctrl)
  op = as.data.frame(res$opt.path)
  expect_true("filter.replace" %nin% colnames(op))

  # now check min dist, set to "inf" so we always replace
  ctrl = makeMBOControl(propose.points = 2L)
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, crit = "cb",
    filter.proposed.points = TRUE, filter.proposed.points.tol = 1000,
    opt = "focussearch", opt.focussearch.points = 100L, opt.focussearch.maxit = 1L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cb")

  res = mbo(testf.fsphere.1d, testd.fsphere.1d, learner = lrn, control = ctrl)
  op = as.data.frame(res$opt.path)
  expect_true(all(is.na(op$filter.replace[seq_row(testd.fsphere.1d)])))
  expect_true(all(op$filter.replace[-seq_row(testd.fsphere.1d)]))
})




