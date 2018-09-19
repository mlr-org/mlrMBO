context("filter proposed points")

test_that("filter proposed points", {
  # now check min dist, set to "inf" so we always replace
  ctrl = makeMBOControl(propose.points = 2L)
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.cb1,
    filter.proposed.points = TRUE, filter.proposed.points.tol = 1000,
    opt = "focussearch", opt.focussearch.points = 100L, opt.focussearch.maxit = 1L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cb")

  res = mbo(testf.fsphere.1d, testd.fsphere.1d, control = ctrl)
  op = as.data.frame(res$opt.path)
  expect_true(all(op$prop.type[seq_row(testd.fsphere.1d)] != "random_filter"))
  expect_true(all(op$prop.type[-seq_row(testd.fsphere.1d)] == "random_filter"))

  # test for functions with discrete values
  res = mbo(testf.mixed, testd.mixed, control = ctrl)
  op = as.data.frame(res$opt.path)
  expect_true(all(op$prop.type[seq_row(testd.mixed)] != "random_filter"))
  expect_true(all(op$prop.type[-seq_row(testd.mixed)] == "random_filter"))
})
