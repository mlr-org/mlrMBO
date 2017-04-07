context("multipoint cb")

test_that("multipoint cb", {
  ctrl = makeMBOControl(propose.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.cb1,
    opt = "focussearch", opt.focussearch.points = 100L, opt.focussearch.maxit = 2L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cb")

  res = mbo(testf.fsphere.1d, testd.fsphere.1d, control = ctrl)
  expect_output(print(res), "Recommended parameters")
  op = as.data.frame(res$opt.path)
  expect_true(all(is.na(op$multipoint.cb.lambda[1:5])))
  expect_numeric(op$multipoint.cb.lambda[6:15], any.missing = FALSE)
  expect_is(res, "MBOResult")
  expect_lt(res$y, 0.15)
})


test_that("multipoint cb with random interleaved points", {
  ctrl = makeMBOControl(propose.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.cb1,
    opt = "focussearch", opt.focussearch.points = 100L,
    opt.focussearch.maxit = 2L, interleave.random.points = 5L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cb")
  res = mbo(testf.fsphere.1d, testd.fsphere.1d, control = ctrl)
  expect_output(print(res), "Recommended parameters")
  op = as.data.frame(res$opt.path)
  op = tail(op, 10)

  # dob should be the same
  expect_true(all(op$dob == 1L))

  # no error messages
  expect_true(all(is.na(op$error.message)))

  # cb should be NA for random points
  expect_true(all(!is.na(head(op$cb, 5))))
  expect_true(all(is.na(tail(op$cb, 5))))

  # propose.time is NA for random points
  expect_true(all(!is.na(head(op$propose.time, 5))))
  expect_true(all(is.na(tail(op$propose.time, 5))))

  ctrl = makeMBOControl(propose.points = 1L)
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.cb1,
    opt = "focussearch", opt.focussearch.points = 100L,
    opt.focussearch.maxit = 2L, interleave.random.points = 1L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cb")

  res = mbo(testf.fsphere.1d, testd.fsphere.1d, control = ctrl)
  op = as.data.frame(res$opt.path)
  op = tail(op, 2)
  expect_identical(is.na(op$cb), c(FALSE, TRUE))
  expect_identical(is.na(op$train.time), c(FALSE, TRUE))
})
