context("multipoint cb")

test_that("multipoint cb", {
  par.set = makeNumericParamSet(len = 1L, lower = -1, upper = 1)
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      sum(x^2)
    },
    par.set = par.set
  )
  lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

  des = generateDesign(30L, smoof::getParamSet(f))
  ctrl = makeMBOControl(propose.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, crit = "cb", opt = "focussearch", opt.focussearch.points = 100L,
    opt.focussearch.maxit = 2L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cb")

  res = mbo(f, des, learner = lrn, control = ctrl)
  op = as.data.frame(res$opt.path)
  expect_true(all(is.na(op$multipoint.cb.lambda[1:30])))
  expect_true(all(!is.na(op$multipoint.cb.lambda[31:35])))
  expect_is(res, "MBOResult")
  expect_true(res$y < 0.1)

  # FIXME: this test must be generalized
  # now check min dist, set to "inf" so we can only propose 1 new point, not 5
  des = generateDesign(30L, smoof::getParamSet(f))
  ctrl = makeMBOControl(propose.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, crit = "cb", opt = "focussearch", opt.focussearch.points = 100L,
    opt.focussearch.maxit = 2L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cb")
  ctrl$cb.min.dist = 10000

  res = mbo(f, des, learner = lrn, control = ctrl)
  expect_equal(getOptPathLength(res$opt.path), 35L)
})


test_that("multipoint cb with random interleaved points", {
  par.set = makeNumericParamSet(len = 1L, lower = -1, upper = 1)
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      sum(x^2)
    },
    par.set = par.set
  )
  lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

  des = generateDesign(30L, smoof::getParamSet(f))
  ctrl = makeMBOControl(propose.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, crit = "cb", opt = "focussearch", opt.focussearch.points = 100L,
    opt.focussearch.maxit = 2L, interleave.random.points = 5L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cb")

  res = mbo(f, des, learner = lrn, control = ctrl)
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

  lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

  des = generateDesign(30L, smoof::getParamSet(f))
  ctrl = makeMBOControl(propose.points = 1L)
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, crit = "cb", opt = "focussearch", opt.focussearch.points = 100L,
    opt.focussearch.maxit = 2L, interleave.random.points = 1L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cb")

  res = mbo(f, des, learner = lrn, control = ctrl)
  op = as.data.frame(res$opt.path)
  op = tail(op, 2)
  expect_identical(is.na(op$cb), c(FALSE, TRUE))
  expect_identical(is.na(op$train.time), c(FALSE, TRUE))
})
