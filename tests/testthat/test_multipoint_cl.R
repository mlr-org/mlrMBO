context("multipoint constant liar")

test_that("multipoint constant liar", {
  ctrl = makeMBOControl(propose.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  ctrl = setMBOControlInfill(ctrl, crit = "ei")
  ctrl = setMBOControlMultiPoint(ctrl, method = "cl")
  res = mbo(testf.fsphere.1d, testd.fsphere.1d, learner = default.kriging, control = ctrl)
  expect_is(res, "MBOResult")
  expect_true(res$y < 0.1)
  expect_equal(getOptPathDOB(res$opt.path), rep(0:2, each = 5))
})
