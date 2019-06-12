context("multipoint constant liar")

test_that("multipoint constant liar", {
  for (crit in list(crit.ei, crit.aei)) {
    ctrl = makeMBOControl(propose.points = 5L)
    ctrl = setMBOControlTermination(ctrl, iters = 2L)
    ctrl = setMBOControlInfill(ctrl, crit = crit)
    ctrl = setMBOControlMultiPoint(ctrl, method = "cl")
    res = mbo(testf.fsphere.1d, testd.fsphere.1d, control = ctrl)
    expect_is(res, "MBOResult", info = crit$id)
    expect_lt(res$y, 0.1)
    expect_equal(getOptPathDOB(res$opt.path), rep(0:2, each = 5), info = crit$ido)
  }
})
