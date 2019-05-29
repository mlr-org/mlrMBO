context("multipoint ensemble")

test_that("multipoint ensemble", {
  ctrl = makeMBOControl(propose.points = 3L)
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.cb1, #crit does not have any effect!
    opt = "focussearch", opt.focussearch.points = 100L, opt.focussearch.maxit = 2L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "ensemble", ensemble.crits = list(crit.cb1, crit.cb2, crit.ei))

  res = mbo(testf.fsphere.1d, testd.fsphere.1d, control = ctrl)
  expect_output(print(res), "Recommended parameters")
  expect_is(res, "MBOResult")
  expect_lt(res$y, 0.15)
  op = as.data.frame(res$opt.path)
  expect_true(all(c("initdesign", "infill_cb", "infill_ei") %in% op$prop.type))
  expect_true(all(1:2 %in% op$lambda))

  # NOTE: colum op$dt exists because this is the infill.crit specified in setMBOControlInfill. This infill.crit is actually is not used and only determines the name for the column in op that contains the infil crit value!

})
