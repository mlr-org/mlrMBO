context("infillopt cmaes")

test_that("infillopt cmaes", {
  obj.fun = testf.fsphere.2d

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 4L)
  cmaes.control = list(sigma = 1.5, lambda = 40, stop.ons = c(list(cmaesr::stopOnMaxIters(100L)), cmaesr::getDefaultStoppingConditions()))
  ctrl = setMBOControlInfill(ctrl, crit = crit.ei, opt = "cmaes", opt.restarts = 2L, opt.cmaes.control = cmaes.control)

  des = testd.fsphere.2d
  res = mbo(obj.fun, des, control = ctrl)
  expect_true(res$y < 0.5, info = "real")
})
