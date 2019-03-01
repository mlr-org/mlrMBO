context("infill optimizers")

test_that("infill optimizers", {
  mycontrol = function(opt, restarts) {
    ctrl = makeMBOControl()
    ctrl = setMBOControlTermination(ctrl, iters = 5L)
    ctrl = setMBOControlInfill(ctrl, opt = opt,
      opt.cmaes.control = list(stop.ons = list(stopOnMaxIters(30L))))
  }
  mycheck = function(or) {
    expect_equal(getOptPathLength(or$opt.path), nrow(testd.fsphere.2d) + 5L)
    expect_number(or$y)
    expect_lt(or$y, 1)
  }

  learner = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE)
  ctrl = mycontrol("cmaes", 1L)
  or = mbo(testf.fsphere.2d, testd.fsphere.2d, learner, ctrl)
  mycheck(or)
  ctrl = mycontrol("cmaes", 2L)
  or = mbo(testf.fsphere.2d, testd.fsphere.2d, learner, ctrl)
  mycheck(or)
})
