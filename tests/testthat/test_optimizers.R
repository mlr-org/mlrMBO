context("infill optimizers")

test_that("infill optimizers", {

  mycontrol = function(opt, restarts) {
    ctrl = makeMBOControl()
    ctrl = setMBOControlTermination(ctrl, iters = 5L)
    ctrl = setMBOControlInfill(ctrl, crit = "mean", opt = opt,
      opt.cmaes.control = list(maxit = 10L))
  }
  mycheck = function(or) {
    expect_equal(getOptPathLength(or$opt.path), nrow(testd.fsphere.2d) + 5L)
    expect_true(!is.na(or$y))
    expect_true(or$y < 1)
  }

  learner = makeLearner("regr.km", nugget.estim = TRUE)
  ctrl = mycontrol("cmaes", 1L)
  or = mbo(testf.fsphere.2d, testd.fsphere.2d, learner, ctrl)
  mycheck(or)
  ctrl = mycontrol("cmaes", 2L)
  or = mbo(testf.fsphere.2d, testd.fsphere.2d, learner, ctrl)
  mycheck(or)
})


