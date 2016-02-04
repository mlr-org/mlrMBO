context("infill optimizers")

test_that("infill optimizers", {
  f = makeSphereFunction(2L)

  mycontrol = function(opt, restarts) {
    ctrl = makeMBOControl(init.design.points = 20L, iters = 5L)
    ctrl = setMBOControlInfill(ctrl, crit = "mean", opt = opt,
      opt.cmaes.control = list(maxit = 10L))
  }
  mycheck = function(or) {
    expect_equal(getOptPathLength(or$opt.path), 25L)
    expect_true(!is.na(or$y))
    expect_true(or$y < 1)
  }

  learner = makeLearner("regr.km", nugget.estim = TRUE)
  ctrl = mycontrol("cmaes", 1L)
  or = mbo(f, des = NULL, learner, ctrl)
  mycheck(or)
  ctrl = mycontrol("cmaes", 2L)
  or = mbo(f, des = NULL, learner, ctrl)
  mycheck(or)
})


