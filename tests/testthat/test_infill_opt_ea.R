context("infillopt ea")

test_that("infillopt ea", {

  objfun = function(x) sum(x^2)

  ps = makeNumericParamSet(len = 2L, lower = -5, upper = 5)

  ctrl = makeMBOControl(init.design.points = 20, iters = 4, propose.points = 1)
  ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "ea", opt.restarts = 2L,
    opt.ea.maxit = 75, opt.ea.lambda = 1L)

  lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

  res = mbo(makeMBOFunction(objfun), ps, learner = lrn, control = ctrl)
  expect_true(res$y < 1e-1)

  objfun = function(x) {
  	x$num1^2 + x$int1
  }

  ps = makeParamSet(
  	makeNumericParam("num1", lower = -1, upper = 1),
  	makeIntegerParam("int1", lower = 0, upper = 2)
  )

  res = mbo(objfun, ps, learner = lrn, control = ctrl)
  expect_true(res$y < 1e-1)
})

