context("infillopt ea")

test_that("infillopt ea", {

  obj.fun = smoof::makeSphereFunction(2L)

  ctrl = makeMBOControl(propose.points = 1L)
  ctrl = setMBOControlTermination(ctrl, iters = 4L)
  ctrl = setMBOControlInfill(ctrl, crit = "ei", opt = "ea", opt.restarts = 2L,
    opt.ea.maxit = 75L, opt.ea.lambda = 1L)

  lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

  des = generateTestDesign(20L, smoof::getParamSet(obj.fun))
  res = mbo(obj.fun, des, learner = lrn, control = ctrl)
  expect_true(res$y < 1e-1)

  obj.fun = smoof::makeSingleObjectiveFunction(
    fn = function(x) x$num1^2 + x$int1,
    par.set = makeParamSet(
      makeNumericParam("num1", lower = -1, upper = 1),
      makeIntegerParam("int1", lower = 0, upper = 2)
    ),
    has.simple.signature = FALSE
  )

  des = generateTestDesign(10L, smoof::getParamSet(obj.fun))
  res = mbo(obj.fun, des, learner = lrn, control = ctrl)
  expect_true(res$y < 1e-1)
})
