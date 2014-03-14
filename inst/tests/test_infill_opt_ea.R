context("infillopt ea")

test_that("infillopt ea", {

  objfun = function(x) sum(x^2)

  ps = makeNumericParamSet(len=2L, lower=-5, upper=5)

  ctrl = makeMBOControl(init.design.points=20, iters=5, propose.points=1,
    infill.crit="ei", infill.opt="ea", infill.opt.restarts=2L,
    infill.opt.ea.maxit=75, infill.opt.ea.lambda = 1L)

  lrn = makeLearner("regr.km", predict.type="se", covtype="matern3_2")

  res = mbo(makeMBOFunction(objfun), ps, learner=lrn, control=ctrl)
  expect_true(res$y < 1e-1)
})

