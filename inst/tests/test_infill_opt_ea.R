context("infillopt ea")

test_that("infillopt ea", {
          
  objfun = generate_sphere_function(2)

  ps = makeNumericParamSet(lower=lower_bounds(objfun), upper=upper_bounds(objfun))

  ctrl = makeMBOControl(init.design.points=20, iters=5, propose.points=1, 
    infill.crit="ei", infill.opt="ea", infill.opt.restarts=2L,
    infill.opt.ea.maxit=100, infill.opt.ea.mu=20L, infill.opt.ea.p=0.5, infill.opt.ea.eta=10)

  lrn = makeLearner("regr.km", predict.type="se", covtype="matern3_2")

  res = mbo(makeMBOFunction(objfun), ps, learner=lrn, control=ctrl)
  expect_true(res$y < global_minimum(objfun)$value + 1e-1)
})

