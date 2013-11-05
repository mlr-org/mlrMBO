context("infillopt es")

test_that("infillopt es", {
          
  objfun = generate_sphere_function(2)

  ps = makeNumericParamSet(lower=lower_bounds(objfun), upper=upper_bounds(objfun))

  ctrl = makeMBOControl(init.design.points=20, iters=5, propose.points=1, 
    infill.crit="ei", infill.opt="es",
    infill.opt.es.maxit=200, infill.opt.es.mu=20L, infill.opt.es.p=0.5, infill.opt.es.eta=10)

  lrn = makeLearner("regr.km", predict.type="se", covtype="matern3_2")

  res = mbo(makeMBOFunction(objfun), ps, learner=lrn, control=ctrl)
  expect_true(res$y < global_minimum(objfun)$value + 1e-1)
})

