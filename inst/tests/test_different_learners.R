context("different learner")

test_that("infill optimizers", {
  
  f = function(x) if (x$x3 == "a") x$x1^2 + x$x2^2 else 5
  ps = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeNumericParam("x2", lower = -1, upper = 2),
    makeDiscreteParam("x3", values = c("a", "b"))
  )
  
  ctrl = makeMBOControl(iters = 2, init.design.points = 10,
    infill.opt.focussearch.points = 100)
  mbo(f, ps, learner = makeLearner("regr.rpart"), control = ctrl)
  mbo(f, ps, learner = makeLearner("regr.lm"), control = ctrl)
  mbo(f, ps, learner = makeLearner("regr.mob"), control = ctrl)
  mbo(f, ps, learner = makeLearner("regr.kknn"), control = ctrl)
  mbo(f, ps, learner = makeLearner("regr.ksvm"), control = ctrl)
  mbo(f, ps, learner = makeLearner("regr.earth"), control = ctrl)
})