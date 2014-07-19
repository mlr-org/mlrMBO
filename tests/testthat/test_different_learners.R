context("different learner")

test_that("mbo works with different learners", {

  f = function(x)
    ifelse(x$disc1 == "a", x$num1 * 2 - 1, 1 - x$num1)
  ps = makeParamSet(
    makeDiscreteParam("disc1", values = c("a", "b")),
    makeNumericParam("num1", lower = 0, upper = 1)
  )

  ctrl = makeMBOControl(iters = 2, init.design.points = 10)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100)

  mbo(f, ps, learner = makeLearner("regr.rpart"), control = ctrl)
  mbo(f, ps, learner = makeLearner("regr.lm"), control = ctrl)
  mbo(f, ps, learner = makeLearner("regr.mob"), control = ctrl)
  mbo(f, ps, learner = makeLearner("regr.kknn"), control = ctrl)
  mbo(f, ps, learner = makeLearner("regr.ksvm"), control = ctrl)
  mbo(f, ps, learner = makeLearner("regr.earth"), control = ctrl)

  ps = makeParamSet(
    makeNumericVectorParam("x", len = 2, lower = 0, upper = 1),
    makeDiscreteParam("z", values = 1:5)
  )
  f = function(x) sum(x$x) + as.numeric(x$z)
  # check with larger initial design so all factor levels are there
  ctrl = makeMBOControl(iters = 2, init.design.points = 30)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100)

  testit = function(lrn) {
    lrn = makeLearner(lrn)
    lrn = makeBaggingWrapper(lrn, bw.iters = 5L)
    lrn = setPredictType(lrn, "se")
    mbo(f, ps,learner = lrn, control = ctrl)
  }

  testit("regr.lm")
  testit("regr.blackboost")
  testit("regr.nnet")

  # check with small initial design, not all levels are evaluated
  # the problem comes with the bagging wrapper
  ctrl = makeMBOControl(iters = 2, init.design.points = 10)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100)

  testit = function(lrn) {
    lrn = makeLearner(lrn)
    lrn = makeBaggingWrapper(lrn, bw.iters = 5L)
    lrn = setPredictType(lrn, "se")
    mbo(f, ps,learner = lrn, control = ctrl)
  }

  testit("regr.lm")
  testit("regr.blackboost")
  testit("regr.nnet")
})

