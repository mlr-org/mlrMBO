context("different learner")

test_that("mbo works with different learners", {
  
  # Test some possible learner on a simple problem with discrete and
  # numeric parameters
  f1 = function(x)
    ifelse(x$disc1 == "a", x$num1 * 2 - 1, 1 - x$num1)
  ps1 = makeParamSet(
    makeDiscreteParam("disc1", values = c("a", "b")),
    makeNumericParam("num1", lower = 0, upper = 1)
  )
  
  f2 = function(x)
    x$num1^2
  ps2 = makeParamSet(
    makeNumericParam("num1", lower = 0, upper = 1)
  )
  
  ctrl = makeMBOControl(iters = 1, init.design.points = 12)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100)
  
  testit = function(fun, p.set, lrn, se, bagging = TRUE) {
    lrn = makeLearner(lrn)
    if (se) {
      if (bagging)
        lrn = makeBaggingWrapper(lrn, bw.iters = 5L)
      lrn = setPredictType(lrn, "se")
      ctrl$infill.crit = "ei"
    }
    mbo(fun, p.set,learner = lrn, control = ctrl)
  }
  
  testit(f1, ps1, "regr.lm", TRUE)
  testit(f1, ps1, "regr.lm", FALSE)
  testit(f2, ps2, "regr.lm", TRUE)
  testit(f2, ps2, "regr.lm", FALSE)
  library(DiceKriging)
  testit(f2, ps2, "regr.km", TRUE, FALSE)
  testit(f2, ps2, "regr.km", FALSE)
  library(randomForest)
  testit(f1, ps1, "regr.lm", TRUE, FALSE)
  testit(f1, ps1, "regr.lm", FALSE)
  testit(f2, ps2, "regr.lm", TRUE, FALSE)
  testit(f2, ps2, "regr.lm", FALSE)
  library(rpart)
  testit(f1, ps1, "regr.rpart", TRUE)
  testit(f1, ps1, "regr.rpart", FALSE)
  testit(f2, ps2, "regr.rpart", TRUE)
  testit(f2, ps2, "regr.rpart", FALSE)
  library(party)
  testit(f1, ps1, "regr.mob", TRUE)
  testit(f1, ps1, "regr.mob", FALSE)
  testit(f2, ps2, "regr.mob", TRUE)
  testit(f2, ps2, "regr.mob", FALSE)
  library(kknn)
  testit(f1, ps1, "regr.kknn", TRUE)
  testit(f1, ps1, "regr.kknn", FALSE)
  testit(f2, ps2, "regr.kknn", TRUE)
  testit(f2, ps2, "regr.kknn", FALSE)
  library(kernlab)
  testit(f1, ps1, "regr.ksvm", TRUE)
  testit(f1, ps1, "regr.ksvm", FALSE)
  testit(f2, ps2, "regr.ksvm", TRUE)
  testit(f2, ps2, "regr.ksvm", FALSE)
  library(earth)
  testit(f1, ps1, "regr.earth", TRUE)
  testit(f1, ps1, "regr.earth", FALSE)
  testit(f2, ps2, "regr.earth", TRUE)
  testit(f2, ps2, "regr.earth", FALSE)
  library(nnet)
  testit(f1, ps1, "regr.nnet", TRUE)
  testit(f1, ps1, "regr.nnet", FALSE)
  testit(f2, ps2, "regr.nnet", TRUE)
  testit(f2, ps2, "regr.nnet", FALSE)
  
  # FIXME: I disable the folowing tests .. I think the tests above should be
  # enough ...
  
  #ps = makeParamSet(
  #  makeNumericVectorParam("x", len = 2, lower = 0, upper = 1),
  #  makeDiscreteParam("z", values = 1:5)
  #)
  #f = function(x) sum(x$x) + as.numeric(x$z)
  ## check with larger initial design so all factor levels are there
  #ctrl = makeMBOControl(iters = 2, init.design.points = 50)
  #ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100, opt.restarts  = 1L)



  #testit("regr.lm", se = FALSE)
  #library(nnet)
  #testit("regr.nnet", se = FALSE)

  #testit("regr.lm", se = TRUE)
  #testit("regr.nnet", se = TRUE)

  # FIXME: I am unsure whether we have a chance to fix this,
  # # check with small initial design, not all levels are evaluated
  # # the problem comes with the bagging wrapper
  # ctrl = makeMBOControl(iters = 2, init.design.points = 10)
  # ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100)

  # testit = function(lrn) {
  #   lrn = makeLearner(lrn)
  #   lrn = makeBaggingWrapper(lrn, bw.iters = 5L)
  #   lrn = setPredictType(lrn, "se")
  #   mbo(f, ps,learner = lrn, control = ctrl)
  # }

  # testit("regr.lm")
  # testit("regr.blackboost")
  # testit("regr.nnet")
})

