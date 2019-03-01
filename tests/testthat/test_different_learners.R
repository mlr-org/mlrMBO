context("different learner")

test_that("mbo works with different learners", {

  # Test some possible learner on a simple problem with discrete and
  # numeric parameters
  ps1 = testp.mixed
  f1 = testf.mixed

  ps2 = testp.fsphere.2d
  f2 = testf.fsphere.2d

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 1L)

  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100L,
    opt.restarts = 1L, opt.focussearch.maxit = 1L)

  testit = function(fun, lrn, se, bagging = TRUE) {
    if (is.character(lrn))
      lrn = makeLearner(lrn)
    if (se) {
      if (bagging)
        lrn = makeBaggingWrapper(lrn, bw.iters = 5L)
      lrn = setPredictType(lrn, "se")
    }
    else {
      ctrl$infill.crit = crit.mr
    }
    des = generateTestDesign(10L, getParamSet(fun))
    res = mbo(fun, des, learner = lrn, control = ctrl)
    expect_class(res, "MBOResult")
  }

  # and now a huge number of runs
  testit(f1, "regr.lm", TRUE)
  testit(f1, "regr.lm", FALSE)
  testit(f2, "regr.lm", TRUE)
  testit(f2, "regr.lm", FALSE)
  testit(f2, "regr.km", TRUE, FALSE)
  testit(f2, "regr.km", FALSE)
  testit(f1, "regr.lm", TRUE, FALSE)
  testit(f1, "regr.lm", FALSE)
  testit(f2, "regr.lm", TRUE, FALSE)
  testit(f2, "regr.lm", FALSE)
  testit(f1, "regr.rpart", TRUE)
  testit(f1, "regr.rpart", FALSE)
  testit(f2, "regr.rpart", TRUE)
  testit(f2, "regr.rpart", FALSE)
  testit(f1, "regr.mob", TRUE)
  testit(f1, "regr.mob", FALSE)
  testit(f2, "regr.mob", TRUE)
  testit(f2, "regr.mob", FALSE)
  testit(f1, "regr.kknn", TRUE)
  testit(f1, "regr.kknn", FALSE)
  testit(f2, "regr.kknn", TRUE)
  testit(f2, "regr.kknn", FALSE)
  testit(f1, "regr.ksvm", TRUE)
  testit(f1, "regr.ksvm", FALSE)
  testit(f2, "regr.ksvm", TRUE)
  testit(f2, "regr.ksvm", FALSE)
  testit(f1, "regr.earth", TRUE)
  testit(f1, "regr.earth", FALSE)
  testit(f2, "regr.earth", TRUE)
  testit(f2, "regr.earth", FALSE)
  testit(f1, "regr.nnet", TRUE)
  testit(f1, "regr.nnet", FALSE)
  testit(f2, "regr.nnet", TRUE)
  testit(f2, "regr.nnet", FALSE)
  testit(f2, makeMBOLearner(ctrl, f1), TRUE, FALSE)
  testit(f2, makeMBOLearner(ctrl, f2), TRUE, FALSE)
})
