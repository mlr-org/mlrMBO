context("mbo impute")

test_that("mbo works with failures", {
  f1 = makeMBOFunction(function(x) {
    y = sum(x^2)
    if (y < 5)
      return(NA)
    return(y)
  })
  f2 = makeMBOFunction(function(x) {
    y = sum(x^2)
    if (y < 5)
      stop("foo")
    return(y)
  })
  ps = makeParamSet(
    makeNumericVectorParam("x", len=2, lower=0, upper=3)
  )
  learner = makeLearner("regr.randomForest")

  ctrl = makeMBOControl(iters = 20, infill.opt.focussearch.points = 500)
  expect_error(mbo(f1, ps, des=NULL, learner, ctrl, show.info = FALSE), "Infeasible y")
  ctrl = makeMBOControl(iters = 20,  infill.opt.focussearch.points = 500, impute = function(x, y, opt.path) 0)
  res = mbo(f1, ps, des=NULL, learner, ctrl, show.info = FALSE)
  # Check for correct error messages
  NA.inds = which(getOptPathY(res$opt.path) == 0)
  for (ind in 1:getOptPathLength(res$opt.path)) {
    if(ind %in% NA.inds) {
      expect_equal(grep("mlrMBO:", getOptPathErrorMessages(res$opt.path)[ind]), 1)
    } else 
      expect_equal("", getOptPathErrorMessages(res$opt.path)[ind])
  }
  ctrl = makeMBOControl(iters = 50,  infill.opt.focussearch.points = 500)
  expect_error(mbo(f2, ps, des = NULL, learner, ctrl, show.info = FALSE), "foo")
  ctrl = makeMBOControl(iters = 50,  infill.opt.focussearch.points = 500,
    impute = function(x, y, opt.path) 0, impute.errors = TRUE)
  res = mbo(f2, ps, des=NULL, learner, ctrl, show.info=FALSE)
  # Check for correct error messages
  NA.inds = which(getOptPathY(res$opt.path) == 0)
  for (ind in 1:getOptPathLength(res$opt.path)) {
    if(ind %in% NA.inds) {
      expect_equal(grep("mlrMBO:", getOptPathErrorMessages(res$opt.path)[ind]), 1)
      expect_equal(grep("foo", getOptPathErrorMessages(res$opt.path)[ind]), 1)
    } else 
      expect_equal("", getOptPathErrorMessages(res$opt.path)[ind])
  }
})
