context("mbo")

test_that("trafo works", {
  f = makeMBOFunction(function(x) {
    stopifnot(x >= 0)
    return(x)
  })
  ps = makeParamSet(
    makeNumericParam("x", lower = -15, upper = 0, trafo = function(x) -x)
  )
  learner = makeLearner("regr.randomForest")
  ctrl = makeMBOControl(iters = 5)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100)
  or = mbo(f, par.set = ps, learner = learner, control = ctrl, show.info = TRUE)
  op = as.data.frame(or$opt.path)
  op$x == -1 * op$y
})
