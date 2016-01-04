context("multipoint infilldistributed")

test_that("multipoint infilldistributed", {
  set.seed(123)
  objfun = function(x) {
    if (x$cat == "a") {
      return(-1 * (-1 * x$x1^2 + 1) * x$x1 %btwn% c(-1,1))
    } else if (x$cat == "b") {
      return(-1 * (-1 * (x$x2+2)^2+1) * x$x2 %btwn% c(-3,-1)) 
    }
  }
  ps = makeParamSet(
    makeDiscreteParam(id = "cat", values = c("a", "b")),
    makeNumericParam(id = "x1", lower = 6, upper = 12, trafo = function(x) x-9, requires = quote(cat == "a")),
    makeNumericParam(id = "x2", lower = -4, upper = 0, requires = quote(cat == "b"))
  )
  lrn = makeImputeWrapper(makeLearner("regr.randomForest", predict.type = "se"), classes = list(numeric = imputeConstant(100)))

  ctrl = makeMBOControl(init.design.points = 10L, iters = 1L, propose.points = 10L)
  ctrl = setMBOControlInfill(ctrl, crit = "lcb", opt = "focussearch", opt.focussearch.points = 100L, opt.focussearch.maxit = 2L, crit.lcb.lambda = 2)
  ctrl = setMBOControlMultiPoint(ctrl, method = "infilldistributed")

  res = mbo(objfun, par.set = ps, learner = lrn, control = ctrl)
  op = as.data.frame(res$opt.path)
  expect_true(mean(op[op$cat == "a" & op$dob > 0, "x1"]) %btwn% c(7,11))
  expect_true(mean(op[op$cat == "b" & op$dob > 0, "x2"]) %btwn% c(-3,-1))
  expect_true(res$y < -0.9)
  expect_equal(getOptPathLength(res$opt.path), 20L)
})
