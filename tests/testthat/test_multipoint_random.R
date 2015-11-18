context("multipoint random")

test_that("multipoint random", {
  set.seed(1)
  objfun = function(x) {
    y = sum(x^2)
  }
  ps = makeNumericParamSet(len = 1L, lower = -1, upper = 1)
  lrn = makeLearner("regr.randomForest", predict.type = "se", ntree = 10)

  ctrl = makeMBOControl(init.design.points = 10L, iters = 1L, propose.points = 20L)
  ctrl = setMBOControlInfill(ctrl, crit = "random")
  ctrl = setMBOControlMultiPoint(ctrl, method = "random")

  res = mbo(makeMBOFunction(objfun), par.set = ps, learner = lrn, control = ctrl)
  op = as.data.frame(res$opt.path)
  expect_true(res$y < 0.1)
  expect_true(nrow(op) == 30)
  expect_true(sum(op$dob) == 20)
  expect_is(res, "MBOResult")
})
