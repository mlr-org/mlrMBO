context("multipoint constant liar")

test_that("multipoint constant liar", {
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      sum(x^2)
    },
    par.set = makeNumericParamSet(len = 1L, lower = -1, upper = 1)
  )

  lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

  ctrl = makeMBOControl(init.design.points = 30L, propose.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, crit = "ei")
  ctrl = setMBOControlMultiPoint(ctrl, method = "cl")

  res = mbo(f, learner = lrn, control = ctrl)
  expect_is(res, "MBOResult")
  expect_true(res$y < 0.1)
})
