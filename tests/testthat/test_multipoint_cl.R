context("multipoint constant liar")

test_that("multipoint constant liar", {
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      sum(x^2)
    },
    par.set = makeNumericParamSet(len = 1L, lower = -1, upper = 1)
  )

  lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

  des = generateTestDesign(30L, smoof::getParamSet(f))
  ctrl = makeMBOControl(propose.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, crit = "ei")
  ctrl = setMBOControlMultiPoint(ctrl, method = "cl")

  res = mbo(f, des, learner = lrn, control = ctrl)
  expect_is(res, "MBOResult")
  expect_true(res$y < 0.1)

  #With mean prediction as lie
  ctrl = setMBOControlMultiPoint(ctrl, method = "cl", mean.liar = TRUE)
  res = mbo(f, des, learner = lrn, control = ctrl)
  expect_is(res, "MBOResult")
  expect_true(res$y < 0.1)
})
