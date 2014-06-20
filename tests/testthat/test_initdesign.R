context("init design")

test_that("init design", {
  f = makeMBOFunction(function(x) sum(x^2))

  # without trafo
  ps = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeNumericParam("x2", lower = -1, upper = 2)
  )

  learner = makeLearner("regr.km", nugget.estim = TRUE)
  ctrl = makeMBOControl(iters = 1)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10)

  des = generateDesign(10, par.set = ps)

  or = mbo(f, ps, des, learner, ctrl)
  expect_equal(getOptPathLength(or$opt.path), 11L)
  expect_true(!any(is.na(or$y)))

  # with precomputed y
  des$y = apply(des, 1, f)
  or = mbo(f, ps, des, learner, ctrl)
  expect_equal(getOptPathLength(or$opt.path), 11L)
  expect_true(!any(is.na(or$y)))


  # with trafo
  ps = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeNumericParam("x2", lower = -1, upper = 2, trafo = function(x) x/2)
  )

  des = generateDesign(10, ps, trafo = TRUE)
  expect_error(mbo(f, ps, des, learner, ctrl))

  # wrong col names in inir design
  des = data.frame(x1 = c(-0.49, 0.24, -1.47), z = c(-0.70, 1.29, 0.73))
  expect_error(mbo(f, ps, des, learner, ctrl))
})

