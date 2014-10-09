context("mbo kmforrester (reinterpolation)")

test_that("mbo works with Kriging reinterpolation method ", {

  f = makeMBOFunction(function(x) sum(x^2) + rnorm(1, sd = 0.5))

  ps = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeNumericParam("x2", lower = -1, upper = 2)
  )
  des = generateDesign(10, par.set = ps)
  y  = sapply(1:nrow(des), function(i) f(as.list(des[i, ])))
  des$y = y
  learner = makeLearner("regr.kmforrester", predict.type = "se")
  ctrl = makeMBOControl(iters = 5, noisy = TRUE)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100)
  or = mbo(f, ps, des, learner = learner, control = ctrl)
  expect_true(!is.na(or$y))
  expect_equal(getOptPathLength(or$opt.path), 15)
  df = as.data.frame(or$opt.path)
  expect_true(is.numeric(df$x1))
  expect_true(is.numeric(df$x2))
  expect_true(is.list(or$x))
  expect_equal(names(or$x), names(ps$pars))
  expect_equal(length(or$models[[1]]$subset), 15)
})
