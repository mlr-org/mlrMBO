context("mbo kmforrester (reinterpolation)")

test_that("mbo works with Kriging reinterpolation method ", {
  par.set = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeNumericParam("x2", lower = -1, upper = 2)
  )
  f = makeSingleObjectiveFunction(
    fn = function(x) sum(x^2) + rnorm(1, sd = 0.5),
    par.set = par.set
  )
  des = generateTestDesign(10, par.set = getParamSet(f))
  y  = sapply(1:nrow(des), function(i) f(as.list(des[i, ])))
  des$y = y
  learner = makeLearner("regr.kmforrester", predict.type = "se")
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100L)
  or = mbo(f, des, learner = learner, control = ctrl)
  expect_true(!is.na(or$y))
  expect_equal(getOptPathLength(or$opt.path), 15L)
  df = as.data.frame(or$opt.path)
  expect_true(is.numeric(df$x1))
  expect_true(is.numeric(df$x2))
  expect_true(is.list(or$x))
  expect_equal(names(or$x), names(par.set$pars))
  expect_equal(length(or$models[[1]]$subset), 15L)
})
