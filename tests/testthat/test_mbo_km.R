context("mbo km")

test_that("mbo works with km", {
  f = makeMBOFunction(function(x) sum(x^2))

  ps = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeNumericParam("x2", lower = -1, upper = 2)
  )
  des = generateDesign(10, par.set = ps)
  y  = sapply(1:nrow(des), function(i) f(as.list(des[i, ])))
  des$y = y
  learner = makeLearner("regr.km", nugget.estim = TRUE)
  ctrl = makeMBOControl(iters = 5)
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

  ps = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeIntegerParam("x2", lower = -1, upper = 2)
  )
  des = generateDesign(10, par.set = ps)
  des$y  = sapply(1:nrow(des), function(i) f(as.list(des[i, ])))
  or = mbo(f, ps, des, learner, ctrl)
  expect_true(!is.na(or$y))
  expect_equal(getOptPathLength(or$opt.path), 15)
  df = as.data.frame(or$opt.path)
  expect_true(is.numeric(df$x1))
  expect_true(is.integer(df$x2))
  expect_true(is.list(or$x))
  expect_equal(names(or$x), names(ps$pars))

  learner = setPredictType(learner, "se")
  ctrl = makeMBOControl(iters = 5)
  ctrl = setMBOControlInfill(ctrl, crit = "ei", opt.focussearch.points = 100)
  or = mbo(f, ps, des, learner, ctrl)
  expect_true(!is.na(or$y))
  expect_equal(getOptPathLength(or$opt.path), 15)
  df = as.data.frame(or$opt.path)
  expect_true(is.numeric(df$x1))
  expect_true(is.integer(df$x2))
  expect_true(is.list(or$x))
  expect_equal(names(or$x), names(ps$pars))
})


test_that("mbo works with impute and failure model", {
  f = makeMBOFunction(function(x) sum(x^2))

  ps = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeNumericParam("x2", lower = -1, upper = 2)
  )
  des = generateDesign(10, par.set = ps)
  # add same point twice with differnent y-vals - will crash km without nugget for sure
  des = rbind(des, data.frame(x1 = c(0, 0), x2 = c(0, 0)))
  y  = sapply(1:nrow(des), function(i) f(as.list(des[i, ])))
  y[length(y)] = 123
  des$y = y
  learner = makeLearner("regr.km")
  ctrl = makeMBOControl(iters = 2)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10)
  suppressWarnings({
    or = mbo(f, ps, des, learner = learner, control = ctrl)
  })
  expect_equal(getOptPathLength(or$opt.path), 14)
  op = as.data.frame(or$opt.path)
  expect_true(!is.na(op$error.model[13L]))
  expect_true(!is.na(op$error.model[14L]))
})
