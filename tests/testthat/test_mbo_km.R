context("mbo km")

test_that("mbo works with km", {
  par.set = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeNumericParam("x2", lower = -1, upper = 2)
  )
  f = makeSingleObjectiveFunction(
    fn = function(x) sum(x^2),
    par.set = par.set
  )
  des = generateDesign(10, par.set = smoof::getParamSet(f))
  y = sapply(1:nrow(des), function(i) f(as.list(des[i, ])))
  des$y = y
  learner = makeLearner("regr.km", nugget.estim = TRUE)
  ctrl = makeMBOControl(iters = 5L)
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

  par.set = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeIntegerParam("x2", lower = -1, upper = 2)
  )
  f = setAttribute(f, "par.set", par.set)

  des = generateDesign(10L, par.set = smoof::getParamSet(f))
  des$y  = sapply(1:nrow(des), function(i) f(as.list(des[i, ])))
  or = mbo(f, des, learner, ctrl)
  expect_true(!is.na(or$y))
  expect_equal(getOptPathLength(or$opt.path), 15L)
  df = as.data.frame(or$opt.path)
  expect_true(is.numeric(df$x1))
  expect_true(is.integer(df$x2))
  expect_true(is.list(or$x))
  expect_equal(names(or$x), names(par.set$pars))

  learner = setPredictType(learner, "se")
  ctrl = makeMBOControl(iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = "ei", opt.focussearch.points = 100L)
  or = mbo(f, des, learner, ctrl)
  expect_true(!is.na(or$y))
  expect_equal(getOptPathLength(or$opt.path), 15)
  df = as.data.frame(or$opt.path)
  expect_true(is.numeric(df$x1))
  expect_true(is.integer(df$x2))
  expect_true(is.list(or$x))
  expect_equal(names(or$x), names(par.set$pars))
})


test_that("mbo works with impute and failure model", {
  f = makeSingleObjectiveFunction(
    fn = function(x) sum(x^2),
    par.set = makeParamSet(
      makeNumericParam("x1", lower = -2, upper = 1),
      makeNumericParam("x2", lower = -1, upper = 2)
    )
  )
  des = generateDesign(10, par.set = smoof::getParamSet(f))
  # add same point twice with differnent y-vals - will crash km without nugget for sure
  des = rbind(des, data.frame(x1 = c(0, 0), x2 = c(0, 0)))
  y  = sapply(1:nrow(des), function(i) f(as.list(des[i, ])))
  y[length(y)] = 123
  des$y = y
  # make sure model does not break, and we get a failure model
  learner = makeLearner("regr.km", config = list(on.learner.error = "quiet"))
  ctrl = makeMBOControl(iters = 2L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10L)
  or = mbo(f, des, learner = learner, control = ctrl)
  expect_equal(getOptPathLength(or$opt.path), 14)
  op = as.data.frame(or$opt.path)
  expect_true(!is.na(op$error.model[13L]))
  expect_true(!is.na(op$error.model[14L]))
})
