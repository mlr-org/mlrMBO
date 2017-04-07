context("mbo rf")

test_that("mbo works with rf", {
  learner = makeLearner("regr.randomForest")
  ctrl = makeMBOControl(store.model.at = c(1,5))
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.mr, opt.focussearch.points = 100L)
  or = mbo(testf.fsphere.2d, testd.fsphere.2d, learner, ctrl, show.info = TRUE)
  expect_number(or$y)
  expect_equal(or$y, testf.fsphere.2d(or$x))
  expect_equal(getOptPathLength(or$opt.path), 15)
  expect_list(or$x)
  expect_set_equal(names(or$x), names(testp.fsphere.2d$pars))
  expect_length(or$models[[1]]$subset, 10)
  expect_length(or$models[[2]]$subset, 14) #In the 15th step we used a model based on 14

  # check errors
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.ei, opt.focussearch.points = 100L)
  expect_error(mbo(testf.fsphere.2d, testd.fsphere.2d, learner, ctrl), "must be set to 'se'")
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100L)

  # f2 = makeMBOFunction(function(x) x^2)
  # expect_error(mbo(f2, des, learner, ctrl), "wrong length")

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100L)
  learner = makeLearner("classif.randomForest")
  expect_error(mbo(testf.fsphere.2d, testd.fsphere.2d, learner, ctrl), "mbo requires regression learner")
  learner = makeLearner("regr.randomForest", predict.type = "se", se.method = "sd")
  # check trafo
  par.set = makeParamSet(
    makeNumericParam("x1", lower = -10, upper = 10, trafo = function(x) abs(x))
  )
  f = makeSingleObjectiveFunction(
    fn = function(x) sum(x^2),
    par.set = par.set
  )
  des = generateTestDesign(10L, par.set = par.set)
  des$y  = vnapply(seq_row(des), function(i) f(as.list(des[i, ])))
  or = mbo(f, des, learner, ctrl)
  expect_number(or$y)
  expect_equal(getOptPathLength(or$opt.path), 15)
  df = as.data.frame(or$opt.path)
  expect_numeric(df$x1)

  # discrete par
  par.set = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeIntegerParam("x2", lower = -1, upper = 2),
    makeDiscreteParam("x3", values = c("a", "b"))
  )

  f = makeSingleObjectiveFunction(
    fn = function(x) {
      if (x[[3]] == "a")
        x[[1]]^2+x[[2]]^2
      else
        x[[1]]^2+x[[2]]^2 + 20
    },
    par.set = par.set,
    has.simple.signature = FALSE
  )

  des = generateTestDesign(10, par.set = par.set)
  des$y = vnapply(seq_row(des), function(i) f(as.list(des[i,])))
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100)
  or = mbo(f, des, learner, ctrl)
  expect_number(or$y)
  expect_equal(getOptPathLength(or$opt.path), 15)
  df = as.data.frame(or$opt.path)
  expect_numeric(df$x1, any.missing = FALSE)
  expect_integer(df$x2, any.missing = FALSE)
  expect_factor(df$x3, any.missing = FALSE)
  expect_numeric(df$y, any.missing = FALSE)
  expect_list(or$x)
  expect_set_equal(names(or$x), names(par.set$pars))

  # check best.predicted
  ctrl = makeMBOControl(final.method = "best.predicted")
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100)
  or = mbo(f, des, learner, ctrl)
  expect_number(or$y)
  expect_equal(getOptPathLength(or$opt.path), 15)

  des = generateTestDesign(10L, getParamSet(f))
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100)
  or = mbo(f, des, learner, ctrl)
  expect_number(or$y)
  expect_equal(getOptPathLength(or$opt.path), 15)
  expect_set_equal(names(or$x), names(par.set$pars))

  # check cmaes
  par.set = makeParamSet(
    makeNumericVectorParam("v", lower = -5, upper = 5, len = 2),
    makeNumericParam("w", lower = -5, upper = 5)
  )
  f = makeSingleObjectiveFunction(
    fn = function(x) sum(x[[1]]^2) + (2 - x[[2]])^2,
    par.set = par.set
  )

  des = generateTestDesign(10L, getParamSet(f))
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 3L)
  ctrl = setMBOControlInfill(ctrl, opt = "cmaes", opt.cmaes.control = list(maxit = 2L))
  or = mbo(f, des, learner, ctrl)
  expect_number(or$y)
  expect_equal(getOptPathLength(or$opt.path), 10 + 3)
  des = generateTestDesign(10L, getParamSet(f))
  ctrl = makeMBOControl(final.method = "best.predicted")
  ctrl = setMBOControlTermination(ctrl, iters = 3L)
  ctrl = setMBOControlInfill(ctrl, opt = "cmaes", opt.cmaes.control = list(maxit = 2L))
  or = mbo(f, des, learner, ctrl)
  expect_number(or$y)
  expect_equal(getOptPathLength(or$opt.path), 10 + 3)

  # check more.args
  par.set = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeNumericParam("x2", lower = -1, upper = 2)
  )

  f = makeSingleObjectiveFunction(
    fn = function(x, shift = 0) {
      sum(x^2) + shift
    },
    par.set = par.set
  )

  des = generateTestDesign(10L, getParamSet(f))
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100L)
  or = mbo(f, des, learner, ctrl, more.args = list(shift = 0))
  expect_number(or$y)

  # check y transformation before modelling
  par.set = makeParamSet(
    makeNumericParam("x1", lower = -1, upper = 1)
  )
  f = makeSingleObjectiveFunction(
    fn = function(x, shift = 0) {
      sum(x^2) + shift
    },
    par.set = par.set
  )

  des = generateTestDesign(10L, getParamSet(f))
  ctrl = makeMBOControl(trafo.y.fun = trafoSqrt(handle.violations = "error"))
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  ctrl = setMBOControlInfill(ctrl, filter.proposed.points = TRUE)
  expect_error(mbo(f, control = ctrl, more.args = list(shift = -1)), "Negative function values occurred during transformation.")
  or = mbo(f, des, control = ctrl, more.args = list(shift = 0))
  expect_number(or$y)
})

# FIXME: we do we get so bad results with so many models for this case?
# analyse visually.
# FIXME: this test is also MUCH too slow
# test_that("mbo works with logicals", {
#   f = function(x) {
#     if (x$a)
#       sum(x$b^2)
#     else
#       sum(x$b^2) + 10
#   }

#   ps = makeParamSet(
#     makeLogicalParam("a"),
#     makeNumericVectorParam("b", len = 2, lower = -3, upper = 3)
#   )
#   learner = makeBaggingWrapper(makeLearner("regr.gbm"), bag.iters = 10)
#   learner = setPredictType(learner, "se")
#   ctrl = makeMBOControl(init.design.points = 50, iters = 10)
#   ctrl = setMBOControlInfill(ctrl, crit = crit.ei, opt = "focussearch",
#     opt.restarts = 3, opt.focussearch.maxit = 3, opt.focussearch.points = 5000)
#   or = mbo(f, ps, learner = learner, control = ctrl, show.info = TRUE)
#   expect_true(!is.na(or$y))
#   expect_equal(getOptPathLength(or$opt.path), 60)
#   expect_equal(or$x$a, TRUE)
#   expect_true(sum(or$x$b^2) < 1)
# })
