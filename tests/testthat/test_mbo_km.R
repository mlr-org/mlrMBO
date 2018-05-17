context("mbo km")

test_that("mbo works with km", {
  des = testd.fsphere.2d
  des$y = apply(des, 1, testf.fsphere.2d)
  learner = makeLearner("regr.km", nugget.estim = TRUE)
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.mr, opt.focussearch.points = 100L)
  expect_output(print(ctrl), "Objectives")
  or = mbo(testf.fsphere.2d, des, learner = learner, control = ctrl)
  expect_number(or$y)
  expect_equal(getOptPathLength(or$opt.path), 15L)
  df = as.data.frame(or$opt.path)
  expect_numeric(df$x1)
  expect_numeric(df$x2)
  expect_list(or$x)
  expect_set_equal(names(or$x), names(testp.fsphere.2d$pars))
  expect_equal(length(or$models[[1]]$subset), 15L)

  f = testf.fsphere.2d
  par.set = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeIntegerParam("x2", lower = -1, upper = 2)
  )
  f = setAttribute(f, "par.set", par.set)

  des = generateTestDesign(10L, par.set = getParamSet(f))
  des$y = apply(des, 1, f)
  or = mbo(f, des, learner, ctrl)
  expect_number(or$y)
  expect_equal(getOptPathLength(or$opt.path), 15L)
  expect_output(print(or), "Recommended parameters")
  df = as.data.frame(or$opt.path)
  expect_numeric(df$x1)
  expect_integer(df$x2)
  expect_list(or$x)
  expect_set_equal(names(or$x), names(par.set$pars))

  # we use kriging by makeMBOLearner
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.ei,
    opt.focussearch.points = 100L)
  # FIXME: We need suppressPackageStartupMessages to suppress rgenoud startup message, as messages are not cought yet by mlr https://github.com/mlr-org/mlr/pull/2273

  expect_silent(suppressPackageStartupMessages({or = mbo(f, des, control = ctrl, show.info = FALSE)}))
  expect_number(or$y)
  expect_equal(getOptPathLength(or$opt.path), 15)
  df = as.data.frame(or$opt.path)
  expect_numeric(df$x1)
  expect_integer(df$x2)
  expect_list(or$x)
  expect_set_equal(names(or$x), names(par.set$pars))
})


test_that("mbo works with impute and failure model", {
  des = testd.fsphere.2d
  # add same point twice with differnent y-vals - will crash km without nugget for sure
  des = rbind(des, data.frame(x1 = c(0, 0), x2 = c(0, 0)))
  des$y = apply(des, 1, testf.fsphere.2d)
  des$y[nrow(des)] = 123
  # make sure model does not break, and we get a failure model
  learner = makeLearner("regr.km")
  ctrl = makeMBOControl(on.surrogate.error = "quiet")
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.mr, opt.focussearch.points = 10L)
  or = mbo(testf.fsphere.2d, des, learner = learner, control = ctrl)
  expect_equal(getOptPathLength(or$opt.path), 14)
  op = as.data.frame(or$opt.path)
  expect_true(!is.na(op$error.model[13L]))
  expect_true(!is.na(op$error.model[14L]))

  # test with an infill that has infill components
  # https://github.com/mlr-org/mlrMBO/issues/302
  ctrl = setMBOControlInfill(ctrl, crit = crit.ei, opt = "focussearch", opt.focussearch.points = 10L)
  learner = setPredictType(learner, "se")
  or = mbo(testf.fsphere.2d, des, learner = learner, control = ctrl)
  op = as.data.frame(or$opt.path)
  expect_character(as.character(op$error.model[13L]), pattern = "leading minor of order")
  expect_true(all(op$prop.type[13:14] == "random_error"))
})
