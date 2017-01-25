context("mboFinalize")

# Here we want to leave the debug-mode to test the saving
options(mlrMBO.debug.mode = FALSE)

test_that("mboFinalize", {
  # make sure there is no or - object in the environment
  or = NULL
  assign(".counter", 0L, envir = .GlobalEnv)
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      .counter = get(".counter", envir = .GlobalEnv)
      assign(".counter", .counter + 1L, envir = .GlobalEnv)
      if (.counter == 12)
        stop("foo")
      sum(x^2)
    },
    par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1)
  )

  # First test smbo
  learner = makeLearner("regr.rpart")
  save.file = tempfile("state", fileext=".RData")
  ctrl = makeMBOControl(save.on.disk.at = 0:4,
    save.file.path = save.file)
  ctrl = setMBOControlTermination(ctrl, iters = 3L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.mr, opt.focussearch.points = 10L)
  des = generateTestDesign(10L, getParamSet(f))
  expect_error(or <- mbo(f, des, learner = learner, control = ctrl), "foo")
  or = mboFinalize(save.file)
  expect_equal(getOptPathLength(or$opt.path), 12L)
  unlink(save.file)

  # now test parEGO
  assign(".counter", 0L, envir = .GlobalEnv)
  f = makeMultiObjectiveFunction(
    fn = function(x) {
      .counter <<- .counter + 1
      if (.counter  == 13L)
        stop ("foo")
      c(sum(x^2), prod(x^2))
    },
    n.objectives = 2L,
    par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1)
  )

  des = generateTestDesign(10L, getParamSet(f))
  ctrl = makeMBOControl(save.on.disk.at = 0:8,
    save.file.path = save.file, n.objectives = 2L)
  ctrl = setMBOControlTermination(ctrl, iters = 7L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.mr, opt.focussearch.points = 100L)
  ctrl = setMBOControlMultiObj(ctrl, method = "parego", parego.s = 100L)
  or = NULL
  expect_error(or <- mbo(f, des, learner = learner, control = ctrl), "foo")
  or = mboFinalize(save.file)
  expect_equal(getOptPathLength(or$opt.path), 12L)
  unlink(save.file)
})


test_that("mboFinalize works when at end", {
  f = makeSingleObjectiveFunction(
    fn = function(x) sum(x^2),
    par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1)
  )
  learner = makeLearner("regr.rpart")
  save.file = tempfile(fileext = ".RData")
  des = generateTestDesign(10L, getParamSet(f))
  ctrl = makeMBOControl(save.on.disk.at = 0:2,
    save.file.path = save.file)
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.mr, opt.focussearch.points = 10L)
  or = mbo(f, des, learner = learner, control = ctrl)
  expect_equal(getOptPathLength(or$opt.path), 11L)
  expect_warning({or = mboFinalize(save.file)}, "No need to finalize")
  expect_equal(getOptPathLength(or$opt.path), 11L)
  unlink(save.file)
})

# reenter debug-mode
options(mlrMBO.debug.mode = TRUE)
