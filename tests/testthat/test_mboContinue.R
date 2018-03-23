context("mboContinue")

# Here we want to leave the debug-mode to test the saving
options(mlrMBO.debug.mode = FALSE)

test_that("mboContinue", {
  # make sure there is no or - object in the environment
  or = NULL
  assign(".counter", 0L, envir = .GlobalEnv)
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      .counter = get(".counter", envir = .GlobalEnv)
      assign(".counter", .counter + 1L, envir = .GlobalEnv)
      if (.counter == 11)
        stop("foo")
      sum(x$x^2)
    },
    par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1),
    has.simple.signature = FALSE
  )

  # First test smbo
  learner = makeLearner("regr.rpart")
  save.file = tempfile("state", fileext=".RData")
  des = generateTestDesign(10L, getParamSet(f))
  ctrl = makeMBOControl(save.on.disk.at = 0:4,
    save.file.path = save.file)
  ctrl = setMBOControlTermination(ctrl, iters = 3L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.mr, opt.focussearch.points = 10L)
  expect_error({or = mbo(f, des, learner = learner, control = ctrl)}, "foo")
  for (i in 1:20) {
    try({or = mboContinue(save.file)}, silent = TRUE)
    if (!is.null(or))
      break
  }
  expect_equal(getOptPathLength(or$opt.path), 13)
  expect_class(or, c("MBOSingleObjResult", "MBOResult"))
  opt.state = load2(save.file)
  expect_output(print(opt.state), "OptSate")
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
    par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1),
    n.objectives = 2L
  )

  des = generateTestDesign(10L, getParamSet(f))
  ctrl = makeMBOControl(save.on.disk.at = 0:8,
    save.file.path = save.file, n.objectives = 2L)
  ctrl = setMBOControlTermination(ctrl, iters = 7L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.mr, opt.focussearch.points = 100)
  ctrl = setMBOControlMultiObj(ctrl, method = "parego", parego.s = 100)
  or = NULL
  try({or = mbo(f, des, learner = learner, control = ctrl)}, silent = TRUE)
  for (i in 1:10) {
    suppressWarnings(try({or = mboContinue(save.file)}, silent = TRUE))
    if (!is.null(or))
      break
  }
  expect_equal(getOptPathLength(or$opt.path), 17)
  expect_class(or, c("MBOMultiObjResult", "MBOResult"))
  unlink(save.file)
})


test_that("mboContinue works when at end", {
  f = testf.fsphere.2d
  learner = makeLearner("regr.rpart")
  save.file = tempfile(fileext = ".RData")
  des = generateTestDesign(10L, getParamSet(f))
  ctrl = makeMBOControl(save.on.disk.at = 0:2,
    save.file.path = save.file)
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.mr, opt.focussearch.points = 10L)
  or = mbo(f, des, learner = learner, control = ctrl)
  expect_equal(getOptPathLength(or$opt.path), 11L)
  expect_warning({or = mboContinue(save.file)}, "No need to continue")
  expect_equal(getOptPathLength(or$opt.path), 11L)
  unlink(save.file)
})

# reenter debug-mode
options(mlrMBO.debug.mode = TRUE)
