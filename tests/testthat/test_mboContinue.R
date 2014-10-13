context("mboContinue")

if (interactive()) {

# Here we want to leave the debug-mode to test the saving
options(mlrMBO.debug.mode = FALSE)

test_that("mboContinue", {
  # make sure there is no or - object in the environment
  or = NULL
  assign(".counter", 0L, envir = .GlobalEnv)
  f = function(x) {
    .counter = get(".counter", envir = .GlobalEnv)
    assign(".counter", .counter + 1L, envir = .GlobalEnv)
    if (.counter == 11)
      stop("foo")
    sum(x$x^2)
  }

  ps = makeNumericParamSet(len = 2L, lower = -2, upper = 1)

  # First test sombo
  learner = makeLearner("regr.rpart")
  save.file = tempfile(fileext = ".RData")
  ctrl = makeMBOControl(iters = 3, save.on.disk.at = 0:4,
    save.file.path = save.file, init.design.points = 10L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10)
  expect_error(or <- mbo(f, ps, learner = learner, control = ctrl), "foo")
  for (i in 1:20) {
    try(or <- mboContinue(save.file), silent = F)
    if (!is.null(or))
      break
  }
  expect_equal(getOptPathLength(or$opt.path), 13)
  unlink(save.file)

  # now test parEGO
  assign(".counter", 0L, envir = .GlobalEnv)
  f = function(x) {
    .counter <<- .counter + 1
    if (.counter  == 13L)
      stop ("foo")
    c(sum(x^2), prod(x^2))
  }

  f = makeMBOFunction(f)
  ctrl = makeMBOControl(iters = 7, save.on.disk.at = 0:8,
    save.file.path = save.file, init.design.points = 10L, number.of.targets = 2)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100)
  ctrl = setMBOControlMultiCrit(ctrl, method = "parego", parego.s = 100)
  or = NULL
  try(or <- mbo(f, ps, learner = learner, control = ctrl), silent = TRUE)
  for (i in 1:10) {
    suppressWarnings(try(or <- mboContinue(save.file), silent = TRUE))
    if (!is.null(or))
      break
  }
  expect_equal(getOptPathLength(or$opt.path), 17)
})


test_that("mboContinue works when we at end", {
  f = function(x) sum(x^2)
  f = makeMBOFunction(f)
  ps = makeNumericParamSet(len = 2L, lower = -2, upper = 1)
  learner = makeLearner("regr.rpart")
  save.file = tempfile(fileext = ".RData")
  ctrl = makeMBOControl(iters = 1L, save.on.disk.at = 0:2,
    save.file.path = save.file, init.design.points = 10L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10)
  or = mbo(makeMBOFunction(f), ps, learner = learner, control = ctrl)
  expect_equal(getOptPathLength(or$opt.path), 11L)
  expect_warning({or = mboContinue(save.file)}, "No need to continue")
  expect_equal(getOptPathLength(or$opt.path), 11L)
})

# reenter debug-mode
options(mlrMBO.debug.mode = TRUE)

}
