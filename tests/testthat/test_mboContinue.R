context("mboContinue")
# Here we want to leave the debug-mode to test the saving
options(mlrMBO.debug.mode = FALSE)

test_that("mboContinue", {
  # make sure there is no or - object in the environment
  if (exists("or"))
    rm("or")
  f = function(x) {
    i <<- i + 1
    if (i > 12)
      if (rbinom(1, 1, 0.5))
        stop("foo")
    sum(x^2)
  }
  environment(f) <- new.env()
  environment(f)$i <- 0

  f = makeMBOFunction(f)
  ps = makeNumericParamSet(len = 2L, lower = -2, upper = 1)

  # First test sombo
  learner = makeLearner("regr.rpart")
  save.file = tempfile(fileext = "RData")
  ctrl = makeMBOControl(iters = 7, save.on.disk.at = 0:8,
    save.file.path = save.file, init.design.points = 10L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10)
  expect_error(or <- mbo(f, ps, learner = learner, control = ctrl, show.info = FALSE), "foo")
  for (i in 1:100) {
    try(or <- mboContinue(save.file), silent = TRUE)
    if (exists("or"))
      break
  }
  expect_equal(getOptPathLength(or$opt.path), 17)

  # now test parEGO
  f = function(x) {
    i <<- i + 1
    if (i > 12)
      if (rbinom(1, 1, 0.5))
        stop ("foo")
    c(sum(x^2), prod(x^2))
  }
  environment(f) <- new.env()
  environment(f)$i <- 0

  f = makeMBOFunction(f)
  ctrl = makeMBOControl(iters = 7, save.on.disk.at = 0:8,
    save.file.path = save.file, init.design.points = 10L, number.of.targets = 2)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100)
  ctrl = setMBOControlMultiCrit(ctrl, parego.s = 100)
  rm("or")
  try(or <- mbo(f, ps, learner = learner, control = ctrl, show.info = FALSE), silent = TRUE)
  for (i in 1:100) {
    try(or <- mboContinue(save.file), silent = TRUE)
    if (exists("or"))
      break
  }
  expect_equal(getOptPathLength(or$opt.path), 17)
})


test_that("mboContinue works when we at end", {
  f = function(x) sum(x^2)
  f = makeMBOFunction(f)
  ps = makeNumericParamSet(len = 2L, lower = -2, upper = 1)
  learner = makeLearner("regr.rpart")
  save.file = tempfile(fileext = "RData")
  ctrl = makeMBOControl(iters = 1, save.on.disk.at = 0:2,
    save.file.path = save.file, init.design.points = 10L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10)
  or = mbo(makeMBOFunction(f), ps, learner = learner, control = ctrl, show.info = FALSE)
  expect_equal(getOptPathLength(or$opt.path), 11L)
  or = mboContinue(save.file)
  expect_equal(getOptPathLength(or$opt.path), 11L)
})

# reenter debug-mode
options(mlrMBO.debug.mode = TRUE)
