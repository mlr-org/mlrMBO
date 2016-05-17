context("asyn MBO")
options(mlrMBO.debug.mode = FALSE)

test_that("asyn MBO works", {
  save.file = file.path(tempdir(), "mbo_asyn", "mbo.RData")
  ctrl = makeMBOControl(schedule.method = "asyn", save.file.path = save.file)
  ctrl = setMBOControlTermination(ctrl, iters = 15L, max.evals = 15L)
  ctrl = setMBOControlInfill(control = ctrl, crit = "cb", crit.cb.lambda = 2, opt.focussearch.maxit = 2L, opt.focussearch.points = 50L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cb")
  surrogat.learner = makeLearner("regr.randomForest", predict.type = "se", ntree = 50, ntree.for.se = 20)
  if (interactive()) parallelMap::parallelStartMulticore(4)
  or = mbo(fun = testf.fsphere.2d, design = testd.fsphere.2d, learner = surrogat.learner, control = ctrl)
  if (interactive()) parallelMap::parallelStop()
  unlink(dirname(save.file), recursive = TRUE, force = TRUE)
  op.df = as.data.frame(or$opt.path)
  expect_true(nrow(op.df) >= 15)
  expect_true(all(!is.na(op.df$train.time[11:15])))
  expect_true(all(!is.na(op.df$multipoint.cb.lambda[11:15])))
  expect_equal(op.df$dob[11:15], 1:5)
})

test_that("asyn MBO works with CL", {
  save.file = file.path(tempdir(), "mbo_asyn", "mbo.RData")
  ctrl = makeMBOControl(schedule.method = "asyn", save.file.path = save.file)
  ctrl = setMBOControlTermination(ctrl, iters = 15L, max.evals = 15L)
  ctrl = setMBOControlInfill(control = ctrl, crit = "ei", opt.focussearch.maxit = 2L, opt.focussearch.points = 50L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cl")
  surrogat.learner = makeLearner("regr.km", predict.type = "se")
  if (interactive()) parallelMap::parallelStartMulticore(4)
  or = mbo(fun = testf.fsphere.2d, design = testd.fsphere.2d, learner = surrogat.learner, control = ctrl)
  if (interactive()) parallelMap::parallelStop()
  unlink(dirname(save.file), recursive = TRUE, force = TRUE)
  op.df = as.data.frame(or$opt.path)
  expect_true(nrow(op.df) >= 15)
  expect_true(all(!is.na(op.df$train.time[11:15])))
  expect_true(all(!is.na(op.df$ei[11:15])))
  expect_equal(op.df$dob[11:15], 1:5)
})

test_that("asyn MBO works with mboContinue", {
  # make sure there is no or - object in the environment
  or = NULL
  assign(".counter", 0L, envir = .GlobalEnv)
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      .counter = get(".counter", envir = .GlobalEnv)
      assign(".counter", .counter + 1L, envir = .GlobalEnv)
      if (.counter == 8)
        stop("foo")
      sum(x$x^2)
    },
    par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1),
    has.simple.signature = FALSE
  )

  learner = makeLearner("regr.randomForest", predict.type = "se", ntree = 50, ntree.for.se = 20)
  save.file = file.path(tempdir(), "mbo_asyn", "mbo.RData")
  dir.create(dirname(save.file))
  des = generateTestDesign(6L, smoof::getParamSet(f))
  ctrl = makeMBOControl(schedule.method = "asyn", save.file.path = save.file, save.on.disk.at = 0:5)
  ctrl = setMBOControlTermination(ctrl, iters = 4L)
  ctrl = setMBOControlInfill(control = ctrl, crit = "cb", crit.cb.lambda = 2, opt.focussearch.maxit = 2L, opt.focussearch.points = 50L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cb")
  #or = mbo(f, des, learner = learner, control = ctrl)
  expect_error({or = mbo(f, des, learner = learner, control = ctrl)}, "foo")
  for (i in 1:10) {
    try({or = mboContinue(save.file)}, silent = TRUE)
    if (!is.null(or))
      break
  }
  expect_equal(getOptPathLength(or$opt.path), 10)
  expect_equal(getOptPathDOB(or$opt.path), c(rep(0,6),1:4))
  expect_class(or, c("MBOSingleObjResult", "MBOResult"))
  unlink(dirname(save.file), recursive = TRUE)
})
