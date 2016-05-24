context("asyn MBO")
options(mlrMBO.debug.mode = FALSE)

test_that("asyn MBO works", {
  save.file = file.path(tempdir(), "mbo_asyn", "mbo.RData")
  ctrl = makeMBOControl(schedule.method = "asyn", save.file.path = save.file, schedule.nodes = 2, propose.points = 2)
  ctrl = setMBOControlTermination(ctrl, iters = 4L, max.evals = 14L)
  ctrl = setMBOControlInfill(control = ctrl, crit = "cb", crit.cb.lambda = 2, opt.focussearch.maxit = 2L, opt.focussearch.points = 50L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cb")
  surrogat.learner = makeLearner("regr.randomForest", predict.type = "se", ntree = 50, ntree.for.se = 20)
  parallelMap::parallelStartMulticore(2)
  or = mbo(fun = testf.fsphere.2d, design = testd.fsphere.2d, learner = surrogat.learner, control = ctrl)
  parallelMap::parallelStop()
  unlink(dirname(save.file), recursive = TRUE, force = TRUE)
  op.df = as.data.frame(or$opt.path)
  expect_true(nrow(op.df) >= 14)
  #expect_true(all(!is.na(op.df$train.time[11:15])))
  expect_true(all(!is.na(op.df$multipoint.cb.lambda[11:14])))
  expect_equal(op.df$dob[11:12], 1:2)
  expect_true(all(!is.na(op.df$scheduled.on[11:14])))
})

test_that("asyn MBO works with CL", {
  fn.sleep = makeSingleObjectiveFunction(
    fn = function(x) {
      Sys.sleep(1 + 2 * Sys.getpid()%%2)
      sum(x^2)
    },
    par.set = makeNumericParamSet(len = 2, lower = -2, upper = 2),
    has.simple.signature = TRUE
  )
  des = generateDesign(n = 10, par.set = smoof::getParamSet(fn.sleep))
  des$y = apply(des, 1, function(x) sum(x^2))
  save.file = file.path(tempdir(), "mbo_asyn", "mbo.RData")
  ctrl = makeMBOControl(schedule.method = "asyn", save.file.path = save.file, propose.points = 2, schedule.nodes = 2, asyn.wait.for.proposals = FALSE, asyn.filter.proposals = TRUE)
  ctrl = setMBOControlTermination(ctrl, time.budget = 10L)
  ctrl = setMBOControlInfill(control = ctrl, crit = "ei", opt.focussearch.maxit = 2L, opt.focussearch.points = 50L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cl")
  surrogat.learner = makeLearner("regr.km", predict.type = "se")
  parallelMap::parallelStartMulticore(2)
  or = mbo(fun = fn.sleep, design = des, learner = surrogat.learner, control = ctrl)
  parallelMap::parallelStop()
  unlink(dirname(save.file), recursive = TRUE, force = TRUE)
  op.df = as.data.frame(or$opt.path)
  expect_true(nrow(op.df) >= 15)
  #expect_true(all(!is.na(op.df$train.time[11:15]))) #FIXME Why is this missing now?
  expect_true(all(!is.na(op.df$ei[11:15])))
  expect_true(all(!is.na(op.df$scheduled.on[11:15])))
  expect_true(all(!is.na(op.df$dob[11:15])))
  expect_equal(op.df$dob[11:13], c(1,1,2))
  expect_true(sum(op.df$exec.time[11:15]>=3)>=2)
})

test_that("asyn MBO works with mboContinue", {
  # make sure there is no or - object in the environment
  save.file = file.path(tempdir(), "mbo_asyn", "mbo.RData")
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
