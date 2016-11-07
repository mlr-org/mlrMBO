context("asyn MBO")
options(mlrMBO.debug.mode = FALSE)
options(parallelMap.logging = FALSE)

test_that("asyn MBO works", {
  skip_on_appveyor()
  save.file = file.path(tempdir(), "mbo_asyn", "mbo.RData")
  ctrl = makeMBOControl(schedule.method = "asyn", save.file.path = save.file, schedule.nodes = 2, propose.points = 1, asyn.wait.for.proposals = TRUE)
  ctrl = setMBOControlTermination(ctrl, iters = 4L, max.evals = 14L)
  ctrl = setMBOControlInfill(control = ctrl, crit = "cb", crit.cb.lambda = 2, opt.focussearch.maxit = 2L, opt.focussearch.points = 50L)
  parallelMap::parallelStartMulticore(2, level = "mlrMBO.asyn")
  or = mbo(fun = testf.fsphere.2d, design = testd.fsphere.2d, control = ctrl)
  parallelMap::parallelStop()
  unlink(dirname(save.file), recursive = TRUE, force = TRUE)
  op.df = as.data.frame(or$opt.path)
  expect_true(nrow(op.df) >= 14)
  #expect_true(all(!is.na(op.df$train.time[11:15])))
  expect_true(all(!is.na(op.df$lambda[11:14])))
  expect_equal(op.df$dob[11:12], 1:2)
  expect_true(all(!is.na(op.df$scheduled.on[11:14])))
})

test_that("asyn MBO works with CL", {
  skip_on_appveyor()
  imp.methods = c("min", "max", "mean", "noisymean", "mc", "quantilemean")
  
  for (imp.method in imp.methods) {
    fn.sleep = makeSingleObjectiveFunction(
      fn = function(x) {
        Sys.sleep(1 + Sys.getpid()%%4)
        sum(x^2)
      },
      par.set = makeNumericParamSet(len = 2, lower = -2, upper = 2),
      has.simple.signature = TRUE)
    des = generateDesign(n = 10, par.set = getParamSet(fn.sleep))
    des$y = apply(des, 1, function(x) sum(x^2))
    save.file = file.path(tempdir(), "mbo_asyn", "mbo.RData")
    ctrl = makeMBOControl(schedule.method = "asyn", save.file.path = save.file, propose.points = 1, schedule.nodes = 4, asyn.wait.for.proposals = FALSE, asyn.skip.filtered.propsals = TRUE, asyn.impute.method = imp.method, store.model.at = 0:10)
    ctrl = setMBOControlTermination(ctrl, time.budget = 9L)
    crit = if (imp.method %in% c("mc", "quantilemean")) "eei" else "ei"
    ctrl = setMBOControlInfill(control = ctrl, crit = crit, opt.focussearch.maxit = 2L, opt.focussearch.points = 50L, filter.proposed.points = TRUE)
    parallelMap::parallelStartMulticore(2, level = "mlrMBO.asyn")
    or = mbo(fun = fn.sleep, design = des, control = ctrl)
    parallelMap::parallelStop()
    unlink(dirname(save.file), recursive = TRUE, force = TRUE)
    op.df = as.data.frame(or$opt.path)
    expect_true(nrow(op.df) >= 15)
    #expect_true(all(!is.na(op.df$train.time[11:15]))) #FIXME Why is this missing now?
    expect_true(all(!is.na(op.df[[crit]][11:15])))
    expect_true(all(!is.na(op.df$scheduled.on[11:15])))
    expect_true(all(!is.na(op.df$dob[11:15])))
    expect_equal(op.df$dob[11:12], c(1,1))
    if (imp.method %in% c("mc", "quantilemean")) {
      expect_class(getFirst(or$models)$learner.model, "AsynModel")
    }
  }
})

# Here we want to leave the debug-mode to test the saving
options(mlrMBO.debug.mode = FALSE)

test_that("asyn MBO works with mboContinue", {
  # make sure there is no or - object in the environment
  save.file = file.path(tempdir(), "mbo_asyn", "mbo.RData")
  dir.create(dirname(save.file), recursive = TRUE)
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
  des = generateTestDesign(6L, getParamSet(f))
  ctrl = makeMBOControl(schedule.method = "asyn", save.file.path = save.file, save.on.disk.at = 0:5)
  ctrl = setMBOControlTermination(ctrl, iters = 4L)
  ctrl = setMBOControlInfill(control = ctrl, crit = "cb", crit.cb.lambda = 2, opt.focussearch.maxit = 2L, opt.focussearch.points = 50L)
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
