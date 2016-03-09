context("smart schedule")

test_that("smart schedule works", {
  set.seed(3)
  objfun = smoof::makeSingleObjectiveFunction(
    fn = function(x) {
      x = (x$x-5)^2
      attr(x,"exec.time") = 10L
      return(x)
    },
    par.set = makeParamSet(
      makeNumericParam("x", lower = 0, upper = 10)
    ),
    has.simple.signature = FALSE
  )
  
  setups = expand.grid(
    schedule.priority = c("infill", "explore", "balanced"),
    multipoint.cb.multiple = c("random", "random.quantiles"),
    schedule.priority.time = FALSE,
    crit.cb.lambda = c(1,4),
    stringsAsFactors = FALSE
    )
  setups = rbind(setups, data.frame(schedule.priority = "infill", multipoint.cb.multiple = "static.quantiles", schedule.priority.time = TRUE, crit.cb.lambda = 2))
  
  surrogat.learner = makeLearner("regr.lm", predict.type = "se")
  #ors = rowLapply(setups, function(x) {
  for (i in seq_row(setups)) {
    x = as.list(setups[i,])
    control = makeMBOControl(
      propose.points = 10L,
      schedule.method = "smartParallelMap",
      schedule.nodes = 5L,
      schedule.priority = x$schedule.priority,
      schedule.priority.time = x$schedule.priority.time
    )
    control = setMBOControlTermination(control, iters = 2L)

    control = setMBOControlInfill(control = control, crit = "cb", crit.cb.lambda = x$crit.cb.lambda, opt.focussearch.maxit = 2L, opt.focussearch.points = 50L)
    control = setMBOControlMultiPoint(control = control, cb.multiple = x$multipoint.cb.multiple)
    des = generateTestDesign(5L, smoof::getParamSet(objfun))
    or = mbo(fun = objfun, design = des, learner = surrogat.learner, control = control)
    op.df = as.data.frame(or$opt.path)
    expect_true(nrow(op.df) == 15)
    expect_true(all(na.omit(op.df$predicted.time) %btwn% c(9,11)))
    expect_true(!all(is.na(op.df$predicted.time.se)))
    expect_true(!all(is.na(op.df$scheduled.at)))
    expect_true(!all(is.na(op.df$scheduled.on)))
    expect_true(!all(is.na(op.df$scheduled.job)))
    expect_true(!all(is.na(op.df$scheduled.priority)))
    expect_true(!all(is.na(op.df$multipoint.cb.lambda)))
    expect_true(abs(mean(op.df$multipoint.cb.lambda, na.rm = TRUE) - x$crit.cb.lambda) < sd(op.df$multipoint.cb.lambda, na.rm = TRUE))
    or
  }
})

test_that("async MBO works", {
  #save.file = tempfile("state")
  save.file = "testMBO/here.Rdata"
  ctrl = makeMBOControl(schedule.method = "asyn", save.file.path = save.file)
  ctrl = setMBOControlTermination(ctrl, iters = 20L, max.evals = 20L)
  ctrl = setMBOControlInfill(control = ctrl, crit = "cb", crit.cb.lambda = 2, opt.focussearch.maxit = 2L, opt.focussearch.points = 50L)
  ctrl = setMBOControlMultiPoint(ctrl, method = "cb")
  surrogat.learner = makeLearner("regr.randomForest", predict.type = "se", ntree = 50, ntree.for.se = 20)
  or = mbo(fun = testf.fsphere.2d, design = testd.fsphere.2d, learner = surrogat.learner, control = ctrl)
  unlink(dirname(save.file), recursive = TRUE, force = TRUE)
  op.df = as.data.frame(or$opt.path)
  expect_true(nrow(op.df) == 20)
  expect_true(all(is.na(op.df$train.time[11:20])))
  expect_true(all(is.na(op.df$multipoint.cb.lambda[11:20])))
})
# test_that("multifid works with smart scheduling", {
#   #for now complicated
#   #get multipoint working with multifid and then we can talk
#   set.seed(1)
#   objfun = function(x) {
#     lvl.par.val = x$.multifid.lvl
#     x = x$x
#     assertNumeric(x, len = 1, lower = 0, upper = 10)
#     Sys.sleep(0.1*(sin(x)+1))
#     3 - lvl.par.val + 0.5*x 
#   }
#   
#   par.set = makeParamSet(
#     makeNumericParam("x", lower = 0, upper = 10)
#   )
#   
#   control = makeMBOControl(
#     init.design.points = 9L,
#     init.design.fun = maximinLHS,
#     iters = 5L,
#     on.learner.error = "stop",
#     show.learner.output = FALSE,
#     propose.points = 2L,
#     schedule.method = "smartParallelMap",
#     schedule.nodes = 1L
#   )
#   control = setMBOControlInfill(control = control, 
#                                 crit = "cb",
#                                 opt = "focussearch",
#                                 opt.restarts = 1L,
#                                 opt.focussearch.maxit = 1L,
#                                 opt.focussearch.points = 10L,
#                                 filter.proposed.points = TRUE,
#                                 filter.proposed.points.tol = 0.01
#   )
#   control = setMBOControlMultiFid(control = control,
#                                   param = "dw.perc",
#                                   costs = 1:3,
#                                   lvls = c(0.1, 0.5, 1),
#                                   cor.grid.points = 40L)
#   
#   surrogat.learner = makeLearner("regr.lm", predict.type = "se")
#   result = mbo(fun = objfun, par.set = par.set, learner = surrogat.learner, control = control)
#   expect_true(result$y < 0.5)
#   
#   ### remove cots so time.model will be estimated
#   control$multifid.costs = NULL
#   objfun.delay = function(x) {
#     Sys.sleep(0.1 + x$.multifid.lvl/3)
#     objfun(x)
#   }
#   set.seed(1)
#   result.time = mbo(fun = objfun.delay, par.set = par.set, learner = surrogat.learner, control = control)
#   
#   #this is pretty hard and migh fail?
#   expect_equal(as.data.frame(result.time$opt.path)$.multifid.lvl, as.data.frame(result$opt.path)$.multifid.lvl)
# })
