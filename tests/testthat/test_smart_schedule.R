context("smart schedule")

test_that("smart schedule works", {
  set.seed(3)
  objfun = function(x) {
    x = (x$x-5)^2
    attr(x,"exec.time") = 10L
    return(x)
  }
  
  par.set = makeParamSet(
    makeNumericParam("x", lower = 0, upper = 10)
  )
  
  setups = expand.grid(
    schedule.priority = c("infill", "explore", "balanced"),
    multipoint.lcb.multiple = c("random", "random.quantiles"),
    schedule.priority.time = FALSE,
    crit.lcb.lambda = c(1,4),
    stringsAsFactors = FALSE
    )
  setups = rbind(setups, data.frame(schedule.priority = "infill", multipoint.lcb.multiple = "static.quantiles", schedule.priority.time = TRUE, crit.lcb.lambda = 2))
  
  surrogat.learner = makeLearner("regr.randomForest", predict.type = "se", ntree = 10)
  #ors = rowLapply(setups, function(x) {
  for (i in seq_row(setups)) {
    x = as.list(setups[i,])
    control = makeMBOControl(
      init.design.points = 5L,
      iters = 2L,
      propose.points = 5L,
      schedule.method = "smartParallelMap",
      schedule.nodes = 5L,
      schedule.priority = x$schedule.priority,
      schedule.priority.time = x$schedule.priority.time
    )
    control = setMBOControlInfill(control = control, crit = "lcb", crit.lcb.lambda = x$crit.lcb.lambda)
    control = setMBOControlMultiPoint(control = control, lcb.multiple = x$multipoint.lcb.multiple)
    or = mbo(fun = objfun, par.set = par.set, learner = surrogat.learner, control = control)
    op.df = as.data.frame(or$opt.path)
    expect_true(nrow(op.df) == 15)
    expect_true(all(na.omit(op.df$predicted.time) %btwn% c(9,11)))
    expect_true(!all(is.na(op.df$predicted.time.se)))
    expect_true(!all(is.na(op.df$scheduled.at)))
    expect_true(!all(is.na(op.df$scheduled.on)))
    expect_true(!all(is.na(op.df$scheduled.job)))
    expect_true(!all(is.na(op.df$scheduled.priority)))
    expect_true(!all(is.na(op.df$lcb.lambda)))
    expect_true(abs(mean(op.df$lcb.lambda, na.rm = TRUE) - x$crit.lcb.lambda) < sd(op.df$lcb.lambda, na.rm = TRUE))
    or
  }
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
#                                 crit = "lcb",
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
