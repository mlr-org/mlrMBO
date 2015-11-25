context("smart schedule")

test_that("smart schedule works", {
  set.seed(2)
  objfun = function(x) {
    x = x$x
    assertNumeric(x, len = 1, lower = 0, upper = 10)
    Sys.sleep(0.25*(sin(x)+1)+0.01)
    (x-3)^2 + 10*sin(x*2)
  }
  
  par.set = makeParamSet(
    makeNumericParam("x", lower = 0, upper = 10)
  )
  
  setups = expand.grid(
    schedule.priority = c("infill", "explore", "balanced"),
    multipoint.lcb.multiple = c("random", "random.quantiles"),
    schedule.priority.time = c(FALSE, TRUE),
    crit.lcb.lambda = c(1,4),
    stringsAsFactors = FALSE
    )
  
  surrogat.learner = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE)
  ors = rowLapply(setups, function(x) {
    control = makeMBOControl(
      init.design.points = 5L,
      iters = 2L,
      propose.points = 9L,
      schedule.method = "smartParallelMap",
      schedule.nodes = 3L,
      schedule.priority = x$schedule.priority,
      schedule.priority.time = x$schedule.priority.time
    )
    control = setMBOControlInfill(control = control, crit = "lcb", crit.lcb.lambda = x$crit.lcb.lambda)
    control = setMBOControlMultiPoint(control = control, lcb.multiple = x$multipoint.lcb.multiple)
    or = mbo(fun = objfun, par.set = par.set, learner = surrogat.learner, control = control)
    op.df = as.data.frame(or$opt.path)
    expect_true(!all(is.na(op.df$predicted.time)))
    expect_true(!all(is.na(op.df$predicted.time.se)))
    expect_true(!all(is.na(op.df$scheduled.at)))
    expect_true(!all(is.na(op.df$scheduled.on)))
    expect_true(!all(is.na(op.df$scheduled.job)))
    expect_true(!all(is.na(op.df$scheduled.priority)))
    expect_true(!all(is.na(op.df$lcb.lambda)))
    expect_true(abs(mean(op.df$lcb.lambda, na.rm = TRUE) - x$crit.lcb.lambda) < 2)
    or
  })
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
