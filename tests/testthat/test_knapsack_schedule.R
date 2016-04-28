context("smart schedule")

test_that("knapsack schedule works", {
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
      schedule.method = "scheduleKnapsack",
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
