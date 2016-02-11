context("multifid")

test_that("basic multifid works", {
  set.seed(1)
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      lvl.par.val = x$.multifid.lvl
      x = x$x
      assertNumeric(x, len = 1, lower = 0, upper = 10)
      3 - lvl.par.val + 0.5 * x
    },
    par.set = makeParamSet(
      makeNumericParam("x", lower = 0, upper = 10)
    ),
    has.simple.signature = FALSE
  )

  control = makeMBOControl(
    init.design.points = 9L,
    init.design.fun = maximinLHS,
    on.learner.error = "stop",
    show.learner.output = FALSE,
  )
  control = setMBOControlTermination(control, iters = 5L)

  control = setMBOControlInfill(control = control,
                                crit = "ei",
                                opt = "focussearch",
                                opt.restarts = 1L,
                                opt.focussearch.maxit = 1L,
                                opt.focussearch.points = 10L,
                                filter.proposed.points = TRUE,
                                filter.proposed.points.tol = 0.01
  )
  control = setMBOControlMultiFid(control = control,
                                  param = "dw.perc",
                                  costs = 1:3,
                                  lvls = c(0.1, 0.5, 1),
                                  cor.grid.points = 40L)

  surrogat.learner = makeLearner("regr.lm", predict.type = "se")
  result = mbo(f, learner = surrogat.learner, control = control)
  expect_true(result$y < 0.5)

  ### remove cots so time.model will be estimated
  control$multifid.costs = NULL
  f.delay = makeSingleObjectiveFunction(
    fn = function(x) {
      Sys.sleep(0.1 + x$.multifid.lvl/3)
      f(x)
    },
    par.set = makeParamSet(
      makeNumericParam("x", lower = 0, upper = 10)
    ),
    has.simple.signature = FALSE
  )
  set.seed(1)
  result.time = mbo(f.delay, learner = surrogat.learner, control = control)

  #this is pretty hard and migh fail?
  expect_equal(as.data.frame(result.time$opt.path)$.multifid.lvl, as.data.frame(result$opt.path)$.multifid.lvl)
})

