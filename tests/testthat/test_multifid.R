context("mutlifid")

test_that("essential mutlifid works", {

  objfun = function(x, fac = 0.5) {
    lvl.par.val = x$dw.perc
    x = x$x
    assertNumeric(x, len = 1, lower = 0, upper = 10)
    res = -1 * sin(x) - exp(x/100) + 10
    lvl.par.val = (1-lvl.par.val) * fac
    add = lvl.par.val + lvl.par.val/10 * (x - lvl.par.val * 10)^2
    res + add
  }

  par.set = makeParamSet(
    makeNumericParam("x", lower = 0, upper = 10),
    makeNumericParam("dw.perc", lower=0, upper=1)
  )

  control = makeMBOControl(
    init.design.points = 9L, #distributed over the different levels, seems not to work for <5 each
    init.design.fun = maximinLHS,
    iters = 3L,
    on.learner.error = "stop",
    show.learner.output = FALSE,
  )
  control = setMBOControlInfill(control = control,
    crit = "multiFid",
    opt = "focussearch",
    opt.restarts = 1L,
    opt.focussearch.maxit = 1L,
    opt.focussearch.points = 10L
  )
  control = setMBOControlMultiFid(control = control,
    param = "dw.perc",
    lvls = c(0.1, 0.5, 1),
    costs = function(cur, last) (last / cur)^0.5,
    cor.grid.points = 40L)
  
  surrogat.learner = makeLearner("regr.km", predict.type="response", nugget.estim = TRUE, jitter = TRUE)
  result = mbo(fun = objfun, par.set = par.set, learner = surrogat.learner, control = control)
  expect_true(inherits(result, "MultiFidResult"))
})

