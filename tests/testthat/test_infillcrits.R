context("infill crits")

test_that("infill crits", {
  f1 = smoof::makeSphereFunction(2L)
  f2 = smoof::makeSingleObjectiveFunction(
    fn = function(x) sum(x^2) + rnorm(1, 0, 0.03),
    par.set = smoof::getParamSet(f1)
  )

  ninit = 20L
  niters = 3L

  mycontrol = function(minimize, crit) {
    ctrl = makeMBOControl(init.design.points = ninit,
      iters = niters, final.evals = 10L)
    ctrl = setMBOControlInfill(ctrl, crit = crit, opt = "focussearch", opt.restarts = 1L,
      opt.focussearch.points = 300L)
    return(ctrl)
  }

  mycheck = function(or, minimize) {
    expect_equal(getOptPathLength(or$opt.path), ninit + niters + 10L)
    expect_true(!is.na(or$y))
    if (minimize)
      expect_true(or$y < 25)
    else
      expect_true(or$y > 30)
  }

  learners = list(
    makeLearner("regr.km", predict.type = "se"),
    makeLearner("regr.randomForest", ntree = 10L, predict.type = "se")
  )

  # FIXME: we see a problem with crit = "mean" here.
  # at some point we will always eval the same point.
  # kriging will then produce numerical errors, but the real problem is that
  # we have converged and just waste time. we need to detect this somehow, or cope with it
  for (noisy in c(TRUE, FALSE)) {
    for (minimize in c(TRUE, FALSE)) {
      crits = if (!noisy) c("mean", "ei") else c("aei", "eqi")
      for (lrn in learners) {
        if (inherits(lrn, "regr.km"))
          lrn = setHyperPars(lrn, nugget.estim = noisy)
        for (crit in crits) {
          ctrl = mycontrol(crit)
          f = if (!noisy) f1 else f2
          f = if (!minimize) setAttribute(f, "minimize", FALSE) else f
          or = mbo(f, learner = lrn, control = ctrl)
          mycheck(or, minimize)
        }
      }
    }
  }

  # check lambda and pi for lcb
  ctrl = makeMBOControl(init.design.points = 8L,
    iters = niters, final.evals = 10L)
  ctrl = setMBOControlInfill(ctrl, crit = "lcb", opt = "focussearch", opt.restarts = 1L,
    opt.focussearch.points = 300L, crit.lcb.lambda = 2)
  mbo(f1, learner = makeLearner("regr.km", predict.type = "se"), control = ctrl)
  expect_error(setMBOControlInfill(ctrl, crit = "lcb", opt = "focussearch", opt.restarts = 1L,
    opt.focussearch.points = 300L, crit.lcb.lambda = 2, crit.lcb.pi = 0.5))
  ctrl = setMBOControlInfill(ctrl, crit = "lcb", opt = "focussearch", opt.restarts = 1L,
    opt.focussearch.points = 300L, crit.lcb.lambda = NULL, crit.lcb.pi = 0.5)
  or = mbo(f1, learner = makeLearner("regr.km", predict.type = "se"), control = ctrl)
  expect_true(or$y < 50)

  # check beta for eqi
  expect_error(setMBOControlInfill(ctrl, crit = "eqi", opt = "focussearch", opt.restarts = 1L,
                                   opt.focussearch.points = 300L, crit.eqi.beta = 2))
  ctrl = setMBOControlInfill(ctrl, crit = "eqi", opt = "focussearch", opt.restarts = 1L,
                             opt.focussearch.points = 300L, crit.eqi.beta = 0.6)
  or = mbo(f1, learner = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE), control = ctrl)
  expect_true(or$y < 50)
})


