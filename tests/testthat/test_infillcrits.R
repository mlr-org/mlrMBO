context("infill crits")

test_that("infill crits", {
  ninit = 20L
  niters = 3L
  f1 = smoof::makeSphereFunction(2L)
  f2 = smoof::makeSingleObjectiveFunction(
    fn = function(x) sum(x^2) + rnorm(1, 0, 0.03),
    par.set = getParamSet(f1)
  )
  des = generateTestDesign(ninit, getParamSet(f1))

  mycontrol = function(crit, minimize) {
    ctrl = makeMBOControl(final.evals = 10L)
    ctrl = setMBOControlTermination(ctrl, iters = niters)
    ctrl = setMBOControlInfill(ctrl, crit = crit, opt = "focussearch", opt.restarts = 1L,
      opt.focussearch.points = 300L)
    return(ctrl)
  }

  mycheck = function(or, minimize) {
    expect_equal(getOptPathLength(or$opt.path), ninit + niters + 10L)
    expect_number(or$y)
    if (minimize)
      expect_true(or$y < 25)
    else
      expect_true(or$y > 30)

    opdf = as.data.frame(or$opt.path)
    opdf = split(opdf, opdf$prop.type)

    if (!is.null(opdf$infill_ei))
      expect_true(!anyMissing(opdf$infill_ei[, c("ei","se","mean")]))
    if (!is.null(opdf$infill_cb))
      expect_true(!anyMissing(opdf$infill_cb[, c("se","mean","lambda")]))
    if (!is.null(opdf$infill_aei))
      expect_true(!anyMissing(opdf$infill_cb[, c("se","mean","tau")]))
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
      crits = if (noisy) list(crit.aei, crit.eqi) else list(crit.mr, crit.se, crit.ei)
      for (lrn in learners) {
        if (inherits(lrn, "regr.km"))
          lrn = setHyperPars(lrn, nugget.estim = noisy)
        for (crit in crits) {
          ctrl = mycontrol(crit)
          f = if (!noisy) f1 else f2
          f = if (!minimize) setAttribute(f, "minimize", FALSE) else f
          or = mbo(f, des, learner = lrn, control = ctrl)
          mycheck(or, minimize)
        }
      }
    }
  }

  # check lambda and pi for cb
  ctrl = makeMBOControl(final.evals = 10L)
  ctrl = setMBOControlTermination(ctrl, iters = niters)
  ctrl = setMBOControlInfill(ctrl, crit = crit.cb2,
    opt = "focussearch", opt.restarts = 1L,
    opt.focussearch.points = 300L)
  mbo(f1, des, learner = makeLearner("regr.km", predict.type = "se"), control = ctrl)

  # check beta for eqi
  expect_error(makeMBOInfillCriterionEQI(eqi.beta = 2))

  ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCriterionEQI(eqi.beta = 0.6),
    opt = "focussearch", opt.restarts = 1L, opt.focussearch.points = 300L)
  or = mbo(f1, des, learner = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE), control = ctrl)
  expect_lt(or$y, 50)
})
