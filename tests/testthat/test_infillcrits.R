context("infill crits")

test_that("infill crits", {
  ninit = 20L
  niters = 3L
  funs = list(
    "2D" = list(
      f1 = testf.fsphere.2d,
      f2 = smoof::makeSingleObjectiveFunction(
        fn = function(x) sum(x^2) + rnorm(1, 0, 0.5),
        par.set = makeNumericParamSet("x", 2, -5, 5),
        noisy = TRUE
      )
    ),
    "1D" = list(
      f1 = testf.fsphere.1d,
      f2 = smoof::makeSingleObjectiveFunction(
        fn = function(x) x^2 + rnorm(1, 0, 0.5),
        par.set = makeNumericParamSet("x", 1, -7, 7),
        noisy = TRUE
      )
    )
  )

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
      expect_true(or$y < 10)
    else
      expect_true(or$y > 20)

    opdf = as.data.frame(or$opt.path)
    opdf = split(opdf, opdf$prop.type)

    if (!is.null(opdf$infill_ei))
      expect_true(!anyMissing(opdf$infill_ei[, c("ei","se","mean")]))
    if (!is.null(opdf$infill_cb)) {
      expect_true(!anyMissing(opdf$infill_cb[, c("se","mean","lambda")]))
      expect_true(all(opdf$infill_cb$lambda == 2))
    }
    if (!is.null(opdf$infill_aei))
      expect_true(!anyMissing(opdf$infill_aei[, c("se","mean","tau")]))
    if (!is.null(opdf$infill_eqi))
      expect_true(!anyMissing(opdf$infill_eqi[, c("se","mean","tau")]))
    expect_true(nrow(opdf$final_eval) == 10L)
  }

  learners = list(
    makeLearner("regr.km", predict.type = "se", nugget.stability = 1e-5),
    makeLearner("regr.randomForest", ntree = 30L, predict.type = "se")
  )

  # FIXME: we see a problem with crit = "mean" here.
  # at some point we will always eval the same point.
  # kriging will then produce numerical errors, but the real problem is that
  # we have converged and just waste time. we need to detect this somehow, or cope with it
  for (noisy in c(TRUE, FALSE)) {
    for (minimize in c(TRUE, FALSE)) {
      crits = if (noisy) list(crit.aei, crit.eqi) else list(crit.mr, crit.se, crit.ei, crit.cb2)
      for (lrn in learners) {
        if (inherits(lrn, "regr.km"))
          lrn = setHyperPars(lrn, nugget.estim = noisy)
        for (crit in crits) {
          ctrl = mycontrol(crit)
          for (set in funs) {
            f = if (!noisy) set$f1 else set$f2
            f = if (!minimize) setAttribute(f, "minimize", FALSE) else f
            des = generateTestDesign(ninit, getParamSet(f))
            or = mbo(f, des, learner = lrn, control = ctrl)
            mycheck(or, minimize)
          }
        }
      }
    }
  }

  # check beta for eqi
  expect_error(makeMBOInfillCritEQI(eqi.beta = 2))

  ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEQI(eqi.beta = 0.6),
    opt = "focussearch", opt.restarts = 1L, opt.focussearch.points = 300L)
  des = generateTestDesign(ninit, getParamSet(funs[[1]]$f1))
  or = mbo(funs[[1]]$f1, des, learner = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE), control = ctrl)
  expect_lt(or$y, 50)
})


test_that("infill crit printer works",  {
  # had a problem here that the NULL default was not displayed
  expect_output(print(crit.cb), "cb.lambda=<NULL>")
  expect_output(print(crit.ei), "Direction of optimization : maximize")
})

test_that("setMBOControlInfill handles errors", {
  #https://github.com/mlr-org/mlrMBO/issues/417
  ctrl = makeMBOControl()
  expect_error(setMBOControlInfill(ctrl, crit = undefined()), "could not find function \"undefined\"")
  expect_error(setMBOControlInfill(ctrl, crit = undefined), "object 'undefined' not found")
})
