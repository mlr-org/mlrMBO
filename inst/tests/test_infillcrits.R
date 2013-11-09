context("infill crits")

test_that("infill crits", {
  f1 = makeMBOFunction(function(x) sum(x^2))
  f2 = makeMBOFunction(function(x) sum(x^2) + rnorm(1, 0, 0.1))
  ps = makeNumericParamSet(len=2, lower=-10, upper=10)

  ninit = 20L; niters = 5L
  mycontrol = function(minimize, noisy, crit) {
    makeMBOControl(minimize=minimize, noisy=noisy, init.design.points=ninit, iters=niters,
    infill.crit=crit, infill.opt="cmaes", infill.opt.restarts=2L,
    infill.opt.cmaes.control = list(maxit=10L, mu=20, lambda=20),
    final.evals = 100)
  }

  mycheck = function(or, minimize) {
    expect_equal(getOptPathLength(or$opt.path), ninit + niters)
    expect_true(!is.na(or$y))
    if (minimize)
      expect_true(or$y < 1)
    else
      expect_true(or$y > 100)
  }

  # FIXME: we see a problem with crit="mean" here.
  # at some point we will always eval the same point.
  # kriging will then produce numerical errors, but the real problem is that
  # we have converged and just waste time. we need to detect this somehow, or cope with it
  for (noisy in c(FALSE, TRUE)) {
    for (minimize in c(TRUE, FALSE)) {
      crits = if (!noisy) c("mean", "ei") else c("aei")
      nugget.estim = if (!noisy) FALSE else TRUE
      learner = makeLearner("regr.km", predict.type="se", nugget.estim=nugget.estim)
      for (crit in crits) {
        ctrl = mycontrol(minimize, noisy, crit)
        f = if (!noisy) f1 else f2
        or = mbo(f, ps, NULL, learner, ctrl, show.info=FALSE)
        mycheck(or, minimize)
      }
    }
  }
})


