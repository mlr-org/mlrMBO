context("multipoint multicrit")

test_that("multipoint multicrit", {
  f = makeBraninFunction()
  f = setAttribute(f, "par.set", makeNumericParamSet(len = 2L, lower = 0, upper = 1))
  lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

  #FIXME how can we test this better?
  for (obj in c("ei.dist", "mean.se", "mean.se.dist")) {
    for (dist in c("nearest.better", "nearest.neighbor")) {
      for (sel in c("hypervolume", "crowdingdist", "first", "last")) {

        ctrl = makeMBOControl(init.design.points = 10L, iters = 1L, propose.points = 4L)
        ctrl = setMBOControlMultiPoint(ctrl,
          method = "multicrit",
          multicrit.objective = obj,
          multicrit.dist = dist,
          multicrit.sel = sel,
          multicrit.maxit = 30L
        )

        res = mbo(f, learner = lrn, control = ctrl)

        gap = res$y - 0.3979
        #expect_true(gap < 0.1)
      }
    }
  }

})
