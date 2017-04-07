context("multipoint multi-objective")

test_that("multipoint multi-objective", {
  f = makeBraninFunction()
  f = setAttribute(f, "par.set", makeNumericParamSet(len = 2L, lower = 0, upper = 1))
  
  #FIXME: how can we test this better?
  for (obj in c("ei.dist", "mean.se", "mean.se.dist")) {
    for (dist in c("nearest.better", "nearest.neighbor")) {
      for (sel in c("hypervolume", "crowdingdist", "first", "last")) {

        des = generateTestDesign(10L, getParamSet(f))
        ctrl = makeMBOControl(propose.points = 4L)
        ctrl = setMBOControlTermination(ctrl, iters = 1L)
        ctrl = setMBOControlMultiPoint(ctrl,
          method = "moimbo",
          moimbo.objective = obj,
          moimbo.dist = dist,
          moimbo.sel = sel,
          moimbo.maxit = 30L
        )

        res = mbo(f, des, control = ctrl)
        expect_output(print(res), "Recommended parameters")

        gap = res$y - 0.3979
        # expect_lt(gap, 0.1)
      }
    }
  }
  
  #test that infill crit is ignored
  crits = list(makeMBOInfillCritCB(), makeMBOInfillCritEI(), makeMBOInfillCritMeanResponse())
  for (i in seq_along(crits)) {
    ctrl = setMBOControlInfill(ctrl, crit = crits[[i]])
    res = mbo(f, control = ctrl)
    expect_output(print(res), "Recommended parameters")
  }

})
