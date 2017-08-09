context("adaptive infill crits")

test_that("adaptive infill crits", {
  terminations = list(
    list(iters = 3L),
    list(time.budget = 1L),
    list(exec.time.budget = 1L, iters = 13, use.for.adaptive.infill = "exec.time.budget"),
    list(target.fun.value = 0.025, iters = 13, use.for.adaptive.infill = "target.fun.value"),
    list(max.evals = 13L)
  )
  des = testd.fsphere.2d
  des$y = apply(des, 1, testf.fsphere.2d)
  ctrl = makeMBOControl()
  ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAdaCB())
  for (i in seq_along(terminations)) {
    ctrl2 = do.call(setMBOControlTermination, c(list(control = ctrl), terminations[[i]]))
    if (i %in% 2:3) {
      fun = testf.fsphere.2d.slow
    } else {
      fun = testf.fsphere.2d
    }
    or = mbo(fun, des, control = ctrl2)
    expect_number(or$y)
    df = as.data.frame(or$opt.path)
    expect_true(any(df$lambda > 1))
    expect_true(length(unique(df$lambda))>2)
    expect_true(all(df$prop.type %in% c("infill_adacb", "initdesign")))
    expect_numeric(df$adacb)
  }
})
