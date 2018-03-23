context("adaptive infill crits")

test_that("adaptive infill crit works with all termination criteria", {

  terminations = list(
    list(iters = 3L),
    list(time.budget = 1L),
    list(exec.time.budget = 1L, iters = 13, use.for.adaptive.infill = "exec.time.budget"),
    list(target.fun.value = 0.025, iters = 13, use.for.adaptive.infill = "target.fun.value"),
    list(target.fun.value = -0.025, iters = 13, use.for.adaptive.infill = "target.fun.value"),
    list(max.evals = 13L)
  )

  f = testf.fsphere.1d
  f.slow = testf.fsphere.1d.slow
  f.max = convertToMaximization(f)
  design = testd.fsphere.1d
  design.max = design
  design$y = apply(design, 1, f)
  design.max$y = apply(design.max, 1, f.max)

  ctrl = makeMBOControl()
  ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAdaCB())

  for (i in seq_along(terminations)) {
    ctrl2 = do.call(setMBOControlTermination, c(list(control = ctrl), terminations[[i]]))
    if (i %in% 2:3) {
      fun = f.slow
      des = design
    } else if (i == 5) {
      fun = f.max
      des = design.max
    } else {
      fun = f
      des = design
    }
    or = mbo(fun, des, control = ctrl2)
    expect_number(or$y)
    df = as.data.frame(or$opt.path)
    expect_true(any(df$lambda > 1))
    expect_true(length(unique(df$lambda))>2)
    expect_true(all(diff(df$lambda[!is.na(df$lambda)])<0))
    expect_true(all(df$prop.type %in% c("infill_adacb", "initdesign")))
    expect_numeric(df$adacb)
  }
})
