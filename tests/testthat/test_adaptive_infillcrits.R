context("adaptive infill crits")

test_that("adaptive infill crit works with all termination criteria", {

  f = testf.fsphere.1d
  f.slow = testf.fsphere.1d.slow
  f.max = convertToMaximization(f)
  design = testd.fsphere.1d
  design.max = design
  design$y = apply(design, 1, f)
  design.max$y = apply(design.max, 1, f.max)

  terminations = list(
    iters = list(iters = 4L),
    time.budget = list(time.budget = 2),
    exec.time.budget = list(exec.time.budget = 1.5, iters = 13, use.for.adaptive.infill = "exec.time.budget"),
    target.fun.value = list(target.fun.value = min(design$y)/3, iters = 13, use.for.adaptive.infill = "target.fun.value"),
    target.fun.value.max = list(target.fun.value = max(design.max$y)/3, iters = 13, use.for.adaptive.infill = "target.fun.value"),
    max.evals = list(max.evals = nrow(design) + 4L)
  )

  ctrl = makeMBOControl()
  ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritAdaCB())

  for (i in names(terminations)) {
    ctrl2 = do.call(setMBOControlTermination, c(list(control = ctrl), terminations[[i]]))
    if (i == "exec.time.budget") {
      fun = f.slow
      des = design
    } else if (i == "target.fun.value.max") {
      fun = f.max
      des = design.max
    } else {
      fun = f
      des = design
    }
    or = mbo(fun, des, control = ctrl2)
    expect_number(or$y, info = i)
    df = as.data.frame(or$opt.path)
    expect_true(any(df$lambda > 1), info = i)
    expect_true(length(unique(df$lambda)) >= 2, info = i)
    expect_true(all(diff(df$lambda[!is.na(df$lambda)])<=0), info = i)
    expect_true(all(df$prop.type %in% c("infill_adacb", "initdesign")), info = i)
    expect_numeric(df$adacb, info = i)
  }
})
