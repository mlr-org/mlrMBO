context("conceptdrift")

test_that("conceptdrift with window", {
  fn = makeBraninFunction()
  # we will take x1 as a time factor and optimize over x2
  w.fn = wrapSmoofConceptDrift(fn = fn, drift.param = "x1")
  mbo.iters = 30
  start.drift = 0.2
  end.drift = -2
  drift.range = unlist(attr(w.fn, "original.par.set")$pars[[attr(w.fn, "drift.param")]][c("lower", "upper")])
  drift.range = drift.range + c(0.2, -2)

  slow.drift = function(dob) {
    drift.range[1] + (dob/mbo.iters) * diff(drift.range)
  }

  tail.window = function(x) {
    tail(x, 20)
  }

  ctrl = makeMBOControl(final.method = "best.predicted")
  ctrl = setMBOControlConceptDrift(
    control = ctrl,
    drift.function = slow.drift,
    window.function = tail.window,
    calculate.th.final.point = TRUE
  )
  ctrl = setMBOControlTermination(ctrl, iter = mbo.iters)

  res = mbo(fun = w.fn, control = ctrl)

  library(ggplot2)
  g = plot(res$final.opt.state)
  expect_class(g, "ggplot")

  op1 = as.data.frame(res$opt.path)
  expect_data_frame(op1, nrows = 20)

  res$opt.path$window.function = identity
  op2 = as.data.frame(res$opt.path)
  expect_data_frame(op2, nrows = 30 + 4)
})

test_that("conceptdrift with time as covariate", {
  fn = makeRosenbrockFunction(2)
  w.fn = wrapSmoofConceptDrift(fn = fn, drift.param = "x1")

  mbo.iters = 10
  drift.range = unlist(attr(w.fn, "original.par.set")$pars[[attr(w.fn, "drift.param")]][c("lower", "upper")])
  drift.range = drift.range + c(0.2, -2)
  slow.drift = function(dob) {
    drift.range[1] + (dob/mbo.iters) * diff(drift.range)
  }

  ctrl = makeMBOControl(final.method = "best.predicted")
  ctrl = setMBOControlConceptDrift(
    control = ctrl,
    drift.function = slow.drift,
    learn.drift = TRUE,
    calculate.th.final.point = TRUE)
  ctrl = setMBOControlTermination(ctrl, iter = mbo.iters)
  ctrl = setMBOControlInfill(ctrl, crit.aei)

  res = mbo(fun = w.fn, control = ctrl)

  g = plot(res$final.opt.state, scale.panels = TRUE)
  expect_class(g, "ggplot")

  op1 = as.data.frame(res$opt.path)
  expect_data_frame(op1, nrows = mbo.iters + 4)
  mbo.cd.colnames = c(paste0("final.x.",getParamIds(getParamSet(fn), TRUE, TRUE)), "final.hat.y")
  expect_subset(mbo.cd.colnames, colnames(op1))
  for (cname in mbo.cd.colnames) {
    expect_numeric(tail(op1[[cname]],-4*4))
  }
  # suggested final.x.x1 (drift param) should equal actual time!
  expect_equal(tail(op1[["x1"]],-4*4), tail(op1[["final.x.x1"]],-4*4))

  ctrl = makeMBOControl(final.method = "predict")
  ctrl = setMBOControlConceptDrift(
    control = ctrl,
    drift.function = slow.drift,
    learn.drift = TRUE,
    calculate.th.final.point = TRUE)
  ctrl = setMBOControlTermination(ctrl, iter = mbo.iters)

  res = mbo(fun = w.fn, control = ctrl)
  op1 = as.data.frame(res$opt.path)
  expect_equal(tail(op1[["x1"]],-4*4), tail(op1[["final.x.x1"]],-4*4))
})


