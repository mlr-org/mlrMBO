context("manual smbo")

test_that("human in the middle smbo works", {
  fun = testf.fsphere.2d
  des = testd.fsphere.2d
  des$y = apply(des, 1, fun)
  ps = testp.fsphere.2d
  ctrl = makeMBOControl()

  opt.state = initSMBO(par.set = ps, design = des, control = ctrl)
  prop = proposePoints(opt.state)
  assertList(prop)
  assertDataFrame(prop$prop.points)
  assertSetEqual(names(prop$prop.points), getParamIds(ps, TRUE, TRUE))

  x = data.frame(x1 = 0, x2 = 0)
  y = fun(x = x)
  updateSMBO(opt.state, x = x, y = y)

  x = data.frame(x1 = -1, x2 = 1)
  y = fun(x = x)
  updateSMBO(opt.state, x = x, y = y)

  plot(opt.state)

  or = finalizeSMBO(opt.state)
  expect_number(or$y)
  expect_equal(getOptPathLength(or$opt.path), 12L)
  df = as.data.frame(or$opt.path)
  expect_numeric(df$x1)
  expect_numeric(df$x2)
  expect_set_equal(names(or$x), names(testp.fsphere.2d$pars))
})

test_that("human in the middle smbo works for multi objective", {
  par.set = getParamSet(testf.zdt1.2d)
  des = testd.zdt1.2d
  res = t(apply(des, 1, testf.zdt1.2d))
  control = makeMBOControl(n.objectives = 2)
  control = setMBOControlInfill(control, crit = crit.dib1)
  colnames(res) = control$y.name
  des = cbind(des,res)
  opt.state = initSMBO(par.set, design = des, control = control, noisy = FALSE, minimize = shouldBeMinimized(testf.zdt1.2d))
  plot(opt.state)
  proposePoints(opt.state)
  x = data.frame(x1 = 0.0002, x2 = 0.1)
  y = testf.zdt1.2d(x = x)
  updateSMBO(opt.state, x = x, y = y)
  or = finalizeSMBO(opt.state)
})

test_that("human in the middle smbo works for mixed spaces", {
  par.set = testp.mixed
  fun = testf.mixed
  design = testd.mixed
  design$y = sapply(convertRowsToList(design, name.list = TRUE, name.vector = TRUE), fun)
  control = makeMBOControl()
  opt.state = initSMBO(par.set = par.set, design = design, control = control, minimize = shouldBeMinimized(fun), noisy = isNoisy(fun))
  plot(opt.state)
  proposePoints(opt.state)
  x = data.frame(disc1 = "a", num1 = 0)
  y = fun(x)
  updateSMBO(opt.state, x = x, y = y)
  or = finalizeSMBO(opt.state)
  expect_equal(or$y, y)
  expect_equal(getOptPathLength(or$opt.path), 11L)
  df = as.data.frame(or$opt.path)
  expect_numeric(df$num1)
  expect_factor(df$disc1)
  expect_set_equal(names(or$x), names(par.set$pars))
})
