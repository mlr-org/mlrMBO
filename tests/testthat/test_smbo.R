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

  or = finalizeSMBO(opt.state)
  expect_number(or$y)
  expect_equal(getOptPathLength(or$opt.path), 12L)
  df = as.data.frame(or$opt.path)
  expect_numeric(df$x1)
  expect_numeric(df$x2)
  expect_set_equal(names(or$x), names(testp.fsphere.2d$pars))
})
