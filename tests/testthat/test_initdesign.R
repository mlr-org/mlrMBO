context("init design")

test_that("init design", {
  # without trafo
  obj.fun = smoof::makeSingleObjectiveFunction(
    fn = function(x) sum(x^2),
    par.set = makeParamSet(
      makeNumericParam("x1", lower = -2, upper = 1),
      makeNumericParam("x2", lower = -1, upper = 2)
    )
  )

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10L)

  des = generateTestDesign(10L, par.set = getParamSet(obj.fun))

  or = mbo(obj.fun, des, control = ctrl)
  expect_equal(getOptPathLength(or$opt.path), 11L)
  expect_number(or$y)

  # with precomputed y
  des$y = apply(des, 1L, obj.fun)
  or = mbo(obj.fun, des, control =  ctrl)
  expect_equal(getOptPathLength(or$opt.path), 11L)
  expect_number(or$y)

  # with trafo
  obj.fun = setAttribute(obj.fun, "par.set",
    makeParamSet(
      makeNumericParam("x1", lower = -2, upper = 1),
      makeNumericParam("x2", lower = -1, upper = 2, trafo = function(x) x/2)
    )
  )

  des = generateTestDesign(10, getParamSet(obj.fun), trafo = TRUE)
  expect_error(mbo(obj.fun, des, control = ctrl))

  # wrong col names in inir design
  des = data.frame(x1 = c(-0.49, 0.24, -1.47), z = c(-0.70, 1.29, 0.73))
  expect_error(mbo(obj.fun, des, control = ctrl))
})
