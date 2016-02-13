context("check design")

test_that("test checks for initial design and new design", {
  par.set = makeParamSet(
    makeIntegerParam("v", lower = 1, upper = 2),
    makeDiscreteParam("w", values = c("a", "b", "e", "g")),
    makeDiscreteParam("x", values = c("a", "b", "c", "d"), requires = quote(w == "a")),
    makeNumericParam("y", lower = 1, upper = 2, requires = quote(w == "b")),
    makeNumericParam("z", lower = 1, upper = 2, requires = quote(w == "e"))
  )

  design1 = generateDesign(8, par.set)
  expect_false(checkInitDesign(design1, par.set))

  design2 = generateDesign(30, par.set)
  expect_true(checkInitDesign(design2, par.set))

  checked = checkPredictionData(design2, design1)
  expect_true(any(checked))
  expect_true(any(!checked))

  design = data.frame(x = c("a", "b"))
  new.design = data.frame(x = c("a", "c", "b", "d", "a", NA))
  expect_equal(checkPredictionData(new.design, design),
    c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
})
