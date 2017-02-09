test_that("infill crit initialization", {

  ps = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeIntegerParam("x2", lower = -1, upper = 2)
  )
  
  expect_equal(crit.cb1, initCrit(crit.cb, ps))
  expect_equal(crit.ei, initCrit(crit.ei, ps))
  
  crit.cb3 = makeMBOInfillCriterionCB(3)
  
  expect_equal(crit.cb3, initCrit(crit.cb3, ps))
  
  ps = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeIntegerParam("x2", lower = -1, upper = 2),
    makeDiscreteParam("x3", values = c("a", "b"))
  )
  
  expect_equal(crit.cb2, initCrit(crit.cb, ps))
  expect_equal(crit.cb3, initCrit(crit.cb3, ps))
  
  
})