test_that("infill crit initialization", {

  fn = makeSphereFunction(2) # 2d continous space

  expect_equal(crit.cb1, initCrit(crit.cb, fn))
  expect_equal(crit.ei, initCrit(crit.ei, fn))

  crit.cb3 = makeMBOInfillCritCB(3)

  expect_equal(crit.cb3, initCrit(crit.cb3, fn))

  fn = makeSwiler2014Function() # 2d continous space + 1 factor variable

  expect_equal(crit.cb2, initCrit(crit.cb, fn))
  expect_equal(crit.cb3, initCrit(crit.cb3, fn))


})
