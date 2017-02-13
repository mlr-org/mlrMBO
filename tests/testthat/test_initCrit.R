test_that("infill crit initialization", {

  fn = makeSphereFunction(2) # 2d continous space

  expect_equal(setValue(crit.cb1, "opt.direction", "minimize"), initCrit(crit.cb, fn))
  expect_equal(crit.ei, initCrit(crit.ei, fn))

  crit.cb3 = makeMBOInfillCritCB(3)

  expect_equal(setValue(crit.cb3, "opt.direction", "minimize"), initCrit(crit.cb3, fn))

  fn = makeSwiler2014Function() # 2d continous space + 1 factor variable

  expect_equal(setValue(crit.cb2, "opt.direction", "minimize"), initCrit(crit.cb, fn))
  expect_equal(setValue(crit.cb3, "opt.direction", "minimize"), initCrit(crit.cb3, fn))


})
