test_that("infill crit initialization", {
  skip_on_covr() #because the crit.cb*$fun field is modified by covr and therefore expect_equal will fail
  fn = makeSphereFunction(2) # 2d continous space

  crit.cb1_val = setValue(crit.cb1, "opt.direction", "minimize")
  crit.cb_init = initCrit(crit.cb, fn)
  expect_equal(crit.cb1_val, crit.cb_init)
  crit.ei_init = initCrit(crit.ei, fn)
  expect_equal(crit.ei, crit.ei_init)

  crit.cb3 = makeMBOInfillCritCB(3)

  crit.cb3_val = setValue(crit.cb3, "opt.direction", "minimize")
  crit.cb3_init = initCrit(crit.cb3, fn)
  expect_equal(crit.cb3_val, crit.cb3_init)

  fn = makeSwiler2014Function() # 2d continous space + 1 factor variable

  crit.cb2_val = setValue(crit.cb2, "opt.direction", "minimize")
  crit.cb_init = initCrit(crit.cb, fn)
  expect_equal(crit.cb2_val, crit.cb_init)

  crit.cb3_val = setValue(crit.cb3, "opt.direction", "minimize")
  crit.cb3_init = initCrit(crit.cb3, fn)
  expect_equal(crit.cb3_val, crit.cb3_init)


})
