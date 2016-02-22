context("randomSearch")

test_that("basic randomSearch works", {
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 20L)
  or = randomSearch(fun = testf.fsphere.2d, design = testd.fsphere.2d, control = ctrl, show.info = TRUE)
  expect_true(!is.na(or$y))
  expect_equal(or$y, testf.fsphere.2d(or$x))
  expect_equal(getOptPathLength(or$opt.path), 30)
  expect_true(is.list(or$x))
  expect_equal(names(or$x), names(testp.fsphere.2d$pars))
  
  #FIXME, check result if time is met before iters
})

