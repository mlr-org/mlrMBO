context("mbo resample")

test_that("mbo works with resampling", {
  learner = makeLearner("regr.randomForest", predict.type = "se", se.method = "sd")
  ctrl = makeMBOControl(resample.at = c(1, 3))
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10L)
  or = mbo(testf.fsphere.2d, testd.fsphere.2d, learner, ctrl)
  x = or$resample.results
  expect_list(x, len = 2)
  expect_true(is.numeric(x[[1]]$aggr) && is.numeric(x[[2]]$aggr))
  expect_equal(names(x), c("1", "3"))
})
