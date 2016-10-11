context("groupinfill")

test_that("basic group infill works", {
  set.seed(1)
  lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
  ctrl = makeMBOControl(propose.points = 5L)
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  ctrl = setMBOControlInfill(ctrl, crit = "cb")
  ctrl = setMBOControlMultiPoint(ctrl, method = "groupinfill")

  res = mbo(testf.fsphere.2d, testd.fsphere.2d, learner = lrn, control = ctrl)
  op = as.data.frame(res$opt.path)
  expect_true(all(is.na(op$cb[1:10])))
  expect_true(all(!is.na(op$cb[11:20])))
  expect_true(all(op$prop.type[11:20] == "grouped.infill_cb")) 
  expect_is(res, "MBOResult")
  expect_true(res$y < 0.5)
})
