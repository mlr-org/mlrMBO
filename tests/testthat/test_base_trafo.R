context("trafo")

test_that("trafo works", {
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      stopifnot(x >= 0)
      return(x)
    },
    par.set = makeParamSet(makeNumericParam("x", lower = -15, upper = 0, trafo = function(x) -x)),
    has.simple.signature = TRUE
  )
  learner = makeLearner("regr.randomForest")
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 100)
  des = generateTestDesign(10L, getParamSet(f))
  or = mbo(f, design = des, learner = learner, control = ctrl, show.info = TRUE)
  op = as.data.frame(or$opt.path)
  expect_true(all(op$x == -1 * op$y))
})
