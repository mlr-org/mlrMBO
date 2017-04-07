context("plotEAF")

test_that("plotEAF works", {
  f = makeMultiObjectiveFunction(
    fn = function(x) c(1, -1) * x^2,
    par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1),
    n.objectives = 2L
  )
  des = generateTestDesign(5L, getParamSet(f))

  # Test normal run
  learner = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE)
  ctrl = makeMBOControl(n.objectives = 2L)
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10)
  ctrl = setMBOControlMultiObj(ctrl, method = "parego", parego.s = 100)

  # build aritificial list of opt pathes for two algorithms
  n.reps = 3L
  opt.pathes = lapply(seq(n.reps), function(i) {
    set.seed(i)
    res = mbo(f, des, learner = learner, control = ctrl)
    return(res$opt.path)
  })
  opt.pathes = list("algo1" = opt.pathes, "algo2" = opt.pathes)

  # plot eaf and check returned data frame
  res = plotEAF(opt.pathes)

  algo.names = c("algo1", "algo2")

  expect_data_frame(res)
  expect_set_equal(colnames(res), c("y_1", "y_2", ".algo", ".repl"))
  expect_subset(as.character(res$.algo), algo.names)
  expect_true(all(res$.repl >= 1) && all(res$.repl <= n.reps))
})
