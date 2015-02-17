context("plotEAF")

test_that("plotEAF works", {

  obj.fun = makeMBOFunction(function(x) c(1, -1) * x^2)
  par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1)

  # Test normal run
  learner = makeLearner("regr.km", nugget.estim = TRUE)
  ctrl = makeMBOControl(iters = 2, number.of.targets = 2L, init.design.points = 5L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = 10)
  ctrl = setMBOControlMultiCrit(ctrl, method = "parego", parego.s = 100)

  # build aritificial list of opt pathes for two algorithms
  n.reps = 3L
  opt.pathes = lapply(seq(n.reps), function(i) {
    set.seed(i)
    res = mbo(obj.fun, par.set, learner = learner, control = ctrl)
    return(res$opt.path)
  })
  opt.pathes = list("algo1" = opt.pathes, "algo2" = opt.pathes)

  # plot eaf and check returned data frame
  res = plotEAF(opt.pathes)

  algo.names = c("algo1", "algo2")

  expect_true(is.data.frame(res))
  expect_true(setequal(colnames(res), c("y_1", "y_2", ".algo", ".repl")))
  expect_true(all(res$.algo %in% algo.names))
  expect_true(all(res$.repl >= 1) && all(res$.repl <= n.reps))
})
