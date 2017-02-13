test_that("makeMBOLearner", {
  
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  
  
  #fully numeric case
  par.set = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeIntegerParam("x2", lower = -1, upper = 2)
  )
  
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      x[1]^2+x[2]^2
    },
    par.set = par.set
  )
  
  lrn = makeMBOLearner(ctrl, f)
  expect_equal(lrn, makeLearner("regr.km", predict.type = "se", 
    par.vals = list(covtype = "matern3_2", optim.method = "gen", nugget.stability = 10^-8)))
  expect_output(print(mbo(f, control = ctrl, learner = lrn)), "Recommended parameters")
  
  # fully numeric, without se prediction
  ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritMeanResponse())
  lrn = makeMBOLearner(ctrl, f)
  expect_equal(lrn, makeLearner("regr.km", 
    par.vals = list(covtype = "matern3_2", optim.method = "gen", nugget.stability = 10^-8)))
  expect_output(print(mbo(f, control = ctrl, learner = lrn)), "Recommended parameters")
  
  ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritCB())
  
  # fully numeric, noisy case
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      x[1]^2+x[2]^2 + rnorm(1, sd = 0.5)
    },
    par.set = par.set,
    noisy = TRUE
  )
  
  lrn = makeMBOLearner(ctrl, f)
  expect_equal(lrn, makeLearner("regr.km", predict.type = "se", 
    par.vals = list(covtype = "matern3_2", optim.method = "gen", nugget.estim = TRUE, jitter = TRUE )))
  expect_output(print(mbo(f, control = ctrl, learner = lrn)), "Recommended parameters")
  
  
  # discrete case
  
  par.set = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeIntegerParam("x2", lower = -1, upper = 2),
    makeDiscreteParam("x3", values = c("a", "b"))
  )
  
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      if (x[[3]] == "a")
        x[[1]]^2+x[[2]]^2
      else
        x[[1]]^2+x[[2]]^2 + 20
    },
    par.set = par.set,
    has.simple.signature = FALSE
  )
  
  lrn = makeMBOLearner(ctrl, f)
  expect_equal(lrn, makeLearner("regr.randomForest", predict.type = "se", 
    par.vals = list(se.method = "jackknife", keep.inbag = TRUE)))
  expect_output(print(mbo(f, control = ctrl, learner = lrn)), "Recommended parameters")
  
  # discrete case, without se estimation
  ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritMeanResponse())
  lrn = makeMBOLearner(ctrl, f)
  expect_equal(lrn, makeLearner("regr.randomForest",
    par.vals = list(se.method = "jackknife", keep.inbag = TRUE)))
  expect_output(print(mbo(f, control = ctrl, learner = lrn)), "Recommended parameters")
  
  ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritCB())
  
  
  # discrete dependend case
  
  par.set = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeIntegerParam("x2", lower = -1, upper = 2, requires = quote(x3 == "b")),
    makeDiscreteParam("x3", values = c("a", "b"))
  )
  
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      if (x[[3]] == "a")
        x[[1]]^2
      else
        x[[1]]^2+x[[2]]^2 + 20
    },
    par.set = par.set,
    has.simple.signature = FALSE
  )
  
  lrn = makeMBOLearner(ctrl, f)
  expect_equal(lrn, makeImputeWrapper(makeLearner("regr.randomForest", predict.type = "se", 
    par.vals = list(se.method = "jackknife", keep.inbag = TRUE)), classes = list(numeric = imputeMax(2), factor = imputeConstant("__miss__"))))
  expect_output(print(mbo(f, control = ctrl, learner = lrn)), "Recommended parameters")
  
  par.set = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeDiscreteParam("x2", values = c("c", "d"), requires = quote(x3 == "b")),
    makeDiscreteParam("x3", values = c("a", "b"))
  )
  
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      if (x[[3]] == "a")
        x[[1]]^2
      else
        if (x[[2]] == "c")
          x[[1]]^2 + 20
        else
          20
    },
    par.set = par.set,
    has.simple.signature = FALSE
  )
  
  lrn = makeMBOLearner(ctrl, f)
  expect_equal(lrn, makeImputeWrapper(makeLearner("regr.randomForest", predict.type = "se", 
    par.vals = list(se.method = "jackknife", keep.inbag = TRUE)), classes = list(numeric = imputeMax(2), factor = imputeConstant("__miss__"))))
  expect_output(print(mbo(f, control = ctrl, learner = lrn)), "Recommended parameters")
  
})