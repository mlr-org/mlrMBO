context("focus search")

test_that("simple random search, no dependencies, no focusing", {
  obj.fun = smoof::makeSingleObjectiveFunction(
    fn = function(x) {
      x$x0 + (x$x2 == "v1") + sum(x$x4^2)
    },
    par.set = makeParamSet(
      makeIntegerParam("x0", lower = 3L, upper = 4L),
      makeNumericParam("x1", lower = -2, upper = 2),
      makeDiscreteParam("x2", values = c("v1", "v2")),
      makeLogicalParam("x3"),
      makeNumericVectorParam("x4", len = 2L, lower = 1, upper = 2)
    ),
    has.simple.signature = FALSE
  )

  des = generateTestDesign(20L, getParamSet(obj.fun))
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  ctrl = setMBOControlInfill(ctrl, opt = "focussearch",
    opt.restarts = 1L, opt.focussearch.maxit = 1L, opt.focussearch.points = 30L)
  or = mbo(obj.fun, des, control = ctrl)
  expect_number(or$y)
})

test_that("dependent params, but no focusing", {
  obj.fun = smoof::makeSingleObjectiveFunction(
    fn = function(x) {
      x = removeMissingValues(x)
      x$x0 + ifelse(x$foo == "a", x$x1, as.numeric(x$x2 == "v2"))
    },
    par.set = makeParamSet(
      makeDiscreteParam("foo", values = c("a", "b")),
      makeNumericParam("x0", lower = 3, upper = 4),
      makeNumericParam("x1", lower = -2, upper = 2, requires = quote(foo == "a")),
      makeDiscreteParam("x2", values = c("v1", "v2"), requires = quote(foo == "b"))
    ),
    has.simple.signature = FALSE
  )

  learner = makeLearner("regr.randomForest", predict.type = "se", se.method = "sd")
  learner = makeImputeWrapper(learner, classes = list(numeric = imputeMedian(), factor = imputeMode()))
  des = generateTestDesign(20L, getParamSet(obj.fun))
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  ctrl = setMBOControlInfill(ctrl, opt = "focussearch", opt.restarts = 1L, opt.focussearch.maxit = 1L,
    opt.focussearch.points = 500L)
  or = mbo(obj.fun, des, learner = learner, control = ctrl)
  expect_true(all(is.na(as.data.frame(or$opt.path)[["error.model"]])))
  expect_number(or$y)
})


test_that("complex param space, dependencies, focusing, restarts", {
  obj.fun = smoof::makeSingleObjectiveFunction(
    fn = function(x) {
      x = removeMissingValues(x)
      tmp1 = (sqrt(x$real1)) + x$int1^2 - mean(x$realVec) + sum(x$intVec)
      if (x$real2 < 0) tmp2 = ifelse(x$disc1 == 'foo', -5, 5)
      if (x$real2 > 0) tmp2 = 5 + x$real3
      if (x$real2 == 0) tmp2 = 0
      if (x$disc2 == 'a') tmp3 = log(x$realA) + x$intA^4 + ifelse(x$discA == 'm', 5, 0)
      if (x$disc2 == 'b') tmp3 = exp(x$realB) + ifelse(x$discB == 'R', sin(x$realBR), sin(x$realBNR))
      if (x$disc2 == "c") tmp3 = 500
      tmp1 + tmp2 + tmp3
    },
    par.set = makeParamSet(
      makeNumericParam("real1", lower = 0, upper = 1000),
      makeIntegerParam("int1", lower = -100, upper = 100),
      makeNumericVectorParam("realVec", len = 10, lower = -50, upper = 50),
      makeIntegerVectorParam("intVec", len = 3, lower = 0, upper = 100),
      makeNumericParam("real2", lower = -1, upper = 1),
      makeDiscreteParam("disc1", values = c("foo", "bar"), requires = quote(real2 < 0)),
      makeNumericParam("real3", lower = -100, upper = 100, requires = quote(real2 > 0)),
      makeDiscreteParam("disc2", values = c("a", "b", "c")),
      makeNumericParam("realA", lower = 0, upper = 100, requires = quote(disc2 == "a")),
      makeIntegerParam("intA", lower = -100, upper = 100, requires = quote(disc2 == "a")),
      makeDiscreteParam("discA", values = c("m", "w"), requires = quote(disc2 == "a")),
      makeNumericParam("realB", lower = -100, upper = 100, requires = quote(disc2 == "b")),
      makeDiscreteParam("discB", values = c("R", "NR"), requires = quote(disc2 == "b")),
      makeNumericParam("realBR", lower = 0, upper = 2*pi,
        requires = quote(identical(discB, "R") && identical(disc2, "b"))),
      makeNumericParam("realBNR", lower = 0, upper = 2*pi,
        requires = quote(identical(discB, "NR") && identical(disc2, "b")))
    ),
    has.simple.signature = FALSE
  )

  des = generateDesign(20L, getParamSet(obj.fun))
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.ei, opt = "focussearch",
    opt.restarts = 2L, opt.focussearch.maxit = 2L, opt.focussearch.points = 100L)
  learner = makeLearner("regr.randomForest", predict.type = "se", se.method = "sd")
  learner = makeImputeWrapper(learner, classes = list(numeric = imputeMedian(), factor = imputeMode()))

  or = mbo(obj.fun, des, learner = learner, control = ctrl)
  expect_number(or$y)
})
