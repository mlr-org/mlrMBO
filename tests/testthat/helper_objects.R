generateTestDesign = function(n = 10L, par.set, ...) {
  for (i in 1:100) {
    design = generateDesign(n = n, par.set = par.set, ...)
    if (checkInitDesign(design, par.set))
      return(design)
  }
  stop("Unable to create a valid design in generateTestDesign!")
}

testf.fsphere.1d = makeSphereFunction(dimensions = 1L)
testp.fsphere.1d = getParamSet(testf.fsphere.1d)
testd.fsphere.1d = generateTestDesign(5L, testp.fsphere.1d)

testf.fsphere.2d = makeSphereFunction(dimensions = 2L)
testp.fsphere.2d = getParamSet(testf.fsphere.2d)
testd.fsphere.2d = generateTestDesign(10L, testp.fsphere.2d)

testf.zdt1.2d = makeZDT1Function(dimensions = 2L)
testp.zdt1.2d = getParamSet(testf.zdt1.2d)
testd.zdt1.2d = generateTestDesign(10L, testp.zdt1.2d)


testfmco1 = makeMultiObjectiveFunction(
  fn = function(x) x^2,
  n.objectives = 2L,
  par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1)
)
testdesmco1 = generateTestDesign(10L, getParamSet(testfmco1))

testfmco2 = makeMultiObjectiveFunction(
  fn = function(x) c(1, -1) * x^2,
  n.objectives = 2L,
  par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1)
)
testdesmco2 = generateTestDesign(10L, getParamSet(testfmco2))

# slow test function
testf.fsphere.1d.slow = makeSingleObjectiveFunction(
  name = "slow.function",
  fn = function(...) {
    Sys.sleep(0.25)
    testf.fsphere.1d(...)
  },
  has.simple.signature = TRUE,
  vectorized = isVectorized(testf.fsphere.1d),
  noisy = isNoisy(testf.fsphere.1d),
  par.set = getParamSet(testf.fsphere.1d),
  minimize = shouldBeMinimized(testf.fsphere.1d),
  fn.mean = getMeanFunction(testf.fsphere.1d),
  tags = getTags(testf.fsphere.1d),
  global.opt.params = getGlobalOptimum(testf.fsphere.1d)$param,
  global.opt.value = getGlobalOptimum(testf.fsphere.1d)$value
)

# mixed space test functio
testp.mixed = makeParamSet(
  makeDiscreteParam("disc1", values = c("a", "b")),
  makeNumericParam("num1", lower = 0, upper = 1)
)
testf.mixed = makeSingleObjectiveFunction(
  fn = function(x) {
    ifelse(x$disc1 == "a", x$num1 * 2 - 1, 1 - x$num1)
  },
  par.set = testp.mixed,
  has.simple.signature = FALSE
)
testd.mixed = generateTestDesign(10L, testp.mixed)
