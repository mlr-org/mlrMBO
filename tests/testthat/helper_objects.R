testf.fsphere.1d = makeSphereFunction(dimensions = 1L)
testp.fsphere.1d = getParamSet(testf.fsphere.1d)
testd.fsphere.1d = generateDesign(5L, testp.fsphere.1d)

testf.fsphere.2d = makeSphereFunction(dimensions = 2L)
testp.fsphere.2d = getParamSet(testf.fsphere.2d)
testd.fsphere.2d = generateDesign(10L, testp.fsphere.2d)

testf.zdt1.2d = makeZDT1Function(dimensions = 2L)
testp.zdt1.2d = getParamSet(testf.zdt1.2d)
testd.zdt1.2d = generateDesign(10L, testp.zdt1.2d)

testfmco1 = makeMultiObjectiveFunction(
  fn = function(x) x^2,
  n.objectives = 2L,
  par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1)
)
testdesmco1 = generateDesign(10L, smoof::getParamSet(testfmco1))

testfmco2 = makeMultiObjectiveFunction(
  fn = function(x) c(1, -1) * x^2,
  n.objectives = 2L,
  par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1)
)
testdesmco2 = generateDesign(10L, smoof::getParamSet(testfmco2))
