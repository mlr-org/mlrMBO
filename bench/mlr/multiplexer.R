
base.learners = list(
  # makeLearner("classif.rpart"),
  makeLearner("classif.ksvm"),
  makeLearner("classif.randomForest"),
  makeLearner("classif.gbm")
  # makeLearner("classif.lda")

)

mplexer = makeModelMultiplexer(base.learners)


par.set = makeModelMultiplexerParamSet(mplexer,

  classif.randomForest = makeParamSet(
    makeIntegerParam("ntree", lower = 10, upper = 500),
    makeIntegerParam("mtry", lower = 1, upper = 20)
  ),

  classif.gbm = makeParamSet(
    makeIntegerParam("n.trees", lower = 100L, upper = 5000L),
    makeIntegerParam("interaction.depth", lower = 1L, upper = 4L),
    makeNumericParam("shrinkage", lower = 1e-5, upper = 1e-1)
  ),

  classif.ksvm = makeParamSet(
    makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
    makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
  )
)


