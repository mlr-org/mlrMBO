makeTrafoWrapper = function(learner, trafo, trafo.inverse, trafo.se = identity, par.set = makeParamSet(), par.vals = list()) {
  learner = checkLearner(learner)
  assertFunction(trafo)
  assertFunction(trafo.inverse)
  if (! all(trafo.inverse(trafo(c(0.2,0.5))) == c(0.2,0.5))) {
    stop("Either trafo does not work on vectors or they trafo.inverse(trafo(x)) != x")
  }
  assertClass(par.set, classes = "ParamSet")
  checkList(par.vals)
  if (!isProperlyNamed(par.vals))
    stop("'par.vals' must be a properly named list!")

  id = paste(learner$id, "trafo", sep = ".")
  x = mlr:::makeBaseWrapper(id, type = learner$type, next.learner = learner, par.set = par.set,
    par.vals = par.vals, learner.subclass = "TrafoWrapper", model.subclass = "TrafoModel")
  x$trafo = trafo
  x$trafo.inverse = trafo.inverse
  x$trafo.se = trafo.se
  return(x)
}

trainLearner.TrafoWrapper = function(.learner, .task, .subset, ...) {
  data = getTaskData(.task, .subset, target.extra = TRUE)
  y = .learner$trafo(data$target)
  data = data$data
  data[,getTaskTargetNames(.task)] = y
  .task = mlr:::changeData(.task, data)
  m = train(.learner$next.learner, .task)
  x = mlr:::makeChainModel(next.model = m, cl = "TrafoModel")
  return(x)
}


predictLearner.TrafoWrapper = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model$next.model, newdata = .newdata)
  y = getPredictionResponse(p)
  y = .learner$trafo.inverse(y)
  if (.model$learner$predict.type == "se") {
    se = .learner$trafo.se(getPredictionSE(p))
    return(matrix(c(y, se), ncol = 2))
  } else {
    return(y)
  }
}

isFailureModel.TrafoModel = function(model) {
  mod = model$learner.model$next.model
  mlr:::isFailureModel(mod)
}

# getLearnerProperties.MultiFidWrapper = function(learner) {
#   mlr::getLearnerProperties(learner$next.learner))
# }
