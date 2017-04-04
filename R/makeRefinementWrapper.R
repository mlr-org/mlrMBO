# @title Refinement Wrapper
#
# @description
# Wraps a mlr learner with a preprocessing wrapper:
# The data is subsetted to (a) rows exactly matching the values provided in argument
# \code{factor.values} and (b) to the numerical features.
# This is used internally for Kriging Refinement.
#
# @param learner [\code{\link[mlr]{Learner}}]\cr
#   A mlr learner.
# @param factor.values [\code{named list}]\cr
#   Named list of factor values to match rows of the data.
#   Only rows that match will be taken for training, the others will be discarded-
makeRefinementWrapper = function(learner, factor.values = list()) {
  learner = checkLearner(learner)
  assertList(factor.values, names = "unique")

  trainfun = function(data, target, args) {
    data = as.data.table(data)
    keep = setdiff(names(data), names(args))
    data = data[args, keep, on = names(args), nomatch = 0L, with = FALSE]
    list(data = setDF(data), control = list())
  }

  # predictfun = function(data, target, args, control) {
  #   data = as.data.table(data)
  #   if (nrow(data[!args, , on = names(args)]) > 0) {
  #     stopf("RefinementWrapper: Predicting on unseen Factors!")
  #   }
  #   data[, !names(args), with = FALSE]
  # }
  id = stri_paste(learner$id, "refinement", sep = ".")
  x = makeBaseWrapper(id, type = learner$type, next.learner = learner, learner.subclass = "RefinementWrapper", model.subclass = "RefinementModel")
  x$train = trainfun
  return(x)
}

#' @export
trainLearner.RefinementWrapper = function(.learner, .task, .subset, ...) {
  trainLearner.PreprocWrapper(.learner, .task, .subset, ...)
}

#' @export
predictLearner.PreprocWrapper = function(.learner, .model, .newdata, ...) {
  .newdata = .learner$predict(.newdata, .model$task.desc$target,
                              .learner$par.vals, .model$learner.model$control)
  if (!is.data.frame(.newdata))
    stop("Preprocessing must result in a data.frame!")
  NextMethod(.newdata = .newdata)
}

#' @export
getLearnerProperties.RefinementWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), "factors")
}
