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
#   Only rows that match will be taken for training, the others will be discarded.

makeRefinementWrapper = function(learner, factor.values = list()) {
  learner = checkLearner(learner)
  assertList(factor.values, names = "unique")

  trainfun = function(data, target, args) {
    data = as.data.table(data)
    keep = setdiff(names(data), names(args))
    data = data[args, keep, on = names(args), nomatch = 0L, with = FALSE]
    list(data = setDF(data), control = list())
  }

  predictfun = function(data, target, args, control) {
    data = as.data.table(data)
    if (nrow(data[!args, , on = names(args)]) > 0) {
      stopf("RefinementWrapper: Predicting on unseen Factors!")
    }
    data[, !names(args), with = FALSE]
  }

  lrn = makePreprocWrapper(learner, trainfun, predictfun, par.vals = factor.values)
  lrn$id = stringi::stri_replace(lrn$id, replacement = ".refined", regex = "[.]preproc$")
  addClasses(lrn, "RefinementWrapper")
}

#' @export
getLearnerProperties.RefinementWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), "factors")
}


#' @export
predictLearner.RefinementWrapper = function(.learner, .model, .newdata, ...) {
  data = .newdata
  args = .learner$par.vals
  #dots = list(...)
  #args2 = dots[setdiff(names(dots), names(.learner$par.vals))]
  #LEARNER <<- .learner
  #MODEL <<- .model
  # We assumes that value is always one element
  # Find all rows that match the key value pairs supplied by args
  # FIXME: as.character is really ugly here to handle factors with different level sets
  matches = Map(function(key, value) {as.character(data[[key]]) == as.character(value)}, key = names(args), value = args)
  good.idx = unlist(do.call(Map, c(f = any, matches)))
  res = matrix(nrow = nrow(data), ncol = 2)
  stopifnot(!is.null(.model$learner.model))
  model = getLearnerModel(.model, more.unwrap = FALSE)
  prediction = predict(model, newdata = data[good.idx, ], ...)
  res[good.idx, ] = as.matrix(prediction$data)
  #FIXME: Only works for minimization but that shoud be okay because internally we always mimimize, right?
  res[!good.idx, 1] = max(res[good.idx, 1]) * 10
  res[!good.idx, 2] = 0
  return(res)
}

#' @export
getLearnerProperties.RefinementWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), "factors")
}
