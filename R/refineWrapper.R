#' @title Refinement Wrapper
#'
#' @template arg_learner
#' @export
makeRefinementWrapper = function(learner, factor.values = list()) {
  learner = checkLearner(learner)
  assertList(factor.values, names = "unique")

  trainfun = function(data, target, args) {
    data = data.table::as.data.table(data)
    args = data.table::as.data.table(args)
    keep = setdiff(names(data), names(args))
    data = data[args, keep, on = names(args), nomatch = 0L, with = FALSE]
    list(data = as.data.frame(data), control = list())
  }

  predictfun = function(data, target, args, control) {
    # TODO: additional check that args match with trainfun
    # -> make sure we are not predicting on different factor levels
    as.data.table(data)[, !names(args), with = FALSE]
  }

  lrn = makePreprocWrapper(learner, trainfun, predictfun, par.vals = factor.values)
  lrn$id = stringi::stri_replace(lrn$id, replacement = ".refined", regex = "[.]preproc$")
  addClasses(lrn, "RefinementWrapper")
}

#' @export
getLearnerProperties.RefinementWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), "factors")
}
