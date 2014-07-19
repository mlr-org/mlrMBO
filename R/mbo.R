#' Optimizes a function with sequential model based optimization.
#'
#' @param fun [\code{function(x, ...)}]\cr
#'   Fitness function to minimize. The first argument has to be a list of values.
#'   The function has to return a single numerical value.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Collection of parameters and their constraints for optimization.
#' @param design [\code{data.frame} | NULL]\cr
#'   Initial design as data frame.
#'   If the parameters have corresponding trafo functions,
#'   the design must not be transformed before it is passed!
#'   If \code{NULL}, one is constructed from the settings in \code{control}.
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   Regression learner to model \code{fun}.
#' @param control [\code{\link{MBOControl}}]\cr
#'   Control object for mbo.
#' @param show.info [\code{logical(1)}]\cr
#'   Verbose output on console?
#'   Default is \code{TRUE}.
#' @param more.args [list]\cr
#'   Further arguments passed to fitness function.
#' @return [\code{\link{MBOSingleObjResult}} | \code{\link{MBOMultiObjResult}}]
#' @note It is possible to parallelize the evaluation of the target function to speed up the computation.
#' Internally the evaluation of the target function is realized with the R package parallelMap. See the mlrMBO tutorial
#' respectively the help pages of \code{\link[parallelMap]{parallelMap}} for instructions on how to set up parallization.
#' @export
mbo = function(fun, par.set, design = NULL, learner, control, show.info = TRUE, more.args = list()) {
  assertFlag(show.info)

  learner = checkLearner(learner, par.set, control)
  #FIXME: impute wrapper must be correctly configureg. TODO for BB
  # also: only do this for dep. params
  learner = makeImputeWrapper(learner, classes = list(
     numeric = imputeMedian(),
     factor = imputeMode()
  ))
  checkStuff(fun, par.set, design, learner, control)

  loadPackages(control)

  # configure mlr in an appropriate way
  configureMlr(on.learner.error = control$on.learner.error,
    show.learner.output = control$show.learner.output)

  # Call the correct mbo function
  mbo.fun = determineMBOFun(control)
  mbo.fun(fun = fun, par.set = par.set, design = design,
    learner = learner, control = control,
    show.info = show.info, more.args = more.args)
}

# Helper function which selects the correct mbo main function
# based on the user settings.
determineMBOFun = function(control) {
  if (control$infill.crit == "multiFid") {
    return(mboMultiFid)
  }
  if (control$number.of.targets == 1L) {
    return(mboSingleObj)
  } else {
    if (control$multicrit.method == "parego") {
      return(mboParEGO)
    }
  }
}
