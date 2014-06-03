#'  Optimizes a function with sequential model based optimization.
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
#' @return [\code{list}]:
#'   \item{x [\code{list}]}{Named list of proposed optimal parameters.}
#'   \item{y [\code{numeric(1)}]}{Value of fitness function at \code{x}, either from evals during optimization or from requested final evaluations, if those were greater than 0.}
#'   \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
#'   \item{times [\code{numeric}]}{Vector of times it took to evaluate the objective.}
#'   \item{models [List of \code{\link[mlr]{WrappedModel}}]}{List of saved regression models.}
#'   \item{multipoint.lcb.lambdas [\code{matrix(iters, proposed.points)}]}{Sampled lambda values for multipoint lcb method.}
#' @note It is possible to parallelize the evaluation of the target function to speed up the computation.
#' Internally the evaluation of the target function is realized with the R package parallelMap. See the mlrMBO tutorial
#' respectively the help pages of \code{\link[parallelMap]{parallelMap}} for instructions on how to set up parallization.
#' @export
#' @aliases MBOResult

mbo = function(fun, par.set, design=NULL, learner, control, show.info=TRUE, more.args=list()) {
  
  #FIXME: more param checks
  checkStuff(fun, par.set, design, learner, control)
  loadPackages(control)
  
  # configure mlr in an appropriate way
  configureMlr(on.learner.error = control$on.learner.error,
    show.learner.output=control$show.learner.output)
  
  # FIXME: I am unsure wether we should do this, but otherwise RF sucks
  # if it is a good idea it is not not general enuff
  if (inherits(learner, "regr.randomForest")) {
    learner = setHyperPars(learner, fix.factors=TRUE)
  }
  
  # Call the correct mbo function
  if (control$number.of.targets == 1L)
    mboSingleObj(fun = fun, par.set = par.set, design = design,
      learner = learner, control = control, show.info = show.info, more.args = more.args)
  else {
    if (control$multicrit.method == "parEGO")
      mboParEGO(fun = fun, par.set = par.set, design = design,
        learner = learner, control = control, show.info = show.info, more.args = more.args)
  }
}