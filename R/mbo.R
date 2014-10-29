#' @title Optimizes a function with sequential model based optimization.
#'
#' @description
#' It is possible to parallelize the evaluation of the target function to speed up the computation.
#' Internally the evaluation of the target function is realized with the R package parallelMap.
#' See the mlrMBO tutorial and the Github project pages of parallelMap for instructions
#' on how to set up parallelization.
#' Currently the following operations are performed in parallel, if parallelization is switched on:
#' \itemize{
#' \item{Evaluation of the initial design.}
#' \item{Evaluation of multiple proposed points in one iteration (if multiple are proposed).}
#' \item{Model fitting / point proposal - in some cases where independent, expensive operations are performed.}
#' }
#' Details regarding the last bullet point above:
#' \describe{
#' \item{Singlecrit MBO with LCB multipoint}{Parallel optimization of LCBs for the lambda-values.}
#' \item{Multicrit MBO with ParEGO}{Parallel optimization of scalarization functions.}
#' }
#'
#' @param fun [\code{function(x, ...)}]\cr
#'   Fitness function to minimize. The first argument has to be a list of values.
#'   The function has to return a single numerical value.
#'   In fact it is possible to return even more information which will be stored
#'   in the optimization path. To achieve this, simply append the attribute \dQuote{extras}
#'   to the return value of the target function. This has to be a named list of scalar values.
#'   Each of these values will be stored additionally in the optimization path.
#' @template arg_parset
#' @param design [\code{data.frame} | NULL]\cr
#'   Initial design as data frame.
#'   If the parameters have corresponding trafo functions,
#'   the design must not be transformed before it is passed!
#'   If \code{NULL}, one is constructed from the settings in \code{control}.
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   Regression learner to model \code{fun}.
#' @template arg_control
#' @template arg_showinfo
#' @param more.args [list]\cr
#'   Further arguments passed to fitness function.
#' @return [\code{\link{MBOSingleObjResult}} | \code{\link{MBOMultiObjResult}}]
#' @export
mbo = function(fun, par.set, design = NULL, learner, control,
  show.info = getOption("mlrMBO.show.info", TRUE), more.args = list()) {

  assertFlag(show.info)
  learner = checkLearner(learner, par.set, control)
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
  } else {
    return(mboTemplate)
  }
}
