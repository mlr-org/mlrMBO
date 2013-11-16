#' Plot example run, either in 1D or 2D.
#'
#' The plot will show the following elements per iteration:
#' - The true objective function (solid line).
#' - The surrogate approximation, represented by its mean response.
#' - Surrogate mean +- 1 standard deviation, from local uncertainty.
#' - Infill criterion.
#'
#' In both plots the following elements are present
#' - Initial design points
#' - Points from previous sequentail iteraions
#' - Proposed point in current iteration.
#' @param object [\code{function}]\cr
#'   Objective function.
#' @param iters [\code{integer}]\cr
#'   Selected iterations of \code{x} to display.
#'   Default is all iterations.
#' @param pause [\code{logical(1)}]\cr
#'   Pause after each iteration?
#'   Default is \code{TRUE}.
#' @param densregion [\code{logical(1)}]\cr
#'   Should the background be shaded by the density of the
#'   posterior distribution?
#'   Looks nice, but is currently pretty slow. You might wnat to
#'   disable this if you want to do stuff more interactively.
#'   Default is \code{TRUE}.
#' @param se.factor [\code{numeric(1)}]\cr
#'   If the model provides local standard error estimation,
#'   in addition to the mean respone \code{yhat(x) +- se.factor * se(x)}
#'   is plotted above and below.
#'   Default is 1.
#' @param xlim [\code{numeric(2)}]\cr
#'   For 1D: \code{xlim} parameter for first and second plot.
#'   Default is range of x-values evaluated in run object \code{x}.
#' @param ylim [\code{numeric(2)}]\cr
#'   For 1D: \code{ylim} parameter for first plot, for the second plot \code{ylim} is always set
#'   automatically, depending on the range of the evaluated infill criterion.
#'   Default for the first plot is a heuristic to have the true function
#'   and \code{yhat(x) +- se.factor2 * se(x)} both in the plot. Note that this heuristic might
#'   change the \code{ylim} setting between plot iterations.
#' @param point.size [\code{numeric(1)}]\cr
#'   Point size for ploted points. Default ist 3.
#' @param line.size [\code{numeric(1)}]\cr
#'   Line width of the graphs of ploted functions.
#' @param trafo [\code{list}]\cr
#'   List of transformation functions of type \code{MBOTrafoFunction} for 
#'   the different plots.
#'   For 1D: The list elements should be named with "y" (applied to objective function and model) or "crit"
#'   (applied to the criterion). Only applied to plots with numeric parameters.
#'   For 2D: The list should contain at least one element "y", "yhat", "crit" or "se". This way one can 
#'   specify different transformations for different plots. If a single function is provided, this function 
#'    is used for all plots.
#FIXME if only these names are allowed, check them!
#' @param ... [\code{list}]\cr
#'   Further parameters.
#' @return [\code{list}]\cr
#'   List containing seperate ggplot plots for each iteration.
#' @S3method autoplot MBOExampleRun
#' @export
autoplot.MBOExampleRun = function(object, iters, pause=TRUE, densregion=TRUE,
  se.factor=1, xlim, ylim, point.size=3, line.size=1, trafo=NULL, ...) {

  iters.max = object$control$iters
  if (missing(iters)) {
    iters = seq_len(iters.max)
  } else {
    iters = convertIntegers(iters)
    checkArg(iters, "integer", min.len=1L, lower=1, upper=iters.max, na.ok=FALSE)
  }
  checkArg(pause, "logical", len=1L, na.ok=FALSE)
  checkArg(densregion, "logical", len=1L, na.ok=FALSE)
  checkArg(se.factor, "numeric", len=1L, na.ok=FALSE)
  if (!missing(xlim))
    checkArg(xlim, "numeric", len=2L, na.ok=FALSE)
  if (!missing(ylim))
    checkArg(ylim, "numeric", len=2L, na.ok=FALSE)

  n.params = object$n.params
  par.types = object$par.types
  trafo = buildTrafoList(n.params, trafo)

  if (n.params == 1) {
    autoplotExampleRun1d(object, iters=iters, xlim=xlim, ylim=ylim, se.factor=se.factor, pause=pause,
      point.size=point.size, line.size=line.size, trafo=trafo, densregion=densregion, ...)
  } else if (n.params == 2) {
    autoplotExampleRun2d(object, iters=iters, xlim=xlim, ylim=ylim, pause=pause,
      point.size=point.size, line.size=line.size, trafo=trafo, ...)
  } else {
    stopf("Functions with greater than 3 parameters are not supported.")
  }
}

# Sets up the correct format for trafo functions used
# by MBOExampleRun plot functions.
#
# @param n.params [\code{integer(1)}]\cr
#   Number of parameters.
# @param input.trafo [\code{list}]\cr
#   List of trafo functions provided by the user.
# @return [\code{list}]\cr
#   List of trafo functions with format that is expected by exampleRun plot functions.
buildTrafoList = function(n.params, input.trafo) {
  if (n.params == 1) {
    checkArg(names(input.trafo), subset=c("y", "crit"))
    trafo.defaults = list("y" = NULL, "crit" = NULL)
  } else {
    checkArg(names(input.trafo), subset=c("y", "yhat", "crit", "se"))
    trafo.defaults = list("y" = NULL, "yhat" = NULL, "crit" = NULL, "se" = NULL)
  }

  if (is.null(input.trafo))
    return(trafo.defaults)

  # if single function provided, apply it to all plots
  if (c("MBOTrafoFunction") %in% class(input.trafo)) {
    if (n.params == 1) {
      trafo = list("y" = input.trafo, "crit" = input.trafo)
    } else {
      trafo = list("y" = input.trafo, "yhat" = input.trafo, "crit" = input.trafo, "se" = input.trafo)
    }
  } else {
    # otherwise check if all elements are of an appropriate type
    lapply(input.trafo, function(t) 
      if(!is.null(t)) checkArg(t, "MBOTrafoFunction")
    )
    trafo = trafo.defaults
    trafo[names(input.trafo)] = input.trafo
  }
  return(trafo)
}
