#' Renders plots for exampleRun objects, either in 1D or 2D and
#' displays them.
#'
#' The graphical output depends on the target function at hand.
#' - For 1D numeric functions the upper plot shows the true function (if known),
#' the model and the (infill) points. The lower plot shows the infill criterion.
#' - For 2D mixed target functions only one plot is displayed.
#' - For 2D numeric only target functions up to four plots are presented to the
#'   viewer:
#'   - levelplot of the true function landscape (with [infill] points),
#'   - levelplot of the model landscape (with [infill] points),
#'   - levelplot of the infill criterion
#'   - levelplot of the standard error (only if learner supports standard error estimation).
#' - For bi-criteria target functions the upper plot shows the target space and the lower
#'   plot displays the x-space.
#'
#' @param object [\code{function}]\cr
#'   \code{MBOExampleRun} or \code{MBOExampleRunMulticrit} object.
#' @param iters [\code{integer}]\cr
#'   Selected iterations of \code{object} to produce plots.
#'   Default is all iterations.
#' @param pause [\code{logical(1)}]\cr
#'   Should the process be paused after each iteration?
#'   Default is \code{TRUE}.
#' @param densregion [\code{logical(1)}]\cr
#'   Should the background be shaded?
#'   Default is \code{TRUE}.
#' @param se.factor [\code{numeric(1)}]\cr
#'   If the model provides local standard error estimation,
#'   in addition to the mean response \code{yhat(x) +- se.factor * se(x)}
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
#'   Line width of the graphs of plotted functions.
#' @param trafo [\code{list}]\cr
#'   List of transformation functions of type \code{MBOTrafoFunction} for
#'   the different plots.
#'   For 1D: The list elements should be named with \dQuote{y} (applied to objective function and model)
#'   or \dQuote{crit} (applied to the criterion). Only applied to plots with numeric parameters.
#'   For 2D: The list should contain at least one element \dQuote{y}, \dQuote{yhat}, \dQuote{crit}
#'   or \dQuote{se}.
#'   This way one can specify different transformations for different plots.
#'   If a single function is provided, this function is used for all plots.
#' @param ... [any]\cr
#'   Currently not used.
#' @return Nothing.
#' @export
plotExampleRun = function(object, iters, pause = TRUE, 
  densregion = TRUE, se.factor = 1,
  xlim = NULL, ylim = NULL,
  point.size = 3, line.size = 1,
  trafo = NULL, ...) {
	iters.max = object$control$iters
	if (missing(iters)) {
		iters = 1:iters.max
  }
  assertInteger(iters, lower = 0L, upper = iters.max, any.missing = FALSE)
	assertFlag(pause)
  assertFlag(densregion)
  assertNumber(se.factor, lower = 0)
  assertNumber(point.size, lower = 1)
  assertNumber(line.size, lower = 1)

	if (!is.null(xlim))
  	assertNumeric(xlim, len = 2L, any.missing = FALSE)
	if (!is.null(ylim))
 		assertNumeric(ylim, len = 2L, any.missing = FALSE)

	requirePackages("gridExtra", why = "plotExampleRun")
	
  # Helper to arragne plot via gridExtra and pause process
  arrangePlots = function(plots) {
    plots = Filter(Negate(isScalarNA), plots)
    n.plots = length(plots)
    n.row = if (n.plots <= 2) 2L else { if (n.plots <= 3) 1L else 2L }
    do.call("grid.arrange", c(plots, nrow = n.row, main = "test"))
    pause()  
  }

	for (iter in iters) {
    # get rendered plot data		
		plots = renderExampleRunPlot(object, iter = iter, densregion = densregion, se.factor = se.factor,
			xlim = xlim, ylim = ylim, point.size = point.size, line.size = line.size, trafo = trafo, ...)
    if (!inherits(plots, "ggplot")) {
      # in this case we have multipoint proposal with parego: list of plots for each proposed point
      for (jter in 1:length(plots)) {
        arrangePlots(plots[[jter]])
      }
    } else {
      arrangePlots(plots)
    }
	}
}