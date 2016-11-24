#' @title Renders plots for exampleRun objects, either in 1D or 2D, or
#' exampleRunMultiObj objects.
#'
#' @description
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
#'   \code{MBOExampleRun} or \code{MBOExampleRunMultiObj} object.
#' @param iter [\code{integer}]\cr
#'   Selected iteration of \code{object} to render plots for.
#' @param densregion [\code{logical(1)}]\cr
#'   Should the background be shaded? Default is \code{TRUE}.
#'   Only used if learner supports computation of standard error.
#' @param se.factor [\code{numeric(1)}]\cr
#'   If the model provides local standard error estimation,
#'   in addition to the mean response \code{yhat(x) +- se.factor * se(x)}
#'   is plotted above and below.
#'   Default is 1.
#' @param single.prop.point.plots [\code{logical(1)}]\cr
#'   Parameter for MOI-MBO Multipoint proposal: Should every proposed point
#'   be displayed in a single plot - or one plot per Iteration? Default is FALSE
#'   indicating single plots per proposed points.
#' @param xlim [\code{numeric(2)}]\cr
#'   For 1D: \code{xlim} parameter for first and second plot.
#'   Default is range of x-values evaluated in run object \code{object}.
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
#' @param colors [\code{character(3)}]
#'   Specify colors for point in the plots. Must be a vector of length 3,
#'   each element a color for the type design, prop and seq respectivly.
#'   Default is red for the initial design, blue for allready proposed points
#'   and green for the actual iteration.
#' @param ... [any]\cr
#'   Currently not used.
#' @return [\code{list}]. List containing seperate ggplot object. The number of plots depends on
#'   the type of MBO problem. See the description for details.
#' @export
renderExampleRunPlot =  function(object, iter, densregion = TRUE,
  se.factor = 1, single.prop.point.plots = FALSE, xlim = NULL, ylim = NULL,
  point.size = 3, line.size = 1, trafo = NULL, colors = c("red", "blue", "green"), ...) {
  UseMethod("renderExampleRunPlot")
}
