#' @title Renders plots for exampleRun objects and displays them.
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
#'   \code{MBOExampleRun} object from \code{exampleRun} or
#'   \code{MBOExampleRunMultiObj} object from \code{exampleRunMultiObj}.
#' @param iters [\code{integer}]\cr
#'   Selected iterations of \code{object} to produce plots.
#'   Default is all iterations.
#' @param pause [\code{logical(1)}]\cr
#'   Should the process be paused after each iteration?
#'   Default is \code{interactive()}.
#' @param densregion [\code{logical(1)}]\cr
#'   Should the background be shaded? Default is \code{TRUE}.
#'   Only used if learner supports computation of standard error.
#' @param se.factor [\code{numeric(1)}]\cr
#'   If the model provides local standard error estimation,
#'   in addition to the mean response \code{yhat(x) +- se.factor * se(x)}
#'   is plotted above and below.
#'   Default is 1.
#' @param single.prop.point.plots [\code{logical(1)}]\cr
#'   Parameter for MOI-MBO Multi-point proposal: Should every proposed point
#'   be displayed in a single plot - or one plot per Iteration? Default is FALSE
#'   indicating single plots per proposed points.
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
#'   Point size for plotted points. Default is 3.
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
#'   each element a color for the type design, prop and seq respectively.
#'   Default is red for the initial design, blue for already proposed points
#'   and green for the actual iteration.
#' @param gg.objects [\code{list)}]
#'   List of \code{gg} objects that should be added to all ggplots.
#' @param ... [any]\cr
#'   Currently not used.
#' @return Nothing.
#' @export
plotExampleRun = function(object, iters, pause = interactive(),
  densregion = TRUE, se.factor = 1, single.prop.point.plots = FALSE,
  xlim = NULL, ylim = NULL,
  point.size = 3, line.size = 1,
  trafo = NULL, colors = c("red", "blue", "green"), gg.objects = list(), ...) {

  iters.max = object$control$iters
  if (missing(iters)) {
    iters = seq_len(iters.max)
  } else {
    iters = asInteger(iters, lower = 0L, upper = iters.max, any.missing = FALSE)
  }
  assertFlag(pause)
  assertFlag(densregion)
  assertNumber(se.factor, lower = 0)
  assertNumber(point.size, lower = 1)
  assertNumber(line.size, lower = 1)

  if (!is.null(xlim))
    assertNumeric(xlim, len = 2L, any.missing = FALSE)
  if (!is.null(ylim))
    assertNumeric(ylim, len = 2L, any.missing = FALSE)

  multi.crit = object$control$n.objectives > 1

  # Helper to arrange plot via gridExtra and pause process
  arrangePlots = function(plots, multi.crit) {
    plots = Filter(Negate(isScalarNA), plots)
    plots = lapply(plots, addGgsToGgplot, gg.objects)
    n.plots = length(plots)
    if (n.plots > 1) {
      requirePackages("gridExtra", why = "plotExampleRun")
      if (multi.crit) {
        if (!inherits(plots[[1L]], "ggplot")) {
          # for mspot first arrange the two plots for X-Space
          plots[[1L]] = do.call(gridExtra::arrangeGrob, c(plots[[1L]], nrow = 2))
        }
        do.call(gridExtra::grid.arrange, c(plots, ncol = 2))
      } else {
        do.call(gridExtra::grid.arrange, c(plots, nrow = 2))
      }
    } else {
      print(plots[[1]])
    }
  }

  for (iter in iters) {
    # get rendered plot data
    plots = renderExampleRunPlot(object, iter = iter, densregion = densregion, se.factor = se.factor,
      single.prop.point.plots = single.prop.point.plots, xlim = xlim, ylim = ylim,
      point.size = point.size, line.size = line.size, trafo = trafo, colors = colors, gg.objects = gg.objects, ...)
    if (!any(vlapply(plots, inherits, what = "ggplot"))) {
      # in this case we have multi-objective multi-point proposal: list of plots for each proposed point
      for (jter in seq_along(plots)) {
        arrangePlots(plots[[jter]], multi.crit)
        if (pause && !(iter == getLast(iters) && jter == length(plots)))
          pause()
      }
    } else {
      arrangePlots(plots, multi.crit)
      if (pause && iter != getLast(iters))
        pause()
    }
  }
}
