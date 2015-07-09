#' Plots any MBO result objects. Plots for X-Space, Y-Space and any coloumn in
#' the optimization path are available. This function uses 
#' \code{\link[ParamHelpers]{plotOptPath}} from package \code{ParamHelpers}.
#' 
#' 
#' @param result [\code{MBOResult}]\cr
#'   \code{MBOSingleObjResult} or \code{MBOMultiObjResult} object.
#' @param iters [\code{integer}]\cr
#'   Iterations to be plotted, 0 indicates the initial design.
#' @param pause [\code{logical(1)}]\cr
#'   Should the process be paused after each iteration?
#'   Default is \code{TRUE}.
#' @param y.over.time [\code{list} | NULL]\cr
#'  See argument \code{y.over.time} of the function 
#'  \code{\link[ParamHelpers]{renderOptPathPlot}} from package \code{ParamHelpers}.
#'  Default is to plot the y variable and the infill crit in the single crit case.
#' @param title [\code{character(1)}]\cr
#'   Main title for the plot.
#' @param ... 
#'  Additional parameters for the \code{\link[ParamHelpers]{plotOptPath}} 
#'  function in package \code{ParamHelpers}.
#' @export

# @template arg_plot_MBO

plotMBOResult = function(result, iters, pause = TRUE, y.over.time, 
  title = "MBO Result", ...) {

  # extract and set params
  opt.path = result$opt.path
  control = result$control
  y.names = opt.path$y.names
  infill.crit = control$infill.crit
  dim.y = length(y.names)

#   # If hv.plot is NA (default), show it allways for multicrit, for single.crit
#   # it is useless
#   if (is.na(hv.plot))
#     hv.plot = control$number.of.targets != 1L
#   if (hv.plot & control$number.of.targets == 1L)
#     stop("You required a Hypervolume Plot, but you have done single crit optimization.")
#   
#   if (is.null(ref.point)) {
#     ref.point = getWorstExtremePoint(
#       as.data.frame(opt.path, include.x = FALSE, include.rest = FALSE), 
#       minimize = control$minimize) + 0.1
#   }
  
  if (missing(y.over.time) && dim.y == 1) {
    y.over.time = list(y.names, infill.crit)
  }
  
  
  plots = plotOptPath(opt.path, iters, pause = pause, title = title, 
    y.over.time = y.over.time, ...)
  
}


