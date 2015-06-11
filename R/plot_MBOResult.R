#' Plots any MBO result objects. Plots for X-Space, Y-Space and any coloumn in
#' the optimization path are available. This function uses \code{\link{renderMBOPlot}}.
#' 
#' 
#' @param result [\code{MBOResult}]\cr
#'   \code{MBOSingleObjResult} or \code{MBOMultiObjResult} object.
#' @param iters [\code{integer}]\cr
#'   Iterations to be plotted, 0 indicates the initial design.
#' @param pause [\code{logical(1)}]\cr
#'   Should the process be paused after each iteration?
#'   Default is \code{TRUE}.
#' @param ... \cr
#'  Additional parameters for the \code{\link[ParamHelpers]{renderOptPathPlot}} 
#'  function in package \code{ParamHelpers}.
#' @template arg_plot_MBO
#' @export

plotMBOResult = function(result, iters, pause = TRUE, hv.plot = NA, ref.point = NULL, 
  y.over.time, title = "MBO Result", ...) {
  
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


