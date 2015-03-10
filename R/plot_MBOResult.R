#' Plots any MBO result objects. Plots for X-Space, Y-Space and any coloumn in
#' the optimization path are available.
#' 
#' 
#' @param result [\code{MBOResult}]\cr
#'   \code{MBOSingleObjResult} or \code{MBOMultiObjResult} object.
#' @param iters [\code{integer}]\cr
#'   Iterations to be plotted, 0 indicates the initial design.
#' @param pause [\code{logical(1)}]\cr
#'   Should the process be paused after each iteration?
#'   Default is \code{TRUE}.
#' @template arg_plot_MBO

plotMBOResult = function(result, iters, pause = TRUE, 
  crit.plot = TRUE, hv.plot = NA,  extra.measures = NULL,
  ref.point = NULL, lim.x = list(), lim.y = list()) {
  
  # Helper to arragne plot via gridExtra and pause process
  arrangePlots = function(plots) {
    plots = Filter(Negate(isScalarNA), plots)
    n.row = if(length(plots) > 2) 2L else 1L
    
    args = plots
    args$main = "MBO Result"
    args$nrow = n.row
    args$heights = c(2, 1)[1:n.row]
    do.call(grid.arrange,  args)
    if (pause)
      pause()
  }
  
  if (is.null(ref.point)) {
    ref.point = getWorstExtremePoint(
      as.data.frame(result$opt.path, include.x = FALSE, include.rest = FALSE), 
      minimize = result$control$minimize) + 0.1
  }
  tmp = getLimits(lim.x, lim.y, result, iters, extra.measures, ref.point)
  lim.x = tmp$lim.x
  lim.y = tmp$lim.y
  
  for (iter in iters) {
    # get rendered plot data
    plots = renderMBOPlot(result, iter = iter, crit.plot = crit.plot,
      hv.plot = hv.plot, extra.measures = extra.measures, 
      ref.point = ref.point, lim.x = lim.x, lim.y = lim.y)
    arrangePlots(plots)
  }
}


