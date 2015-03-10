#' Renders plots for MBO result objects, every dimension is supported. Renders
#' a plot for X and Y Space respectively, additional diagnostic plots are available.
#' 
#' 
#' @param result [\code{MBOResult}]\cr
#'   \code{MBOSingleObjResult} or \code{MBOMultiObjResult} object.
#' @param iter [\code{integer(1)}]\cr
#'   Selected iteration of \code{object} to render plots for.
#' @template arg_plot_MBO

renderMBOPlot =  function(result, iter,
  crit.plot = TRUE, hv.plot = NA,  extra.measures = NULL,
  ref.point = NULL, lim.x = list(), lim.y = list()) {
  
  # extract and set params
  opt.path = result$opt.path
  control = result$control
  
  if (is.null(ref.point)) {
    ref.point = getWorstExtremePoint(
      as.data.frame(opt.path, include.x = FALSE, include.rest = FALSE), 
      minimize = control$minimize) + 0.1
  }
  tmp = getLimits(lim.x, lim.y, result, iter, extra.measures, ref.point)
  lim.x = tmp$lim.x
  lim.y = tmp$lim.y
  
  # If hv.plot is NA (default), show it allways for multicrit, for single.crit
  # it is useless
  if (is.na(hv.plot))
    hv.plot = control$number.of.targets != 1L
  if (hv.plot & control$number.of.targets == 1L)
    stop("You required a Hypervolume Plot, but you have done single crit optimiation.")
  
  # checks
  assertIntegerish(iter, lower = 0, upper = max(getOptPathDOB(opt.path)),
    len = 1L, any.missing = FALSE)
  assertFlag(hv.plot)
  assertFlag(crit.plot)
  
  # do not allow to produce more than 4 plots at a time. since X and Y space
  # is allways included, only 2 more plots are allowed
  if (hv.plot + crit.plot + length(extra.measures) > 2)
    stop("You required more than 2 extra plots. Pleas deactivate some.")
  
  # And call all plot functions
  # X and Y Space
  plots = renderOptPathPlot(opt.path, iter, 
    lim.x = lim.x[c("XSpace", "YSpace")], lim.y = lim.y[c("XSpace", "YSpace")])
  pl1 = plots[[1]]
  pl2 = plots[[2]]
  
  # Infill Crit over Time
  if (crit.plot) {
    # We have to treat MSPOT special, since mspot has multiple crit values
    if (control$number.of.targets > 1L && control$multicrit.method == "mspot") {
      # We have MSPOT - here we use the plot methods from ParamHelpers to Plot
      # Multipli criterias and alpha-shading to show the effect of time
      # FIXME: Do this
      pl3 = NULL
    } else {
      pl3 = renderMeasureOverTimePlot(opt.path, iter, control, measure = control$infill.crit,
        lim.x = lim.x[["CritPlot"]], lim.y = lim.y[["CritPlot"]])
    }
  } else {
    pl3 = NULL
  }
  
  # Hypervoume ofer time
  if (hv.plot) {
    pl4 = renderHVPlot(opt.path, iter, ref.point,
      lim.x = lim.x[["HVPlot"]], lim.y = lim.y[["HVPlot"]])
  } else {
    pl4 = NULL
  }
  
  # Extra measure plots
  if (length(extra.measures) > 0) {
    pl5 = renderMeasureOverTimePlot(opt.path, iter, control, measure = extra.measures[1])
  } else {
    pl5 = NULL
  }
  if (length(extra.measures) > 1) {
    pl6 = renderMeasureOverTimePlot(opt.path, iter, control, measure = extra.measures[2])
  } else {
    pl6 = NULL
  }
  
  plots = list(X_Space = pl1, Y_Space = pl2, critPlot = pl3, HVPlot = pl4,
     extraPlot1 = pl5, extraPlot2 = pl6)
  plots = plots[!sapply(plots, is.null)]
  
  return(plots)
}

# renders plot: Hypervolum versus iteration
# ref.point - the reference point for hypervolume calculation
# lim.x, lim.y - numeric vector of length 2, x and y limits of plot
# return ggplot object
renderHVPlot = function(op, iter, ref.point, lim.x, lim.y) {
  hvs = sapply(0:iter, function(i) 
    emoa::dominated_hypervolume(t(getOptPathParetoFront(op, dob = 0:i)), ref = ref.point))
  hv.data = data.frame(hv = hvs, dob = 0:iter)  
  pl = ggplot(hv.data, aes(x = dob, y = hv)) + geom_point()
  pl
  pl = pl + ggtitle(paste("Dominated Hypervolume, ref.point = (",
      collapse((round(ref.point, 1)), sep = ", "), ")", sep = ""))
  pl = pl + theme(plot.title = element_text(size = rel(1)))
  pl = pl + xlab("iteration") + xlim(lim.x) + ylim(lim.y)
  
  if (nrow(hv.data) > 1)
    pl = pl + geom_line()
  return(pl)
}

# renders plot: measure versus iteration
# measure - character, which measure should be ploted? must be present as
#           col in the op
# lim.x, lim.y - numeric vector of length 2, x and y limits of plot
# return ggplot object

# FIND THE ERROR HERE

renderMeasureOverTimePlot = function(op, iter, control, measure, lim.x, lim.y) {
  par.points = control$propose.points
  
  measure.values = getOptPathCol(op, measure, dob = 0:iter)
  dob = getOptPathDOB(op, dob = 0:iter)
  
  # now we have to be careful - there can be a lot of NAs in measure.values
  # since many measure are NA for the initial design und some measures are 
  # NA for all but one parallel proposed point.
  na.inds = is.na(measure.values)
  
  #  if measure.values is empty, nothing can be plotted
  if (length(measure.values[!na.inds]) == 0L)
    return(NULL)
  
  measure.data = data.frame(
    measure = measure.values[!na.inds],
    dob = dob[!na.inds])
  
  pl = ggplot(measure.data, aes_string(x = "dob", y = "measure"))
  pl = pl + geom_point()
  pl = pl + theme(plot.title = element_text(size = rel(1)))
  pl = pl + ylab(measure) + xlab("Iteration")
  pl = pl + ggtitle("Measure over time")
  #pl = pl + xlim(lim.x) + ylim(lim.y)
  
  # plot lines only if more than one crit observation exists
  # if more than 1 value per dob exists and if there is more than 1 unique
  # dob value
  if (length(table(dob[!na.inds])) > 1L && max(table(dob[!na.inds])) > 1L) {
    d = data.frame(
      measure = tapply(measure.data$measure, measure.data$dob, mean),
      dob = 1:iter)
    pl = pl + geom_line(data = d)
  }
  
  return(pl)
}

