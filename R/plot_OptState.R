#' @title Generate ggplot2 Object
#'
#' @description Plots the values of the infill criterion for a 1- and 2-dimensional numerical search space for a given \code{\link{OptState}}.
#'
#' @param x [\code{OptState}]\cr
#'   The OptState.
#' @param scale.panels [\code{logical(1)}]\cr
#'   If \code{TRUE} the values in each panel will be scaled to [0,1].
#' @param ... [any] \cr
#'   Not used.
#'
#' @export
plot.OptState = function(x, scale.panels = FALSE, ...) {

  # all the variables we need
  opt.state = x
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  par.set = getOptProblemParSet(opt.problem)
  par.dim = getParamNr(par.set, devectorize = TRUE)
  if (par.dim > 2) {
    stop("Only plotting for 1- and 2-dimensional search spaces is possible.")
  }
  par.types = getParamTypes(par.set, use.names = TRUE, with.nr = TRUE, df.cols = TRUE, df.discretes.as.factor = TRUE)
  par.is.numeric = par.types %in% c("numeric", "integer")
  par.count.numeric = sum(par.is.numeric)
  par.count.discrete = par.dim - par.count.numeric
  opt.path = getOptStateOptPath(opt.state)
  models = getOptStateModels(opt.state)$models
  designs = getOptStateDesigns(opt.state)
  x.ids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  y.ids = control$y.name
  infill = control$infill.crit

  # the data we need to plot
  points = generateGridDesign(par.set, 100, trafo = TRUE)
  infill.res = infill$fun(points = points, models = models, control = control, par.set = par.set, designs = designs, attributes = TRUE, iter = getOptStateLoop(opt.state))

  crit.components = attr(infill.res, "crit.components")
  if (!is.null(crit.components)) {
    plot.data = data.table::data.table(infill = infill.res, crit.components, points)
  } else {
    plot.data = data.table::data.table(infill = infill.res, points)
  }

  if (infill$opt.direction == "maximize") {
    plot.data$infill = -1 * plot.data$infill
  }
  colnames(plot.data)[1] = control$infill.crit$id

  design = designs[[1]]

  # add types to points
  design$type = ifelse(getOptPathDOB(opt.path) == 0, "init", "seq")

  # reduce to usefull infill components
  use.only.columns = c(x.ids, control$infill.crit$id, "mean", "se")
  use.only.columns = intersect(use.only.columns, colnames(plot.data))
  plot.data = plot.data[, use.only.columns, with = FALSE]

  # prepare data for ggplot2
  mdata = data.table::melt(plot.data, id.vars = x.ids)
  mdata$variable = factor(mdata$variable, levels = intersect(use.only.columns, levels(mdata$variable)))
  if (scale.panels && par.dim == 2) {
    predict.range = range(mdata[get("variable")=="mean", "value"])
    mdata[, ':='("value", normalize(x = get("value"), method = "range")), by = "variable"]
    design[[y.ids]] = (design[[y.ids]] + (0 - predict.range[1])) / diff(predict.range)
  }
  if (par.count.numeric == 2) {
    g = ggplot2::ggplot(mdata, ggplot2::aes_string(x = x.ids[1], y = x.ids[2], fill = "value"))
    g = g + ggplot2::geom_tile()
    g = g + ggplot2::geom_point(data = design, mapping = ggplot2::aes_string(x = x.ids[1], y = x.ids[2], fill = y.ids[1], shape = "type"))
    g = g + ggplot2::facet_grid(~variable)
    brewer.div = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"), interpolate = "spline")
    g = g + ggplot2::scale_fill_gradientn(colours = brewer.div(200))
  } else if (par.count.numeric == 1) {
    g = ggplot2::ggplot(mdata, ggplot2::aes_string(x = x.ids[par.is.numeric], y = "value"))
    g = g + ggplot2::geom_line()
    g = g + ggplot2::geom_vline(data = design, mapping = ggplot2::aes_string(xintercept = x.ids[par.is.numeric]), alpha = 0.5, size = 0.25)
    g = g + ggplot2::geom_point(data = cbind(design, variable = "mean"), mapping = ggplot2::aes_string(x = x.ids[par.is.numeric], y = y.ids[1], shape = "type", color = "type"))
    g = g + ggplot2::scale_color_manual(values = c(init = "red", seq = "green"))
    if (par.count.discrete == 1) {
      formula.txt = paste0(names(par.types[!par.is.numeric]),"~variable")
    } else {
      formula.txt = "~variable"
    }
    if (scale.panels && par.dim == 1) {
      g = g + ggplot2::facet_wrap(as.formula(formula.txt), nrow = 1, scales = "free_y")
    } else {
      g = g + ggplot2::facet_grid(as.formula(formula.txt))
    }
  }
  g = g + ggplot2::scale_shape_manual(values = c(init = 16, seq = 15))
  g
}
