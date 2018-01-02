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
  par.set.complete = getOptProblemParSet(opt.problem, original.par.set = TRUE)
  par.set = getOptStateParSet(opt.state)
  par.dim = getParamNr(par.set.complete, devectorize = TRUE)
  if (par.dim > 2) {
    stop("Only plotting for 1- and 2-dimensional search spaces is possible.")
  }
  par.types = getParamTypes(par.set.complete, use.names = TRUE, with.nr = TRUE, df.cols = TRUE, df.discretes.as.factor = TRUE)
  par.is.numeric = par.types %in% c("numeric", "integer")
  par.count.numeric = sum(par.is.numeric)
  par.count.discrete = par.dim - par.count.numeric
  opt.path = getOptStateOptPath(opt.state)
  if ("OptPathNgCd" %in% class(opt.path)) {
    old.window.function = opt.path$window.function
    opt.path$window.function = identity
  }
  design = convertOptPathToDf(opt.path, control)
  opdf = as.data.frame(opt.path)
  dobs = getOptPathDOB(opt.path)
  if ("OptPathNgCd" %in% class(opt.path)) {
    opt.path$window.function = old.window.function
  }
  models = getOptStateModels(opt.state)$models
  x.ids.complete = getParamIds(par.set.complete, repeated = TRUE, with.nr = TRUE)
  x.ids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  y.ids = control$y.name
  infill = control$infill.crit

  # add types to points
  design$type = ifelse(dobs == 0, "init", "seq")
  if (!all(x.ids.complete %in% x.ids)) {
    design.complete = cbind(design, opdf[, setdiff(x.ids.complete, x.ids), drop = FALSE])
  } else {
    design.complete = design
  }

  # the data we need to plot
  points = generateGridDesign(par.set.complete, 100, trafo = TRUE)
  infill.res = infill$fun(points = points[, x.ids, drop = FALSE], models = models, control = control, par.set = par.set, design = design, attributes = TRUE, iter = getOptStateLoop(opt.state))

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

  # reduce to usefull infill components
  use.only.columns = c(x.ids.complete, control$infill.crit$id, "mean", "se")
  use.only.columns = intersect(use.only.columns, colnames(plot.data))
  plot.data = plot.data[, use.only.columns, with = FALSE]

  # prepare data for ggplot2
  mdata = data.table::melt(plot.data, id.vars = x.ids.complete)
  mdata$variable = factor(mdata$variable, levels = intersect(use.only.columns, levels(mdata$variable)))
  if (scale.panels) {
    #predict.range = range(mdata[get("variable")=="mean", "value"])
    #mdata[, ':='("value", normalize(x = get("value"), method = "range")), by = "variable"]
    #design.complete[[y.ids]] = design.complete[[y.ids]] + (0 - predict.range[1])) / diff(predict.range)
    mdata[,  ':='('value', normalize(get("value"), method = "range")), by = "variable"]
  }
  if (par.count.numeric == 2) {
    g = ggplot2::ggplot(mdata, ggplot2::aes_string(x = x.ids.complete[1], y = x.ids.complete[2]))
    g = g + ggplot2::geom_tile(aes_string(fill = "value"))
    g = g + ggplot2::geom_point(data = design.complete, mapping = ggplot2::aes_string(x = x.ids.complete[1], y = x.ids.complete[2], shape = "type"))
    g = g + ggplot2::facet_grid(~variable)
    brewer.div = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"), interpolate = "spline")
    g = g + ggplot2::scale_fill_gradientn(colours = brewer.div(200))
  } else if (par.count.numeric == 1) {
    g = ggplot2::ggplot(mdata, ggplot2::aes_string(x = x.ids.complete[par.is.numeric], y = "value"))
    g = g + ggplot2::geom_line()
    g = g + ggplot2::geom_vline(data = design.complete, mapping = ggplot2::aes_string(xintercept = x.ids.complete[par.is.numeric]), alpha = 0.5, size = 0.25)
    g = g + ggplot2::geom_point(data = cbind(design.complete, variable = "mean"), mapping = ggplot2::aes_string(x = x.ids.complete[par.is.numeric], y = y.ids[1], shape = "type", color = "type"))
    g = g + ggplot2::scale_color_manual(values = c(init = "red", seq = "green"))
    if (par.count.discrete == 1) {
      formula.txt = paste0(names(par.types[!par.is.numeric]),"~variable")
    } else {
      formula.txt = "~variable"
    }
    g = g + ggplot2::facet_grid(as.formula(formula.txt))
  }
  g = g + ggplot2::scale_shape_manual(values = c(init = 16, seq = 15))
  g
}
