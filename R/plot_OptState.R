#' @title Generate ggplot2 Object
#'
#' @description Plots the values of the infill criterion for a 1- and 2-dimensional numerical search space for a given \code{\link{OptState}}.
#'
#' @param x [\code{OptState}]\cr
#'   The OptState.
#' @param scale.panels [\code{logical(1)}]\cr
#'   If \code{TRUE} the values in each panel will be scaled to [0,1].
#' @param points.per.dim [\code{integer}]\cr
#'   Number of (regular spaced) points at which to
#'   evaluate the surrogate per dimension.
#'   Default is 100.
#' @param ... [any] \cr
#'   Not used.
#'
#' @export
plot.OptState = function(x, scale.panels = FALSE, points.per.dim = 100, ...) {

  # all the variables we need
  opt.state = x
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  par.set = getOptProblemParSet(opt.problem)
  par.dim = getParamNr(par.set, devectorize = TRUE)
  par.types = getParamTypes(par.set, use.names = TRUE, with.nr = TRUE, df.cols = TRUE, df.discretes.as.factor = TRUE)
  par.is.numeric = par.types %in% c("numeric", "integer")
  par.count.numeric = sum(par.is.numeric)
  par.count.discrete = par.dim - par.count.numeric
  opt.path.df = as.data.frame(getOptStateOptPath(opt.state))
  models = getOptStateModels(opt.state)$models
  designs = getOptStateDesigns(opt.state)
  x.ids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  y.ids = control$y.name
  x.ids.num = names(par.types[par.is.numeric])
  x.ids.cat = names(par.types[!par.is.numeric])
  infill = control$infill.crit

  # the data we need to plot
  points = generateGridDesign(par.set, points.per.dim, trafo = FALSE)
  # calculate true numeric dimension
  par.count.numeric.effective = max(apply(points[, x.ids.num, drop = FALSE], 1, function(x) sum(!is.na(x))))
  if (par.count.numeric.effective > 2) {
    stop("Cannot plot for more than two numerical dimensions.")
  }

  #data: data.frame with multiple x-columns
  #result: data.frame with 1 or 2 effective dimensions
  #cases: 1 num - do nothing
  #       2 num - do nothing
  #       1 num, 1cat - do nothing
  #       1 num, 2+cat - paste all cats to 1 cat
  #       2 num, 1cat - do nothing
  #       2 num, 2+cat - paste all cats to 1 cat
  project_x_effective = function(data) {
    data = copy(data)
    setDT(data)

    # if we have categorical vars, combine them to just one single one with multiple levels
    if (length(x.ids.cat) > 0) {
      data[, (x.ids.cat) := lapply(x.ids.cat, function(xid) sprintf("%s=%s", xid, .SD[[xid]])), .SDcols = x.ids.cat]
      data[, ".facet.y" := do.call(paste, c(data[, x.ids.cat, with = FALSE], sep = ", "))]
      data[, (x.ids.cat) := NULL]
    }
    if (par.count.numeric == par.count.numeric.effective) {
      data.table::setnames(data, x.ids.num, c(".axis.x", ".axis.y")[seq_along(x.ids.num)])
    } else if (par.count.numeric > par.count.numeric.effective) {
      if (par.count.numeric.effective == 1) {
        data[, ".num.par.active" := ifelse(is.na(get(x.ids.num[1])), x.ids.num[2], x.ids.num[1])]
        data[, ".axis.x" := .SD[[get(".num.par.active")]], by = ".num.par.active"]
        data[, ".facet.y" := paste0(get(".facet.y"), ", x=", get(".num.par.active")), by = ".num.par.active"]
        data[, c(x.ids.num, ".num.par.active") := NULL]
      } else {
        stop ("Plot not supported for more than 1 effective numerical dimension yet.")
      }
    }
    return(data)
  }


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

  #if mean response is missing we will calculate it manually
  if ("mean" %nin% colnames(plot.data)) {
    plot.data$mean = crit.mr$fun(points = points, models = models, control = control, par.set = par.set, designs = designs, attributes = TRUE, iter = getOptStateLoop(opt.state))
    crit.components = c(crit.components, "mean")
  }

  design = designs[[1]]

  # add types to points
  # remove final evals from plot because they do not belong to the model that is printed
  opt.path.df = opt.path.df[opt.path.df$prop.type != "final_eval",]
  design$type = ifelse(opt.path.df$dob == 0, "init", "seq")

  # reduce to usefull infill components
  use.only.columns = c(x.ids, control$infill.crit$id, "mean", "se")
  use.only.columns = intersect(use.only.columns, colnames(plot.data))
  plot.data = plot.data[, use.only.columns, with = FALSE]

  # prepare data for ggplot2
  design_effective = project_x_effective(design)
  plot.data = project_x_effective(plot.data)
  possible.id.vars = c(".facet.y", ".axis.x", ".axis.y")
  mdata = data.table::melt(plot.data, id.vars = intersect(colnames(plot.data), possible.id.vars))
  mdata$variable = factor(mdata$variable, levels = intersect(use.only.columns, levels(mdata$variable)))
  if (scale.panels && par.dim == 2) {
    predict.range = range(mdata[get("variable")=="mean", "value"])
    mdata[, ':='("value", normalize(x = get("value"), method = "range")), by = "variable"]
    design[[y.ids]] = (design[[y.ids]] + (0 - predict.range[1])) / diff(predict.range)
  }

  if (".facet.y" %in% colnames(mdata)) {
    if (scale.panels) {
      g_facet = ggplot2::facet_grid(as.formula(".facet.y ~ variable"), scales = "free")
    } else {
      g_facet = ggplot2::facet_grid(as.formula(".facet.y ~ variable"))
    }
  } else {
    if (scale.panels) {
      g_facet = ggplot2::facet_wrap(as.formula("~ variable"), scales = "free", nrow = 1)
    } else {
      g_facet = ggplot2::facet_grid(as.formula("~ variable"))
    }
  }
  if (all(c(".axis.x", ".axis.y") %in% colnames(mdata))) {
    g = ggplot2::ggplot(mdata, ggplot2::aes_string(x = ".axis.x", y = ".axis.y", fill = "value"))
    g = g + ggplot2::geom_raster()
    g = g + ggplot2::geom_point(data = design_effective, mapping = ggplot2::aes_string(x = ".axis.x", y = ".axis.y", fill = y.ids[1], shape = "type"))
    g = g + ggplot2::scale_fill_gradientn(colours = getColorPalette())
    g = g + ggplot2::labs(x = x.ids.num[1], y = x.ids.num[2])
  } else if (".axis.x" %in% colnames(mdata)) {
    g = ggplot2::ggplot(mdata, ggplot2::aes_string(x = ".axis.x", y = "value"))
    g = g + ggplot2::geom_line()
    g = g + ggplot2::geom_vline(data = design_effective, mapping = ggplot2::aes_string(xintercept = ".axis.x"), alpha = 0.5, size = 0.25)
    g = g + ggplot2::geom_point(data = cbind(design_effective, variable = "mean"), mapping = ggplot2::aes_string(x = ".axis.x", y = y.ids[1], shape = "type", color = "type"))
    g = g + ggplot2::scale_color_manual(values = c(init = "red", seq = "green"))
    g = g + ggplot2::labs(x = if(length(x.ids.num)>1) "x" else x.ids.num)
  }
  g = g + g_facet
  g = g + ggplot2::scale_shape_manual(values = c(init = 16, seq = 15))
  g
}

