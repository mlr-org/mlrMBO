# multi-objective
#' @export
renderExampleRunPlot.MBOExampleRunMultiObj = function(object, iter, densregion = TRUE,
  se.factor = 1, single.prop.point.plots = FALSE, xlim = NULL, ylim = NULL, point.size = 3,
  line.size = 1, trafo = NULL, colors = c("red", "blue", "green"), ...) {


  # extract variables and some short names
  mbo.res = object$mbo.res
  opt.path = as.data.frame(mbo.res$opt.path)
  models = object$mbo.res$models[[iter]]
  models = if (inherits(models, "WrappedModel")) list(models) else models

  par.set = object$par.set
  control = object$control
  x.name = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  y.name = control$y.name
  method = control$multiobj.method
  infill.crit.id = getMBOInfillCritId(control$infill.crit)

  # get x space and y space data
  data.y = as.data.frame(mbo.res$opt.path, include.x = FALSE, include.rest = FALSE)
  data.y = setRowNames(rbind(data.y, object$nsga2.paretofront), NULL)

  data.x = as.data.frame(mbo.res$opt.path, include.y = FALSE, include.rest = FALSE)
  data.x = setRowNames(rbind(data.x, object$nsga2.paretoset), NULL)

  idx = getIDX(opt.path, iter)
  idx.nsga2.paretofront = (getOptPathLength(mbo.res$opt.path) + 1):nrow(data.y)

  plots = list()
  if (control$propose.points == 1L || single.prop.point.plots) {
    # Render X Space Plot.
    pl.xspace = makeXPlot(data.x, idx, idx.nsga2.paretofront, method, x.name,
      infill.crit.id, models, control, par.set, opt.path, object$points.per.dim,
      iter, control$propose.points, object, colors)

    # Render Y Space Plot
    pl.yspace = makeYPlot(data.y, idx, idx.nsga2.paretofront, method, y.name,
      opt.path, control, iter, control$propose.points, object, colors)

    plots = list(pl.set = pl.xspace, pl.front = pl.yspace)

  } else {
    idx.propose = idx
    for (propose.iter in seq_len(control$propose.points)) {
      # set idx - add propose.iter - 1 to seq points, propose is only propose [propose.iter]
      idx.propose$seq = c(idx.propose$seq, idx$propose[propose.iter - 1])
      idx.propose$past = c(idx.propose$past, idx$propose[propose.iter - 1])
      idx.propose$proposed = idx$proposed[propose.iter]

      # Render X Space Plot.
      if (method == "parego") {
        prop.models = models[propose.iter]
      } else {
        prop.models = models
      }
      pl.xspace = makeXPlot(data.x, idx.propose, idx.nsga2.paretofront, method,
        x.name, infill.crit.id, prop.models, control, par.set, opt.path,
        object$points.per.dim, iter, 1L, object, colors)

      # Render Y Space Plot
      pl.yspace = makeYPlot(data.y, idx.propose, idx.nsga2.paretofront, method,
        y.name, opt.path, control, iter, 1L, object, colors)

      plots[[propose.iter]] = list(pl.set = pl.xspace, pl.front = pl.yspace)
    }
  }
  return(plots)
}


makeXPlot = function(data.x, idx, idx.nsga2.paretofront, method, x.name, crit.name,
  models, control, par.set, opt.path, points.per.dim, iter, propose.points, object, colors) {
  pl.xspace = ggplot2::ggplot()
  pl.xspace = pl.xspace + ggplot2::guides(colour = FALSE, shape = FALSE)

  gg.points.xspace = getPlotData(data.x, idx, idx.nsga2.paretofront, x.name)
  # first, fill background if possible. note: 2 different plots for mspot since
  # we have 2 infill crits, one per model
  if (method == "mspot") {
    data.crit1 = getInfillCritGrid(crit.name, points.per.dim, models[1],
      control, par.set, opt.path[idx$past, ])
    data.crit2 = getInfillCritGrid(crit.name, points.per.dim, models[2],
      control, par.set, opt.path[idx$past, ])

    crit1.plot = fillBackgroundWithInfillCrit(pl.xspace, data.crit1, x.name, crit.name) +
      ggplot2::ggtitle("XSpace - model 1")
    crit2.plot = fillBackgroundWithInfillCrit(pl.xspace, data.crit2, x.name, crit.name) +
      ggplot2::ggtitle("XSpace - model 2")

    pl.xspace = list(
      crit1 = createBasicSpacePlot(crit1.plot, gg.points.xspace, iter, object, x.name, 0.8, "x", colors),
      crit2 = createBasicSpacePlot(crit2.plot, gg.points.xspace, iter, object, x.name, 0.8, "x", colors)
    )
  }
  if (method %in% c("parego", "dib")) {
    if (propose.points == 1L) {
      data.crit = getInfillCritGrid(crit.name, points.per.dim, models,
        control, par.set, opt.path[idx$past, ], iter)
      pl.xspace = fillBackgroundWithInfillCrit(pl.xspace, data.crit, x.name, crit.name) +
        ggplot2::ggtitle("XSpace")
    }
    pl.xspace = createBasicSpacePlot(pl.xspace, gg.points.xspace, iter, object, x.name, 0.8, "x", colors)
  }
  return(pl.xspace)
}


makeYPlot = function(data.y, idx, idx.nsga2.paretofront, method, y.name, opt.path,
  control, iter, propose.points, object, colors) {
  gg.points.yspace = getPlotData(data.y, idx, idx.nsga2.paretofront, y.name)
  pl.yspace = ggplot2::ggplot()
  pl.yspace = createBasicSpacePlot(pl.yspace, gg.points.yspace, iter, object, y.name, 0.4, "y", colors)
  if (method == "parego" && propose.points == 1L)
    pl.yspace = addParegoWeightLines(pl.yspace, data.y, idx, opt.path, 1L, control$multiobj.parego.rho)
  pl.yspace = pl.yspace + ggplot2::ggtitle("YSpace")
  return(pl.yspace)
}
