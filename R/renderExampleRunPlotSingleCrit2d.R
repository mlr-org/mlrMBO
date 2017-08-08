# Function for plotting 2d numeric respectively mixed discrete/numeric functions.
# see plotExampleRun for details on each argument

# @param xlim
#  Not used
# @param ylim
#  Not used
# @param denseregion
#  Not used
# @param ... [\code{list}]\cr
#   Not used.

# @return [\code{list}] List of ggplot2 objects.
renderExampleRunPlot2d = function(x, iter,
  densregion = TRUE,
  se.factor = 1,
  xlim = NULL, ylim = NULL,
  point.size, line.size,
  trafo = NULL,
  colors = c("red", "blue", "green"), ...)  {
  requirePackages("ggplot2")

  # extract information from example run object
  par.set = x$par.set
  names.x = x$names.x
  name.x1 = names.x[1]
  name.x2 = names.x[2]
  name.y = x$name.y
  evals = x$evals
  control = x$control
  proppoints = control$propose.points
  mbo.res = x$mbo.res
  infill.crit.id = getMBOInfillCritId(control$infill.crit)
  critfun = control$infill.crit$fun
  se = (x$learner$predict.type == "se")

  # we need to maximize expected improvement
  opt.direction = getMBOInfillCritMultiplier(control$infill.crit)

  opt.path = as.data.frame(mbo.res$opt.path)

  idx.init = which(opt.path$dob == 0)

  # FIXME: what to plot if not infillcrit that uses se?
  models = mbo.res$models[[iter]]
  models = if (inherits(models, "WrappedModel")) list(models) else models
  evals.x = evals[, names.x, drop = FALSE]

  idx.seq = which(opt.path$dob > 0 & opt.path$dob < iter)
  idx.proposed = which(opt.path$dob == iter)
  idx.past = which(opt.path$dob < iter)

  model.ok = !inherits(models[[1L]], "FailureModel")
  infill.mean = makeMBOInfillCritMeanResponse()$fun
  infill.ei = makeMBOInfillCritEI()$fun
  infill.se = makeMBOInfillCritStandardError()$fun

  if (model.ok) {
    evals$yhat = ifelse(control$minimize, 1, -1) * infill.mean(evals.x, models, control, par.set, opt.path[idx.past, ])
    if (se) {
      evals$se = -infill.se(evals.x, models, control, par.set, opt.path[idx.past, ])
    }
    if (proppoints == 1L) {
      evals[[infill.crit.id]] = opt.direction * critfun(evals.x, models, control, par.set, opt.path[idx.past, ])
    } else {
      objective = control$multipoint.moimbo.objective
      if (control$multipoint.method == "cb") {
        evals[[infill.crit.id]] = opt.direction * infill.mean(evals.x, models, control, par.set, opt.path[idx.past, ])
      } else {
        if (objective == "mean.dist") {
          evals[[infill.crit.id]] = opt.direction * infill.mean(evals.x, models, control, par.set, opt.path[idx.past, ])
        } else if (objective == "ei.dist") {
          evals[[infill.crit.id]] = opt.direction * infill.ei(evals.x, models, control, par.set, opt.path[idx.past, ])
        } else if (objective %in% c("mean.se", "mean.se.dist")) {
          evals[[infill.crit.id]] = opt.direction * infill.mean(evals.x, models, control, par.set, opt.path[idx.past, ])
        }
      }
    }
  }
  idx = c(idx.init, idx.seq, idx.proposed)

  # helper which applies different theme settings to ggplot object
  applyMBOTheme = function(pl, title, trafo = NULL) {
    requirePackages("ggplot2")
    if (!is.null(trafo)) {
      title = paste(title, " (", attr(trafo, "name"), "-transformed)", sep = "")
    }
    pl = pl + ggplot2::ggtitle(title)
    pl = pl + ggplot2::xlab(NULL) # remove axis labels
    pl = pl + ggplot2::ylab(NULL)
    pl = pl + ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "bold"), # decrease font size and weight
      plot.margin = grid::unit(c(0.2, 0.2, 0.2, 0.2), "cm") # adapt margins
    )
    return(pl)
  }

  # helper function for single plot
  plotSingleFunNumericOnly = function(data, points, name.x1, name.x2, name.z, xlim, ylim, trafo = NULL) {
    requirePackages("ggplot2")
    if (!is.null(trafo)) {
      data[, name.z] = trafo(data[, name.z])
    }

    # set up nice colour palette
    brewer.palette = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"), interpolate = "spline")

    pl = ggplot2::ggplot(data = data, ggplot2::aes_string(x = name.x1, y = name.x2))
    pl = pl + ggplot2::geom_tile(ggplot2::aes_string(fill = name.z))
    pl = pl + ggplot2::scale_fill_gradientn(colours = brewer.palette(200))

    # sometimes contour lines cannot be plotted for EI
    if (name.z != "ei") {
      pl = pl + ggplot2::stat_contour(ggplot2::aes_string(z = name.z), bins = 10, colour = "gray", alpha = 0.8)
    }

    # Keep in mind, that for the points the z value is always "name.y"
    pl = pl + ggplot2::geom_point(data = points, ggplot2::aes_string(x = name.x1, y = name.x2,
        colour = "type", shape = "type"), size = point.size)

    pl = pl + ggplot2::scale_colour_manual(name = "type", values = colors)# c("#000000", "red", "gray"))
    pl = applyMBOTheme(pl, title = name.z, trafo = trafo)
    return(pl)
  }

  # Keep in mind: name.x2 must be the name of the discrete/logical parameter by convention
  plotSingleFunMixed = function(data, points, name.x1, name.x2, name.y, xlim, ylim, trafo = NULL, marry.fun.and.mod = FALSE) {
    data[[name.x2]] = as.factor(data[[name.x2]])
    pl = ggplot2::ggplot(data = data, ggplot2::aes_string(x = name.x1, y = name.y))
    if (marry.fun.and.mod) {
      pl = pl + ggplot2::geom_line(ggplot2::aes_string(linetype = "type"))
    } else {
      pl = pl + ggplot2::geom_line()
    }
    # draw standard error in y/yhat-plot
    if (se && densregion && name.y == "y") {
      pl = pl + ggplot2::geom_ribbon(ggplot2::aes_string(x = name.x1, ymin = "se.min", ymax = "se.max"), alpha = 0.2)
    }
    if (name.y %in% c(x$name.y)) {
      pl = pl + ggplot2::geom_point(data = points, ggplot2::aes_string(x = name.x1, y = name.y, colour = "type", shape = "type"), size = point.size)
    }
    pl = pl + ggplot2::facet_grid(reformulate(name.x2, "."))
    pl = applyMBOTheme(pl, title = name.y, trafo = trafo)
    pl = pl + ggplot2::theme(legend.position = "top", legend.box = "horizontal")
    return(pl)
  }

  # make up data structures for ggplot2
  gg.fun = evals
  gg.points = data.frame(
    x1 = opt.path[idx, name.x1],
    x2 = opt.path[idx, name.x2],
    y = opt.path[idx, name.y],
    type = as.factor(c(
      rep("init", length(idx.init)),
      rep("seq", length(idx.seq)),
      rep("prop", length(idx.proposed)
  ))))
  names(gg.points) = c(name.x1, name.x2, name.y, "type")
  # build single plots
  plotSingleFun = plotSingleFunNumericOnly
  if (hasDiscrete(par.set) || hasLogical(par.set)) {
    plotSingleFun = plotSingleFunMixed
    # determine which parameter is discrete and which one is numeric
    par.types = getParamTypes(par.set)
    idx.discrete = which(par.types %in% c("logical", "discrete"))
    idx.numeric = setdiff(c(1,2), idx.discrete)
    name.x2 = names.x[idx.discrete]
    name.x1 = names.x[idx.numeric]
  }

  pl.se = pl.mod = NA

  if (hasDiscrete(par.set) || hasLogical(par.set)) {
    # in this case we display fun and model in one plot
    n = nrow(evals)

    # FIXME: the following ~ 15 lines is copy and paste stuff (see exampleRun_autplot_1d.R)
    if (se) {
      evals.x = evals[, names.x, drop = FALSE]
      evals$se = -infill.se(evals.x, models, control, par.set, opt.path[idx.past, ])
      evals$se.min = evals$yhat - se.factor * evals$se
      evals$se.max = evals$yhat + se.factor * evals$se
    }

    # data frame with real fun and model fun evaluations
    gg.fun2 = data.frame(
      x1 = rep(evals[, names.x[1]], 2),
      x2 = rep(evals[, names.x[2]], 2),
      y = c(evals[, name.y], evals[, "yhat"]),
      crit = rep(evals[, infill.crit.id], 2),
      se.min = if (se) rep(evals[, "se.min"], 2) else NA,
      se.max = if (se) rep(evals[, "se.max"], 2) else NA,
      type = as.factor(rep(c(name.y, "yhat"), each = n))
    )
    names(gg.fun2) = c(names.x, name.y, infill.crit.id, "se.min", "se.max", "type")
    pl.fun = plotSingleFun(gg.fun2, gg.points, name.x1, name.x2, name.y, trafo = trafo[["y"]], marry.fun.and.mod = TRUE)

  } else {
    pl.fun = plotSingleFun(gg.fun, gg.points, name.x1, name.x2, name.y, trafo = trafo[["y"]])
    pl.mod = plotSingleFun(gg.fun, gg.points, name.x1, name.x2, "yhat", trafo = trafo[["yhat"]])
    if (se) {
      pl.se = plotSingleFun(gg.fun, gg.points, name.x1, name.x2, "se", trafo = trafo[["se"]])
    }
  }
  pl.crit = plotSingleFun(gg.fun, gg.points, name.x1, name.x2, infill.crit.id, trafo = trafo[["crit"]])

  list(
    pl.fun = pl.fun,
    pl.mod = pl.mod,
    pl.crit = pl.crit,
    pl.se = pl.se
  )
}
