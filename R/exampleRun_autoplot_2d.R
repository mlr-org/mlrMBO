# Function for plotting 2d numeric respectively mixed discrete/numeric functions.
#
# @param x [\code{function}]\cr
#   Objective function.
# @param iters [\code{integer}]\cr
#   Selected iterations of \code{x} to display.
#   Default is all iterations.
# @param pause [\code{logical(1)}]\cr
#   Pause after each iteration?
#   Default is \code{TRUE}.
# @param densregion [\code{logical(1)}]\cr
#   Should the background be shaded by the density of the posterior distribution? Default ist \code{TRUE}.
#   Only used if learner supports computation of standard error.
# @param point.size [\code{numeric(1)}]\cr
#   Size of the points in the plots.
# @param line.size [\code{numeric(1)}]\cr
#   Line width of the functions graphs plotted.
# @param trafo [\code{list}]\cr
#   List of transformation functions of type \code{\link[mlrMBO]{MBOTrafoFunction}} for
#   the different plots.
#   For 1D: The list elements should be named with "y" (applied to objective function and model) or "crit"
#   (applied to the criterion). Only applied to plots with numeric parameters.
#   For 2D: The list should contain at least one element "y", "yhat", "crit" or "se". This way one can
#   specify different transformations for different plots. If a single function is provided, this function
#    is used for all plots.
# @param ... [\code{list}]\cr
#   Not used.
# @return [\code{list}] List of length \code{iters}. Each list element is a list of plots.
renderExampleRunPlots2d = function(x, iters,
  pause = TRUE, densregion = TRUE,
  point.size, line.size,
  se.factor,
  trafo = NULL, ...)  {

  # extract information from example run object
  par.set = x$par.set
  names.x = x$names.x
  name.x1 = names.x[1]
  name.x2 = names.x[2]
  name.y = x$name.y
  evals = x$evals
  global.opt = x$global.opt
  control = x$control
  proppoints = control$propose.points
  mbo.res = x$mbo.res
  x1 = unique(evals[, name.x1])
  x2 = unique(evals[, name.x2])
  name.crit = control$infill.crit
  critfun = getInfillCritFunction(name.crit)
  se = (x$learner$predict.type == "se")

  opt.direction = 1
  if (name.crit %in% c("ei")) {
    opt.direction = -1
  }
  opt.path = as.data.frame(mbo.res$opt.path)

  idx.init = which(opt.path$dob == 0)

  # save sequence of opt plots here
  plot.sequence = list()

  # FIXME: what to plot if not infillcrit that uses se?
  for (i in iters) {
    catf("Iter %i", i)
    model = mbo.res$models[[i]]
    evals.x = evals[, names.x, drop = FALSE]

    idx.seq = which(opt.path$dob > 0 & opt.path$dob < i)
    idx.proposed = which(opt.path$dob == i)
    idx.past = which(opt.path$dob < i)
    idx.pastpresent = which(opt.path$dob <= i)

    model.ok = !inherits(model, "FailureModel")

    if (model.ok) {
      evals$yhat = infillCritMeanResponse(evals.x, model, control, par.set, opt.path[idx.past, ])
      if (se) {
        evals$se = -infillCritStandardError(evals.x,
          model, control, par.set, opt.path[idx.past, ])
      }
      if (proppoints == 1L) {
        evals[[name.crit]] = opt.direction * critfun(evals.x,
          model, control, par.set, opt.path[idx.past, ])
      } else {
        objective = control$multipoint.multicrit.objective
        if (control$multipoint.method == "lcb") {
          evals[[name.crit]] = opt.direction * infillCritMeanResponse(evals.x, model, control, par.set, opt.path[idx.past, ])
        } else {
          if (objective == "mean.dist") {
            evals[[name.crit]] = opt.direction * infillCritMeanResponse(evals.x, model, control, par.set, opt.path[idx.past, ])
          } else if (objective == "ei.dist") {
            evals[[name.crit]] = opt.direction * infillCritEI(evals.x, model, control, par.set, opt.path[idx.past, ])
          } else if (objective %in% c("mean.se", "mean.se.dist")) {
            evals[[name.crit]] = opt.direction * infillCritMeanResponse(evals.x, model, control, par.set, opt.path[idx.past, ])
          }
        }
      }
    }
    idx = c(idx.init, idx.seq, idx.proposed)

    # helper which applies different theme settings to ggplot object
    applyMBOTheme = function(pl, title, trafo = NULL) {
      if (!is.null(trafo)) {
        title = paste(title, " (", attr(trafo, "name"), "-transformed)", sep = "")
      }
      pl = pl + ggtitle(title)
      pl = pl + xlab(NULL) # remove axis labels
      pl = pl + ylab(NULL)
      pl = pl + theme(
        plot.title = element_text(size = 11, face = "bold"), # decrease font size and weight
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm") # adapt margins
      )
      return(pl)
    }

    # helper function for single plot
    plotSingleFunNumericOnly = function(data, points, name.x1, name.x2, name.z, xlim, ylim, trafo = NULL) {
      if (!is.null(trafo)) {
        data[, name.z] = trafo(data[, name.z])
      }
      pl = ggplot(data = data, aes_string(x = name.x1, y = name.x2, z = name.z))
      pl = pl + geom_tile(aes_string(fill = name.z))
      pl = pl + scale_fill_gradientn(colours = topo.colors(7))
      # sometimes contour lines cannot be plotted for EI
      if (name.z != "ei") {
        pl = pl + stat_contour(aes_string(fill = name.z), binwidth = 5)
      }
      # Keep in mind, that for the points the z value is always "name.y"
      pl = pl + geom_point(data = points, aes_string(x = name.x1, y = name.x2, z = name.y,
          colour = "type", shape = "type"), size = point.size)

      pl = pl + scale_colour_manual(name = "type", values = c("#000000", "red", "gray"))
      pl = applyMBOTheme(pl, title = name.z, trafo = trafo)
      return(pl)
    }

    # Keep in mind: name.x2 must be the name of the discrete/logical parameter by convention
    plotSingleFunMixed = function(data, points, name.x1, name.x2, name.y, xlim, ylim, trafo = NULL, marry.fun.and.mod = FALSE) {
      data[[name.x2]] = as.factor(data[[name.x2]])
      pl = ggplot(data = data, aes_string(x = name.x1, y = name.y))
      if (marry.fun.and.mod) {
        pl = pl + geom_line(aes_string(linetype = "type"))
      } else {
        pl = pl + geom_line()
      }
      # draw standard error in y/yhat-plot
      if (se & densregion & name.y == "y") {
        pl = pl + geom_ribbon(aes_string(x = name.x1, ymin = "se.min", ymax = "se.max"), alpha = 0.2)
      }
      if (name.y %in% c(x$name.y)) {
        pl = pl + geom_point(data = points, aes_string(x = name.x1, y = name.y, colour = "type", shape = "type"), size = point.size)
      }
      pl = pl + facet_grid(reformulate(name.x2, "."))
      pl = applyMBOTheme(pl, title = name.y, trafo = trafo)
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
        evals$se = -infillCritStandardError(evals.x, model, control, par.set, opt.path[idx.past, ])
        evals$se.min = evals$yhat - se.factor * evals$se
        evals$se.max = evals$yhat + se.factor * evals$se
      }

      # data frame with real fun and model fun evaluations
      gg.fun2 = data.frame(
        x1 = rep(evals[, names.x[1]], 2),
        x2 = rep(evals[, names.x[2]], 2),
        y = c(evals[, name.y], evals[, "yhat"]),
        crit = rep(evals[, name.crit], 2),
        se.min = if (se) rep(evals[, "se.min"], 2) else NA,
        se.max = if (se) rep(evals[, "se.max"], 2) else NA,
        type = as.factor(rep(c(name.y, "yhat"), each = n))
      )
      names(gg.fun2) = c(names.x, name.y, name.crit, "se.min", "se.max", "type")
      pl.fun = plotSingleFun(gg.fun2, gg.points, name.x1, name.x2, name.y, trafo = trafo[["y"]], marry.fun.and.mod = TRUE)

    } else {
      pl.fun = plotSingleFun(gg.fun, gg.points, name.x1, name.x2, name.y, trafo = trafo[["y"]])
      pl.mod = plotSingleFun(gg.fun, gg.points, name.x1, name.x2, "yhat", trafo = trafo[["yhat"]])
      if (se) {
        pl.se = plotSingleFun(gg.fun, gg.points, name.x1, name.x2, "se", trafo = trafo[["se"]])
      }
    }
    pl.crit = plotSingleFun(gg.fun, gg.points, name.x1, name.x2, name.crit, trafo = trafo[["crit"]])

    title = sprintf("Iter %i, x-axis: %s, y-axis: %s", i, name.x1, name.x2)

    # Helper for nice alignment of multiple ggplots.
    #
    # @param plot.list [\code{list}]\cr
    #   List of ggplot objects.
    # @param title [\code{character(1)}]\cr
    #   Main title printed above grid-arranged plots.
    # @return Nothing. Plots are printed to the display as a side effect.
    arrangeAndPrintPlots = function(plot.list, title) {
      plot.list = Filter(Negate(isScalarNA), plot.list)
      n.plots = length(plot.list)
      n.row = if (n.plots <= 3) 1L else 2L
      do.call("grid.arrange", c(plot.list, nrow = n.row, main = title))
      pause()
    }

    # Removes not available plots in plot list.
    #
    # @param plot.list [\code{list}]\cr
    #   List of ggplot objects (NAs allowed).
    # @return plot.list without NAs.
    removeNAPlots = function(plot.list) {
      Filter(plot.list, f = function(x) !is.na(x))
    }

    plot.sequence[[i]] = list(
      pl.fun = pl.fun,
      pl.mod = pl.mod,
      pl.crit = pl.crit,
      pl.se = pl.se
    )

    if (pause) {
      arrangeAndPrintPlots(plot.sequence[[i]], title)
    }
  }
  return(plot.sequence)
}
