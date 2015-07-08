# Function for plotting 1d numeric respectively discrete functions.
#
# @param x [\code{function}]\cr
#  \code{MBOExampleRun} object.
# @param iter [\code{integer}]\cr
#   Selected iteration of \code{x} to render plots for.
# @param densregion [\code{logical(1)}]\cr
#   Should the background be shaded? Default ist \code{TRUE}.
#   Only used if learner supports computation of standard error.
# @param se.factor [\code{numeric(1)}]\cr
#   If the model provides local standard error estimation,
#   in addition to the mean response \code{yhat(x) +- se.factor * se(x)}
#   is plotted above and below.
#   Default is 1.
# @param xlim [\code{numeric(2)}]\cr
#   For 1D: \code{xlim} parameter for first and second plot.
#   Default is range of x-values evaluated in run object \code{x}.
# @param ylim [\code{numeric(2)}]\cr
#   For 1D: \code{ylim} parameter for first plot, for the second plot \code{ylim} is always set
#   automatically, depending on the range of the evaluated infill criterion.
#   Default for the first plot is a heuristic to have the true function
#   and \code{yhat(x) +- se.factor2 * se(x)} both in the plot. Note that this heuristic might
#   change the \code{ylim} setting between plot iterations.
# @param point.size [\code{numeric(1)}]\cr
#   Size of the points in the plots.
# @param line.size [\code{numeric(1)}]\cr
#   Line width of the functions aphs plotted.
# @param trafo [\code{list}]\cr
#   List of transformation functions of type \code{\link[mlrMBO]{MBOTrafoFunction}} for
#   the different plots.
#   For 1D: The list elements should be named with "y" (applied to objective function and model) or "crit"
#   (applied to the criterion). Only applied to plots with numeric parameters.
#   For 2D: The list should contain at least one element "y", "yhat", "crit" or "se". This way one can
#   specify different transformations for different plots. If a single function is provided, this function
#    is used for all plots.
# @param densregion [\code{logical(1)}]\cr
#   Should the background be shaded by the density of the posterior distribution? Default ist \code{TRUE}.
#   Only used if learner supports computation of standard error.
# @param colors [\code{character(3)}]
#   Specify colors for point in the plots. Must be a vector of length 3,
#   each element a color for the type design, prop and seq respectivly.
#   Default is red for the initial design, blue for allready proposed points
#   and green for the actual iteration.
# @param ... [\code{list}]\cr
#   Not used.
# @return [\code{list}] List of ggplot2 objects.
renderExampleRunPlot1d = function(x, iter,
  densregion = TRUE,
  se.factor = 1,
  xlim = NULL, ylim = NULL,
  point.size, line.size,
  trafo = NULL, 
  colors = c("red", "blue", "green"), ...)  {
  # extract relevant data from MBOExampleRun
  par.set = x$par.set
  names.x = x$names.x
  name.y = x$name.y
  control = x$control
  noisy = control$noisy
  mbo.res = x$mbo.res
  models = mbo.res$models
  
  # check if standard error is available
  se = (x$learner$predict.type == "se")

  propose.points = control$propose.points
  name.crit = control$infill.crit
  if(control$multifid) {
    name.crit = "mfEI"
    critfun = infillCritMultiFid.external
  } else {
    critfun = getInfillCritFunction(name.crit)
  }
    
  
  opt.direction = 1

  # we need to maximize expected improvement
  if (name.crit %in% c("ei")) {
    opt.direction = -1
  }

  # if no iterations provided take the total number of iterations in optimization process
  assertInteger(iter, lower = 0, upper = length(models), len = 1L, any.missing = FALSE)

  global.opt = x$global.opt
  if (is.na(global.opt)) {
    global.opt = x$global.opt.estim
  }

  evals = x$evals
  opt.path = mbo.res$opt.path
  evals.x = evals[, getParamIds(opt.path$par.set) , drop = FALSE]

  # helper function for building up data frame of different points
  # i.e., initial design points, infilled points, proposed points for ggplot
  getType = function(x, iter) {
    if(x==0) return("init") else 
    if(x>0 && x<iter) return("seq") else
    if(x==iter) return("prop") else
      return ("future")
  }
  buildPointsData = function(opt.path, iter) {
    type = sapply(getOptPathDOB(opt.path), getType, iter = iter)
    res = data.frame(
      convertOptPathToDf(opt.path, control),
      type = type
    )
    res[res$type %nin% "future",]
  }

  plots = list()

  model = models[[iter]]
  type = sapply(getOptPathDOB(opt.path), getType, iter = iter)
  idx.past = which(type %in% c("init", "seq"))
  idx.pastpresent = which(type %in% c("init", "seq", "prop"))

  # compute model prediction for current iter
  if (!inherits(model, "FailureModel")) {
    evals$yhat = infillCritMeanResponse(evals.x, model,
      control, par.set, convertOptPathToDf(opt.path, control)[idx.past, ])

    if (propose.points == 1L) {
      evals[[name.crit]] = opt.direction *
        critfun(evals.x, model, control, par.set, convertOptPathToDf(opt.path, control)[idx.past, ])
    } else {
      objective = control$multipoint.multicrit.objective
      if (objective == "mean.dist") {
        evals[[name.crit]] = opt.direction * infillCritMeanResponse(evals.x, model, control, par.set, convertOptPathToDf(opt.path, control)[idx.past, ])
      } else if (objective == "ei.dist") {
        evals[[name.crit]] = opt.direction * infillCritEI(evals.x, model, control, par.set, convertOptPathToDf(opt.path, control)[idx.past, ])
      } else if (objective %in% c("mean.se", "mean.se.dist")) {
        evals[[name.crit]] = opt.direction * infillCritMeanResponse(evals.x, model, control, par.set, convertOptPathToDf(opt.path, control)[idx.past, ])
      }
    }
    # prepare drawing of standard error (confidence interval)
    if (se) {
      evals$se = -infillCritStandardError(evals.x, model, control, par.set, convertOptPathToDf(opt.path, control)[idx.past, ])
    }
  }

  if (isNumeric(par.set, include.int = FALSE)) {
    gg.fun = melt(evals, id.vars = c(getParamIds(opt.path$par.set), "se"))
    gg.fun$se = gg.fun$se * se.factor
    
    # if trafo for y is provided, indicate transformation on the y-axis
    ylab = name.y
    if (!is.null(trafo$y)) {
      ylab = paste(name.y, " (", attr(trafo$y, "name"), "-transformed)", sep = "")
    }
    #determine in wich pane (facet_grid) the points belong to
    pane.names = c(ylab, name.crit)
    gg.fun$pane = ifelse(gg.fun$variable %in% c(name.y, "yhat"), 1, 2)
    gg.fun$pane = factor(gg.fun$pane, labels = pane.names)
    
    
    # data frame with points of different type (initial design points, infill points, proposed points)
    gg.points = buildPointsData(opt.path, iter)
    gg.points$pane = pane.names[1]

    # transform y and yhat values according to trafo function
    if (!is.null(trafo$y)) {
      tr = trafo$y
      gg.fun[[name.y]] = tr(gg.fun[[name.y]])
      gg.points[[name.y]] = tr(gg.points[[name.y]])
    } else {
      tr = identity
    }
    
    # finally build the ggplot object(s)
    g = ggplot(data = gg.fun)
    next.aes = aes_string(x = names.x, y = "value", color = "as.factor(.multifid.lvl)", group = "paste(variable,.multifid.lvl)", linetype = "variable")
    if (!control$multifid) {
      next.aes = dropNamed(next.aes, "group")
    }
    g = g + geom_line(next.aes, size = line.size)
    g = g + facet_grid(pane~., scales = "free")
    if (se & densregion) {
      #FIXME: We might lose transformation information here tr()
      next.aes = aes_string(x = names.x, ymin = "value-se", ymax = "value+se*se", group = ".multifid.lvl")
      if (!control$multifid) {
        next.aes = dropNamed(next.aes, "group")
      }
      g = g + geom_ribbon(data = gg.fun[gg.fun$variable == "yhat", ], next.aes, alpha = 0.2)
    }
    g = g + geom_point(data = gg.points, aes_string(x = names.x, y = name.y, colour = "type", shape = "type"), size = point.size)
    g = g + scale_y_continuous(name = ylab)
    if (control$multifid) {
      palette = c("#f7fcf0","#e0f3db","#ccebc5","#a8ddb5","#7bccc4","#4eb3d3","#2b8cbe","#0868ac","#084081")
      g.colors = c(tail(palette, length(control$multifid.lvls)), colors)
    } else {
      g.colors = colors
    }
    g = g + scale_colour_manual(values = g.colors, name = "type")
    g = g + scale_linetype(name = "type")

    if (noisy) {
      if (!any(is.na(x$y.true))) {
        source = data.frame(x$y.true)
        names(source) = name.y
        gap = calculateGap(source[idx.pastpresent, , drop = FALSE], global.opt, control)
      } else {
        gap = NA
      }
    } else {
      gap = calculateGap(convertOptPathToDf(opt.path, control)[idx.pastpresent, ], global.opt, control)
    }

    g = g + ggtitle(
      sprintf("Iter = %i, Gap = %.4e", iter, gap)
    )

    g = g + theme(
      axis.text.y = element_text(),
      plot.title = element_text(size = 11, face = "bold")
    )

    plots = list(
      pl.fun = g
    )

    # if (pause) {
    #   do.call(grid.arrange, c(plots[[i]], nrow = 2))
    # }
  } else if (isDiscrete(par.set)) {
    if (!noisy) {
      stopf("Deterministic 1d function with a single factor parameter are not supported.")
    }

    gg.points = buildPointsData(opt.path, iter)

    if (se & densregion) {
      gg.points$se = -infillCritStandardError(gg.points[, names.x, drop = FALSE],
        model, control, par.set, opt.path[idx.past, , drop = FALSE])
      gg.points$se.min = gg.points[[name.y]] - se.factor * gg.points$se
      gg.points$se.max = gg.points[[name.y]] + se.factor * gg.points$se
    }

    pl.fun = ggplot(data = gg.points, aes_string(x = names.x, y = name.y, colour = "type", shape = "type"))
    pl.fun = pl.fun + geom_point(size = point.size)
    if (se & densregion) {
      pl.fun = pl.fun + geom_errorbar(aes_string(ymin = "se.min", ymax = "se.max"), width = .1, alpha = .5)
    }

    pl.fun = pl.fun + xlab(names.x)
    pl.fun = pl.fun + scale_colour_discrete(name = "type")
    pl.fun = pl.fun + ggtitle(
      sprintf("Iter = %i, Gap = %.4e", iter,
      calculateGap(opt.path[idx.pastpresent,], global.opt, control))
    )

    pl.fun = pl.fun + theme(
      legend.position = "top",
      legend.box = "horizontal",
      plot.title = element_text(size = 11, face = "bold")
    )

    plots = list(
      pl.fun = pl.fun
    )
  }
  return(plots)
}
