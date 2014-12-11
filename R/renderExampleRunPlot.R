#' Renders plots for exampleRun objects, either in 1D or 2D, or
#' exampleRunMultiCrit objects.
#'
#' The graphical output depends on the target function at hand.
#' - For 1D numeric functions the upper plot shows the true function (if known),
#' the model and the (infill) points. The lower plot shows the infill criterion.
#' - For 2D mixed target functions only one plot is displayed.
#' - For 2D numeric only target functions up to four plots are presented to the
#'   viewer:
#'   - levelplot of the true function landscape (with [infill] points),
#'   - levelplot of the model landscape (with [infill] points),
#'   - levelplot of the infill criterion
#'   - levelplot of the standard error (only if learner supports standard error estimation).
#' - For bi-criteria target functions the upper plot shows the target space and the lower
#'   plot displays the x-space.
#'
#' @param object [\code{function}]\cr
#'   \code{MBOExampleRun} or \code{MBOExampleRunMulticrit} object.
#' @param iter [\code{integer}]\cr
#'   Selected iteration of \code{object} to render plots for.
#' @param densregion [\code{logical(1)}]\cr
#'   Should the background be shaded? Default is \code{TRUE}.
#'   Only used if learner supports computation of standard error.
#' @param se.factor [\code{numeric(1)}]\cr
#'   If the model provides local standard error estimation,
#'   in addition to the mean response \code{yhat(x) +- se.factor * se(x)}
#'   is plotted above and below.
#'   Default is 1.
#' @param xlim [\code{numeric(2)}]\cr
#'   For 1D: \code{xlim} parameter for first and second plot.
#'   Default is range of x-values evaluated in run object \code{object}.
#' @param ylim [\code{numeric(2)}]\cr
#'   For 1D: \code{ylim} parameter for first plot, for the second plot \code{ylim} is always set
#'   automatically, depending on the range of the evaluated infill criterion.
#'   Default for the first plot is a heuristic to have the true function
#'   and \code{yhat(x) +- se.factor2 * se(x)} both in the plot. Note that this heuristic might
#'   change the \code{ylim} setting between plot iterations.
#' @param point.size [\code{numeric(1)}]\cr
#'   Point size for ploted points. Default ist 3.
#' @param line.size [\code{numeric(1)}]\cr
#'   Line width of the graphs of plotted functions.
#' @param trafo [\code{list}]\cr
#'   List of transformation functions of type \code{MBOTrafoFunction} for
#'   the different plots.
#'   For 1D: The list elements should be named with \dQuote{y} (applied to objective function and model)
#'   or \dQuote{crit} (applied to the criterion). Only applied to plots with numeric parameters.
#'   For 2D: The list should contain at least one element \dQuote{y}, \dQuote{yhat}, \dQuote{crit}
#'   or \dQuote{se}.
#'   This way one can specify different transformations for different plots.
#'   If a single function is provided, this function is used for all plots.
#' @param ... [any]\cr
#'   Currently not used.
#' @return [\code{list}]. List containing seperate ggplot object. The number of plots depends on
#'   the type of MBO problem. See the description for details.
#' @export
renderExampleRunPlot =  function(object, iter, densregion = TRUE,
  se.factor = 1, xlim = NULL, ylim = NULL, point.size = 3, line.size = 1, trafo = NULL, ...) {
  UseMethod("renderExampleRunPlot")
}

# single-objective
renderExampleRunPlot.MBOExampleRun = function(object, iter, densregion = TRUE,
  se.factor = 1, xlim = NULL, ylim = NULL, point.size = 3, line.size = 1, trafo = NULL, ...) {

  iters.max = object$control$iters
  assertInteger(iter, lower = 0L, upper = iters.max, len = 1L, any.missing = FALSE)
  assertFlag(densregion)
  assertNumber(se.factor, lower = 0)
  assertNumber(point.size, lower = 1)
  assertNumber(line.size, lower = 1)

  if (!is.null(xlim)) {
    assertNumeric(xlim, len = 2L, any.missing = FALSE)
  }
  if (!is.null(ylim)) {
    assertNumeric(ylim, len = 2L, any.missing = FALSE)
  }

  n.params = object$n.params
  par.types = object$par.types
  par.set = object$par.set
  trafo = buildTrafoList(n.params, trafo)

  if (n.params == 1) {
    if (par.types %nin% c("numeric", "numericvector", "discrete", "discretevector")) {
      stopf("For 1D function only plotting of numeric or discrete functions possible, but your function is '%s'.", par.types)
    }
    return(renderExampleRunPlot1d(object, iter = iter, xlim = xlim, ylim = ylim, se.factor = se.factor, pause = pause,
      point.size = point.size, line.size = line.size, trafo = trafo, densregion = densregion, ...))
  } else if (n.params == 2) {
    if (!hasNumeric(par.set)) {
      stopf("At least one parameter of the target function must be numeric!")
    }
    return(renderExampleRunPlot2d(object, iter = iter, xlim = xlim, ylim = ylim, se.factor = se.factor, pause = pause,
      point.size = point.size, line.size = line.size, trafo = trafo, ...))
  } else {
    stopf("Functions with greater than 3 parameters are not supported.")
  }
}

# multi-objective
renderExampleRunPlot.MBOExampleRunMultiCrit = function(object, iter, densregion = TRUE,
  se.factor = 1, xlim = NULL, ylim = NULL, point.size = 3, line.size = 1, trafo = NULL, ...) {

  iters.max = object$control$iters
  assertInteger(iter, len = 1L, lower = 1L, upper = iters.max, any.missing = FALSE)

  points.per.dim = asCount(object$points.per.dim)
  assertCount(points.per.dim, na.ok = FALSE, positive = TRUE)

  # extract information from example run object
  par.set = object$par.set
  control = object$control
  y.name = control$y.name
  x.name = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  proppoints = control$propose.points
  rho = control$multicrit.parego.rho
  mbo.res = object$mbo.res
  nsga2.paretofront = object$nsga2.paretofront
  nsga2.paretoset = object$nsga2.paretoset
  opt.path = as.data.frame(mbo.res$opt.path)
  mbo.paretofront = getOptPathY(mbo.res$opt.path)
  isparego = control$multicrit.method == "parego"
  issmspar = control$multicrit.method == "dib" && control$multicrit.dib.indicator == "sms" &&
    control$propose.points > 1L
  # build essential data frames for target values ...
  yy = rbind(mbo.paretofront, nsga2.paretofront)

  # ... and for parameters
  x = as.data.frame(mbo.res$opt.path, include.y = FALSE, include.rest = FALSE)
  xx = setRowNames(rbind(x, nsga2.paretoset), NULL)

  idx.nsga2.paretofront = (nrow(mbo.paretofront) + 1):nrow(yy)
  # we need the range vor normalization. If no limits given, we use the ranges
  # for plotting limits too
  y1range = range(yy[, 1L])
  if (is.null(xlim))
    y1lim = y1range
  y2range = range(yy[, 2L])
  if (is.null(ylim))
    y2lim = y2range

  name.crit = control$infill.crit
  critfun = getInfillCritFunction(name.crit)
  xgrid = generateGridDesign(par.set = par.set, resolution = points.per.dim)
  xgrid2 = xgrid
  opt.direction = 1
  if (name.crit %in% c("ei"))
    opt.direction = -1

  idx.init = which(opt.path$dob == 0)

  # save sequence of opt plots here
  plots = list()

  getIDX = function(opt.path, i) {
    list(
      seq = which(opt.path$dob > 0 & opt.path$dob < i),
      proposed = which(opt.path$dob == i),
      past = which(opt.path$dob < i),
      pastpresent = which(opt.path$dob <= i)
    )
  }

  getGGPointsFront = function(yy, idx, idx.all) {
    data.frame(
      y1 = yy[idx.all, 1L],
      y2 = yy[idx.all, 2L],
      type = as.factor(c(
        rep("init", length(idx.init)),
        rep("seq", length(idx$seq)),
        rep("prop", length(idx$proposed)),
        rep("front", length(idx.nsga2.paretofront))
      ))
    )
  }

  getGGPointsSet = function(xx, idx, idx.all) {
    data.frame(
      x1 = xx[idx.all, 1L],
      x2 = xx[idx.all, 2L],
      type = as.factor(c(
          rep("init", length(idx.init)),
          rep("seq", length(idx$seq)),
          rep("prop", length(idx$proposed)),
          rep("front", length(idx.nsga2.paretofront))
      ))
    )
  }

  addApproxMBO = function(pl, gg.mbo, cols, isdom, col, lty) {
    if (!is.null(gg.mbo)) {
      gg.mbo = setColNames(gg.mbo[!isdom, 1:2], cols)
      gg.mbo = sortByCol(gg.mbo, cols)
      pl = pl + geom_line(aes_string(x = cols[1L], y = cols[2L]), gg.mbo,
        col = col, linetype = lty, alpha = 0.8)
    }
    return(pl)
  }

  createPlFront = function(gg.points.front, iter) {
    pl.front = ggplot(data = gg.points.front, aes_string(x = "y1", y = "y2"))

    pl.front = pl.front + geom_point(
      mapping = aes_string(colour = "type", shape = "type"),
      data = gg.points.front[which(gg.points.front$type == "front"), ],
      size = 2, alpha = 0.4)
    pl.front = pl.front + geom_point(
      mapping = aes_string(colour = "type", shape = "type"),
      data = gg.points.front[which(gg.points.front$type != "front"), ],
      size = 4)
    if (isparego)
      pl.front = pl.front + geom_abline(intercept = intercept, slope = slope)
    pl.front = pl.front + xlab(y.name[1L])
    pl.front = pl.front + ylab(y.name[2L])
    pl.front = pl.front + xlim(y1lim) + ylim(y2lim)
    #FIXME: this labels look very ugly and moreover overlap on scaling the plot
    #pl.front = pl.front + geom_text(data = NULL, x = 3/12 * y1lim[2L], y = 24/24 * y2lim[2L],
    #  label = paste("lambda[1] == ", round(weights[1L], 2), sep = ""), parse = TRUE, col = "black", size = 5)
    #pl.front = pl.front + geom_text(data = NULL, x = 3/12 * y1lim[2L], y = 23/24 * y2lim[2L],
    #  label = paste("lambda[2] == ", round(weights[2L], 2), sep = ""), parse = TRUE, col = "black", size = 5)
    if (isparego) {
      pl.front = pl.front + annotate("text", label = expression(paste(lambda[1], round(weights[1L], 2),
        lambda[2], round(weights[2L], 2)), sep = ""), x = y1lim[2], y = y2lim[1])
      pl.front = pl.front + geom_line(data = gg.line, col = "blue", shape = 1)
    }
    pl.front = addApproxMBO(pl.front, object$mbo.pred.grid.mean[[iter]], c("y1", "y2"),
      object$mbo.pred.grid.mean[[iter]]$.is.dom, "brown", "solid")
    pl.front = addApproxMBO(pl.front, object$mbo.pred.grid.lcb[[iter]], c("y1", "y2"),
      object$mbo.pred.grid.lcb[[iter]]$.is.dom, "brown", "dotted")
    return(pl.front)
  }

  createPlSet = function(gg.points.set, iter) {
    pl.set = ggplot()
    # only for parego, color background with scalar model / crit
    brewer.palette = colorRampPalette(brewer.pal(11, "Spectral"), interpolate = "spline")

    if (control$multicrit.method == "parego") {
      pl.set = pl.set + geom_tile(data = xgrid2, aes_string(x = x.name[1L], y = x.name[2L], fill = name.crit))
      pl.set = pl.set + scale_fill_gradientn(colours = brewer.palette(200))
    }
    pl.set = pl.set +  geom_point(data = gg.points.set[which(gg.points.set$type == "front"), ],
      aes_string(x = "x1", y = "x2", colour = "type", shape = "type"), size = 2, alpha = 0.8)
    pl.set = pl.set + geom_point(data = gg.points.set[which(gg.points.set$type != "front"), ],
      aes_string(x = "x1", y = "x2", colour = "type", shape = "type"), size = 4)
    pl.set = addApproxMBO(pl.set, object$mbo.pred.grid.x, x.name,
      object$mbo.pred.grid.mean[[iter]]$.is.dom, "brown", "solid")
    pl.set = addApproxMBO(pl.set, object$mbo.pred.grid.x, x.name,
      object$mbo.pred.grid.lcb[[iter]]$.is.dom, "brown", "dotted")
    return(pl.set)
  }

  if (isparego) {
    for (j in 1:proppoints) {
      # if we propose 1 point, parego stores a list of models,
      # otherwise a list of model-list (1 per parallel proposal)
      model = if (proppoints == 1L)
        mbo.res$models[[iter]]
      else
        mbo.res$models[[iter]][[j]]
      idx = getIDX(opt.path, iter)
      weights = as.numeric(opt.path[idx$proposed[j], c(".weight1", ".weight2")])
      model.ok = !inherits(model, "FailureModel")
      if (model.ok) {
        xgrid2[[name.crit]] = opt.direction *
        critfun(xgrid, model, control, par.set, opt.path[idx$past, ])
      }
      idx.all = c(idx.init, idx$seq, idx$proposed, idx.nsga2.paretofront)

      gg.points.front = getGGPointsFront(yy, idx, idx.all)
      gg.points.set = getGGPointsSet(xx, idx, idx.all)

      # make dataframe for lines to show rho
      m.seq = seq(y1lim[1], y1lim[2], length.out = 10000)

      # slope and intercept defined by lambda - a bit ugly due to normalization
      slope  = weights[1L] * (y2range[2L] - y2range[1L]) /
      weights[2L] / (y1range[2L] - y1range[1L])
      intercept = y2range[1L] - y1range[1L] * slope

      # Function to get the values for the rho visualization
      f = function(x, lambda, rho, const) {
        x = (x - y1range[1L]) / (y1range[2L] - y1range[1L])
        y.left = (const - lambda[1L] * x * (1 + rho)) / (rho * lambda[2L])
        y.left = y.left * (y2range[2L] - y2range[1L]) + y2range[1L]

        y.right = (const - lambda[1L] * x * rho) / ((1 + rho) * lambda[2L])
        y.right = y.right * (y2range[2L] - y2range[1L]) + y2range[1L]
        pmin(y.left, y.right)
      }

      # FIXME: find a good way to set this constant. I tried a lot and i found
      # nothing that worked really good. this is the best i got ... it works somehow,
      # but is far from perfect.
      tmp.x = sqrt(slope^2 / 4 + 1 - intercept) - slope / 2
      tmp.y = tmp.x * slope + intercept
      const = optimize(function(x) (f(tmp.x, weights, rho, x) - tmp.y)^2, interval = c(0, 10))$minimum
      gg.line = data.frame(
        y1 = m.seq,
        y2 = f(m.seq, weights, rho, const),
        type = rep("init", 100)
      )

      pl.front = createPlFront(gg.points.front, iter)
      pl.set = createPlSet(gg.points.set)

      plots[[j]] = list(pl.front = pl.front, pl.set = pl.set)
    }
  } else {
    models = mbo.res$models[[iter]]
    idx = getIDX(opt.path, iter)
    models.ok = !any(sapply(models, inherits, what = "FailureModel"))
    # if (models.ok) {
      # xgrid2[[name.crit]] = opt.direction *
      # critfun(xgrid, model, control, par.set, opt.path[idx$past, ])
    # }
    idx.all = c(idx.init, idx$seq, idx$proposed, idx.nsga2.paretofront)

    gg.points.front = getGGPointsFront(yy, idx, idx.all)
    gg.points.set = getGGPointsSet(xx, idx, idx.all)

    pl.front = createPlFront(gg.points.front, iter)
    pl.set = createPlSet(gg.points.set, iter)

    plots = list(pl.front = pl.front, pl.set = pl.set)
  }
  return(plots)
}


# Sets up the correct format for trafo functions used
# by MBOExampleRun plot functions.
#
# @param n.params [\code{integer(1)}]\cr
#   Number of parameters.
# @param input.trafo [\code{list}]\cr
#   List of trafo functions provided by the user.
# @return [\code{list}]\cr
#   List of trafo functions with format that is expected by exampleRun plot functions.
buildTrafoList = function(n.params, input.trafo) {
  if (n.params == 1) {
    assertSubset(names(input.trafo), choices = c("y", "crit"))
    trafo.defaults = list("y" = NULL, "crit" = NULL)
  } else {
    assertSubset(names(input.trafo), choices = c("y", "yhat", "crit", "se"))
    trafo.defaults = list("y" = NULL, "yhat" = NULL, "crit" = NULL, "se" = NULL)
  }

  if (is.null(input.trafo))
    return(trafo.defaults)

  # if single function provided, apply it to all plots
  if (c("MBOTrafoFunction") %in% class(input.trafo)) {
    if (n.params == 1) {
      trafo = list("y" = input.trafo, "crit" = input.trafo)
    } else {
      trafo = list("y" = input.trafo, "yhat" = input.trafo, "crit" = input.trafo, "se" = input.trafo)
    }
  } else {
    # otherwise check if all elements are of an appropriate type
    lapply(input.trafo, function(t)
      if(!is.null(t)) assertClass(t, "MBOTrafoFunction")
    )
    trafo = trafo.defaults
    trafo[names(input.trafo)] = input.trafo
  }
  return(trafo)
}
