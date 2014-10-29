# Function for plotting 1d numeric respectively discrete functions.
#
# @param x [\code{function}]\cr
#   Objective function.
# @param iters [\code{integer}]\cr
#   Selected iterations of \code{x} to display.
#   Default is all iterations.
# @param pause [\code{logical(1)}]\cr
#   Pause after each iteration?
#   Default is \code{TRUE}.
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
# @param ... [\code{list}]\cr
#   Not used.
# @return [\code{list}] List of length \code{iters}. Each list element is a list of plots.
renderExampleRunPlots1d = function(x, iters,
  xlim, ylim,
  pause, se.factor,
  point.size, line.size,
  trafo, densregion = TRUE, ...) {
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
  critfun = getInfillCritFunction(name.crit)
  opt.direction = 1

  # we need to maximize expected improvement
  if (name.crit %in% c("ei")) {
    opt.direction = -1
  }

  # if no iterations provided take the total number of iterations in optimization process
  if(missing(iters)) {
    iters = length(models)
  }

  global.opt = x$global.opt
  if (is.na(global.opt)) {
    global.opt = x$global.opt.estim
  }

  evals = x$evals
  evals.x = evals[, names.x, drop = FALSE]
  opt.path = as.data.frame(mbo.res$opt.path)

  idx.init = which(opt.path$dob == 0)

  # helper function for building up data frame of different points
  # i.e., initial design points, infilled points, proposed points for ggplot
  buildPointsData = function(opt.path, names.x, name.y, idx, idx.init, idx.seq, idx.proposed) {
    points = data.frame(
      x = opt.path[idx, names.x],
      y = opt.path[idx, name.y],
      type = as.factor(c(
        rep("init", length(idx.init)),
        rep("seq", length(idx.seq)),
        rep("prop", length(idx.proposed)))
      )
    )
    names(points) = c(names.x, name.y, "type")
    return(points)
  }

  plot.sequence = list()

  for (i in iters) {
    catf("Iter %i", i)

    model = models[[i]]
    idx.seq = which(opt.path$dob > 0 & opt.path$dob < i)
    idx.proposed = which(opt.path$dob == i)
    idx.past = which(opt.path$dob < i)
    idx.pastpresent = which(opt.path$dob <= i)
    idx = c(idx.init, idx.seq, idx.proposed)

    model.ok = !inherits(model, "FailureModel")

    # compute model prediction for current iter
    if (model.ok) {
      evals$yhat = infillCritMeanResponse(evals.x, model,
        control, par.set, opt.path[idx.past, ])

      if (propose.points == 1L) {
        evals[[name.crit]] = opt.direction *
          critfun(evals.x, model, control, par.set, opt.path[idx.past, ])
      } else {
        objective = control$multipoint.multicrit.objective
        if (objective == "mean.dist") {
          evals[[name.crit]] = opt.direction * infillCritMeanResponse(evals.x, model, control, par.set, opt.path[idx.past, ])
        } else if (objective == "ei.dist") {
          evals[[name.crit]] = opt.direction * infillCritEI(evals.x, model, control, par.set, opt.path[idx.past, ])
        } else if (objective %in% c("mean.se", "mean.se.dist")) {
          evals[[name.crit]] = opt.direction * infillCritMeanResponse(evals.x, model, control, par.set, opt.path[idx.past, ])
        }
      }
      # prepare drawing of standard error (confidence interval)
      if (se) {
        evals$se = -infillCritStandardError(evals.x, model, control, par.set, opt.path[idx.past, ])
        evals$se.min = evals$yhat - se.factor * evals$se
        evals$se.max = evals$yhat + se.factor * evals$se
      }
    }

    if (isNumeric(par.set, include.int = FALSE)) {
      # ggplot stuff
      n = nrow(evals)

      # data frame with real fun and model fun evaluations      
      gg.fun = data.frame(
        x = rep(evals[, names.x], 2),
        y = c(evals[, name.y], evals[, "yhat"]),
        se.min = if (se) rep(evals[, "se.min"], 2) else NA,
        se.max = if (se) rep(evals[, "se.max"], 2) else NA,
        type = as.factor(rep(c(name.y, "yhat"), each = n))
      )
      names(gg.fun) = c(names.x, name.y, "se.min", "se.max", "type")
      
      # data frame with points of different type (initial design points, infill points, proposed points)
      gg.points = buildPointsData(opt.path, names.x, name.y, idx, idx.init, idx.seq, idx.proposed)

      # transform y and yhat values according to trafo function
      if (!is.null(trafo$y)) {
        tr = trafo$y
        gg.fun[[name.y]] = tr(gg.fun[[name.y]])
        gg.fun$se.min = tr(gg.fun$se.min)
        gg.fun$se.max = tr(gg.fun$se.max)
        gg.points[[name.y]] = tr(gg.points[[name.y]])
      }

      # finally build the ggplot object(s)
      pl.fun = ggplot(data = gg.fun)
      pl.fun = pl.fun + geom_line(aes_string(x = names.x, y = name.y, linetype = "type"), size = line.size)

      if (se & densregion) {
        #gg.se = gg.fun[which(gg.fun$type == "yhat"), ]
        pl.fun = pl.fun + geom_ribbon(aes_string(x = names.x, ymin = "se.min", ymax = "se.max"), alpha = 0.2)
      }

      pl.fun = pl.fun + geom_point(data = gg.points, aes_string(x = names.x, y = name.y, colour = "type",
          shape = "type"), size = point.size)
      pl.fun = pl.fun + xlab(NULL)

      # if trafo for y is provided, indicate transformation on the y-axis
      ylab = name.y
      if (!is.null(trafo$y)) {
        ylab = paste(name.y, " (", attr(trafo$y, "name"), "-transformed)", sep = "")
      }
      pl.fun = pl.fun + scale_y_continuous(name = ylab)


      if (noisy) {
        if (!any(is.na(x$y.true))) {
          source = data.frame(x$y.true)
          names(source) = name.y
          gap = calculateGap(source[idx.pastpresent, , drop = FALSE], global.opt, control)
        } else {
          gap = NA
        }
      } else {
        gap = calculateGap(opt.path[idx.pastpresent, ], global.opt, control)
      }

      pl.fun = pl.fun + ggtitle(
        sprintf("Iter = %i, Gap = %.4e", i, gap)
      )

      pl.fun = pl.fun + theme(
        legend.position = "top",
        legend.box = "horizontal",
        axis.text.x = element_blank(),
        panel.margin = unit(0, "lines"),
        plot.title = element_text(size = 11, face = "bold")
      )

      gg.crit = evals
      # transform cirterion if corresponding trafo function provided
      if (!is.null(trafo$crit)) {
        gg.fun[[name.crit]] = trafo$crit(gg.fun[[name.crit]])
      }
      pl.crit = ggplot(data = gg.crit, aes_string(x = names.x, y = name.crit))
      pl.crit = pl.crit + geom_line(linetype = "dotted", colour = "black", size = line.size)
      pl.crit = pl.crit + geom_vline(xintercept = opt.path[idx.proposed, names.x],
        linetype = "dashed", colour = "darkgray", size = line.size)

      # if trafo for criterion is provided, indicate transformation on the y-axis
      ylab = name.crit
      if (!is.null(trafo$crit)) {
        ylab = paste(name.crit, " (", attr(trafo$crit, "name"), "-transformed)", sep = "")
      }
      pl.crit = pl.crit + scale_y_continuous(name = ylab)

      plot.sequence[[i]] = list(
        pl.fun = pl.fun,
        pl.crit = pl.crit
      )

      if (pause) {
        do.call(grid.arrange, c(plot.sequence[[i]], nrow = 2))
      }
    } else if (isDiscrete(par.set)) {
      if (!noisy) {
        stopf("Deterministic 1d function with a single factor parameter are not supported.")
      }

      gg.points = buildPointsData(opt.path, names.x, name.y, idx, idx.init, idx.seq, idx.proposed)

      if (se & densregion) {
        gg.points$se = -infillCritStandardError(data.frame(x = gg.points[[names.x]]), model, control, par.set, opt.path[idx.past, ])
        gg.points$se.min = gg.points[[name.y]] - se.factor * gg.points$se
        gg.points$se.max = gg.points[[name.y]] + se.factor * gg.points$se
      }

      pl.fun = ggplot(data = gg.points, aes_string(x = names.x, y = name.y, colour = "type", shape = "type"))
      pl.fun = pl.fun + geom_point(size = point.size)
      if (se & densregion) {
        pl.fun = pl.fun + geom_errorbar(aes_string(ymin = "se.min", ymax = "se.max"), width = .1, alpha = .5)
      }

      pl.fun = pl.fun + xlab(names.x)
      pl.fun = pl.fun + ylab(name.y)
      pl.fun = pl.fun + scale_colour_discrete(name = "type")
      pl.fun = pl.fun + ggtitle(
        sprintf("Iter = %i, Gap = %.4e", i,
        calculateGap(opt.path[idx.pastpresent,], global.opt, control))
      )

      pl.fun = pl.fun + theme(
        legend.position = "top",
        legend.box = "horizontal",
        plot.title = element_text(size = 11, face = "bold")
      )

      plot.sequence[[i]] = list(
        pl.fun = pl.fun
      )

      if (pause) {
        print(pl.fun)
      }
    }

    if (pause) {
      pause()
    }
  }
  return(plot.sequence)
}
