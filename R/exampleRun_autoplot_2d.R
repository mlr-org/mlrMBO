# work around "no visible binding for global variable" notes of R CMD check
if (getRversion() >= "2.15.1")
  utils::globalVariables(c("se.min", "se.max"))

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
autoplotExampleRun2d = function(x, iters,
  pause = TRUE, densregion = TRUE,
  point.size, line.size,
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
  # FIXME: how do we display noise? do we at all?
  for (i in iters) {
    catf("Iter %i", i)
    model = mbo.res$models[[i]]

    idx.seq = which(opt.path$dob > 0 & opt.path$dob < i)
    idx.proposed = which(opt.path$dob == i)
    idx.past = which(opt.path$dob < i)
    idx.pastpresent = which(opt.path$dob <= i)

    model.ok = !inherits(model, "FailureModel")

    if (model.ok) {
      evals$yhat = infillCritMeanResponse(evals[, names.x, drop = FALSE],
        model, control, par.set, opt.path[idx.past, ])
      if (se) {
        evals$se = -infillCritStandardError(evals[, names.x, drop = FALSE],
          model, control, par.set, opt.path[idx.past, ])
      }
      #FIXME this does not work for multipoint proposal
      # write unit tests for this AND MANY OTHER CASES to check this
      if (proppoints == 1L) {
        evals[[name.crit]] = opt.direction * critfun(evals[, names.x, drop = FALSE],
          model, control, par.set, opt.path[idx.past, ])
      }
    }

    idx = c(idx.init, idx.seq, idx.proposed)

    # helper function for single plot
    plotSingleFun = function(data, points, name.z, xlim, ylim, trafo = NULL) {
      if (!is.null(trafo)) {
        data[, name.z] = trafo(data[, name.z])
      }
      pl = ggplot(data = data, aes_string(x = "x1", y = "x2", z = name.z))
      pl = pl + geom_tile(aes_string(fill = name.z))
      pl = pl + scale_fill_gradientn(colours = topo.colors(7))
      if (name.z != "ei") {
        pl = pl + stat_contour(aes_string(fill = name.z), binwidth = 5)
      }
      pl = pl + geom_point(data = points, aes(x = x1, y = x2, z = y, colour = type, shape = type),
        size = point.size)

      title = name.z
      if (!is.null(trafo)) {
        title = paste(title, " (", attr(trafo, "name"), "-transformed)", sep = "")
      }

      pl = pl + ggtitle(title)
      pl = pl + scale_colour_manual(name = "type", values = c("#000000", "red","gray"))
      pl = pl + xlab(NULL) # remove axis labels
      pl = pl + ylab(NULL)
      pl = pl + theme(
        plot.title = element_text(size = 11, face = "bold"), # decrease font size and weight
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm") # adapt margins
      )
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

    # build single plots
    pl.fun = plotSingleFun(gg.fun, gg.points, "y", trafo = trafo[["y"]])
    pl.mod = plotSingleFun(gg.fun, gg.points, "yhat", trafo = trafo[["yhat"]])
    pl.crit = plotSingleFun(gg.fun, gg.points, name.crit, trafo = trafo[["crit"]])
    if (se) {
      pl.se = plotSingleFun(gg.fun, gg.points, "se", trafo = trafo[["se"]])
    }

    title = sprintf("Iter %i, x-axis: %s, y-axis: %s", i, name.x1, name.x2)

    plot.sequence[[i]] = list(
      "pl.fun" = pl.fun,
      "pl.mod" = pl.mod,
      "pl.crit" = pl.crit,
      "pl.se" = if (exists("pl.se")) pl.se else NA)

    if (pause) {
      if (se) {
        grid.arrange(pl.fun, pl.mod, pl.crit, pl.se, nrow = 2, main = title)
      } else {
        grid.arrange(pl.fun, pl.mod, pl.crit, nrow = 1)
      }
      pause()
    }
  }

  return(plot.sequence)
}
