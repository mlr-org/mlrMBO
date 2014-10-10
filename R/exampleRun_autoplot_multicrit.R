# FIXME: Describe Plot
# FIXME: This function is not well tested - not sure if we want to export it now

#' @title Plot example run, either in 1D or 2D.
#'
#' @param object [\code{}]\cr
#'   Object of type \code{MBOExampleRunMultiCrit}.
#' @param iters [\code{integer}]\cr
#'   Selected iterations of \code{x} to display.
#'   Default is all iterations.
#' @param pause [\code{logical(1)}]\cr
#'   Pause after each iteration? In this case the plots are arranged in a grid making use
#'   of the gridExtra package.
#'   Default is \code{TRUE}.
#' @param y1lim [\code{numeric(2)}]\cr
#'   Axis limits for the first target value.
#'   Default is range of y1-values evaluated in run object \code{y1}.
#' @param y2lim [\code{numeric(2)}]\cr
#'   Axis limits for the second target value.
#'   Default is range of y2-values evaluated in run object \code{y2}.
#' @param ... [any]\cr
#'   Currently not used.
#' @return [\code{list}]. List of lists. For each iteration the sublist contains separate ggplot
#'   objects for the pareto set and the pareto front.
#' @export
autoplot.MBOExampleRunMultiCrit = function(object, iters, pause = TRUE, y1lim = NULL, y2lim = NULL, ...) {
  requirePackages(packs = c("ggplot2", "gridExtra"), why = "autoplot.MBOExampleRun")

  iters.max = object$control$iters
  if (missing(iters)) {
    iters = seq_len(iters.max)
  } else {
    iters = asInteger(iters, min.len = 1L, lower = 1L, upper = iters.max, any.missing = FALSE)
  }

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
  if (is.null(y1lim))
    y1lim = y1range
  y2range = range(yy[, 2L])
  if (is.null(y2lim))
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
  plot.sequence = list()

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
    if (control$multicrit.method == "parego") {
      pl.set = pl.set + geom_tile(data = xgrid2, aes_string(x = x.name[1L], y = x.name[2L], fill = name.crit))
      pl.set = pl.set + scale_fill_gradientn(colours = topo.colors(7))
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
    for (i in iters) {
      for (j in 1:proppoints) {
        plot.sequence[[i]] = list()
        # if we propose 1 point, parego stores a list of models,
        # otherwise a list of model-list (1 per parallel proposal)
        model = if (proppoints == 1L)
          mbo.res$models[[i]]
        else
          mbo.res$models[[i]][[j]]
        idx = getIDX(opt.path, i)
        weights = as.numeric(opt.path[idx$proposed, c(".weight1", ".weight2")])
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

        pl.front = createPlFront(gg.points.front, i)
        pl.set = createPlSet(gg.points.set)

        if (pause) {
          title = sprintf("Iter %i", i)
          pl.all = grid.arrange(pl.front, pl.set, nrow = 1, main = title)
          print(pl.all)
          pause()
        }
        plot.sequence[[i]][[j]] = list(pl.front = pl.front, pl.set = pl.set)
      }
    }
  } else {
    for (i in iters) {
      models = mbo.res$models[[i]]
      idx = getIDX(opt.path, i)
      models.ok = !any(sapply(models, inherits, what = "FailureModel"))
      # if (models.ok) {
        # xgrid2[[name.crit]] = opt.direction *
        # critfun(xgrid, model, control, par.set, opt.path[idx$past, ])
      # }
      idx.all = c(idx.init, idx$seq, idx$proposed, idx.nsga2.paretofront)

      gg.points.front = getGGPointsFront(yy, idx, idx.all)
      gg.points.set = getGGPointsSet(xx, idx, idx.all)

      pl.front = createPlFront(gg.points.front, i)
      pl.set = createPlSet(gg.points.set, i)

      if (pause) {
        title = sprintf("Iter %i", i)
        pl.all = grid.arrange(pl.front, pl.set = pl.set, nrow = 1, main = title)
        print(pl.all)
        pause()
      }
      plot.sequence[[i]] = list(pl.front = pl.front, pl.set = pl.set)
    }
  }
  return(plot.sequence)
}
