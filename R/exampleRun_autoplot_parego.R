#' Plot example run, either in 1D or 2D.
#'
#' FIXME: Describe Plot
#' FIXME: This function is not well tested - not sure if we want to export it now
#'
#' @param run [\code{}]\cr
#'   Objective function.
#' @param iters [\code{integer}]\cr
#'   Selected iterations of \code{x} to display.
#'   Default is all iterations.
#' @param pause [\code{logical(1)}]\cr
#'   Pause after each iteration?
#'   Default is \code{TRUE}.
#' @param y1lim [\code{numeric(2)}]\cr
#'   axis limits for the Plot
#'   Default is range of y1-values evaluated in run object \code{y1}.
#' @param y2lim [\code{numeric(2)}]\cr
#'   axis limits for the Plot
#'   Default is range of y2-values evaluated in run object \code{y2}.
#' @param ... [any]\cr
#'   Currently not used.
#' @return [\code{list}]. List containing seperate ggplot plots for each iteration.

autoplot.ParEGOExampleRun = function(run, iters, pause = TRUE, y1lim = NULL, y2lim = NULL, ...) {

  requirePackages("gridExtra", why = "autoplot.MBOExampleRun")
  requirePackages("ggplot2", why = "autoplot.MBOExampleRun")

  points.per.dim = run$points.per.dim
  points.per.dim = convertInteger(points.per.dim)
  assertCount(points.per.dim, na.ok = FALSE, positive = TRUE)

  # extract information from example run object
  par.set = run$par.set
  control = run$control
  names.y = control$y.name
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  proppoints = control$propose.points
  rho = control$parego.rho
  mbo.res = run$mbo.res
  opt.path = as.data.frame(mbo.res$opt.path)
  y = getOptPathY(mbo.res$opt.path)
  yy = rbind(setRowNames(y, NULL), setRowNames(run$nsga2.paretofront, NULL))
  x = as.data.frame(mbo.res$opt.path, include.y = FALSE, include.rest = FALSE)
  xx = rbind(setRowNames(x, NULL), setRowNames(run$nsga2.paretoset, NULL))
  idx.nsga2front = (nrow(y)+1):nrow(yy)
  # we need the range vor normalization. if no limits given, use range
  # for plotting limits, too
  y1range = range(yy[, 1L])
  if (is.null(y1lim))
    y1lim = y1range
  y2range = range(yy[, 2L])
  if (is.null(y2lim))
    y2lim = y2range
  name.crit = control$infill.crit
  critfun = getInfillCritFunction(name.crit)
  xgrid = generateGridDesign(par.set = par.set, resolution = points.per.dim)
  # xgrid = generateGridDesign(par.set = par.set, resolution = 4)
  xgrid2 = xgrid
  #nsga2.paretoset = x$nsga2.paretoset
  opt.direction = 1
  if (name.crit %in% c("ei"))
    opt.direction = -1

  idx.init = which(opt.path$dob == 0)

  # save sequence of opt plots here
  plot.sequence = list()

  for (i in iters) {
    for (j in 1:proppoints) {
      catf("Iter %i; Point %i", i, j)
      model = mbo.res$models[[i]][[j]]

      idx.seq = which(opt.path$dob > 0 & opt.path$dob < i)
      idx.proposed = which(opt.path$dob == i)
      idx.past = which(opt.path$dob < i)
      idx.pastpresent = which(opt.path$dob <= i)
      weights = as.numeric(opt.path[idx.proposed, c(".weight1", ".weight2")])

      model.ok = !inherits(model, "FailureModel")

      if (model.ok) {
        xgrid2[[name.crit]] = opt.direction *
          critfun(xgrid, model, control, par.set, opt.path[idx.past, ])
      }
      print(summary(xgrid2))
      idx = c(idx.init, idx.seq, idx.proposed, idx.nsga2front)

      gg.points.front = data.frame(
        y1 = yy[idx, 1L],
        y2 = yy[idx, 2L],
        type = as.factor(c(
          rep("init", length(idx.init)),
          rep("seq", length(idx.seq)),
          rep("prop", length(idx.proposed)),
          rep("front", length(idx.nsga2front))
        ))
      )

      gg.points.set = data.frame(
        x1 = xx[idx, 1L],
        x2 = xx[idx, 2L],
        type = as.factor(c(
          rep("init", length(idx.init)),
          rep("seq", length(idx.seq)),
          rep("prop", length(idx.proposed)),
          rep("front", length(idx.nsga2front))
        ))
      )

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

      ply = ggplot(data = gg.points.front, aes_string(x = "y1", y = "y2",
        colour = "type", shape = "type"))
      ply = ply + geom_point(data = subset(gg.points.front, type == "front"),
        size = 2, alpha = 0.4)
      ply = ply + geom_point(data = subset(gg.points.front, type != "front"),
        size = 4)
      ply = ply + geom_abline(intercept = intercept, slope = slope)
      ply = ply + xlab(names.y[1L])
      ply = ply + ylab(names.y[2L])
      ply = ply + xlim(y1lim) + ylim(y2lim)
      ply = ply + geom_text(data = NULL, x = 3/12 * y1lim[2L], y = 24/24 * y2lim[2L],
        label = paste("lambda[1] == ", round(weights[1L], 2), sep = ""), parse = TRUE, col = "black", size = 5)
      ply = ply + geom_text(data = NULL, x = 3/12 * y1lim[2L], y = 23/24 * y2lim[2L],
        label = paste("lambda[2] == ", round(weights[2L], 2), sep = ""), parse = TRUE, col = "black", size = 5)
      ply = ply + geom_line(data = gg.line, col = "blue", shape = 1)

      plx = ggplot()
      plx = plx + geom_tile(data = xgrid2, aes_string(x = names.x[1L], y = names.x[2L], fill = name.crit))
      plx = plx + scale_fill_gradientn(colours = topo.colors(7))
      plx = plx +  geom_point(data = subset(gg.points.set, type == "front"),
        aes_string(x = "x1", y = "x2", colour = "type", shape = "type"), size = 2, alpha = 0.8)
      plx = plx + geom_point(data = subset(gg.points.set, type != "front"),
        aes_string(x = "x1", y = "x2", colour = "type", shape = "type"), size = 4)

      title = sprintf("Iter %i", i)
      pl.all = grid.arrange(ply, plx, nrow = 1, main = title)
      print(pl.all)
      if (pause)
        pause()
    }
  }
  return(pl.all)
}
