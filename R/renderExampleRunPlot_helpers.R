
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
      if (!is.null(t)) assertClass(t, "MBOTrafoFunction")
    )
    trafo = trafo.defaults
    trafo[names(input.trafo)] = input.trafo
  }
  return(trafo)
}


getIDX = function(opt.path, i) {
  list(
    init = which(opt.path$dob == 0),
    seq = which(opt.path$dob > 0 & opt.path$dob < i),
    proposed = which(opt.path$dob == i),
    past = which(opt.path$dob < i),
    pastpresent = which(opt.path$dob <= i)
  )
}


# Get Dataset for Plotting 2D XSpace
getPlotData = function(data, idx, idx.nsga2.paretofront, name) {
  idx.all = c(idx$init, idx$seq, idx$proposed, idx.nsga2.paretofront)
  df = data.frame(
    data[idx.all, 1L],
    data[idx.all, 2L],
    as.factor(c(
      rep("init", length(idx$init)),
      rep("seq", length(idx$seq)),
      rep("prop", length(idx$proposed)),
      rep("front", length(idx.nsga2.paretofront))
    ))
  )
  names(df) = c(name, "type")
  return(df)
}


# get Dataset for fill background with infill.crit
# Note: models is a single model for parego and mspot, for dib a list of models
getInfillCritGrid = function(crit.name, points.per.dim, models, control, par.set, opt.path, iter) {
  critfun = control$infill.crit$fun
  xgrid = generateGridDesign(par.set = par.set, resolution = points.per.dim)
  opt.direction = if (crit.name %in% c("ei")) -1 else 1
  if (inherits(models, "FailureModel"))
    xgrid[[crit.name]] = NA
  else
    xgrid[[crit.name]] = opt.direction * critfun(xgrid, models, control, par.set, opt.path, iter)
  return(xgrid)
}


# create basic plot for X or Y space
createBasicSpacePlot = function(pl, points, iter, object, name, alpha, space, colors) {
  pl = pl +  ggplot2::geom_point(data = points[points$type == "front", ],
    ggplot2::aes_string(x = name[1L], y = name[2L], colour = "type", shape = "type"), size = 2, alpha = alpha)
  pl = pl + ggplot2::geom_point(data = points[points$type != "front", ],
    ggplot2::aes_string(x = name[1L], y = name[2L], colour = "type", shape = "type"), size = 4)
  pl = pl + ggplot2::scale_colour_manual(values = c("black", colors))
  pl = pl + ggplot2::scale_shape_manual(values = c(16, 16, 17, 15))

  # add appr. of non dominated model points
  grid = if (space == "x") object$mbo.pred.grid.x else object$mbo.pred.grid.mean[[iter]]
  pl = addApproxMBO(pl, grid, name, object$mbo.pred.grid.mean[[iter]]$.is.dom, "brown", "solid")

  # add appr. of cb of non dominated model points
  grid = if (space == "x") object$mbo.pred.grid.x else object$mbo.pred.grid.cb[[iter]]
  pl = addApproxMBO(pl, grid, name, object$mbo.pred.grid.cb[[iter]]$.is.dom, "brown", "dotted")
  return(pl)
}


# add pareto front estimated by model
addApproxMBO = function(pl, points, col.names, isdom, colour, lty) {
  if (!is.null(points)) {
    points = setColNames(points[!isdom, 1:2, drop = FALSE], col.names)
    points = sortByCol(points, col.names)
    pl = pl + ggplot2::geom_line(ggplot2::aes_string(x = col.names[1L], y = col.names[2L]),
      points, colour = colour, linetype = lty, alpha = 0.8)
  }
  return(pl)
}


fillBackgroundWithInfillCrit = function(pl, data, x.name, crit.name) {
  brewer.palette = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"), interpolate = "spline")
  pl = pl + ggplot2::geom_tile(data = data, ggplot2::aes_string(x = x.name[1L], y = x.name[2L], fill = crit.name))
  pl = pl + ggplot2::scale_fill_gradientn(colours = brewer.palette(200))
  return(pl)
}


addParegoWeightLines = function(pl, data.y, idx, opt.path, proposed.counter, rho) {
  y1range = range(data.y[idx$past, 1L])
  y2range = range(data.y[idx$past, 2L])
  weights = as.numeric(opt.path[idx$proposed[proposed.counter], c("parego.weight.1", "parego.weight.2")])

  slope  = weights[2L] * (y2range[2L] - y2range[1L]) /
    (weights[1L] * (y1range[2L] - y1range[1L]))
  intercept = y2range[1L] - y1range[1L] * slope
  pl = pl + ggplot2::geom_abline(intercept = intercept, slope = slope)

  # now add visualization for rho
  #   FIXME: Rethink this!
  #   # make dataframe for lines to show rho
  #   m.seq = seq(y1range[1], y1range[2], length.out = 10000)
  #   # Function to get the values for the rho visualization
  #   f = function(x, lambda, rho, const) {
  #     x = (x - y1range[1L]) / (y1range[2L] - y1range[1L])
  #     y.left = (const - lambda[2L] * x * (1 + rho)) / (rho * lambda[1L])
  #     y.left = y.left * (y2range[2L] - y2range[1L]) + y2range[1L]
  #
  #     y.right = (const - lambda[2L] * x * rho) / ((1 + rho) * lambda[1L])
  #     y.right = y.right * (y2range[2L] - y2range[1L]) + y2range[1L]
  #     pmin(y.left, y.right)
  #   }
  #
  #   # FIXME: find a good way to set this constant. I tried a lot and i found
  #   # nothing that worked really good. this is the best i got ... it works somehow,
  #   # but is far from perfect.
  #   tmp.x = sqrt(slope ^ 2 / 4 + 1 - intercept) - slope / 2
  #   tmp.y = tmp.x * slope + intercept
  #   const = optimize(function(x) (f(tmp.x, weights, rho, x) - tmp.y) ^ 2, interval = c(0, 10))$minimum
  #   gg.line = data.frame(
  #     y1 = m.seq,
  #     y2 = f(m.seq, weights, rho, const)
  #   )
  #   pl + ggplot2::geom_line(data = gg.line, aes(x = y1, y = y2), col = "blue", shape = 1)
  pl
}

# @param g [ggplot object]
#   A normal ggplot object
# @ggs [various ggplot objects]
#   Elements that you want to add to ggplot as custom scales or themes
addGgsToGgplot = function(g, ggs) {
  if (!inherits(g, "gg")) return(g) #sometimes there are NAs
  for (gg in ggs) {
    g = g + gg
  }
  return(g)
}
