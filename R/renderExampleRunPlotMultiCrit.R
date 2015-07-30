# multi-objective
#' @export
renderExampleRunPlot.MBOExampleRunMultiCrit = function(object, iter, densregion = TRUE,
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
  crit.name = control$infill.crit
  method = control$multicrit.method
  points.per.dim = object$points.per.dim
  
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
    pl.xspace = makeXPlot(data.x, idx, idx.nsga2.paretofront, method, x.name, crit.name,
      models, control, par.set, opt.path, points.per.dim, iter, control$propose.points,
      object)
    
    # Render Y Space Plot
    pl.yspace = makeYPlot(data.y, idx, idx.nsga2.paretofront, method, y.name,
      opt.path, control, iter, control$propose.points, object) 
    
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
      pl.xspace = makeXPlot(data.x, idx.propose, idx.nsga2.paretofront, method, x.name, crit.name,
        prop.models, control, par.set, opt.path, points.per.dim, iter, 1L, object)
      
      # Render Y Space Plot
      pl.yspace = makeYPlot(data.y, idx.propose, idx.nsga2.paretofront, method,
        y.name, opt.path, control, iter, 1L, object) 
      
      plots[[propose.iter]] = list(pl.set = pl.xspace, pl.front = pl.yspace)
    }
  }
  return(plots)
}


makeXPlot = function(data.x, idx, idx.nsga2.paretofront, method, x.name, crit.name,
  models, control, par.set, opt.path, points.per.dim, iter, propose.points, object) {
  
  pl.xspace = ggplot()
  pl.xspace = pl.xspace + guides(colour = FALSE, shape = FALSE)
  
  gg.points.xspace = getPlotData(data.x, idx, idx.nsga2.paretofront, x.name)
  # first, fill background if possible. note: 2 different plots for mspot since
  # we have 2 infill crits, one per model
  if (method == "mspot") {
    data.crit1 = getInfillCritGrid(crit.name, points.per.dim, models[1],
      control, par.set, opt.path[idx$past, ])
    data.crit2 = getInfillCritGrid(crit.name, points.per.dim, models[2],
      control, par.set, opt.path[idx$past, ])
    
    crit1.plot = fillBackgroundWithInfillCrit(pl.xspace, data.crit1, x.name, crit.name) + 
      ggtitle("XSpace - model 1")
    crit2.plot = fillBackgroundWithInfillCrit(pl.xspace, data.crit2, x.name, crit.name) + 
      ggtitle("XSpace - model 2")
    
    pl.xspace = list(
      crit1 = createBasicSpacePlot(crit1.plot, gg.points.xspace, iter, object, x.name, 0.8, "x"),
      crit2 = createBasicSpacePlot(crit2.plot, gg.points.xspace, iter, object, x.name, 0.8, "x")
    )
  } 
  if (method %in% c("parego", "dib")) {
    if (propose.points == 1L) {
      data.crit = getInfillCritGrid(crit.name, points.per.dim, models,
        control, par.set, opt.path[idx$past, ], iter)
      pl.xspace = fillBackgroundWithInfillCrit(pl.xspace, data.crit, x.name, crit.name) + 
        ggtitle("XSpace")
    }
    pl.xspace = createBasicSpacePlot(pl.xspace, gg.points.xspace, iter, object, x.name, 0.8, "x")
  }
  return(pl.xspace)
}


makeYPlot = function(data.y, idx, idx.nsga2.paretofront, method, y.name, opt.path,
  control, iter, propose.points, object) {
  gg.points.yspace = getPlotData(data.y, idx, idx.nsga2.paretofront, y.name)
  pl.yspace = ggplot()
  pl.yspace = createBasicSpacePlot(pl.yspace, gg.points.yspace, iter, object, y.name, 0.4, "y")
  if (method == "parego" && propose.points == 1L)
    pl.yspace = addParegoWeightLines(pl.yspace, data.y, idx, opt.path, 1L, control$multicrit.parego.rho)
  pl.yspace = pl.yspace + ggtitle("YSpace")
  return(pl.yspace)
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
  critfun = getInfillCritFunction(crit.name)
  xgrid = generateGridDesign(par.set = par.set, resolution = points.per.dim)
  opt.direction = if (crit.name %in% c("ei")) -1 else 1
  if (inherits(models, "FailureModel"))
    xgrid[[crit.name]] = NA
  else 
    xgrid[[crit.name]] = opt.direction * critfun(xgrid, models, control, par.set, opt.path, iter)
  return(xgrid)
}

# create plot for X-space
createBasicSpacePlot = function(pl, points, iter, object, name, alpha, space) {
  pl = pl +  geom_point(data = points[which(points$type == "front"), ],
    aes_string(x = name[1L], y = name[2L], colour = "type", shape = "type"), size = 2, alpha = alpha)
  pl = pl + geom_point(data = points[which(points$type != "front"), ],
    aes_string(x = name[1L], y = name[2L], colour = "type", shape = "type"), size = 4)
  
  # add appr. of non dominated model points
  grid = if(space == "x") object$mbo.pred.grid.x else object$mbo.pred.grid.mean[[iter]]
  pl = addApproxMBO(pl, grid, name, object$mbo.pred.grid.mean[[iter]]$.is.dom, "brown", "solid")
  
  # add appr. of lcb of non dominated model points
  grid = if(space == "x") object$mbo.pred.grid.x else object$mbo.pred.grid.lcb[[iter]]
  pl = addApproxMBO(pl, grid, name, object$mbo.pred.grid.lcb[[iter]]$.is.dom, "brown", "dotted")
  return(pl)
}

# add pareto front estimated by model
addApproxMBO = function(pl, points, col.names, isdom, colour, lty) {
  if (!is.null(points)) {
    points = setColNames(points[!isdom, 1:2, drop = FALSE], col.names)
    points = sortByCol(points, col.names)
    pl = pl + geom_line(aes_string(x = col.names[1L], y = col.names[2L]),
      points, colour = colour, linetype = lty, alpha = 0.8)
  }
  return(pl)
}

fillBackgroundWithInfillCrit = function(pl, data, x.name, crit.name) {
  brewer.palette = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"), interpolate = "spline")
  pl = pl + geom_tile(data = data, aes_string(x = x.name[1L], y = x.name[2L], fill = crit.name))
  pl = pl + scale_fill_gradientn(colours = brewer.palette(200))
  return(pl)
}

addParegoWeightLines = function(pl, data.y, idx, opt.path, proposed.counter, rho) {
  y1range = range(data.y[idx$past, 1L])
  y2range = range(data.y[idx$past, 2L])
  weights = as.numeric(opt.path[idx$proposed[proposed.counter], c(".weight1", ".weight2")])
  
  slope  = weights[2L] * (y2range[2L] - y2range[1L]) /
    (weights[1L] * (y1range[2L] - y1range[1L]))
  intercept = y2range[1L] - y1range[1L] * slope
  pl = pl + geom_abline(intercept = intercept, slope = slope)
  
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
  #   pl + geom_line(data = gg.line, aes(x = y1, y = y2), col = "blue", shape = 1)
  pl
}

