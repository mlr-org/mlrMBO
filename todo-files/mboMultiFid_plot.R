genPlotData = function(compound.model, opt.path, control, fun, res = 100, lvl.cors, lvl.sds, par.set, best.points, merge = TRUE) {
  if (!control$multifid.generate.plot.data)
    return(NULL)
  requirePackages(packs=c("ggplot2", "reshape2"), why="generate MultiFid Plot")
  # par set without the multifid lvl param
  par.set.lower = dropParams(par.set, ".multifid.lvl")
  # generate a grid design for without respect to the multifid level
  grid.design = generateGridDesign(par.set = par.set.lower, resolution = res)
  # expand the design to the same points on each multifid level
  grid.design = expandDesign(design = grid.design, control = control)
  # get the points we already have evaluated during the algorithm
  old.points = convertToDesign(opt.path, control)
  # combine the grid and the already calculated points so plot lines also have the exact points
  if (merge) grid.design = rbind(grid.design, old.points[,colnames(grid.design)])
  # predict all the y values by the model
  p = predict(compound.model, newdata = grid.design)
  z = infillCritMultiFid2(
    points = dropNamed(grid.design, ".multifid.lvl"), 
    model = compound.model, 
    control = control, 
    par.set = par.set, 
    design = old.points, 
    lvl.cors = lvl.cors, 
    lvl.sds = lvl.sds, 
    lvl = grid.design$.multifid.lvl)
  z.df = do.call(cbind.data.frame, z)
  all = cbind(grid.design, y = p$data$response, z)
  xname = getParamIds(par.set.lower)
  best.points.z = infillCritMultiFid2(
    points = dropNamed(best.points, ".multifid.lvl"), 
    model = compound.model, 
    control = control, 
    par.set = par.set, 
    design = old.points, 
    lvl.cors = lvl.cors, 
    lvl.sds = lvl.sds, 
    lvl = best.points$.multifid.lvl)
  best.points$y = predict(compound.model, newdata = best.points)$data$response
  best.points = do.call(cbind, c(list(best.points), best.points.z))
  return(list(all = all, xname = xname, old.points = old.points, best.points = best.points, lvls = control$multifid.lvls, param = control$multifid.param))
}

plotMultiFidStep = function(plotdata, subset.variable = character(0), title = character(0), add.g = list()) {
  assertList(add.g)
  assertCharacter(title)
  assertCharacter(subset.variable)
  assertList(plotdata)
  assertSubset(subset.variable, colnames(plotdata$all))
  dim = length(plotdata$xname)
  if(dim == 1) {
    plotMultiFidStep1d(plotdata, subset.variable, title, add.g)
  } else if (dim == 2) {
    plotMultiFidStep2d(plotdata, subset.variable, title, add.g)
  }
}

plotMultiFidStep1d = function(plotdata, subset.variable = character(0), title = character(0), add.g = list()) {
  xname = plotdata$xname
  m.all = melt(plotdata$all, id.vars = c(xname, ".multifid.lvl"))
    
  m.all = m.all[m.all$variable != "ei" | (m.all$variable == "ei" & m.all$.multifid.lvl == max(m.all$.multifid.lvl)), ] # drop EI for not last .multifid.lvl
  old.points = rename(plotdata$old.points, c("y"="value"))
  old.points$variable = "y"
#  best.points = rename(plotdata$best.points, c("y"="value"))
#  best.points.txt = best.points; best.points.txt$variable = "y"
  m.best.points = melt(plotdata$best.points, id.vars = c(xname, ".multifid.lvl"))
  if (length(subset.variable)>0) {
    m.all = subset(m.all, subset = m.all$variable %in% subset.variable)
    m.best.points = subset(m.best.points, subset = m.best.points$variable %in% subset.variable)
  }
  vars.needed = c("value", xname, ".multifid.lvl", "variable") 
  
  assertSubset(vars.needed, colnames(m.all))
  assertSubset(vars.needed, colnames(old.points))
  assertSubset(vars.needed, colnames(m.best.points))

  g = ggplot(m.all, aes_string(x = xname, y = "value", color = "as.factor(.multifid.lvl)", group = "as.factor(.multifid.lvl)"))
  g = g + geom_line()
  g = g + geom_point(data = old.points)
  g = g + geom_vline(data = m.best.points, aes_string(xintercept = xname, color = "as.factor(.multifid.lvl)"), lty=2)
  g = g + geom_text(data = m.best.points, aes(label = sprintf("%.4g", value)), hjust = 0, vjust = 0, size = 3, color = "black", alpha = 0.7)
  for (i in seq_along(add.g)) {
    g = g + add.g[[i]]
  }
  g = g + facet_grid(variable~., scales="free_y")
  g = g + scale_color_discrete(name = plotdata$param, labels = plotdata$lvls)
  g = g + theme(legend.position = "bottom")
  if (length(title)>0) {
    g = g + ggtitle(title)
  }
  return(g)
}

plotMultiFidStep2d = function(plotdata, subset.variable = character(0), title = character(0), add.g = list()) {
  plots = plotMultiFidStep2dRaw(plotdata, subset.variable, add.g = add.g)
  gs = do.call(grid.arrange, c(plots, list(nrow = 1, main = title)))
  return(gs)
}

plotMultiFidStep2dRaw = function(plotdata, subset.variable = character(0), add.g = list()) {
  plots = lapply(subset.variable, function(var) {
    xname = plotdata$xname
    m.all = melt(plotdata$all, id.vars = c(xname, ".multifid.lvl"))
    if (length(subset.variable))
      m.all = m.all[m.all$variable == var,]
    g = plotMultiFidStep2dRawEach(m.all, xname, 
      old.points = plotdata$old.points[,c(xname, ".multifid.lvl")], 
      best.points = plotdata$best.points[,c(xname, ".multifid.lvl")],
      add.g = add.g)
    return(g)
  })
  return(plots)
}

plotMultiFidStep2dRawEach = function(m.spec, xname, old.points, best.points, add.g = list()){
  assertSubset(c("value", xname, "variable", ".multifid.lvl"), colnames(m.spec))
  g = ggplot(m.spec, aes_string(x = xname[1], y = xname[2]))
  g = g + geom_tile(aes(fill = value))
  g = g + stat_contour(aes(z = value), size = 0.5, alpha = 0.5)
  g = g + geom_point(data = old.points, size = 2)
  g = g + geom_point(data = best.points, pch = 4, size = 4)
  g = g + facet_grid(.multifid.lvl ~ variable, scales = "free")
  g = g + theme(legend.position = "bottom")
  g = g + scale_fill_gradient2(low = "red", mid = "yellow", high = "blue", midpoint = mean(range(m.spec$value)))
  for (i in seq_along(add.g)) {
    g = g + add.g[[i]]
  }
  return(g)
}

# make a design (data.frame) with the exact same points for each multifid level.
# if the requested number of points is a bit lower than design, we randomly drop some rows
expandDesign = function(design, control, npoints.per.lvl = NULL) {
  n = nrow(design)
  k = length(control$multifid.lvls)
  # default is to replicate the design for all levels
  if (is.null(npoints.per.lvl))
    npoints.per.lvl = rep(n, times = k)
  all.inds = seq_len(n)
  designs = lapply(seq_len(k), function(i) {
    nppl = npoints.per.lvl[i]
    # do we need to drop some rows of design? lets also keep the order of all.inds
    inds = if (nppl < n)
      setdiff(all.inds, sample(all.inds, n - nppl))
    else
      all.inds
    cbind(design[inds, , drop = FALSE], .multifid.lvl = i)
  })
  do.call(rbind.data.frame, designs)
}