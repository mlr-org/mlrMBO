genPlotData = function(compound.model, opt.path, control, fun, res = 100, lvl.cors, lvl.sds, time.model, par.set, best.points, merge = TRUE) {
  requirePackages(packs=c("ggplot2", "reshape2"), why="generate MultiFid Plot")
  # par set without the multifid lvl param
  par.set.lower = dropParams(par.set, ".multifid.lvl")
  # generate a grid design for without respect to the multifid level
  grid.design = generateGridDesign(par.set = par.set.lower, resolution = res)
  # expand the design to the same points on each multifid level
  grid.design = expandDesign(design = grid.design, control = control)
  # get the points we already have evaluated during the algorithm
  old.points = convertMFOptPathToDesign(opt.path)
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
    time.model = time.model, 
    lvl = grid.design$.multifid.lvl)
  z.df = do.call(cbind.data.frame, z)
  all = cbind(grid.design, y = p$data$response, z)
  xname = getParamIds(par.set.lower)
  #m.all = melt(all, id.vars=c(xname, ".multifid.lvl"))
  #ei last extra care
  #m.all[m.all[["variable"]] == "ei", control$multifid.param] = getLast(control$multifid.lvls)
  #old.points = rename(old.points, c("y"="value")); old.points$variable = "response"
  best.points$y = predict(compound.model, newdata = best.points)$data$response
  return(list(all = all, xname = xname, old.points = old.points, best.points = best.points))
}

genGgplot = function(plotdata, subset.variable = character(0), title = character(0), add.g = list()) {
  assertList(add.g)
  assertCharacter(title)
  assertCharacter(subset.variable)
  assertList(plotdata)
  assertSubset(subset.variable, colnames(plotdata$all))
  dim = length(plotdata$xname)
  if(dim == 1) {
    genGgplot1d(plotdata, subset.variable, title, add.g)
  } else if (dim == 2) {
    genGgplot2d(plotdata, subset.variable, title, add.g)
  }
}

genGgplot1d = function(plotdata, subset.variable = character(0), title = character(0), add.g = list()) {
  xname = plotdata$xname
  m.all = melt(plotdata$all, id.vars = c(xname, ".multifid.lvl"))
    
  m.all = m.all[m.all$variable != "ei" | (m.all$variable == "ei" & m.all$.multifid.lvl == max(m.all$.multifid.lvl)), ] # drop EI for not last .multifid.lvl
  if (length(subset.variable)>0) {
    m.all = subset(m.all, subset = m.all$variable %in% subset.variable)
  }
  old.points = rename(plotdata$old.points, c("y"="value"))
  old.points$variable = "y"
  best.points = rename(plotdata$best.points, c("y"="value"))
  best.points.txt = best.points; best.points.txt$variable = "y"
  vars.needed = c("value", xname, ".multifid.lvl", "variable") 
  
  assertSubset(vars.needed, colnames(m.all))
  assertSubset(vars.needed, colnames(old.points))
  assertSubset(vars.needed[-4], colnames(best.points))

  g = ggplot(m.all, aes_string(x = xname, y = "value", color = "as.factor(.multifid.lvl)", group = ".multifid.lvl"))
  g = g + geom_line()
  g = g + geom_point(data = old.points)
  g = g + geom_vline(data = best.points, aes_string(xintercept = xname, color = "as.factor(.multifid.lvl)"), lty=2)
  g = g + geom_text(data = best.points.txt, aes(label = .multifid.lvl), hjust = 0, vjust = 0, size = 5)
  for (i in seq_along(add.g)) {
    g = g + add.g[[i]]
  }
  g = g + facet_grid(variable~., scales="free_y")
  if (length(title)>0) {
    g = g + ggtitle(title)
  }
  return(g)
}

genGgplot2d = function(plotdata, subset.variable = character(0), title = character(0), add.g = list()) {
  plots = genGgplot2dRaw(plotdata, subset.variable, title)
  gs = do.call(grid.arrange, c(plots, list(nrow = 1, main = title)))
  return(gs)
}

genGgplot2dRaw = function(plotdata, subset.variable = character(0), add.g = list()) {
  plots = lapply(subset.variable, function(var) {
    xname = plotdata$xname
    m.all = melt(plotdata$all, id.vars = c(xname, ".multifid.lvl"))
    if (length(subset.variable))
      m.all = m.all[m.all$variable == var,]
    g = genGgplot2dRawEach(m.all, xname, 
      old.points = plotdata$old.points[,c(xname, ".multifid.lvl")], 
      best.points = plotdata$best.points[,c(xname, ".multifid.lvl")])
    return(g)
  })
  return(plots)
}

genGgplot2dRawEach = function(m.spec, xname, old.points, best.points, add.g = list()){
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