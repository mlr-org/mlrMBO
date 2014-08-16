genPlotData = function(compound.model, opt.path, control, fun, res = 100, model.cor, model.sd, model.cost, par.set, best.points) {
  requirePackages(packs=c("ggplot2", "reshape2"), why="generate MultiFid Plot")
  par.set.lower = dropParams(par.set, control$multifid.param)
  grid.design = generateGridDesign(par.set = par.set.lower, resolution = res)
  grid.design = expandDesign(design = grid.design, control = control)
  old.points = convertOptPathToDesign(opt.path)
  grid.design = rbind(grid.design, old.points[,colnames(grid.design)]) #so plot lines also have the exact points
  p = predict(compound.model, newdata = grid.design)
  z = infillCritMultiFid2(points = grid.design, model = compound.model, control = control, par.set = par.set, design = old.points , model.cor = model.cor, model.sd = model.sd, model.cost = model.cost)
  z.df = do.call(cbind.data.frame, z)
  all = cbind(grid.design, response = p$data$response, z)
  stopifnot(length(par.set.lower$pars) == 1) #nur 1d-plots
  xname = names(par.set.lower$pars)
  zname = control$multifid.param
  m.all = melt(all, id.vars=c(xname, zname))
  #ei last extra care
  m.all[m.all[["variable"]] == "ei", control$multifid.param] = tail(control$multifid.lvls,1)
  old.points = rename(old.points, c("y"="value")); old.points$variable = "response"
  best.points$value = predict(compound.model, newdata = best.points)$data$response
  return(list(m.all = m.all, xname = xname, zname = zname, old.points = old.points, best.points = best.points))
}

genGgplot = function(plotdata, subset.variable = NULL, title = character(0), add.g = list()) {
  assertList(add.g)
  m.all = plotdata$m.all
  if (!is.null(subset.variable)) {
    m.all = subset(m.all, subset=m.all[["variable"]] %in% subset.variable)
  }
  xname = plotdata$xname
  zname = plotdata$zname
  old.points = plotdata$old.points
  best.points = plotdata$best.points
  best.points.txt = best.points; best.points.txt$variable = "response"
  g = ggplot(m.all, aes_string(x = xname, y = "value", color = zname, group = zname))
  g = g + geom_line()
  g = g + geom_point(data = old.points)
  g = g + geom_vline(data = best.points, aes_string(xintercept = xname, color = zname), lty=2)
  g = g + geom_text(data = best.points.txt, aes_string(x = xname, y = "value", label = zname), hjust = 0, vjust = 0, size = 5)
  for (i in seq_along(add.g)) {
    g = g + add.g[[i]]
  }
  g = g + facet_wrap(~variable, scales="free_y", ncol=1)
  if (length(title)>0) {
    g = g + ggtitle(title)
  }
  return(g)
}
