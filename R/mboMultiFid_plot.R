genPlotData = function(compound.model, opt.path, control, fun, res = 100, model.cor, model.sd, model.cost, par.set, best.points) {
  requirePackages(packs="ggplot2", why="generate MultiFid Plot")
  par.set.lower = parSetWithout(par.set, control$multifid.param)
  grid.design = generateGridDesign(par.set = par.set.lower, resolution = res)
  grid.design = expandDesign(design = grid.design, control = control)
  old.points = convertOptPathToDesign(opt.path)
  grid.design = rbind(grid.design, old.points[,colnames(grid.design)]) #so plot lines also have the exact points
  p = predict(compound.model, newdata = grid.design)
  z = infillCritMultiFid2(points = grid.design, model = compound.model, control = control, par.set = par.set, design = old.points , model.cor = model.cor, model.sd = model.sd, model.cost = model.cost)
  z.df = do.call(cbind.data.frame, z)
  all = cbind(grid.design, response=p$data$response, z)
  stopifnot(length(par.set.lower$pars) == 1) #nur 1d-plots
  xname = names(par.set.lower$pars)
  zname = control$multifid.param
  m.all = melt(all, id.vars=c(xname, zname))
  old.points = rename(old.points, c("y"="value")); old.points$variable = "response"
  
  return(list(m.all = m.all, xname = xname, zname = zname, old.points = old.points, best.points = best.points))
}

genGgplot = function(plotdata) {
  m.all = plotdata$m.all
  xname = plotdata$xname
  zname = plotdata$zname
  old.points = plotdata$old.points
  best.points = plotdata$best.points
  g = ggplot(m.all, aes_string(x=xname, y="value", color=zname, group=zname))
  g = g + geom_line()
  g = g + geom_point(data = old.points)
  g = g + geom_vline(data = best.points, aes_string(xintercept=xname, color=zname), lty=2)
  g = g + facet_wrap(~variable, scales="free_y", ncol=1)
  return(g)
}
