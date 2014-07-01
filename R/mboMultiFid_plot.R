genPlotData = function(compound.model, opt.path, control, fun, res = 100, model.cor, model.sd, model.cost, par.set, best.points) {
  library("ParamHelpers")
  par.set.lower = parSetWithout(par.set, control$multifid.param)
  grid.design = generateGridDesign(par.set = par.set.lower, resolution = res)
  grid.design = expandDesign(design = grid.design, control = control)
  old.points = convertOptPathToDesign(opt.path)
  p = predict(compound.model, newdata = grid.design)
  z = infillCritMultiFid2(points = grid.design, model = compound.model, control = control, par.set = par.set, design = old.points , model.cor = model.cor, model.sd = model.sd, model.cost = model.cost)
  z.df = do.call(cbind.data.frame, z)
  all = cbind(grid.design, response=p$data$response, z)
  library("reshape2")
  library("ggplot2")
  library("plyr")
  stopifnot(length(par.set.lower$pars) == 1) #nur 1d-plots
  m.all = melt(all, id.vars=c(names(par.set.lower$pars), control$multifid.param))
  old.points = rename(old.points, c("y"="value")); old.points$variable = "response"
  g = ggplot(m.all, aes(x=sigma, y=value, color=as.factor(dw.perc), group=dw.perc))
  g = g + geom_line()
  g = g + geom_point(data = old.points)
  g = g + geom_vline(xintercept=best.points$sigma, alpha = 0.5, lty=2)
  g = g + facet_wrap(~variable, scales="free_y", ncol=1)
  return(g)
}