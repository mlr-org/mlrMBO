#store the predictions of the infill for each mbo iteration and then plot it
fn = makeRosenbrockFunction(2)
w.fn = wrapSmoofConceptDrift(fn = fn, drift.param = "x1")

mbo.iters = 40
drift.range = unlist(attr(w.fn, "original.par.set")$pars[[attr(w.fn, "drift.param")]][c("lower", "upper")])
drift.range = drift.range + c(0.2, -2)
slow.drift = function(dob) {
  drift.range[1] + (dob/mbo.iters) * diff(drift.range)
}

ctrl = makeMBOControl(final.method = "best.predicted")
ctrl = setMBOControlConceptDrift(
  control = ctrl,
  drift.function = slow.drift,
  learn.drift = TRUE,
  calculate.th.final.point = TRUE)
ctrl = setMBOControlTermination(ctrl, iter = mbo.iters)
ctrl = setMBOControlInfill(ctrl, crit.aei)
ctrl$hook = function(opt.state) {
  models = getOptStateModels(opt.state)
  model = models$models[[1]]
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  par.set = getOptStateParSet(opt.state)
  points = generateGridDesign(par.set, 100)
  fun = getOptProblemFun(opt.problem)
  drift.param = attr(fun, "drift.param")
  design = getOptStateDesigns(opt.state)[[1]]

  y = apply(points[, getParamIds(par.set, TRUE, TRUE), drop = FALSE] , 1, fun)
  task = makeRegrTask(target = "y", data = cbind.data.frame(points, y = y))

  infill.res = control$infill.crit$fun(points = points, models = getOptStateModels(opt.state)$models, control = control, par.set = par.set, designs = getOptStateDesigns(opt.state), attributes = TRUE, iter = getOptStateLoop(opt.state))

  crit.components = attr(infill.res, "crit.components")
  plot.data = data.table::data.table(infill = infill.res, crit.components, points)
  plot.data$y = y
  return(plot.data)
}

res = mbo(fun = w.fn, control = ctrl)

res$final.opt.state$opt.path$window.function.active = FALSE
opdf = as.data.frame(res$final.opt.state$opt.path)

all_data = tidyr::unnest(res$final.opt.state$hook.store)
mdata = data.table::melt(all_data, id.vars = getParamIds(res$opt.path$par.set, TRUE, TRUE)
)

mdata[, ':='("value", normalize(x = get("value"), method = "range")), by = "variable"]
mdata[variable %in% c("infill"), ':='("value", normalize(x = get("value"), method = "range")), by = c("variable", "x1")]
#y.range = range(mdata[get("variable")=="y", "value"])
#design[[y.ids]] = (design[[y.ids]] + (0 - predict.range[1])) / diff(predict.range)
library(ggplot2)
g = ggplot(mdata[variable %in% c("y", "mean", "se", "infill"),], aes(x = x1, y = x2))
g = g + geom_raster(aes(fill = value))
g = g + facet_grid(~variable)
g = g + ggplot2::scale_fill_gradientn(colours = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"), interpolate = "spline")(200))
#g

g = g + geom_point(data = opdf, shape = 1)
g = g + geom_path(data = opdf, aes(x = final.x.x1, y = final.x.x2), alpha = 0.2)
g
