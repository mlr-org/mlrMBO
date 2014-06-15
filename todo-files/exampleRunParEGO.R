exampleRunParEGO = function(fun, par.set, learner, control, show.info = TRUE,
  nsga2.args = list(), ...) {

  checkArg(fun, "function")
  checkArg(control, "MBOControl")
  checkArg(par.set, "ParamSet")
  par.types = getParamTypes(par.set)

  learner = checkLearner(learner, par.set, control, ...)

  checkArg(show.info, "logical", len = 1L, na.ok = FALSE)
  ny = control$number.of.targets

  if (ny >= 3L)
    stopf("exampleRun can only be applied for functions with at most 2 objectives, but you have %iD", ny)

  control$save.model.at = 1:control$iters
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  names.y = control$y.name

  showInfo(show.info, "Running NSGA2 to approximate pareto front.")

  args = list(fun, idim = getParamNr(par.set, devectorize = TRUE), odim = ny,
    lower.bounds = getLower(par.set), upper.bounds = getUpper(par.set),
    popsize = 60L, generations = 100L)
  args = insert(args, nsga2.args)
  nsga2.res = do.call(nsga2, args)
  nsga2.paretoset = setColNames(as.data.frame(nsga2.res$par[nsga2.res$pareto.optimal, ]), names.x)
  nsga2.paretofront = setColNames(as.data.frame(nsga2.res$value[nsga2.res$pareto.optimal, ]), names.y)

  #show some info on console
  showInfo(show.info, "Performing MBO on function.")
  showInfo(show.info, "Initial design: %i. Sequential iterations: %i.", control$init.design.points, control$iters)
  showInfo(show.info, "Learner: %s. Settings:\n%s", learner$id, mlr:::getHyperParsString(learner))

  # run optimizer now
  res = mbo(fun, par.set, learner = learner, control = control, show.info = show.info)

  makeS3Obj(c("ParEGOExampleRun", "MBOExampleRun"),
    par.set = par.set,
    par.types = par.types,
    names.x = names.x,
    names.y = names.y,
    learner = learner,
    control = control,
    nsga2.paretofront = nsga2.paretofront,
    nsga2.paretoset = nsga2.paretoset,
    mbo.res = res
  )
}


autoplo = function(x, iters, pause = TRUE, y1lim = NULL, y2lim = NULL, points.per.dim, ...)  {
  points.per.dim = convertInteger(points.per.dim)
  checkArg(points.per.dim, "integer", len = 1L, na.ok = FALSE, lower = 1L)

  # extract information from example run object
  par.set = x$par.set
  control = x$control
  names.y = control$y.name
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  proppoints = control$propose.points
  mbo.res = x$mbo.res
  opt.path = as.data.frame(mbo.res$opt.path)
  y = getOptPathY(mbo.res$opt.path)
  yy = rbind(setRowNames(y, NULL), setRowNames(x$nsga2.paretofront, NULL))
  idx.nsga2front = (nrow(y)+1):nrow(yy)
  if (is.null(y1lim))
    y1lim = range(yy[,1L])
  if (is.null(y2lim))
    y2lim = range(yy[,2L])
  name.crit = control$infill.crit
  critfun = getInfillCritFunction(name.crit)
  xgrid = generateGridDesign(par.set = par.set, resolution = points.per.dim)
  # xgrid = generateGridDesign(par.set = par.set, resolution = 4)
  xgrid2 = xgrid
  nsga2.paretoset = x$nsga2.paretoset
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

      gg.points = data.frame(
        y1 = yy[idx, 1L],
        y2 = yy[idx, 2L],
        type = as.factor(c(
          rep("init", length(idx.init)),
          rep("seq", length(idx.seq)),
          rep("prop", length(idx.proposed)),
          rep("front", length(idx.nsga2front))
        ))
      )

      ply = ggplot(data = gg.points, aes_string(x = "y1", y = "y2",
          colour = "type", shape = "type"))
      ply = ply + geom_point(data = subset(gg.points, type == "front"),
        size = 1.5, alpha = 0.3)
      ply = ply + geom_point(data = subset(gg.points, type != "front"),
        size = 4)
      # # FIXME: weights / line is probably wrong here because y values are scaled to (0,1) in parego first?
      ply = ply + geom_abline(intercept = 0, slope = weights[2L] / weights[1L])
      ply = ply + xlab(names.y[1L])
      ply = ply + ylab(names.y[2L])
      ply = ply + xlim(y1lim) + ylim(y2lim)

      plx = ggplot(data = xgrid2, aes_string(x = names.x[1L], y = names.x[2L]))
      plx = plx + geom_tile(aes_string(fill = name.crit))
      plx = plx + scale_fill_gradientn(colours = topo.colors(7))
      plx = plx + geom_point(data = nsga2.paretoset, size = 3)

      title = sprintf("Iter %i", i)
      pl.all = grid.arrange(ply, plx, nrow = 1, main = title)
      print(pl.all)
      if (pause)
        pause()
    }
  }
  return(pl.all)
}


load_all(".")

f = zdt2
ps = makeNumericParamSet(len = 2L, lower = 0, upper = 1)


ctrl = makeMBOControl(number.of.targets = 2L, init.design.points = 10L, iters = 10,
  infill.crit = "ei", infill.opt.focussearch.points = 10000L,
  parego.s = 1000)

#run = exampleRunParEGO(makeMBOFunction(f), ps, control = ctrl)

autoplo(run, iters = 1:10, pause = T, resolution = ,points.per.dim = 50)
