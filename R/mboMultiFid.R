# fun     : function(x) --> numeric(1), basically like in normal mbo, x is a list,
#           but: one element is named <control$multifid.lvl.par> which selects the level
# par.set : Normal ParamSet, but also contains <multifid.lvl.par> as a NumericParam
# design  : We do not allow to pass one, because it is hard to contruct. must be done by multifid
# rest is like in mbo

mboMultiFid = function(fun, par.set, design, learner, control, show.info = TRUE, more.args = list()) {

  if (!is.null(design))
    stopf("mboMultiFid does not work with a preconstructed design!")

  # save currently set options
  oldopts = list(
    ole = getOption("mlr.on.learner.error"),
    slo = getOption("mlr.show.learner.output")
  )

  # termination
  start.time = Sys.time()
  iters = control$iters
  time.budget = control$time.budget
  exec.time.budget = control$exec.time.budget

  ##### LOCAL FUNCTIONS FOR MEI CRIT VALUES: START #####

  # estimate process noise tau of real process belonging to lvl, we use residual sd here
  calcModelSD = function(lvl) {
    newdata = convertMFOptPathToDesign(opt.path)
    newdata = newdata[newdata$.multifid.lvl == lvl, ]
    sqrt(estimateResidualVariance(model, data = newdata, target = "y"))
  }

  # calculate GLOBAL correlation between model w/ lvl and last-lvl. currently rank correlation.
  calcModelCor = function(lvl, grid) {
    new.grid = rbind.data.frame(
      cbind.data.frame(grid, .multifid.lvl = lvl),
      cbind.data.frame(grid, .multifid.lvl = nlvls)
    )
    p = predict(model, newdata = new.grid)$data$response
    p1 = p[new.grid$.multifid.lvl == lvl]
    p2 = p[new.grid$.multifid.lvl == nlvls]
    
    # check whether vectors are constant, cor = NA then
    if (diff(range(p1)) < sqrt(.Machine$double.eps) || diff(range(p2)) < sqrt(.Machine$double.eps))
      0
    else
      max(cor(p1, p2, method = "spearman"), 0)
  }

  ##### LOCAL FUNCTIONS FOR MEI CRIT VALUES: END #####

  # some short names and data extractions
  iters = control$iters
  nlvls = length(control$multifid.lvls)
  lvls = 1:nlvls
  # extra param set, with int param for lvl
  par.set2 = c(par.set, makeParamSet(
    makeIntegerParam(".multifid.lvl", lower = 1L, upper = nlvls)))

  # init design optpath, we store lvl there as well
  design = generateMBOMultiFidDesign(par.set2, control)
  opt.path = makeOptPathDF(par.set2, control$y.name, control$minimize,
    include.error.message = TRUE, include.exec.time = TRUE, include.extra = FALSE)
  # eval + log to opt.path
  xs = dfRowsToList(design, par.set2)
  evalTargetFun(fun, par.set2, 0L, xs, opt.path, control, show.info, oldopts, more.args, extras = NULL)

  # contruct multifid learner, with summed model and bootstrapping
  # FIXME: we need to exactly define how bootstrapping should work for multifid
  learner = mlr:::setPredictType(learner, predict.type = "se")
  learner = makeMultiFidWrapper(learner, control)
  #learner = makeMultiFidBaggingWrapper(learner)

  # now fit to init design
  task = convertMFOptPathToTask(opt.path)
  model = train(learner, task = task)

  #time learner
  if (is.null(control$multifid.costs)) {
    time.learner = makeLearner("regr.km", nugget.estim = TRUE, jitter = TRUE)
    time.task = convertMFOptPathToTimeTask(opt.path)
    time.model = train(time.learner, task = time.task) 
  } else {
    time.model = NULL
  }
  

  # generate the x values we want to use to calculate the correlation between the surrogat models
  corgrid = generateDesign(n = control$multifid.cor.grid.points, par.set = par.set)

  plot.data = list()

  # if we are restarting from a save file, we possibly start in a higher iteration
  loop = max(getOptPathDOB(opt.path)) + 1L

  # multifid main iteration
  repeat {
    showInfo(show.info, "loop = %i", loop)
    
    # evaluate numbers we need for MEI (all vectors with length = #levels)
    lvl.sds = vnapply(lvls, calcModelSD)
    lvl.cors = vnapply(lvls, calcModelCor, grid = corgrid)

    showInfo(show.info, "Estimated cor to last model = %s", collapse(sprintf("%.3g", lvl.cors), ", "))
    showInfo(show.info, "Estimated residual var = %s", collapse(sprintf("%.3g", lvl.sds), ", "))

    # return a list, get a proposed point for each level
    prop = proposePointsMultiFid(model = model, par.set = par.set,
      control = control, opt.path = opt.path,
      lvl.cors = lvl.cors, time.model = time.model, lvl.sds = lvl.sds)
    # find the level where the crit val / infill vals is smallest
    infill.vals = extractSubList(prop, "crit.vals")
    showInfo(show.info, "Infill vals = %s", collapse(sprintf("%.3g", infill.vals), ", "))
    
    # every couple of levels we only optimize the last one
    # to ensure that we update that model and see what happens here
    # also on last eval
    if(loop %% control$multifid.force.last.level.evals == 0 | loop == iters)
      min.index = nlvls
    else
      min.index = getMinIndex(infill.vals)
    # get one x point (see as par vector) and the level parameter
    best.points = prop[[min.index]]$prop.points

    plot.data[[loop]] = genPlotData(
      compound.model = model, 
      par.set = par.set2, 
      control = control, 
      fun = fun, 
      opt.path = opt.path,
      lvl.cors = lvl.cors, lvl.sds = lvl.sds, time.model = time.model,
      best.points = best.points,
      merge = getParamNr(par.set) == 1L,
      res = ifelse(getParamNr(par.set) == 1L, 100, 50)) #merge opt.path with new grid for plot prediction data, only when 1d, because of geom_tile

    # FIXME: BB: we need to talk about this
    # # should we always also update the models of the lower levels. Fixes some theoretical problems.
    # if (control$multifid.eval.lower) {
    #   tmp = data.frame(best.points[1,1], lvls[lvls <= best.points[,mfp]])
    #   colnames(tmp) = colnames(best.points)
    #   best.points = tmp
    # }

    # evaluate the new points (and thus add them to the opt path)
    evalProposedPoints(loop = loop, prop.points = best.points, par.set = par.set2,
      opt.path = opt.path, control = control, fun = fun, show.info = show.info,
      oldopts = oldopts, more.args = more.args, extras = NULL)

    # train the model again with new data
    model = train(learner, task = convertMFOptPathToTask(opt.path))
    
    if (is.null(control$multifid.costs)) {
      time.model = train(time.learner, task = convertMFOptPathToTimeTask(opt.path))
    }

    # increase loop counter and check if done
    loop = loop + 1L
    terminate = shouldTerminate(iters, loop, time.budget, start.time, exec.time.budget, opt.path)
    if (terminate >= 0)
      break
  }
  final.index = chooseFinalPoint(fun, par.set2, model, y.name = "y",
    opt.path = opt.path, control = control)
  proposed = getOptPathEl(opt.path, final.index)
  if (getOption("warn") != 2 && proposed$x$.multifid.lvl != nlvls) {
    # warningf("best y is not on level %i instead of %i!", proposed$.multifid.lvl, nlvls)
  }

  # restore mlr configuration
  configureMlr(on.learner.error = oldopts[["ole"]], show.learner.output = oldopts[["slo"]])

  makeS3Obj("MultiFidResult",
    opt.path = opt.path,
    model = model,
    y = proposed$y,
    x = dropNamed(proposed$x, ".multifid.lvl"),
    plot.data = plot.data
  )
}

# generates initial design for multifid
generateMBOMultiFidDesign = function(par.set, control) {
  design = generateDesign(control$init.design.points, par.set, fun = control$init.design.fun, fun.args = control$init.design.args, trafo = FALSE)
}

# convert OP to a df, of a specified structure with col .multifid.lvl, so we can model on it
convertMFOptPathToDesign = function(opt.path, ...) {
  as.data.frame(opt.path, include.rest = FALSE, discretes.as.factor = TRUE, ...)
}

# convert OP to a mlr task, of a specified structure with col .multifid.lvl, so we can model on it
convertMFOptPathToTask = function(opt.path, ...) {
  d = convertMFOptPathToDesign(opt.path, ...)
  makeRegrTask(id = "multifid.task", data = d, target = "y")
}