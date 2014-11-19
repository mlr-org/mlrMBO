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

  # some short names and data extractions
  mfp = control$multifid.param
  iters = control$iters
  lvls = control$multifid.lvls
  nlvls = length(lvls)
  # extra param set, with int param for lvl
  par.set2 = c(par.set, makeParamSet(
    makeIntegerParam(".multifid.lvl", lower = 1L, upper = nlvls)))

  # init design optpath, we store lvl there as well
  design = generateMBOMultiFidDesign(par.set, control)
  opt.path = makeOptPathDF(par.set2, control$y.name, control$minimize,
    include.error.message = TRUE, include.exec.time = TRUE)
  # eval + log to opt.path
  xs = dfRowsToList(design, par.set2)
  evalTargetFun(fun, par.set2, 0L, xs, opt.path, control, show.info, oldopts, more.args, extras = NULL)

  # contruct multifid learner, with summed model and bootstrapping
  # FIXME: we need to exactly define how bootstrapping should work for multifid
  learner = makeMultiFidWrapper(learner, control)
  learner = makeMultiFidBaggingWrapper(learner)
  learner = setPredictType(learner, predict.type = "se")

  task = convertOptPathToTask(opt.path, control = control)
  print(task)

  # now fit to init design
  model = train(learner, task = convertOptPathToTask(opt.path, control = control))

  ##### LOCAL FUNCTIONS FOR MEI CRIT VALUES: START #####

  # calculate cost relation between model w/ par.val and last model
  calcModelCost = function(par.val) {
    control$multifid.costs(par.val, last.lvl)
  }

  # estimate process noise tau of real process belonging to par.val, we use residual sd here
  calcModelSD = function(par.val) {
    newdata = convertOptPathToDesign(opt.path)
    newdata = newdata[newdata[[mfp]] == par.val, ]
    sqrt(estimateResidualVariance(model, data = newdata, target = "y"))
  }

  # calculate GLOBAL correlation between model w/ par.val and last model. currently rank correlation.
  calcModelCor = function(par.val, grid) {
    grid1 = grid; grid1[[mfp]] = par.val
    grid2 = grid; grid2[[mfp]] = last.lvl
    p1 = predict(model, newdata = grid1)$data$response
    p2 = predict(model, newdata = grid2)$data$response
    # check whether vectors are constant, cor = NA then
    if (diff(range(p1)) < sqrt(.Machine$double.eps) || diff(range(p2)) < sqrt(.Machine$double.eps))
      0
    else
      max(cor(p1, p2, method = "spearman"), 0)
  }

  ##### LOCAL FUNCTIONS FOR MEI CRIT VALUES: END #####

  # generate the x values we want to use to calculate the correlation between the surrogat models
  corgrid = generateDesign(n = control$multifid.cor.grid.points, par.set = dropParams(par.set, mfp))

  plot.data = list()

  # multifid main iteration
  for (loop in seq_len(iters)) {
    showInfo(show.info, "loop = %i", loop)

    # evaluate numbers we need for MEI (all vectors with length = #levels)
    model.sds = vnapply(lvls, calcModelSD)
    model.cors = vnapply(lvls, calcModelCor, grid = corgrid)
    model.costs = vnapply(lvls, calcModelCost)

    showInfo(show.info, "Estimated cor to last model = %s", collapse(sprintf("%.3g", model.cors), ", "))
    showInfo(show.info, "Estimated residual var = %s", collapse(sprintf("%.3g", model.sds), ", "))

    # every couple of levels we only optimize the last one
    # to ensure that we update that model and see what happens here
    control.mod = control
    if (loop %% control$multifid.force.last.level.evals == 0)
      control.mod$multifid.lvls = tail(control.mod$multifid.lvls, 1L)

    # return a list, get a proposed point for each level
    prop = proposePointsMultiFid(model = model, par.set = par.set,
      control = control.mod, opt.path = opt.path,
      model.cor = model.cor, model.cost = model.cost, model.sd = model.sd)
    # find the level where the crit val / infill vals is smallest
    infill.vals = extractSubList(prop, "crit.vals")
    messagef("Infill vals = %s", collapse(sprintf("%.3g", infill.vals), ", "))
    # get one x point (see as par vector) and the level parameter
    best.points = prop[[getMinIndex(infill.vals)]]$prop.points

    # only generate plot data, if we are in a 1D case
    if (length(dropParams(par.set, mfp)$pars) == 1) {
      plot.data[[loop]] = genPlotData(model = model, par.set = par.set,
        control = control, fun = fun, opt.path = opt.path,
        model.cor = model.cor, model.sd = model.sd,
        model.cost = model.cost, best.points = best.points)
    }

    # should we always also update the models of the lower levels. Fixes some theoretical problems.
    if (control$multifid.eval.lower) {
      tmp = data.frame(best.points[1,1], lvls[lvls <= best.points[,mfp]])
      colnames(tmp) = colnames(best.points)
      best.points = tmp
    }
    # evaluate the new points (and thus add them to the opt path)
    evalProposedPoints(loop = loop, prop.points = best.points, par.set = par.set,
      opt.path = opt.path, control = control, fun = fun, show.info = show.info,
      oldopts = oldopts, more.args = more.args, extras = NULL)

    # train the model again with new data
    model = train(learner, task = convertOptPathToTask(opt.path, control = control))
  }

  # return complete designs for all levels
  proposed.index = chooseFinalPoint(NULL, par.set, model, y.name = "y",
    opt.path = opt.path, control = control)
  proposed = convertOptPathToDesign(opt.path, drop = TRUE)[proposed.index, ]
  proposed$y = NULL
  proposed[,mfp] = last.lvl
  proposed = convertDfCols(proposed, factors.as.char = TRUE)
  y.hat = predict(model, newdata = proposed)$data$response
  y = evalProposedPoints(loop = iters+1, prop.points = proposed, par.set = par.set, opt.path = opt.path,
    control = control, fun = fun, show.info = show.info, oldopts = oldopts,
    more.args = more.args, extras = NULL)

  makeS3Obj("MultiFidResult",
    opt.path = opt.path,
    model = model,
    y.hat = y.hat,
    y = y,
    x = as.list(proposed[, colnames(proposed) %nin% mfp, drop = FALSE]),
    plot.data = plot.data
  )
}
