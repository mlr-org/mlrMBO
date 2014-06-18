#  Optimizes a multicrit optimization problem  with sequential model based
#  optimization using the parEGO algorithm
#
# @param fun [\code{function(x, ...)}]\cr
#   Fitness functions to minimize. The first argument has to be a list of values.
#   The function has to return a numerical vector of the length defined via the
#   parameter number.of.targets in \code{control}.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Collection of parameters and their constraints for optimization.
# @param design [\code{data.frame} | NULL]\cr
#   One of this 3:
#   - Initial design as data frame.
#     If the parameters have corresponding trafo functions,
#     the design must not be transformed before it is passed!
#   - A opt.path object:
#     The design and all saved infos will be extracted from this
#   - \code{NULL}:
#     The design is constructed from the settings in \code{control}.
# @param learner [\code{\link[mlr]{Learner}}]\cr
#   Regression learner to model \code{fun}.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object for mbo.
# @param show.info [\code{logical(1)}]\cr
#   Verbose output on console?
#   Default is \code{TRUE}.
# @param more.args [any]\cr
#   Further arguments passed to fitness function.
# @param perf.param [\code{character(1)}]\cr
#   Necessary if \code{infill.crit = "multiFid"}. The name of the parameter which increases the performance but also calculation costs. Has to belong to a discrete Parameter.
# @param cor.grid.points [\code{integer(1)}]\cr
#   Numbers of points used to calculate the correlation between the different levels of the \code{multiFid.perf.param}.
# @return [\code{list}]:
#   \item{pareto.front [\code{matrix}]}{Pareto Front of all evaluated points.}
#   \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
# FIXME EGO nach ego
mboMultiFid = function(fun, par.set, design=NULL, learner, control, show.info=TRUE, perf.param, cor.grid.points, more.args=list()) {
  # save currently set options
  oldopts = list(
    ole = getOption("mlr.on.learner.error"),
    slo = getOption("mlr.show.learner.output")
  )
  
  if (is.null(control$multiFid.control$costs)) {
    costs = function(cur, last) (last / cur)^2
  } else {
    costs = control$multiFid.control$costs
  }
  
  # generate initial design
  # FIXME mbo.design
  if(is.null(design)) {
    design = generateMBOMultiFidDesign(par.set, control)
  }
  opt.path = makeOptPathDF(par.set, control$y.name, control$minimize,
                           # include.error.message = control$do.impute,
                           # FIXME HOW TO DECIDE?
                           include.error.message = FALSE,
                           include.exec.time = TRUE)
  mbo.design = generateMBODesign(design=design, fun=fun, par.set=par.set, opt.path=opt.path, control=control, show.info=show.info, oldopts=oldopts, more.args=more.args)
  
  times = mbo.design$times
  
  mf.learner = makeMultiFidLearner(surrogat.learner = learner, par.set = par.set, perf.param.id = control$multiFid.control$perf.param)
  compound.model = train.MultiFidLearner(obj = mf.learner, task = convertOptPathToTask(opt.path))
  
  budget = control$iters
  
  # local needed functions
  # calculate cost relation between model w/ par.val and last model
  calcModelCost = function(par.val) {
    costs(par.val, unlist(tail(par.set$pars[[control$multiFid.control$perf.param]]$values, 1L)))
  }
  
  #subsets a data.frame to a given value of the perf.param
  subsetOnPar = function(data, par.val){
    data = subset(data, data[[control$multiFid.control$perf.param]] == par.val)
  }
  
  # estimate process noise tau of real process belonging to par.val, we use residual sd here
  calcModelSD = function(par.val) {
    newdata = convertOptPathToDesign(opt.path)
    newdata = subsetOnPar(newdata, par.val)
    newdata[[control$multiFid.control$perf.param]] = factor(newdata[[control$multiFid.control$perf.param]])
    sqrt(estimateResidualVariance(compound.model, data = newdata, target = "y"))
  }
  
  # calculate GLOBAL correlation between model w/ par.val and last model. currently rank correlation.
  calcModelCor = function(par.val, grid) {
    grid1 = grid; grid1[[control$multiFid.control$perf.param]] = par.val
    grid2 = grid; grid2[[control$multiFid.control$perf.param]] = unlist(tail(par.set$pars[[control$multiFid.control$perf.param]]$values, 1L))
    p1 = predict(compound.model, newdata=grid1)$data$response
    p2 = predict(compound.model, newdata=grid2)$data$response
    # check whether vectors are constant, cor = NA then
    if (diff(range(p1)) < sqrt(.Machine$double.eps) || diff(range(p2)) < sqrt(.Machine$double.eps))
      0
    else
      max(cor(p1, p2, method="spearman"), 0)
  }
  
  par.set.lower = par.set; par.set.lower$pars = Filter(function(p) p$id %nin% control$multiFid.control$perf.param, par.set$pars)
  corgrid = generateDesign(n=control$multiFid.control$cor.grid.points, par.set=par.set.lower)
  
  # do the mbo magic
  for(loop in seq_len(budget)) {
    if(show.info)
      messagef("loop=%i", loop)
    
    # evaluate stuff we need for MEI
    models.sd = vnapply(par.set$pars[[control$multiFid.control$perf.param]]$values, calcModelSD)
    models.cor = vnapply(par.set$pars[[control$multiFid.control$perf.param]]$values, calcModelCor, grid=corgrid)
    models.cost = vnapply(par.set$pars[[control$multiFid.control$perf.param]]$values, calcModelCost)
    messagef("Estimated cor to last model = %s", collapse(sprintf("%.3g", models.cor), ", "))
    messagef("Estimated residual var = %s", collapse(sprintf("%.3g", models.sd), ", "))
    
    # every couple of levels we only optimize the last one
    # to ensure that we update that model and see what happens here
    if (loop %% control$multiFid.control$force.last.level.evals == 0)
      avail.pars = tail(par.set$pars[[control$multiFid.control$perf.param]]$values, 1L)
    else
      avail.pars = par.set$pars[[control$multiFid.control$perf.param]]$values
    
    prop = proposePoints(model = compound.model, par.set = par.set, control = control, opt.path = opt.path, model.cor = models.cor, model.cost = models.cost, model.sd = models.sd)
    #xs = dfRowsToList(prop$prop.points, par.set)
    evalProposedPoints(loop = loop, prop.points = prop$prop.points, par.set = par.set, opt.path = opt.path, control = control, fun = objfun, show.info = show.info, oldopts = oldopts, more.args = more.args, extras = NULL)
    #evals = evalTargetFun(fun = objfun, par.set = par.set, dobs = loop, xs = xs, opt.path = opt.path, control = control, show.info = show.info, oldopts = oldopts, more.args = more.args, extras = NULL)
    compound.model = train.MultiFidModel(compound.model, task = convertOptPathToTask(opt.path))
  }
  
  # return complete designs for all levels
  proposed.index = chooseFinalPoint(NULL, par.set, compound.model, y.name = "y",
                                    opt.path = opt.path, control = control)
  proposed = convertOptPathToDesign(opt.path, drop = TRUE)[proposed.index, ]
  proposed$y = NULL
  proposed = convertDfCols(proposed, factors.as.char = TRUE)
  y.hat = predict(compound.model, newdata = proposed)$data$response
  
  # restore mlr configuration
  configureMlr(on.learner.error=oldopts[["ole"]], show.learner.output=oldopts[["slo"]])
  
  makeS3Obj("MultiFidResult",
    opt.path = opt.path,
    model = compound.model,
    y.hat = y.hat,
    proposed = proposed
  )
}