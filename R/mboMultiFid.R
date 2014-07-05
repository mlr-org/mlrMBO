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
# @param fid.param [\code{character(1)}]\cr
#   Necessary if \code{infill.crit = "multiFid"}. The name of the parameter which increases the performance but also calculation costs. Has to belong to a discrete Parameter.
# @param cor.grid.points [\code{integer(1)}]\cr
#   Numbers of points used to calculate the correlation between the different levels of the \code{multiFid.fid.param}.
# @return [\code{list}]:
#   \item{pareto.front [\code{matrix}]}{Pareto Front of all evaluated points.}
#   \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
# FIXME EGO nach ego
mboMultiFid = function(fun, par.set, design=NULL, learner, control, show.info=TRUE, more.args=list()) {
  # save currently set options
  oldopts = list(
    ole = getOption("mlr.on.learner.error"),
    slo = getOption("mlr.show.learner.output")
  )
  
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
  
  mf.learner = makeMultiFidLearner(surrogat.learner = learner, par.set = par.set, control = control)
  compound.model = train.MultiFidLearner(obj = mf.learner, task = convertOptPathToTask(opt.path))
  
  budget = control$iters
  
  # local needed functions
  
  #subsets a data.frame to a given value of the fid.param
  subsetOnPar = function(data, par.val){
    data = subset(data, data[[control$multifid.param]] == par.val)
  }
  
  # calculate cost relation between model w/ par.val and last model
  calcModelCost = function(par.val) {
    control$multifid.costs(par.val, tail(control$multifid.lvls, 1))
  }
  
  # estimate process noise tau of real process belonging to par.val, we use residual sd here
  calcModelSD = function(par.val) {
    newdata = convertOptPathToDesign(opt.path)
    newdata = subsetOnPar(newdata, par.val)
    sqrt(estimateResidualVariance(compound.model, data = newdata, target = "y"))
  }
  
  # calculate GLOBAL correlation between model w/ par.val and last model. currently rank correlation.
  calcModelCor = function(par.val, grid) {
    grid1 = grid; grid1[[control$multifid.param]] = par.val
    grid2 = grid; grid2[[control$multifid.param]] = tail(control$multifid.lvls, 1)
    p1 = predict(compound.model, newdata=grid1)$data$response
    p2 = predict(compound.model, newdata=grid2)$data$response
    # check whether vectors are constant, cor = NA then
    if (diff(range(p1)) < sqrt(.Machine$double.eps) || diff(range(p2)) < sqrt(.Machine$double.eps))
      0
    else
      max(cor(p1, p2, method="spearman"), 0)
  }
  
  corgrid = generateDesign(n=control$multifid.cor.grid.points, par.set=parSetWithout(par.set, control$multifid.param))
  
  plot.data = list()
  
  # do the mbo magic
  for(loop in seq_len(budget)) {
    if(show.info)
      messagef("loop=%i", loop)
    
    # evaluate stuff we need for MEI
    model.sd = vnapply(control$multifid.lvls, calcModelSD)
    names(model.sd) = control$multifid.lvls
    model.cor = vnapply(control$multifid.lvls, calcModelCor, grid=corgrid)
    names(model.cor) = control$multifid.lvls
    model.cost = vnapply(control$multifid.lvls, calcModelCost)
    names(model.cost) = control$multifid.lvls
    messagef("Estimated cor to last model = %s", collapse(sprintf("%.3g", model.cor), ", "))
    messagef("Estimated residual var = %s", collapse(sprintf("%.3g", model.sd), ", "))
    
    # every couple of levels we only optimize the last one
    # to ensure that we update that model and see what happens here
    if (loop %% control$multifid.force.last.level.evals == 0)
      avail.pars = tail(control$multifid.lvls, 1L)
    else
      avail.pars = control$multifid.lvls
    
    prop = proposePoints.MultiFid(model = compound.model, par.set = par.set, control = control, opt.path = opt.path, model.cor = model.cor, model.cost = model.cost, model.sd = model.sd)
    infill.vals = extractSubList(prop, "crit.vals")
    messagef("Infill vals = %s", collapse(sprintf("%.3g", infill.vals), ", "))
    # we still technically minimize
    best.points = prop[[getMinIndex(infill.vals)]]$prop.points
    
    plot.data[[loop]] = genPlotData(compound.model = compound.model, par.set = par.set, control = control, fun = objfun, opt.path = opt.path, model.cor = model.cor, model.sd = model.sd, model.cost = model.cost, best.points = best.points)
    
    evalProposedPoints(loop = loop, prop.points = best.points, par.set = par.set, opt.path = opt.path, control = control, fun = objfun, show.info = show.info, oldopts = oldopts, more.args = more.args, extras = NULL)
    #evals = evalTargetFun(fun = objfun, par.set = par.set, dobs = loop, xs = xs, opt.path = opt.path, control = control, show.info = show.info, oldopts = oldopts, more.args = more.args, extras = NULL)
    compound.model = update.MultiFidModel(compound.model, task = convertOptPathToTask(opt.path))
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
            proposed = proposed,
            plot.data = plot.data
  )
}