generateMBOMultiFidDesign = function(par.set, control){
  budget = control$init.design.points
  k = length(par.set$pars[[control$multiFid.control$fid.param]]$values)
  b = ceiling(budget / k)
  ns = viapply(chunk(seq_len(budget), n.chunks=k), length)
  design = generateDesign(max(ns), par.set, fun = control$init.design.fun,
                          fun.args = control$init.design.args, trafo = FALSE)
  designs = lapply(seq_along(ns), function(i) {
    des = design[seq_len(ns[i]),, drop=FALSE]
    des[[control$multiFid.control$fid.param]] = par.set$pars[[control$multiFid.control$fid.param]]$values[[i]]
    des
  })
  res = do.call(rbind.data.frame, designs)
  res[[control$multiFid.control$fid.param]] = factor(res[[control$multiFid.control$fid.param]]) #FIXME: Not the BEST
  res
}

#' Generates the control object for the multiFid infill criteria
#' 
#' @param fid.param [\code{character(1)}]\cr
#'   The name of the parameter which increases the performance but also calculation costs. Has to belong to a discrete Parameter.
#' @param cor.grid.points [\code{integer(1)}]\cr
#'   Numbers of points used to calculate the correlation between the different levels of the \code{multiFid.fid.param}.
#' @param costs [\code{function}]\cr
#'   Vektorized (?) cost function with the params \code{cur} and \code{last}.
#' @param force.last.level.evals [\code{integer(1)}]
#'   How many evaluations should be done on the last value of fid.param.
#' @return \code{MBOMultiFidControl}
#' @export
makeMBOMultiFidControl = function(fid.param, cor.grid.points = 10L, costs = NULL, force.last.level.evals = 10L){
  assertCharacter(fid.param, len = 1L)
  assertInt(cor.grid.points)
  if (is.null(costs)) {
    costs = function(cur, last) (last / cur)^2
  } else {
    assertFunction(costs, args = c("cur", "last"))
  }
  assertInt(force.last.level.evals)
  structure(
    list(
      fid.param = fid.param,
      cor.grid.points = cor.grid.points,
      costs = costs,
      force.last.level.evals = force.last.level.evals
    ), class = "MBOMultiFidControl")
}


# convert opt path to a data.frame, maybe drop technical columns so we only have features and y
convertOptPathToDesign = function(opt.path, drop=TRUE) {
  d = as.data.frame(opt.path, discretes.as.factor = TRUE)
  drop.names = setdiff(colnames(d), c(opt.path$y.names, names(opt.path$par.set$pars)))
  if (drop)
    dropNamed(d, drop.names)
  else
    d
}

convertOptPathToTask = function(opt.path, control, drop=TRUE) {
  d = convertOptPathToDesign(opt.path)
  makeRegrTask(id="surrogate", data=d, target="y")
}

# return only crit vector
infillCritMultiFid = function(points, model, control, par.set, design, model.cor, model.sd, model.cost, ...) {
  infillCritMultiFid2(points, model, control, par.set, design, model.cor, model.sd, model.cost, ...)$crit
}

# return all crap so we can plot it later
infillCritMultiFid2 = function(points, model, control, par.set, design, model.cor, model.sd, model.cost, ...) {
  # note: mbo returns the negated EI (and SE), so have to later minimize the huang crit.
  # which is done by default by our optimizer anyway
  ei.last = infillCritEI(points=points, model=model, control=control, par.set=par.set, design=design)
  alpha1 = replaceByList(points[[control$multiFid.control$fid.param]], model.cor)
  se = -infillCritStandardError(points=points, model=model, control=control, par.set=par.set, design=design)
  # FIXME: do we really have to adapt this? alpha2 should be 0 when?
  model.sd.vec = replaceByList(points[[control$multiFid.control$fid.param]], model.sd)
  alpha2 = 1 - (sqrt(2) * model.sd.vec / sqrt(se^2 + model.sd.vec^2))
  alpha3 = replaceByList(points[[control$multiFid.control$fid.param]], model.cost)
  crit = ei.last * alpha1 * alpha2 * alpha3
  list(crit=crit, ei=ei.last, se=se, alpha1=alpha1, alpha2=alpha2, alpha3=alpha3)
}

replaceByList = function(x, rep.list){
  x = as.character(x)
  res = rep(rep.list[[1]], times = length(x)) #res gets the right type
  for(key in names(rep.list)) {
    res[x == key] = rep.list[[key]]
  }
  res
}
