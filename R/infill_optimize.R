# Optimizers for infill criteria

# General interface
#
# @param infill.crit [\code{function}]\cr
#   Infill criterion function.
# @param design [\code{data.frame}]\cr
#   Design of already visited points.
# @param model [\code{\link{WrappedModel}}]\cr
#   Model fitted on design.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param opt.path [\code{\link[ParamHelpers{OptPath}}]\cr
#   Optimization path / archive.
# @return [\code{data.frame}]. One proposed point that should be evaluated.

#FIXME we need other optimizers for mixed, depenent param spaces. dont forget refactorNAS then

# random search, where we shrink the region of interest after restarts
# around the currently best point. only numeric / ints are currently "shrunken"
# works for ALL parameter sets

#FIXME it would be nice to have a REASONABLE way to shrink categorical stuff too.

infillOptRandom = function(infill.crit, model, control, par.set, opt.path, design) {
  iterater = 0L
  repeat {
    iterater = iterater + 1L
    newdesign1 = generateDesign(control$infill.opt.random.points, par.set,
      randomLHS, ints.as.num=TRUE, logicals.as.factor=TRUE)
    # predict on design where NAs were imputed, but return propsed points with NAs
    newdesign2 = refactorNAs(newdesign1, par.set)
    y = infill.crit(newdesign2, model, control, par.set, design)
    best = newdesign1[rank(y, ties.method="random") == 1L, , drop=FALSE]

    if (iterater == control$infill.opt.restarts)
      break

    # now shrink par.set object so we search more locally, then iterate
    for (i in seq_along(par.set$pars)) {
      par = par.set$pars[[i]]
      if (par$type %in% c("numeric", "integer", "numericvector", "integervector")) {
        range = par$upper - par$lower
        if (best[[i]] < par$lower + range / 4)
          par$upper = par$upper - range / 2
        else if (best[[i]] > par$upper - range / 4)
          par$lower = par$lower + range / 2
        else {
          par$lower = par$lower + range / 4
          par$upper = par$upper - range / 4
        }
        par.set$pars[[i]] = par
      }
    }
  }
  # convert back to correct type
  types = unlist(lapply(par.set$pars, function(p) rep(p$type, p$len)))
  for (i in seq_col(best)) {
    if (types[i] %in% c("integer", "integervector"))
      best[,i] = as.integer(best[,i])
    else if (types[i] %in% c("logical", "logicalvector"))
      best[,i] = as.logical(as.character(best[,i]))
  }
  best
}

# cmaes with simple random restarts.
# the first start is always at the best point of the current opt.path.
# works only for numerics and integers, latter are simply rounded.

infillOptCMAES = function(infill.crit, model, control, par.set, opt.path, design) {
  # extract lower and upper bound for params
  low = getLower(par.set)
  upp = getUpper(par.set)

  rep.pids = getParamIds(par.set, repeated=TRUE, with.nr=TRUE)
  #eval all points of 1 generation at once
  cmaes.control = control$cmaes.control
  cmaes.control$vectorized = TRUE
  f = function(x) {
    newdata = as.data.frame(t(x))
    colnames(newdata) = rep.pids
    infill.crit(newdata, model, control, par.set, design)
  }

  results = vector("list", control$infill.opt.restarts)
  # restart optimizer, first start point is currently best
  for (i in 1:control$infill.opt.restarts) {
    if (i == 1) {
      start = getOptPathEl(opt.path, getOptPathBestIndex(opt.path))$x
    } else {
      start = sampleValue(par.set)
    }
    start = unlist(start)
    results[[i]] = cma_es(par=start, fn=f, lower=low, upper=upp, control=control$infill.opt.cmaes.control)
  }
  ys = extractSubList(results, "value")
  j = which(rank(ys, ties.method="random") == 1L)
  as.data.frame(t(results[[j]]$par))
}

# FIXME: allow DiceOptim optimizer later...
# infillOptEI = function(infill.crit, model, control, par.set, opt.path) {
#   # extract lower and upper bound for params
#   low = getLower(par.set)
#   upp = getUpper(par.set)
#
#   i = getOptPathBestIndex(opt.path, ties="random")
#   start = unlist(getOptPathEl(opt.path, i)$x)
#   capture.output(design <- max_EI(model$learner.model,
#     lower=low, upper=upp, parinit=start)$par)
#   as.data.frame(design)
# }


# simple ES that uses operators from emao functions
# kind of mimics our multicrit approach, so we can
# compare more honestly

#FIXME potentially allow more than 1 kid
#FIXME check what this optimizer can do in checkStuff and allow rounding of integers
infillOptSimpleES = function(infill.crit, model, control, par.set, opt.path, design) {
  requirePackages("emoa", why="infillOptSimpleES")

  # get constants and init shit
  repids = getParamIds(par.set, repeated=TRUE, with.nr = TRUE)
  d = sum(getParamLengths(par.set))
  mu = control$infill.opt.es.mu
  mutate = pm_operator(control$infill.opt.es.eta, control$infill.opt.es.p,
    getLower(par.set), getUpper(par.set))
  crossover = sbx_operator(control$infill.opt.es.eta, control$infill.opt.es.p,
    getLower(par.set), getUpper(par.set))

  best.x = NULL
  best.y = Inf

  for (restart.iter in 1:control$infill.opt.restarts) {

    # random inital population:
    X = generateDesign(mu, par.set, fun=randomLHS)
    y = infill.crit(X, model, control, par.set, design)

    for (i in 1:control$infill.opt.es.maxit) {
      # Create new individual (mu + 1)
      parents = sample(1:mu, 2)
      # get two kids from CX, sel. 1 randomly, mutate
      child = crossover(t(X[parents, , drop=FALSE]))
      child1 = child[,sample(c(1, 2), 1)]
      child1 = mutate(child1)
      # Add new individual:
      X[nrow(X) + 1,] = child1
      child2 = setColNames(as.data.frame(as.list(child1)), repids)
      y[length(y) + 1] = infill.crit(child2, model, control, par.set, design)

      # get element we want to remove from current pop
      to.kill = getMaxIndex(y, ties.method="random")
      X = X[-to.kill, ,drop=FALSE]
      y = y[-to.kill]
    }
    select = getMinIndex(y)
    y1 = y[select] 
    x1 = X[select, , drop=FALSE]
    if (y1 < best.y) {
      best.x = x1
      best.y = y1
    }
    rownames(best.x) = NULL
    return(best.x)
  }
}




