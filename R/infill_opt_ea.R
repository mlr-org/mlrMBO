# simple EA that uses operators from emao functions
# kind of mimics our multicrit approach, so we can
# compare more honestly

#FIXME potentially allow more than 1 kid
#FIXME check what this optimizer can do in checkStuff and allow rounding of integers
infillOptEA = function(infill.crit, model, control, par.set, opt.path, design) {
  requirePackages("emoa", why="infillOptEA")

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



