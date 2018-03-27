# Simple EA that uses operators from emoa functions
# kind of mimics our multi-objective approach, so we can
# compare more honestly.
# See infillOptCMAES.R for interface explanation.
infillOptEA = function(infill.crit, models, control, par.set, opt.path, designs, iter, ...) {
  requirePackages("emoa", why = "infillOptEA")

  # get constants and init shit
  repids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)

  # (mu + lambda) strategy
  mu = control$infill.opt.ea.mu
  lambda = control$infill.opt.ea.lambda
  mutate = emoa::pm_operator(control$infill.opt.ea.pm.eta, control$infill.opt.ea.pm.p,
    getLower(par.set), getUpper(par.set))
  crossover = emoa::sbx_operator(control$infill.opt.ea.sbx.eta, control$infill.opt.ea.sbx.p,
    getLower(par.set), getUpper(par.set))

  best.x = NULL
  best.y = Inf

  for (restart.iter in seq_len(control$infill.opt.restarts)) {

    # random inital population:
    X = generateDesign(mu, par.set, fun = randomLHS)
    y = infill.crit(X, models, control, par.set, designs, iter, ...)

    for (i in seq_len(control$infill.opt.ea.maxit)) {
      # Create new individual (mu + lambda)
      parentSet = replicate(lambda, sample(seq_len(mu), 2))

      # get two kids from crossover, select 1 randomly and mutate
      for (i in seq_col(parentSet)) {
        parents = parentSet[, i]
        child = crossover(t(X[parents, , drop = FALSE]))
        child1 = child[, sample(c(1, 2), 1)]
        child1 = mutate(child1)
        if (hasInteger(par.set)) {
          # convert to list of parameter values (already rounded internally by dfRowToList)
          child1 = dfRowToList(as.data.frame(as.list(child1)), i = 1L, par.set = par.set)
          # possibly the values are out of bounds
          child1 = unlist(repairPoint(par.set, child1))
        }
        # Add new individual:
        X[nrow(X) + 1, ] = child1
        child2 = setColNames(as.data.frame(as.list(child1)), repids)
        y[length(y) + 1] = infill.crit(child2, models, control, par.set, designs, iter, ...)
      }

      # get elements we want to remove from current population
      to.survive = head(order(y), mu)
      X = X[to.survive, , drop = FALSE]
      y = y[to.survive]
    }
    select = getMinIndex(y)
    y1 = y[select]
    x1 = X[select, , drop = FALSE]
    if (y1 < best.y) {
      best.x = x1
      best.y = y1
    }
    rownames(best.x) = NULL
    return(best.x)
  }
}
