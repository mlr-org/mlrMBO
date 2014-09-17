# Calculate distance to nearest neighbor for each point in a set
# @param X [\code{matrix(n, d)}]\cr
#   Matrix of n points of dimension d.
# @return [\code{numeric(n)}]. Distances to nearest neighbor.
distToNN = function(X, ...) {
  d = as.matrix(dist(X))
  diag(d) = Inf
  apply(d, 1, min)
}

# Calculate distance to nearest better neighbor for each point in a set
# @param X [\code{matrix(n, d)}]\cr
#   Matrix of n points of dimension d.
# @param y [\code{matrix(n, d)}]\cr
#   Vector of numerical values for all points.
#   Smaller is better.
# @return [\code{numeric(n)}]. Distances to nearest better neighbor.
distToNB = function(X, y) {
  d = as.matrix(dist(X))
  sapply(seq_col(d), function(i) {
    better = y < y[i]
    #FIXME the emoa wont work with Infs
    if (sum(better) == 0)
      1e50
    else
      min(d[better, i])
  })
}

nds_1d_selection = function(values, n = 1, index = 1, ...) {
  # order according to non-dominated front, then break ties by objective value at index
  # if still tied, break randomly
  ranks = nds_rank(values)
  o = order(ranks, values[index, ], runif(length(ranks)))
  return(tail(o, n))
}

# Implements our new infill criterion which optimizes EI and diversity in X space
#
# Currently only numerical paramaters are handled, for them pm_operator and
# sbx_operator from emoa are used in the EA.
#
proposePointsMOIMBO = function(models, par.set, control, opt.path, iter, ...) {
  requirePackages("emoa", why = "multipointInfillOptMulticrit")

  n = control$propose.points
  objective = control$multipoint.multicrit.objective
  design = convertOptPathToDf(par.set, opt.path, control)
  model = models[[1L]]

  ch = checkFailedModels(models, par.set, n)
  if (!ch$ok) {
    return(ch$prop)
  }

  if (objective == "mean.dist") {
    y.dim = 2
    y.names = c("mean", "dist")
  } else if (objective == "ei.dist") {
    y.dim = 2
    y.names = c("ei", "dist")
  } else if (objective == "mean.se") {
    y.dim = 2
    y.names = c("mean", "se")
  } else if (objective == "mean.se.dist") {
    y.dim = 3
    y.names = c("mean", "se", "dist")
  }

  repids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  d = sum(getParamLengths(par.set))
  mu = n
  # FIXME: what are good defaults?
  mutate = pm_operator(control$multipoint.multicrit.pm.eta, control$multipoint.multicrit.pm.p,
    getLower(par.set), getUpper(par.set))
  crossover = sbx_operator(control$multipoint.multicrit.sbx.eta, control$multipoint.multicrit.sbx.p,
    getLower(par.set), getUpper(par.set))
  mydist = switch(control$multipoint.multicrit.dist,
    nearest.neighbor = distToNN,
    nearest.better = distToNB
  )

  # Random inital population:
  X = generateDesign(mu, par.set, fun = randomLHS)
  Y = matrix(NA, mu, y.dim)
  # mbo infill crits are always minimized
  if (objective == "mean.dist") {
    Y[, 1] = infillCritMeanResponse(X, model, control, par.set, design)
  } else if (objective == "ei.dist") {
    Y[, 1] = infillCritEI(X, model, control, par.set, design)
  } else if (objective %in% c("mean.se", "mean.se.dist")) {
    Y[, 1] = infillCritMeanResponse(X, model, control, par.set, design)
    Y[, 2] = infillCritStandardError(X, model, control, par.set, design)
  }

  # use first Y criterion to for nearest better
  if (objective %in% c("mean.dist", "ei.dist", "mean.se.dist"))
    Y[, y.dim] = -mydist(as.matrix(X), Y[,1])

  for (i in 1:control$multipoint.multicrit.maxit) {
    # Create new individual (mu + 1)
    parents = sample(1:mu, 2)
    # get two kids from CX, sel. 1 randomly, mutate
    child = crossover(t(X[parents, , drop = FALSE]))
    child1 = child[,sample(c(1, 2), 1)]
    child1 = mutate(child1)
    # Add new individual:
    X[nrow(X) + 1,] = child1
    child2 = setColNames(as.data.frame(as.list(child1)), repids)
    # FIXME
    # distanace has potentielly calculated avccording to Q = P + A
    # best try arhoive with design and empthy both...
    Y = rbind(Y, rep(NA, y.dim))
    # mbo infill crits are always minimized
    if (objective == "mean.dist") {
      Y[nrow(Y), 1] = infillCritMeanResponse(child2, model, control, par.set, design)
    } else if (objective == "ei.dist") {
      Y[nrow(Y), 1] = infillCritEI(child2, model, control, par.set, design)
    } else if (objective %in% c("mean.se", "mean.se.dist")) {
      Y[nrow(Y), 1] = infillCritMeanResponse(child2, model, control, par.set, design)
      Y[nrow(Y), 2] = infillCritStandardError(child2, model, control, par.set, design)
    }
    # use first Y criterion to for nearest better
    if (objective %in% c("mean.dist", "ei.dist", "mean.se.dist"))
      Y[, y.dim] = -mydist(as.matrix(X), Y[,1])

    # get elements we want to remove from current pop as index vector
    to.kill = if (control$multipoint.multicrit.selection == "hypervolume") {
      nds_hv_selection(t(Y))
    } else if (control$multipoint.multicrit.selection == "crowdingdist") {
      nds_cd_selection(t(Y))
    } else if (control$multipoint.multicrit.selection == "first") {
      nds_1d_selection(t(Y), index = 1)
    } else if (control$multipoint.multicrit.selection == "last") {
      nds_1d_selection(t(Y), index = y.dim)
    }
    X = X[-to.kill, ,drop = FALSE]
    Y = Y[-to.kill, ,drop = FALSE]
  }
  rownames(X) = NULL
  return(list(prop.points = X, crit.vals = Y, errors.model = NA_character_))
}


