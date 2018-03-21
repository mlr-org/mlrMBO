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
  vnapply(seq_col(d), function(i) {
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
  ranks = emoa::nds_rank(values)
  o = order(ranks, values[index, ], runif(length(ranks)))
  return(tail(o, n))
}

# Implements our new infill criterion which optimizes EI and diversity in X space
#
# Currently only numerical paramaters are handled, for them pm_operator and
# sbx_operator from emoa are used in the EA.
#
proposePointsMOIMBO = function(opt.state, ...) {

  opt.problem = getOptStateOptProblem(opt.state)
  models = getOptStateModels(opt.state)$models
  par.set = getOptProblemParSet(opt.problem)
  control = getOptProblemControl(opt.problem)
  opt.path = getOptStateOptPath(opt.state)

  requirePackages("emoa", why = "multi-point InfillOpt MOI-MBO")

  n = control$propose.points
  objective = control$multipoint.moimbo.objective
  design = convertOptPathToDf(opt.path, control)

  ch = checkFailedModels(models, par.set, n)
  if (!ch$ok) {
    return(ch$prop)
  }

  y.dim = if (objective == "mean.se.dist") 3L else 2L
  repids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  mu = n
  # FIXME: what are good defaults?
  mutate =  emoa::pm_operator(control$multipoint.moimbo.pm.eta, control$multipoint.moimbo.pm.p,
    getLower(par.set), getUpper(par.set))
  crossover = emoa::sbx_operator(control$multipoint.moimbo.sbx.eta, control$multipoint.moimbo.sbx.p,
    getLower(par.set), getUpper(par.set))
  mydist = switch(control$multipoint.moimbo.dist,
    nearest.neighbor = distToNN,
    nearest.better = distToNB
  )

  # Random inital population:
  X = generateDesign(mu, par.set, fun = randomLHS)
  Y = matrix(NA, mu, y.dim)
  infill.mean = makeMBOInfillCritMeanResponse()$fun
  infill.se = makeMBOInfillCritStandardError()$fun
  infill.ei = makeMBOInfillCritEI()$fun
  # mbo infill crits are always minimized
  if (objective == "mean.dist") {
    Y[, 1] = infill.mean(X, models, control, par.set, design)
  } else if (objective == "ei.dist") {
    Y[, 1] = infill.ei(X, models, control, par.set, design)
  } else if (objective %in% c("mean.se", "mean.se.dist")) {
    Y[, 1] = infill.mean(X, models, control, par.set, design)
    Y[, 2] = infill.se(X, models, control, par.set, design)
  }

  # use first Y criterion to for nearest better
  if (objective %in% c("mean.dist", "ei.dist", "mean.se.dist"))
    Y[, y.dim] = -mydist(as.matrix(X), Y[,1])

  secs = measureTime({
    for (i in seq_len(control$multipoint.moimbo.maxit)) {
      # Create new individual (mu + 1)
      parents = sample(seq_len(mu), 2)
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
        Y[nrow(Y), 1] = infill.mean(child2, models, control, par.set, design)
      } else if (objective == "ei.dist") {
        Y[nrow(Y), 1] = infill.ei(child2, models, control, par.set, design)
      } else if (objective %in% c("mean.se", "mean.se.dist")) {
        Y[nrow(Y), 1] = infill.mean(child2, models, control, par.set, design)
        Y[nrow(Y), 2] = infill.se(child2, models, control, par.set, design)
      }
      # use first Y criterion to for nearest better
      if (objective %in% c("mean.dist", "ei.dist", "mean.se.dist"))
        Y[, y.dim] = -mydist(as.matrix(X), Y[,1])

      # get elements we want to remove from current pop as index vector
      to.kill = if (control$multipoint.moimbo.selection == "hypervolume") {
        emoa::nds_hv_selection(t(Y))
      } else if (control$multipoint.moimbo.selection == "crowdingdist") {
        emoa::nds_cd_selection(t(Y))
      } else if (control$multipoint.moimbo.selection == "first") {
        nds_1d_selection(t(Y), index = 1)
      } else if (control$multipoint.moimbo.selection == "last") {
        nds_1d_selection(t(Y), index = y.dim)
      }
      X = X[-to.kill, ,drop = FALSE]
      Y = Y[-to.kill, ,drop = FALSE]
    }
  }) # measureTime
  rownames(X) = NULL

  makeProposal(
    control = control,
    prop.points = X,
    propose.time = secs,
    crit.vals = Y,
    prop.type = rep("infill_moimbo", n)
  )
}
