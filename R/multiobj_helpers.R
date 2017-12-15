# operates on row-stored-points. but more importanty: can deal with maximization
getDominatedHV = function(points, ref.point, minimize) {
  mults = ifelse(minimize, 1, -1)
  points2 = t(as.matrix(points) %*% diag(mults))
  emoa::dominated_hypervolume(points2, ref.point)
}

# returns logical index
isDominated = function(points, minimize) {
  mults = ifelse(minimize, 1, -1)
  points2 = t(as.matrix(points) %*% diag(mults))
  emoa::is_dominated(points2)
}

# get subset of point which are non-dominated
getNonDominatedPoints = function(points, minimize) {
  d = isDominated(points, minimize)
  points[!d, , drop = FALSE]
}

# returns  max (or min) of all coords, depending on minimization (or maximization)
getWorstExtremePoint = function(points, minimize) {
  mults = ifelse(minimize, 1, -1)
  apply(as.matrix(points) %*% diag(mults), 2L, max) * mults
}

# return the hypervolume contribution of each elemt (row) of xs (matrix with length
# n.objectives cols) with respect to the ys (matrix with number.of.target cols)
# ref is the used reference point for hv calculation
# return vector of hypervolume contributions
getHypervolumeContributions = function(xs, ys, ref.point, minimize) {
  hv.old = getDominatedHV(points = ys, ref.point = ref.point, minimize)
  hv.new = apply(xs, 1, function(x)
    getDominatedHV(rbind(ys, x), ref.point, minimize))
  return(hv.new - hv.old)
}

# determines the reference point for multi-objective optimization
# Returns reference-point, numeric vector of length n.objectives
getMultiObjRefPoint = function (ys, control, minimize = control$minimize) {
  switch(control$multiobj.ref.point.method,
    const = control$multiobj.ref.point.val,
    all = getWorstExtremePoint(ys, minimize) + control$multiobj.ref.point.offset,
    front = {
      front = getNonDominatedPoints(ys, minimize)
      getWorstExtremePoint(front, control$minimize) + control$multiobj.ref.point.offset
    }
  )
}

# evaluate an infill crit on multiple models (one per objective in multi-objective)
# returns matrix of crit vals, rows = points, cols = crits
evalCritFunForMultiObjModels = function(infill.crit.fun, points, models, control, par.set, designs, iter) {
  asMatrixCols(lapply(seq_along(models), function(i) {
      # we need to make sure mininimize in control is a scalar, so we can multiply it in infill crits...
      control$minimize = control$minimize[i]
      control$y.name = control$y.name[i]
      infill.crit.fun(points, models[i], control, par.set, designs[i], iter)
  }))
}
