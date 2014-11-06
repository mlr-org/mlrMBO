# operates on row-stored-points. but more importanty: can deal with maximization
getDominatedHV = function(points, ref.point, minimize) {
  mults = ifelse(minimize, 1, -1)
  points2 = t(as.matrix(points) %*% diag(mults))
  dominated_hypervolume(points2, ref.point)
}

# returns logical index
isDominated = function(points, minimize) {
  mults = ifelse(minimize, 1, -1)
  points2 = t(as.matrix(points) %*% diag(mults))
  is_dominated(points2)
}

# get subset of point which are non-dominated
getNonDominatedPoints = function(points, minimize) {
  d = isDominated(points, minimize)
  points[!d, , drop = FALSE]
}

# returns  max (or min) of all coords, depending on minimization (or maximization)
getWorstExtremePoint = function(points, minimize) {
  mults = ifelse(minimize, 1, -1)
  apply(as.matrix(points) %*% diag(mults), 2L, min) * mults
}

# return the hypervolume contribution of each elemt (row) of xs (matrix with length
# number.of.targets cols) with respect to the ys (matrix with number.of.target cols)
# ref is the used reference point for hv calculation
# return vector of hypervolume contributions
getHypervolumeContributions = function(xs, ys, ref.point, minimize) {
  hv.old = getDominatedHV(points = ys, ref.point = ref.point, minimize)
  hv.new = apply(xs, 1, function(x)
    getDominatedHV(rbind(ys, x), ref.point, minimize))
  return(hv.new - hv.old)
}

# determines the reference point for multicrit optimization
# Returns reference-point, numeric vector of length number.of.targets
getMultiCritRefPoint = function (ys, control, minimize = control$minimize) {
  switch(control$multicrit.ref.point.method,
    const = control$multicrit.ref.point.val,
    all = getWorstExtremePoint(ys, minimize) + control$multicrit.ref.point.offset,
    front = {
      front = getNonDominatedPoints(ys, minimize)
      getWorstExtremePoint(front, control$minimize) + control$multicrit.ref.point.offset
    }
  )
}

# evaluate an infill crit on multiple models (one per objective in multicrit)
# returns matrix of crit vals, rows = points, cols = crits
evalCritFunForMultiCritModels = function(infill.crit.fun, points, models, control, par.set, design, iter) {
  control2 = control
  crit.vals = asMatrixCols(lapply(seq_along(models), function(i) {
      # we need to make sure mininimize in control is a scalar, so we can multiply it in infill crits...
      control2$minimize = control$minimize[i]
      infill.crit.fun(points, models[[i]], control2, par.set, design, iter)
  }))
  
  # FIXME: This is the second time we have to calc this, we also to it
  # in infillOptNSGA2. could we avoid this?
  
  # We also want to have the hypervolume contribution for every new point
  # But we use a different infill.crit for candidate selection
  # So, calc the second infill.crit, two
  hv.contr.crit = getInfillCritFunction(control$mspot.select.crit)
  candidate.vals = asMatrixCols(lapply(seq_along(models), function(i) {
    # we need to make sure mininimize in control is a scalar, so we can multiply it in infill crits...
    control2$minimize = control$minimize[i]
    hv.contr.crit(points, models[[i]], control2, par.set, design, iter)
  }))
  
  # Now, calc the hv contribution
  ys = design[, control$y.name]
  ref.point = getMultiCritRefPoint(design[, control$y.name], control)
  hv.contr = numeric(nrow(points))
  for (i in seq_row(crit.vals)) {
    hv.contr[i] = getHypervolumeContributions(xs = candidate.vals[i, , drop = FALSE],
      ys = ys, ref.point = ref.point, minimize = control$minimize)
    # add point to ys, since next hv.contr. is with reference to larger front
    ys = rbind(ys, candidate.vals[i, ])
  }
  
  return(cbind(crit.vals, hv.contr))
}

