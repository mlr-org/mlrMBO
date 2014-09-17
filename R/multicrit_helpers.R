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

