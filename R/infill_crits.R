# Infill criteria.
# Used to select update/infill points to increase (global) model accuracy.
# CONVENTION: INFILL CRITERIA ARE ALWAYS MINIMIZED. SO A FEW BELOW ARE NEGATED VERSIONS!

# Please stick to the following general interface.
#
# @param points [data.frame]r
#  n points where to evaluate.
# @param models [WrappedModel or listy<WM>]\cr
#   Model(s) fitted on design.
# @param control [MBOControl]
#   Control object.
# @param par.set [ParamSet]r
#   Parameter set.
# @param design [data.frame]\cr
#   Design of already visited points.
# @param iter [integer(1)]
#   Current iteration
# @return [\code{numeric(n)}].
#   Criterion values at points.

# MEAN RESPONSE OF MODEL
# (useful for deterministic and noisy)
infillCritMeanResponse = function(points, model, control, par.set, design, iter) {
  ifelse(control$minimize, 1, -1) * predict(model, newdata = points)$data$response
}

# MODEL UNCERTAINTY
# (on its own not really useful for anything I suppose ...)
infillCritStandardError = function(points, model, control, par.set, design, iter) {
  -predict(model, newdata = points)$data$se
}

# EXPECTED IMPROVEMENT
# (useful for deterministic, for noisy only with reinterpolation)
infillCritEI = function(points, model, control, par.set, design, iter) {
  maximize.mult = ifelse(control$minimize, 1, -1)
  y = maximize.mult * design[, control$y.name]
  p = predict(model, newdata = points)$data
  p.mu = maximize.mult * p$response
  p.se = p$se
  y.min = min(y)
  d = y.min - p.mu
  xcr = d / p.se
  xcr.prob = pnorm(xcr)
  xcr.dens = dnorm(xcr)
  ei = d * xcr.prob + p.se * xcr.dens
  # FIXME: magic number
  # if se too low set 0 (numerical problems), negate due to minimization
  ifelse(p.se < 1e-6, 0, -ei)
}

# LOWER CONFIDENCE BOUND
# (useful for deterministic and also naively for noisy)
infillCritLCB = function(points, model, control, par.set, design, iter) {
  maximize.mult = ifelse(control$minimize, 1, -1)
  p = predict(model, newdata = points)$data
  lcb = maximize.mult * p$response - control$infill.crit.lcb.lambda * p$se
  return(lcb)
}

######################  MCO criteria ###########################################################

# SMS-EGO: LOWER CONFIDENCE BOUND of points, then HV contribution of these wrt to design
# (useful for deterministic and stochastic MCO)
infillCritSMS = function(points, models, control, par.set, design, iter) {
  maximize.mult = ifelse(control$minimize, 1, -1)
  ys = maximize.mult * design[, control$y.name]
  # get lcb-value-matrix for new points (x -1 for max. objectives)
  ps = lapply(models, predict, newdata = points)
  means = extractSubList(ps, c("data", "response"), simplify = "cols")
  ses = extractSubList(ps, c("data", "se"), simplify = "cols")
  lcbs = means - control$infill.crit.lcb.lambda * ses
  lcbs = lcbs %*% diag(maximize.mult)
  ref.point = getMultiCritRefPoint(control, design)
  # dont substract dominated_hypervolume(lcbs), since this is const, maximize hv contribution ...
  hvs = -1 * sapply(seq_row(lcbs), function(i)
    getDominatedHV(rbind(ys, lcbs[i, ]), ref = ref.point, minimize = control$minimize))

  # epsilon for epsilon-dominace - set adaptively or use given constant value
  if (is.null(control$multicrit.sms.eps)) {
    c.val = 1 - 1 / 2^control$number.of.targets
    front = getNonDominatedPoints(design[, control$y.name], control$minimize)
    eps = (max(front) - min(front)) / (ncol(front) + c.val * (control$iters - iter))
  } else {
    eps = control$multicrit.sms.eps
  }

  # Penalty term
  penalty = function (lcb) {
    f = function(lcb, y) {
      if (all(y <= lcb + eps))
        -1 + prod(1 + pmax(lcb - y, 0))
      else
        0
    }
    max(apply(ys, 1, f, lcb = lcb))
  }
  penalties = apply(lcbs, 1, penalty)
  return(hvs + penalties)
}


#FIXME: unify code with crit above

# epsilon-EGO: LOWER CONFIDENCE BOUND of points, then epsilon indicator contribution of these wrt to design
# (useful for deterministic and stochastic MCO)
infillCritEpsilon = function(points, models, control, par.set, design, iter) {
  maximize.mult = ifelse(control$minimize, 1, -1)
  ys = maximize.mult * design[, control$y.name]
  # get lcb-value-matrix for new points (x -1 for max. objectives)
  ps = lapply(models, predict, newdata = points)
  means = extractSubList(ps, c("data", "response"), simplify = "cols")
  ses = extractSubList(ps, c("data", "se"), simplify = "cols")
  lcbs = means - control$infill.crit.lcb.lambda * ses
  lcbs = lcbs %*% diag(maximize.mult)
  # epsilon-indicator: "maximin-fitness"
  epsInd = function (y) {
    diffs = lcbs - matrix(y, ncol = length(y), nrow = nrow(lcbs), byrow = TRUE)
    min(apply(diffs, 2, max))
  }
  epss = -1 * sapply(seq_row(lcbs), function(i) epsInd(lcbs[i, ]))
  return(epss)
}
