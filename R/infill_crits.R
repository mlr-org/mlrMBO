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

# SMS-EGO / DIB (direct indicator based):
# direct.sms LOWER CONFIDENCE BOUND of points, then HV contribution of these wrt to design
# direct.eps: LOWER CONFIDENCE BOUND of points, then epsilon indicator contribution of these wrt to design
# (useful for deterministic and stochastic MCO)
infillCritDIB = function(points, models, control, par.set, design, iter) {
  # get ys and lcb-value-matrix for new points, minimize version
  maximize.mult = ifelse(control$minimize, 1, -1)
  ys = design[, control$y.name] %*% diag(maximize.mult)
  ps = lapply(models, predict, newdata = points)
  means = extractSubList(ps, c("data", "response"), simplify = "cols")
  ses = extractSubList(ps, c("data", "se"), simplify = "cols")
  lcbs = means %*% diag(maximize.mult) - control$infill.crit.lcb.lambda * ses
  # from here on ys and lcbs are ALWAYS minimized
  all.mini = rep(TRUE, control$number.of.targets)
  ys.front = getNonDominatedPoints(ys, minimize = all.mini)

  if (control$multicrit.dib.indicator == "sms") {
    ref.point = getMultiCritRefPoint(ys.front, control, minimize = all.mini)
    # dont substract dominated_hypervolume(lcbs), since this is const, maximize hv contribution ...
    hvs = -1 * sapply(seq_row(lcbs), function(i)
      getDominatedHV(rbind(ys.front, lcbs[i, ]), ref.point = ref.point, minimize = all.mini))
    # get epsilon for epsilon-dominace - set adaptively or use given constant value
    if (is.null(control$dib.sms.eps)) {
      c.val = 1 - 1 / 2^control$number.of.targets
      eps = (max(ys.front) - min(ys.front)) /
        (ncol(ys.front) + c.val * (control$iters - iter))
    } else {
      eps = control$multicrit.dib.sms.eps
    }
    # penalty term
    # FIXME: double apply, try to make this faster
    penalties = apply(lcbs, 1, function (lcb) {
      f = function(lcb, y) {
        if (all(y <= lcb + eps))
          -1 + prod(1 + pmax(lcb - y, 0))
        else
          0
      }
      max(apply(ys.front, 1, f, lcb = lcb))
    })
    crit.vals = hvs + penalties
  } else {
    # direct.eps, epsilon-indicator: "maximin-fitness"
    # we measure the (signed) L_inf distance between an lcb-point and its closest neighbor in ys.front
    n.lcb = nrow(lcbs)
    n.ys = nrow(ys.front)
    # try to be fast: all L_inf dists between 1 lcb point and all ys, blockwise in rows
    lcbs2 = lcbs[rep(1:n.lcb, each = n.ys), ]
    ys2 = ys.front[rep(1:n.ys, n.lcbs), ]
    # get L_inf dists
    z = apply(ys2 - lcbs2, 1, max)
    # put dists of 1 lcb point to all front points in one row + and get min dist to front set
    z = matrix(z, nrow = n.lcb, ncol = n.ys, byrow = TRUE)
    crit.vals = apply(z, 1, min)
  }

  return(crit.vals)
}


