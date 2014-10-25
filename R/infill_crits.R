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
  ys = as.matrix(design[, control$y.name]) %*% diag(maximize.mult)
  ps = lapply(models, predict, newdata = points)
  means = extractSubList(ps, c("data", "response"), simplify = "cols")
  ses = extractSubList(ps, c("data", "se"), simplify = "cols")
  lcbs = means %*% diag(maximize.mult) - control$infill.crit.lcb.lambda * ses
  # from here on ys and lcbs are ALWAYS minimized
  all.mini = rep(TRUE, control$number.of.targets)

  ys.front = getNonDominatedPoints(ys, minimize = all.mini)

  if (control$multicrit.dib.indicator == "sms") {
    ref.point = getMultiCritRefPoint(ys.front, control, minimize = all.mini)
    # get epsilon for epsilon-dominace - set adaptively or use given constant value
    if (is.null(control$dib.sms.eps)) {
      c.val = 1 - 1 / 2^control$number.of.targets
      eps = vnapply(seq_col(ys.front), function(i) {
        (max(ys.front[,i]) - min(ys.front[,i])) /
          (ncol(ys.front) + c.val * (control$iters - iter))
      })
    } else {
      # FIXME: user should be allowed to set a vector
      eps = control$multicrit.dib.sms.eps
    }
    ys.front = as.matrix(ys.front)
    # allocate mem for adding points to front for HV calculation in C
    front2 = t(rbind(ys.front, 0))
    crit.vals = .Call("c_sms_indicator", as.matrix(lcbs), ys.front, front2, eps, ref.point)
  } else {
    crit.vals = .Call("c_eps_indicator", as.matrix(lcbs), as.matrix(ys.front))
  }
  return(crit.vals)
}


# AUGMENTED EXPECTED IMPROVEMENT
# (useful for noisy functions)
infillCritAEI = function(points, model, control, par.set, design, iter) {
  maximize.mult = ifelse(control$minimize, 1, -1)
  p = predict(model, newdata = points)$data
  p.mu = maximize.mult * p$response
  p.se = p$se

  ebs = getEffectiveBestPoint(design = design, model = model, par.set = par.set, control = control)
  # calculate EI with plugin, plugin val is mean response at ebs solution
  d = ebs$mu - p.mu 
  xcr = d / p.se
  xcr.prob = pnorm(xcr)
  xcr.dens = dnorm(xcr)
  
  # noise estimation
  pure.noise.var = if (inherits(model$learner, "regr.km"))
    pure.noise.var = model$learner.model@covariance@nugget
  else
    estimateResidualVariance(model, data = design, target = control$y.name)
  
  tau = sqrt(pure.noise.var)
  aei = ifelse(p.se < 1e-06, 0,
               (d * xcr.prob + p.se * xcr.dens) * (1 - tau / sqrt(tau^2 + p.se^2)))
  return(-aei)
}


# EXPECTED QUANTILE IMPROVEMENT
# (useful for noisy functions)
infillCritEQI = function(points, model, control, par.set, design, iter) {
  maximize.mult = ifelse(control$minimize, 1, -1)
  # compute q.min
  design_x = design[, (colnames(design) %nin% control$y.name)]
  p.current.model = predict(object = model, newdata = design_x)$data
  q.min = min(maximize.mult * p.current.model$response + qnorm(control$infill.crit.eqi.beta) * p.current.model$se)
  
  p = predict(object = model, newdata = points)$data
  p.mu = maximize.mult * p$response 
  p.se = p$se 
  
  pure.noise.var = if (inherits(model$learner, "regr.km")) {
    pure.noise.var = model$learner.model@covariance@nugget
  } else {
    estimateResidualVariance(model, data = design, target = control$y.name)
  }
  tau = sqrt(pure.noise.var)

  mq = p.mu + qnorm(control$infill.crit.eqi.beta) * sqrt((tau * p.se^2) / (tau + p.se^2))
  sq = p.se^2 / sqrt(pure.noise.var + p.se^2)
  d = q.min - mq 
  xcr = d / sq
  xcr.prob = pnorm(xcr)
  xcr.dens = dnorm(xcr)
  
  eqi = ifelse(p.se < 1e-06, 0, (sq * (xcr * xcr.prob + xcr.dens)))
  return(-eqi)
}


