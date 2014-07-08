# augmented expected improvement, as designed by huang
# useful for noisy
infillCritAEI = function(points, model, control, par.set, design) {
  # FIXME: ugly!
  # FIXME check completely
  # design2 = imputeFeatures(design, par.set, control)
  # design2 = convertDfCols(design2, chars.as.factor = TRUE)

  #FIXME: generalize new.noise.var for all models
  maximize.mult = ifelse(control$minimize, 1, -1)
  p = predict(model, newdata = points)$data
  p.mu = maximize.mult * p$response
  p.se = p$se
  design2 = dropNamed(design2, c("dob", "eol"))
  # FIXME: what a mess! do we want to pass design or design2?
  ebs = getEffectiveBestPoint(design = design2, model = model, par.set = par.set, control = control)
  # calculate EI with plugin, plugin val is mean response at ebs solution
  d = ebs$mu - p.mu
  xcr = d / p.se
  xcr.prob = pnorm(xcr)
  xcr.dens = dnorm(xcr)

  #FIXME: how do we select here best?
  pure.noise.var = if (inherits(model$learnem, "regr.km"))
    pure.noise.var = model$learner.model@covariance@nugget
  else
    estimateResidualVariance(model, data = design2, target = control$y.name)

  tau = sqrt(pure.noise.var)
  #if (sk < sqrt(model@covariance@sd2)/1e+06) {
  #FIXME: What actually happens here. Find out in DiceOptim
  aei = ifelse(p.se < 1e-06, 0,
    (d * xcr.prob + p.se * xcr.dens) * (1 - tau / sqrt(tau^2 + p.se^2)))
  return(-aei)
}





