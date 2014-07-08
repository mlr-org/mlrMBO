#expected improvement  with Plugin
# FIXME: control$plugin.beta in MBOControl
infillCritEIPI = function(points, model, control, par.set, design) {
  maximize.mult = ifelse(control$minimize, 1, -1)
  y = maximize.mult * design[, control$y.name]
  p = predict(model, newdata = points)$data
  p.mu = maximize.mult * p$response
  p.se = p$se
  #y.min = min(y)

  if(is.null(control$plugin.beta)==T){beta=0}else{beta=control$plugin.quantile}
  q <- qnorm(beta)

  lower <- p.mu - q * sqrt(p.se)

  T_ei=min(lower)
  d = T_ei - p.mu
  xcr = d / p.se
  #FIXME: what is done in DiceOption::EI here for numerical reasons?
  #if (kriging.sd/sqrt(model@covariance@sd2) < 1e-06) {
  #  res = 0
  #  xcr = xcr.prob = xcr.dens = NULL
  #
  xcr.prob = pnorm(xcr)
  xcr.dens = dnorm(xcr)
  ei = d * xcr.prob + p.se * xcr.dens
  # FIXME magic number
  # if se too low set 0 (numerical problems), negate due to minimization
  ifelse(p.se < 1e-6, 0, -ei)
}



