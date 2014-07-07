# Minimal Quantil Criteria
# FIXME: control$mq.beta in MBOControl!

infillCritMQ = function(points, model, control, par.set, design) {
  maximize.mult = ifelse(control$minimize, 1, -1)
  y = maximize.mult * design[, control$y.name]
  p = predict(model, newdata = points)$data
  p.mu = maximize.mult * p$response
  p.se = p$se
  #y.min = min(y)

  if(is.null(control$mq.beta)==T){beta=0.9}else{beta=control$mq.quantile}
  q <- qnorm(beta)

  mq=p.mu+pnorm(beta)^(-1)*p.se

  # FIXME magic number
  # if se too low set 0 (numerical problems), negate due to minimization
  ifelse(p.se < 1e-6, 0, -mq)
}



