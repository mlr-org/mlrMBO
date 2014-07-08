# Expected quantil improvement (EQI).
# FIXME: control$eqi.beta in MBOControl!
infillCritEQI = function(points, model, control, par.set, design) {

  if(is.null(control$eqi.beta)==T){beta=0.75}else{beta=control$eqi.beta}

  #FIXME: generalize new.noise.var for all models
  maximize.mult = ifelse(control$minimize, 1, -1)

  # q.min
  # FIXME: generalize model$learner.model@X for all models
  p.current.model <- predict(object=model, newdata=as.data.frame(model$learner.model@X))$data
  q.min <- min(model$learner.model*p.current.model$response + qnorm(beta) * p.current.model$se)


  p = predict(object=model, newdata = points)$data
  p.mu = p$response #m_k(x^(n+1))
  p.se = p$se # #s_k(x^(n+1))


  #FIXME: how do we select here best?
  pure.noise.var = if (inherits(model$learner, "regr.km"))
    pure.noise.var = model$learner.model@covariance@nugget
  else
    estimateResidualVariance(model, data = design2, target = control$y.name)
  tau = sqrt(pure.noise.var)

  #  new.noise.var = model$learner.model@covariance@nugget
  #  if(length(new.noise.var)==0) new.noise.var=0 # FIXME: nugget.estim=FALSE -> no noise, EQI does not work

  mq <- p.mu + qnorm(beta) * sqrt((tau * p.se^2)/(tau + p.se^2))
  sq <- p.se^2/sqrt(new.noise.var + p.se^2)

  d = q.min - mq

  xcr <- d/sq
  xcr.prob <- pnorm(xcr)
  xcr.dens <- dnorm(xcr)

  eqi = ifelse(p.se < 1e-06, 0,
               (sq * (xcr * xcr.prob + xcr.dens)))

  return(-eqi)
}



