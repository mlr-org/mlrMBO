# Infill criteria.
# CONVENTION: INFILL CRITERIA ARE ALWAYS MINIMIZED. SO A FEW BELOW ARE NEGATED VERSIONS!
#FIXME think about which criterias are for determinitic, which are for noisy case
# below is just guessed this...

# General interface
#
# @param points [\code{data.frame}]\cr
#   Points where to evaluate.
# @param model [\code{\link{WrappedModel}}]\cr
#   Model fitted on design.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param design [\code{data.frame}]\cr
#   Design of already visited points.
# @return [\code{numeric}]. Criterion values at \code{points}.

# mean response of model
# useful for deterministic and noisy
infillCritMeanResponse = function(points, model, control, par.set, design) {
  ifelse(control$minimize, 1, -1) * predict(model, newdata=points)$data$response
}

# model uncertainty
# on its own not really useful for anything I suppose...
infillCritStandardError = function(points, model, control, par.set, design) {
  -predict(model, newdata=points)$data$se
}


# expected improvement
# useful for deterministic
infillCritEI = function(points, model, control, par.set, design) {
  maximize.mult = ifelse(control$minimize, 1, -1)
  y = maximize.mult * design[, control$y.name]
  p = predict(model, newdata = points)$data
  p.mu = maximize.mult * p$response
  p.se = p$se
  y.min = min(y)
  d = y.min - p.mu
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


# lower confidence bound
# useful for deterministic
infillCritLCB = function(points, model, control, par.set, design) {
  maximize.mult = ifelse(control$minimize, 1, -1)
  p = predict(model, newdata = points)$data
  lcb = maximize.mult * (p$response - control$infill.crit.lcb.lambda * p$se)
  return(lcb)
}



# augmented expected improvement, as designed by huang
# useful for noisy
infillCritAEI = function(points, model, control, par.set, design) {
  # FIXME: ugly!
  design2 = imputeFeatures(design, par.set, control)
  design2 = convertDfCols(design2, chars.as.factor = TRUE)

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

# infillCritAKG = function(points, model, ctrl=NULL) {
#   if(is.null(ctrl$new.noise.var)) ctrl$new.noise.var=0
#   apply(des, 1, AEI, model=model$learner.model, new.noise.var=ctrl$new.noise.var)
# }
#
# infillCritAEIold = function(points, model, ctrl=NULL) {
#   if(is.null(ctrl$new.noise.var)) ctrl$new.noise.var=0
#   if(is.null(ctrl$y.min)) ctrl$y.min=NULL
#   apply(points, 1, AEI, model=model$learner.model, new.noise.var=ctrl$new.noise.var, y.min=ctrl$y.min)
# }
#



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


# Approximate Knowlwdge gradient
# FIXME: just for kriging at the moment
infillCritAKG = function(points, model, control, par.set, design) {


  maximize.mult = ifelse(control$minimize, 1, -1)

  x=points
  model=model$learner.model
  type="SK"
  new.noise.var = model@covariance@nugget

  if(length(new.noise.var)==0) new.noise.var=0 # nugget.estim=FALSE -> no noise

  newdata.num <- as.numeric(x)
  newdata <- data.frame(t(newdata.num))
  colnames(newdata) = colnames(model@X)
  tau2.new <- new.noise.var
  predx <- predict.km(model, newdata = newdata, type = type,
                      checkNames = FALSE)
  mk.x <- maximize.mult*predx$mean
  sk.x <- predx$sd
  c.x <- predx$c
  V.x <- predx$Tinv.c
  T <- model@T
  z <- model@z
  U <- model@M
  F.x <- model.matrix(model@trend.formula, data = newdata)
  if (sk.x < sqrt(model@covariance@sd2)/1e+06 || model@covariance@sd2 <
        1e-20) {
    AKG <- 0
    tuuinv <- mk.X <- V.X <- mu.x <- cn <- sQ <- Isort <- Iremove <- A1 <- at <- bt <- ct <- NULL
  }
  else {
    predX <- predict.km(model, newdata = model@X, type = type,
                        checkNames = FALSE)
    mk.X <-maximize.mult*predX$mean
    V.X <- predX$Tinv.c
    F.X <- model@F
    m_min <- min(c(mk.X, mk.x))
    if (type == "UK") {
      tuuinv <- solve(t(U) %*% U)
      mu.x <- (F.X - t(V.X) %*% U) %*% tuuinv %*% t(F.x -
                                                      t(V.x) %*% U)
    }
    else {
      tuuinv <- mu.x <- 0
    }
    cn <- c.x - t(V.X) %*% V.x + mu.x
    cn <- c(cn, sk.x^2)
    A <- c(mk.X, mk.x)
    B <- cn/sqrt(tau2.new + sk.x^2)
    sQ <- B[length(B)]
    A <- -A
    nobs <- model@n
    Isort <- order(x = B, y = A)
    b <- B[Isort]
    a <- A[Isort]
    Iremove <- numeric()
    for (i in 1:(nobs)) {
      if (b[i + 1] == b[i]) {
        Iremove <- c(Iremove, i)
      }
    }
    if (length(Iremove) > 0) {
      b <- b[-Iremove]
      a <- a[-Iremove]
    }
    nobs <- length(a) - 1
    C <- rep(0, nobs + 2)
    C[1] <- -1e+36
    C[length(C)] <- 1e+36
    A1 <- 0
    for (k in 2:(nobs + 1)) {
      nondom <- 1
      if (k == nobs + 1) {
        nondom <- 1
      }
      else if ((a[k + 1] >= a[k]) && (b[k] == b[k + 1])) {
        nondom <- 0
      }
      if (nondom == 1) {
        loopdone <- 0
        count <- 0
        while (loopdone == 0 && count < 1000) {
          count <- count + 1
          u <- A1[length(A1)] + 1
          C[u + 1] <- (a[u] - a[k])/(b[k] - b[u])
          if ((length(A1) > 1) && (C[u + 1] <= C[A1[length(A1) -
                                                      1] + 2])) {
            A1 <- A1[-length(A1)]
          }
          else {
            A1 <- c(A1, k - 1)
            loopdone <- 1
          }
        }
      }
    }
    at <- a[A1 + 1]
    bt <- b[A1 + 1]
    ct <- C[c(1, A1 + 2)]
    maxNew <- 0
    for (k in 1:length(at)) {
      maxNew <- maxNew + at[k] * (pnorm(ct[k + 1]) - pnorm(ct[k])) +
        bt[k] * (dnorm(ct[k]) - dnorm(ct[k + 1]))
    }
    AKG <- maxNew - (-m_min)
  } #end of else

  return(-AKG)
}

