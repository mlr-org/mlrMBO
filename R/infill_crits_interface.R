library(checkmate)

makeMBOInfillCriterion = function(
  infill.fn,
  name,
  id,
  minimize = TRUE)
  assertFunction(
    infill.fn,
    args = c("points", "models", "control",
      "par.set", "design", "iter", "attributes"),
    ordered = TRUE)
  assertString(name)
  assertString(id)
  assertFlag(minimize)

  # ...
}

makeMBOInfillCriterionMeanResponse = function() {
  makeMBOInfillCriterion(
    infill.fn = function(points, models, control, par.set, design, iter, attributes = FALSE) {
      ifelse(control$minimize, 1, -1) * predict(models[[1L]], newdata = points)$data$response
    },
    name = "Mean response",
    id = "mean"
  )
}

makeMBOInfillCriterionStandardError = function() {
  makeMBOInfillCriterion(
    infill.fn = function(points, models, control, par.set, design, iter, attributes = FALSE) {
      -predict(models[[1L]], newdata = points)$data$se
    },
    name = "Standard error",
    id = "sd"
  )
}


makeMBOInfillCriterionEI = function() {
  makeMBOInfillCriterion(
    infill.fun = function(points, models, control, par.set, design, iter, attributes = FALSE) {
      model = models[[1L]]
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
      res = ifelse(p.se < 1e-6, 0, -ei)
      if (attributes) {
        res = setAttribute(res, "crit.components", data.frame(se = p$se, mean = p$response))
      }
      return(res)
    },
    name = "Expected improvement",
    id = "EI",
    minimize = FALSE
  )
}

makeMBOInfillCriterionCB = function(cb.lambda = 1, cb.inflate.se = FALSE, cb.pi = NULL) {
  # lambda value for cb - either given, or set via given pi, the other one must be NULL!
  if (!is.null(cb.lambda) && !is.null(cb.pi))
    stop("Please specify either 'cb.lambda' or 'cb.pi' for the CB crit, not both!")
  if (is.null(cb.pi))
    assertNumeric(cb.lambda, len = 1L, any.missing = FALSE, lower = 0)
  if (is.null(cb.lambda)) {
    assertNumeric(cb.pi, len = 1L, any.missing = FALSE, lower = 0, upper = 1)
    # This is the formula from TW diss for setting lambda.
    # Note, that alpha = -lambda, so we need the negative values
    cb.lambda = -qnorm(0.5 * cb.pi^(1 / control$n.objectives))
  }
  assertFlag(cb.inflate.se)
  makeMBOInfillCriterion(
    infillCritCB = function(points, models, control, par.set, design, iter, attributes = FALSE) {
      force(cb.lambda)
      force(cb.inflate)
      model = models[[1L]]
      maximize.mult = ifelse(control$minimize, 1, -1)
      p = predict(model, newdata = points)$data
      if (cb.inflate.se) {
        r.response = diff(range(p$response))
        r.se = diff(range(p$se))
        tol = .Machine$double.eps^0.5
        if (r.response < tol)
          return(r.se)
        if (r.se < tol)
          return(r.response)
        inflate = r.response / r.se
      } else {
        inflate = 1
      }
      res = maximize.mult * p$response - inflate * cb.lambda * p$se
      if (attributes) {
        res = setAttribute(res, "crit.components",
          data.frame(se = p$se, mean = p$response, lambda = cb.lambda))
      }
      return(res)
    },
    name = "Lower confidence bound",
    id = "cb"
  )
}
