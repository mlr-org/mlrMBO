#' @title Infill criteria.
#'
#' @description
#' \pkg{mlrMBO} contains most of the most popular infill criteria, e.g., expected
#' improvement, (lower) confidence bound etc. Moreover, custom infill criteria
#' may be generated with the \code{\link{makeMBOInfillCrit}} function.
#'
#' @details
#' In the multi-objective case we recommend to set \code{cb.lambda} to
#' \eqn{q(0.5 \cdot \pi_{CB}^(1 / n))} where \eqn{q} is the quantile
#' function of the standard normal distribution, \eqn{\pi_CB} is the probability
#' of improvement value and \eqn{n} is the number of objectives of the considered problem.
#'
#' @param se.threshold [\code{numeric(1)}]\cr
#'   In order to avoid numerical problems the standard error estimation is assumed to
#'   be exactly zero, if it is below \code{se.threshold}.
#'   Default is 1e-6.
#' @param cb.lambda [\code{numeric(1)} | \code{NULL}]\cr
#'   Lambda parameter for confidence bound infill criterion.
#'   Default is \code{NULL}, which means 1 in case of a fully numeric parameter set and 2 otherwise.
#FIXME: removed cb.inflate.se for now (see issue #309)
# @param cb.inflate.se [\code{logical(1)}]\cr
#   Try to inflate or deflate the estimated standard error to get to the same scale as the mean?
#   Calculates the range of the mean and standard error and multiplies the standard error
#   with the quotient of theses ranges.
#   Default is \code{FALSE}.
#' @param aei.use.nugget [\code{logical(1)}]\cr
#'   Should the nugget effect be used for the pure variance estimation for augmented
#'   expected improvement?
#'   Default is \code{FALSE}.
#' @param eqi.beta [\code{numeric(1)}]\cr
#'   Beta parameter for expected quantile improvement criterion.
#'   Default is 0.75.
#' @param sms.eps [\code{numeric(1)} | \code{NULL}]\cr
#'   Epsilon for epsilon-dominance for \code{dib.indicator = "sms"}.
#'   Default is \code{NULL}, in this case it is adaptively set.
#' @name infillcrits
#' @seealso \code{\link{MBOInfillCrit}}
#' @rdname infillcrits
NULL

# =====================
# SINGLE-CRITERIA STUFF
# =====================

#' @export
#' @rdname infillcrits
makeMBOInfillCritMeanResponse = function() {
  makeMBOInfillCrit(
    fun = function(points, models, control, par.set, design, iter, attributes = FALSE) {
      ifelse(control$minimize, 1, -1) * predict(models[[1L]], newdata = points)$data$response
    },
    name = "Mean response",
    id = "mean",
    opt.direction = "objective"
  )
}

#' @export
#' @rdname infillcrits
makeMBOInfillCritStandardError = function() {
  makeMBOInfillCrit(
    fun = function(points, models, control, par.set, design, iter, attributes = FALSE) {
       -predict(models[[1L]], newdata = points)$data$se
    },
    name = "Standard error",
    id = "se",
    requires.se = TRUE,
    opt.direction = "maximize"
  )
}

#' @export
#' @rdname infillcrits
makeMBOInfillCritEI = function(se.threshold = 1e-6) {
  assertNumber(se.threshold, lower = 1e-20)
  force(se.threshold)
  makeMBOInfillCrit(
    fun = function(points, models, control, par.set, design, iter, attributes = FALSE) {
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
      res = ifelse(p.se < se.threshold, 0, -ei)
      if (attributes) {
        res = setAttribute(res, "crit.components", data.frame(se = p$se, mean = p$response))
      }
      return(res)
    },
    name = "Expected improvement",
    id = "ei",
    components = c("se", "mean"),
    params = list(se.threshold = se.threshold),
    opt.direction = "maximize",
    requires.se = TRUE
  )
}

#' @export
#' @rdname infillcrits
makeMBOInfillCritCB = function(cb.lambda = NULL) {
  assertNumber(cb.lambda, lower = 0, null.ok = TRUE)
  force(cb.lambda)
  makeMBOInfillCrit(
    fun = function(points, models, control, par.set, design, iter, attributes = FALSE) {
      model = models[[1L]]
      maximize.mult = ifelse(control$minimize, 1, -1)
      p = predict(model, newdata = points)$data
      #FIXME: removed cb.inflate.se for now (see issue #309)
      # if (cb.inflate.se) {
      #   r.response = diff(range(p$response))
      #   r.se = diff(range(p$se))
      #   tol = .Machine$double.eps^0.5
      #   if (r.response < tol)
      #     return(r.se)
      #   if (r.se < tol)
      #     return(r.response)
      #   inflate = r.response / r.se
      # } else {
      inflate = 1
      #}
      res = maximize.mult * p$response - inflate * cb.lambda * p$se
      if (attributes) {
        res = setAttribute(res, "crit.components",
          data.frame(se = p$se, mean = p$response, lambda = cb.lambda))
      }
      return(res)
    },
    name = "Confidence bound",
    id = "cb",
    components = c("se", "mean", "lambda"),
    params = list(cb.lambda = cb.lambda),
    opt.direction = "objective",
    requires.se = TRUE
  )
}

#' @export
#' @rdname infillcrits
makeMBOInfillCritAEI = function(aei.use.nugget = FALSE, se.threshold = 1e-6) {
  assertFlag(aei.use.nugget)
  assertNumber(se.threshold, lower = 1e-20)
  force(aei.use.nugget)
  force(se.threshold)

  makeMBOInfillCrit(
    fun = function(points, models, control, par.set, design, iter, attributes = FALSE) {
      model = models[[1L]]
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
      pure.noise.var = if (aei.use.nugget)
        pure.noise.var = model$learner.model@covariance@nugget
      else
        estimateResidualVariance(model, data = design, target = control$y.name)

      tau = sqrt(pure.noise.var)
      res = (-1) * ifelse(p.se < se.threshold, 0,
        (d * xcr.prob + p.se * xcr.dens) * (1 - tau / sqrt(tau^2 + p.se^2)))
      if (attributes) {
        res = setAttribute(res, "crit.components", data.frame(se = p$se, mean = p$response, tau = tau))
      }
      return(res)
    },
    name = "Augmeted expected improvement",
    id = "aei",
    components = c("se", "mean", "tau"),
    params = list(aei.use.nugget = aei.use.nugget),
    opt.direction = "maximize",
    requires.se = TRUE
  )
}

#' @export
#' @rdname infillcrits
makeMBOInfillCritEQI = function(eqi.beta = 0.75, se.threshold = 1e-6) {
  assertNumber(eqi.beta, lower = 0.5, upper = 1)
  assertNumber(se.threshold, lower = 1e-20)
  force(eqi.beta)
  force(se.threshold)

  makeMBOInfillCrit(
    fun = function(points, models, control, par.set, design, iter, attributes = FALSE) {
      model = models[[1L]]
      maximize.mult = ifelse(control$minimize, 1, -1)
      # compute q.min
      design_x = design[, (colnames(design) %nin% control$y.name), drop = FALSE]
      p.current.model = predict(object = model, newdata = design_x)$data
      q.min = min(maximize.mult * p.current.model$response + qnorm(eqi.beta) * p.current.model$se)

      p = predict(object = model, newdata = points)$data
      p.mu = maximize.mult * p$response
      p.se = p$se

      pure.noise.var = if (inherits(model$learner, "regr.km")) {
        pure.noise.var = model$learner.model@covariance@nugget
        #FIXME: What if kriging is wrapped?
      } else {
        estimateResidualVariance(model, data = design, target = control$y.name)
      }
      tau = sqrt(pure.noise.var)

      mq = p.mu + qnorm(eqi.beta) * sqrt((tau * p.se^2) / (tau + p.se^2))
      sq = p.se^2 / sqrt(pure.noise.var + p.se^2)
      d = q.min - mq
      xcr = d / sq
      xcr.prob = pnorm(xcr)
      xcr.dens = dnorm(xcr)

      res = -1 * ifelse(p.se < se.threshold, 0, (sq * (xcr * xcr.prob + xcr.dens)))
      if (attributes) {
        res = setAttribute(res, "crit.components", data.frame(se = p.se, mean = p.mu, tau = tau))
      }
      return(res)
    },
    name = "Expected quantile improvement",
    components = c("se", "mean", "tau"),
    id = "eqi",
    params = list(eqi.beta = eqi.beta),
    opt.direction = "maximize",
    requires.se = TRUE
  )
}

# ====================
# MULTI-CRITERIA STUFF
# ====================

#' @export
#' @rdname infillcrits
makeMBOInfillCritDIB = function(cb.lambda = 1, sms.eps = NULL) {
  assertNumber(cb.lambda, lower = 0)
  if (!is.null(sms.eps))
    assertNumber(sms.eps, lower = 0, finite = TRUE)
  makeMBOInfillCrit(
    fun = function(points, models, control, par.set, design, iter, attributes = FALSE) {
      # get ys and cb-value-matrix for new points, minimize version
      maximize.mult = ifelse(control$minimize, 1, -1)
      ys = as.matrix(design[, control$y.name]) %*% diag(maximize.mult)

      ps = lapply(models, predict, newdata = points)
      means = extractSubList(ps, c("data", "response"), simplify = "cols")
      ses = extractSubList(ps, c("data", "se"), simplify = "cols")
      cbs = means %*% diag(maximize.mult) - cb.lambda * ses
      # from here on ys and cbs are ALWAYS minimized
      all.mini = rep(TRUE, control$n.objectives)

      ys.front = getNonDominatedPoints(ys, minimize = all.mini)

      if (control$multiobj.dib.indicator == "sms") {
        # get refpoint by ctrl-method, ys could be scaled by -1 (if yi = max!)
        ref.point = getMultiObjRefPoint(ys, control, minimize = all.mini)
        # get epsilon for epsilon-dominace - set adaptively or use given constant value
        if (is.null(sms.eps)) {
          c.val = 1 - 1 / 2^control$n.objectives
          sms.eps = vnapply(seq_col(ys.front), function(i) {
            (max(ys.front[, i]) - min(ys.front[, i])) /
              (ncol(ys.front) + c.val * (control$iters - iter))
          })
        }
        ys.front = as.matrix(ys.front)
        # allocate mem for adding points to front for HV calculation in C
        front2 = t(rbind(ys.front, 0))
        crit.vals = .Call("c_sms_indicator", PACKAGE = "mlrMBO", as.matrix(cbs), ys.front, front2, sms.eps, ref.point)
      } else {
        crit.vals = .Call("c_eps_indicator", PACKAGE = "mlrMBO", as.matrix(cbs), as.matrix(ys.front))
      }
      return(crit.vals)
    },
    name = "Direct indicator-based",
    id = "dib",
    params = list(cb.lambda = cb.lambda, sms.eps = sms.eps),
    opt.direction = "maximize",
    requires.se = TRUE
  )
}
