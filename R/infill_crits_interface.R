#' @title Create an infill criterion.
#'
#' @description The infill criterion guides the model based search process.
#' The most prominent infill criteria, e.g., expected improvement, lower
#' confidence bound and others, are already implemented in mlrMBO. Moreover,
#' the package allows for the creation of proprietary infill criteria.
#'
#' @param infill.fun [\code{function}]\cr
#'   A function which expects the following parameters in exactly this order
#'   and return a numeric vector of criteria values at the points:
#'   \describe{
#'     \item{points [\code{data.frame}]}{n points where to evaluate}.
#'     \item{models [\code{\link[mlr]{WrappedModel}} | \code{list}]}{Model(s) fitted on design.}
#'     \item{control [\code{MBOControl}]}{Control object.}
#'     \item{par.set [ParamSet]}{Parameter set.}
#'     \item{design [data.frame]}{Design of already visited points.}
#'     \item{iter [integer(1)]}{Current iteration.}
#'   }
#' @param name [\code{character(1)}]\cr
#'   Full name of the criterion.
#' @param id [\code{character(1)}]\cr
#'   Short name of the criterion.
#'   Used internally and in plots.
#' @param minimize [\code{character(1)}]\cr
#'   Shall the criterion be minimized or maximized?
#'   Default is \code{TRUE}.
#' @param components [\code{character}]\cr
#'   Infill criteria may not return proposed point(s) only. Additional
#'   information can be returned by appending a named \code{list} \dQuote{crit.components}
#'   to the returned value as an attribute.
#'   The \code{components} argument takes a character vector of the names of the
#'   meta information, i.e., the names of the named \dQuote{crit.components} list.
#'   Default is the empty character vector.
#' @return [\code{MBOInfillCriterion}]
#'   Basically \code{infill.fun} with an additional class.
#'   All other parameters are appended as attributes.
makeMBOInfillCriterion = function(
  infill.fun,
  name,
  id,
  minimize = TRUE,
  components = character(0L)) {
  assertFunction(
    infill.fun,
    args = c("points", "models", "control",
      "par.set", "design", "iter", "attributes"),
    ordered = TRUE)

  assertString(name)
  assertString(id)
  assertFlag(minimize)
  assertCharacter(components)

  infill.fun = setAttribute(infill.fun, "name", name)
  infill.fun = setAttribute(infill.fun, "id", id)
  infill.fun = setAttribute(infill.fun, "minimize", minimize)
  infill.fun = setAttribute(infill.fun, "components", components)
  infill.fun = addClasses(infill.fun, "MBOInfillCriterion")
  return(infill.fun)
}

getMBOInfillCritParams = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(attr(x, "params"))
}

getMBOInfillCritParam = function(x, par.name) {
  assertClass(x, "MBOInfillCriterion")
  assertString(par.name)
  return(getMBOInfillCritParams(x)[[par.name]])
}

getMBOInfillCritName = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(attr(x, "name"))
}

getMBOInfillCritId = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(attr(x, "id"))
}

getMBOInfillCritId = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(attr(x, "id"))
}

getMBOInfillCritComponents = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(attr(x, "components"))
}

getMBOInfillDummyCritComponents = function(x) {
  assertClass(x, "MBOInfillCriterion")
  ns = getMBOInfillCritComponents(x)
  as.data.frame(BBmisc::namedList(ns, NA_real_))
}

getMBOInfillCritDirection = function(x) {
  assertClass(x, "MBOInfillCriterion")
  ifelse(attr(x, "minimize"), 1, -1)
}

#' @title Infill criteria.
#' @name infillcrits
#'
#' @description
#' \pkg{mlrMBO} contains most of the most popular infill criteria.
#'
#' @param cb.lambda [\code{numeric(1)}]\cr
#'   Lambda parameter for confidence bound infill criterion.
#'   Only used if \code{crit == "cb"}, ignored otherwise.
#'   Default is 1.
#' @param cb.pi [\code{numeric(1)}]\cr
#'   Probability-of-improvement value to determine the lambda parameter for cb infill criterion.
#'   It is an alternative to set the trade-off between \dQuote{mean} and \dQuote{se}.
#'   Only used if \code{crit == "cb"}, ignored otherwise.
#'   If specified, \code{cb.lambda == NULL} must hold.
#'   Default is \code{NULL}.
#' @param cb.inflate.se [\code{logical(1)}]\cr
#'   Try to inflate or deflate the estimated standard error to get to the same scale as the mean?
#'   Calculates the range of the mean and standard error and multiplies the standard error
#'   with the quotient of theses ranges.
#'   Default is \code{FALSE}.
#' @param aei.use.nugget [\code{logical(1)}]\cr
#'   Should the nugget effect be used for the pure variance estimation for augmented
#'   expected improvement?
#'   Default is \code{FALSE}.
#' @param eqi.beta [\code{numeric(1)}]\cr
#'   Beta parameter for expected quantile improvement criterion.
#'   Default is 0.75.
#' @name infillcrits
#' @rdname infillcrits
NULL

# =====================
# SINGLE-CRITERIA STUFF
# =====================

#' @export
#' @rdname infillcrits
makeMBOInfillCriterionMeanResponse = function() {
  makeMBOInfillCriterion(
    infill.fun = function(points, models, control, par.set, design, iter, attributes = FALSE) {
      ifelse(control$minimize, 1, -1) * predict(models[[1L]], newdata = points)$data$response
    },
    name = "Mean response",
    id = "mean"
  )
}

#' @export
#' @rdname infillcrits
makeMBOInfillCriterionStandardError = function() {
  makeMBOInfillCriterion(
    infill.fun = function(points, models, control, par.set, design, iter, attributes = FALSE) {
      -predict(models[[1L]], newdata = points)$data$se
    },
    name = "Standard error",
    id = "sd"
  )
}

#' @export
#' @rdname infillcrits
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
    id = "ei",
    components = c("se", "mean"),
    minimize = FALSE
  )
}

#' @export
#' @rdname infillcrits
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
    infill.fun = function(points, models, control, par.set, design, iter, attributes = FALSE) {
      force(cb.lambda)
      force(cb.inflate.se)
      force(cb.pi)
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
    id = "cb",
    components = c("se", "mean", "lambda")
  )
}

#' @export
#' @rdname infillcrits
makeMBOInfillCriterionAEI = function(aei.use.nugget = FALSE) {
  assertFlag(aei.use.nugget)
  force(aei.use.nugget)

  makeMBOInfillCriterion(
    infill.fun = function(points, models, control, par.set, design, iter, attributes = FALSE) {
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
      res = (-1) * ifelse(p.se < 1e-06, 0,
        (d * xcr.prob + p.se * xcr.dens) * (1 - tau / sqrt(tau^2 + p.se^2)))
      if (attributes) {
        res = setAttribute(res, "crit.components", data.frame(se = p$se, mean = p$response, tau = tau))
      }
      return(res)
    },
    name = "Augmeted expected improvement",
    id = "aei",
    components = c("se", "mean", "tau")
  )
}

#' @export
#' @rdname infillcrits
makeMBOInfillCriterionEQI = function(eqi.beta = 0.75) {
  assertNumber(eqi.beta, na.ok = FALSE, lower = 0.5, upper = 1)
  force(eqi.beta)

  makeMBOInfillCriterion(
    infill.fun = function(points, models, control, par.set, design, iter, attributes = FALSE) {
      model = models[[1L]]
      maximize.mult = ifelse(control$minimize, 1, -1)
      # compute q.min
      design_x = design[, (colnames(design) %nin% control$y.name)]
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

      eqi = ifelse(p.se < 1e-06, 0, (sq * (xcr * xcr.prob + xcr.dens)))
      return(-eqi)
    },
    name = "Expected quantile improvement",
    id = "eqi"
  )
}

# ====================
# MULTI-CRITERIA STUFF
# ====================

#' @export
#' @rdname infillcrits
makeMBOInfillCriterionDIB = function(cb.lambda = 1, cb.inflate.se = FALSE, cb.pi = NULL) {
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
    infill.fun = function(points, models, control, par.set, design, iter, attributes = FALSE) {
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
        if (is.null(control$dib.sms.eps)) {
          c.val = 1 - 1 / 2^control$n.objectives
          eps = vnapply(seq_col(ys.front), function(i) {
            (max(ys.front[, i]) - min(ys.front[, i])) /
              (ncol(ys.front) + c.val * (control$iters - iter))
          })
        } else {
          # FIXME: user should be allowed to set a vector
          eps = control$multiobj.dib.sms.eps
        }
        ys.front = as.matrix(ys.front)
        # allocate mem for adding points to front for HV calculation in C
        front2 = t(rbind(ys.front, 0))
        crit.vals = .Call("c_sms_indicator", PACKAGE = "mlrMBO", as.matrix(cbs), ys.front, front2, eps, ref.point)
      } else {
        crit.vals = .Call("c_eps_indicator", PACKAGE = "mlrMBO", as.matrix(cbs), as.matrix(ys.front))
      }
      return(crit.vals)
    },
    name = "Whatever dib stands for :-)",
    id = "dib"
  )
}
