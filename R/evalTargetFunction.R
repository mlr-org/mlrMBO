# Evaluates target fitness function on given set of points.
#
# @param fun [\code{function(x, ...)}]\cr
#   Fitness function to minimize. The first argument has to be a list of values.
#   The function has to return a single numerical value.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Collection of parameters and their constraints for optimization.
# @param xs [\code{list}]\cr
#   Set of points on which fun shall be evaluated.
# @param opt.path [\code{\link[ParamHelpers]{OptPath}}]\cr
#   Optimization path to save of type \code{\link[ParamHelpers]{OptPath}}.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object for mbo.
# @param show.info [\code{logical(1)}]\cr
#   Show info message after each function evaluation?
#   Default is \code{TRUE}.
# @param oldopts [\code{list}]\cr
#   Old mlr configuration.
# @param ... [\code{list}]\cr
#   Further arguments passed to fitness function.
# @return [\code{list}]:
#   \item{x [\code{list}]}{Named list of proposed optimal parameters.}
#   \item{y [\code{numeric(1)}]}{Value of fitness function at \code{x}, either from evals during optimization or from requested final evaluations, if those were greater than 0.}
#   \item{path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
#   \item{models [List of \code{\link[mlr]{WrappedModel}}]}{List of saved regression models.}
evalTargetFun = function(fun, par.set, xs, opt.path, control, show.info, oldopts, ...) {
  xs = lapply(xs, trafoValue, par=par.set)
  fun2 = function(x) {
    if (control$impute.errors) {
      y = try(fun(x, ...), silent=control$suppress.eval.errors)
      if (is.error(y))
        y = NA_real_
    } else {
      y = fun(x, ...)
    }
    if(length(y) != control$number.of.targets) {
      stop("function output has wrong dimension!")
    }
    if (show.info) {
      dob = opt.path$env$dob
      dob = if (length(dob) == 0L) 0 else max(dob) + 1
      messagef("[mbo] %i: %s : %s", dob,
               paramValueToString(par.set, x), paste(sprintf("%s=%.3f", control$y.name, y),
                                                     collapse = ", "))
    }
    return(y)
  }
  # restore mlr configuration
  configureMlr(on.learner.error=oldopts[["ole"]], show.learner.output=oldopts[["slo"]])
  ys = sapply(xs, fun2)
  configureMlr(on.learner.error=control$on.learner.error,
               show.learner.output=control$show.learner.output)
  if(is.matrix(ys)) {
    for(i in 1:ncol(ys)) {
      j = which(is.na(ys[, i]) | is.nan(ys[, i]) | is.infinite(ys[, i]))
      if (length(j) > 0L) {
        ys[j, i] = mapply(control$impute, xs[j], ys[j, i],
          MoreArgs=list(opt.path=opt.path), USE.NAMES=FALSE)
      }
    }
    return(t(ys))
  } else {
  j = which(is.na(ys) | is.nan(ys) | is.infinite(ys))
  if (length(j) > 0L) {
    ys[j] = mapply(control$impute, xs[j], ys[j],
                   MoreArgs=list(opt.path=opt.path), USE.NAMES=FALSE)
  }
  return(ys)
  }
}
