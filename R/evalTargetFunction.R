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
# @param more.args [\code{list}]\cr
#   Further arguments passed to fitness function.
# @return [\code{list}]:
#   \item{ys}{Vector or matrix of objective values. First dim = length(xs).}
#   \item{times}{Vector of times it took to evaluate the objective).}
evalTargetFun = function(fun, par.set, xs, opt.path, control, show.info, oldopts, more.args = list()) {
  xs = lapply(xs, trafoValue, par=par.set)
  fun2 = function(x) {
    st = system.time({
      if (control$impute.errors) {
        y = try(do.call(fun, insert(list(x=x), more.args)), silent=control$suppress.eval.errors)
        if (is.error(y))
          y = NA_real_
      } else {
        y = do.call(fun, insert(list(x=x), more.args))
      }
    })
    if (length(y) != control$number.of.targets)
      stopf("Objective function output has wrong length: %i. Should be %i.",
        length(y), control$number.of.targets)
    #FIXME: show.info must be called on master
    return(list(y = y, time = st[3]))
  }
  # restore mlr configuration
  configureMlr(on.learner.error = oldopts[["ole"]], show.learner.output = oldopts[["slo"]])

  # apply fun2 and extract parts
  z = parallelMap(fun2, xs)
  # ys is row-matrix with number.of.targets cols
  ys = if (control$number.of.targets == 1L)
    matrix(extractSubList(z, "y"), ncol = 1L)
  else
    setColNames(extractSubList(z, "y", simplify = "rows"), control$y.name)
  times = extractSubList(z, "time")

  # show some info on the console
  if (show.info) {
    num.format = control$output.num.format
    num.format.string = paste("%s=", num.format, sep="")
    dob = opt.path$env$dob
    dob = if (length(dob) == 0L) 0 else max(dob) + 1
    for (ind in seq_along(xs)) {
      #FIXME: show time?
      messagef("[mbo] %i: %s : %s", dob, paramValueToString(par.set, xs[[ind]], num.format = num.format),
        paste(sprintf(num.format.string, control$y.name, ys[ind, ]), collapse = ", "))
    }
  }

  configureMlr(on.learner.error=control$on.learner.error,
    show.learner.output=control$show.learner.output)
  # FIXME: this does not look good. we need to define sematincs of impute better.
  # in imputation we probably need to know which objective we refer to?
  for (dim in seq_len(control$number.of.targets)) {
    # impute all broken y values in ys by imputation function of user
    broken = which(is.na(ys[, dim]) | is.nan(ys[, dim]) | is.infinite(ys[, dim]))
    if (length(broken) > 0L) {
      ys[broken, dim] = mapply(control$impute[[dim]], xs[broken], ys[broken, dim],
        MoreArgs = list(opt.path = opt.path), USE.NAMES = FALSE)
    }
  }
#   if (is.matrix(ys)) {
#     for (i in seq_row(ys)) {
#       j = which(is.na(ys[i, ]) | is.nan(ys[i, ]) | is.infinite(ys[i, ]))
#       if (length(j) > 0L) {
#         ys[i, j] = mapply(control$impute, xs[j], ys[i, j],
#           MoreArgs=list(opt.path=opt.path), USE.NAMES=FALSE)
#       }
#     }
#   } else {
#     # impute all broken y values in ys by imputation function of user
#     broken = which(is.na(ys) | is.nan(ys) | is.infinite(ys))
#     if (length(broken) > 0L) {
#       ys[broken] = mapply(control$impute, xs[broken], ys[broken],
#         MoreArgs = list(opt.path = opt.path), USE.NAMES = FALSE)
#     }
#   }
  return(list(ys = ys, times = times))
}

