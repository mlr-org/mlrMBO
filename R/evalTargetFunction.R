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
        y = do.call(fun, insert(list(x=x), more.args))
    })
    if (length(y) != control$number.of.targets)
      stopf("Objective function output has wrong length: %i. Should be %i.",
        length(y), control$number.of.targets)
    return(list(y = y, time = st[3]))
  }
  # restore mlr configuration
  configureMlr(on.learner.error = oldopts[["ole"]], show.learner.output = oldopts[["slo"]])


  
  # If an error occurs in f and we don't want to impute, stop - in this case
  # parallelMap gets NULL as impute function. In this case, execution will
  # stop at this line. Otherwise we use the identity for imputation - now
  # every element in z could be an error object.
  z = parallelMap(fun2, xs, impute.error = if(control$impute.errors) identity else NULL)
  # Check for errors - impute them with NA for now and save the error message
  error.messages = character(length(z))
  for (index in seq_along(z)) {
    if(is.error(z[[index]])) {
      error.messages[index] = z[[index]]$message
      z[[index]] = list(y = rep(NA_real_, control$number.of.targets), time = NA_real_)
    }
  }
  
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
  
  # FIXME: Do we need this here? This function is for function evaluation,
  # we should not need to configure mlr in this function.
  configureMlr(on.learner.error = control$on.learner.error,
    show.learner.output = control$show.learner.output)
  
  # Generate impute-error-messages for the opt-path
  for (ind in seq_len(nrow(ys))) {
    na.inds = is.na(ys[ind, ])
    if (any(na.inds)) {
      new.error.message = paste("mlrMBO: NAs detected and imputed for: ",
        collapse(control$y.name[is.na(ys[ind, ])], sep = ", "), sep = "")
      if (error.messages[ind] == "")
        error.messages[ind] = new.error.message
      else
        error.messages[ind] = paste(new.error.message, ". Original error message from target function: ",
          error.messages[ind])
    }
  }
  
  # Now impute every NA with the given imputation function
  for (dim in seq_len(control$number.of.targets)) {
    # impute all broken y values in ys by imputation function of user
    broken = which(is.na(ys[, dim]) | is.nan(ys[, dim]) | is.infinite(ys[, dim]))
    if (length(broken) > 0L) {
      ys[broken, dim] = mapply(control$impute[[dim]], xs[broken], ys[broken, dim],
        MoreArgs = list(opt.path = opt.path), USE.NAMES = FALSE)
    }
  }
  
  return(list(ys = ys, times = times, error.messages = error.messages))
}

