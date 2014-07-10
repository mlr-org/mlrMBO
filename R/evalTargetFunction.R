# Evaluates target fitness function on given set of points.
#
# xs = list of points
# dobs = dobs values for xs, same len or 1 int
# extras = extra values to be logged in the opt.path.
#
# - trafo xs
# - evals all xs, measures time
# - potentially imputes errors, NAs, NaNs, Infs
#
# returns numeric-vector (matrix for multicrit) of y-vals

evalTargetFun = function(fun, par.set, dobs, xs, opt.path, control, show.info, oldopts,
  more.args = list(), extras) {

  # short names and so on
  y.name = control$y.name
  nevals = length(xs)
  ny = control$number.of.targets
  num.format = control$output.num.format
  num.format.string = paste("%s = ", num.format, sep = "")
  dobs = ensureVector(dobs, n = nevals, cl = "integer")
  imputeY = control$impute.y.fun

  # trafo - but we only want to use the Trafo for function eval, not for logging
  xs.trafo = lapply(xs, trafoValue, par = par.set)

  # function to measure of fun call
  wrapFun = function(x) {
    st = proc.time()
    y = do.call(fun, insert(list(x = x), more.args))
    st = proc.time() - st
    list(y = y, time = st[3])
  }

  # do we have a valid y object?
  isYValid = function(y) {
    !is.error(y) && is.numeric(y) && (length(y) == ny) && !any(is.na(y) | is.nan(y) | is.infinite(y))
  }

  # restore mlr configuration
  configureMlr(on.learner.error = oldopts[["ole"]], show.learner.output = oldopts[["slo"]])

  # return error objects if we impute
  res = parallelMap(wrapFun, xs.trafo, impute.error = if (is.null(imputeY)) NULL else identity)

  # loop evals and to some post-processing
  for (i in 1:nevals) {
    r = res[[i]]; x = xs[[i]]; x.trafo = xs.trafo[[i]]; dob = dobs[i]
    # y is now either error object or return val
    if (is.error(r)) {
      y = r; ytime = NA_real_; errmsg = r$message
    } else {
      y = r$y; ytime = r$time; errmsg = NA_character_
    }
    y.valid = isYValid(y)

    # objective fun problem? allow user to handle it
    y2 = y # changed y that we will use in opt.path
    if (!y.valid) {
      if (is.null(imputeY)) { # ok then stop
        if (is.error(y))
          stopf("Objective function error: %s ", y$message)
        else
          stopf("Objective function output must be a numeric of length %i, but we got: %s",
            ny, convertToShortString(y))
      } else { # let user impute
        if (!is.error(r) && !y.valid)
          errmsg = sprintf("mlrMBO: Imputed invalid objective function output. Original value was: %s",
            convertToShortString(y))
        y2 = imputeY(x, y, opt.path)
        if (!isYValid(y2))
          stopf("Y-Imputation failed. Must return a numeric of length: %i, but we got: %s",
            ny, convertToShortString(y2))
      }
    }

    # showInfo - use the trafo'd value here!
    showInfo(show.info, "[mbo] %i: %s : %s : %.1f secs%s", dob,
      paramValueToString(par.set, x.trafo, num.format = num.format),
      collapse(sprintf(num.format.string, y.name, y2), ", "),
      ytime,
      ifelse(y.valid, "", " (imputed)")
    )
    # log to opt path - make sure to log the untrafo'd x-value!
    addOptPathEl(opt.path, x = x, y = y2, dob = dob,
      error.message = errmsg, exec.time = ytime, extra = extras[[i]])
  }
  
  # FIXME: See issue
  configureMlr(on.learner.error = control$on.learner.error,
    show.learner.output = control$show.learner.output)

  extractSubList(res, "y")
}

