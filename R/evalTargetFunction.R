# Evaluates target fitness function on given set of points.
#
# xs = list of points
# dobs = dobs values for xs, same len or 1 int
#
# - trafo xs
# - evals all xs, measures time
# - potentially imputes errors, NAs, NaNs, Infs
#
# returns numeric-vector of y-vals

evalTargetFun = function(fun, par.set, dobs, xs, opt.path, control, show.info, oldopts,
  more.args = list(), extras) {

  # short names and so on
  y.name = control$y.name
  nevals = length(xs)
  ny = control$number.of.targets
  num.format = control$output.num.format
  num.format.string = paste("%s = ", num.format, sep = "")
  dobs = ensureVector(dobs, n = nevals, cl = "integer")

  # trafo
  xs = lapply(xs, trafoValue, par = par.set)

  # function to measure of fun call
  wrapfun = function(x) {
    st = proc.time()
    y = do.call(fun, insert(list(x = x), more.args))
    st = proc.time() - st
    return(list(y = y, time = st[3], err = NA_character_))
  }
  # restore mlr configuration
  configureMlr(on.learner.error = oldopts[["ole"]], show.learner.output = oldopts[["slo"]])

  # If an error occurs in f and we don't want to impute, stop - in this case
  # parallelMap gets NULL as impute function. In this case, execution will
  # stop at this line. Otherwise we use the identity for imputation - now
  # every element in z could be an error object.
  res = parallelMap(wrapfun, xs, impute.error = if (control$impute.errors) identity else NULL)

  # loop evals and to some post-processing
  for (i in 1:nevals) {
    r = res[[i]]; x = xs[[i]]; dob = dobs[i]

    if (is.error(r)) {
      # if error produce fake result list
      res[[i]] = list(y = rep(NA_real_, ny), time = NA_real_, err = r$message)
    } else {
      if (!(is.numeric(r$y) && length(r$y) == ny))
        stopf("Objective function output must be a numeric of length: %i!", ny)
    }
    y = r$y
    # are there NAs or NANs in the output? if so, produce custom error and impute with user fun
    isna = is.na(y) | is.nan(y) | is.infinite(y)
    if (any(isna)) {
      r$err = sprintf("mlrMBO: NA or NaN in objective output and imputed: %s",
        collapsef("%s = %g", y.name, y), sep = ", ")
      y[isna] = mapply(which(isna), function(d)
        control$impute[[d]], x, y[d], MoreArgs = list(opt.path = opt.path))
    }

    if (show.info) { # show info on console
      #FIXME: show time?
      messagef("[mbo] %i: %s : %s", dob, paramValueToString(par.set, x, num.format = num.format),
        collapse(sprintf(num.format.string, y.name, y), ", "))
    }
    # log to opt path
    addOptPathEl(opt.path, x = x, y = r$y, dob = dob,
      error.message = r$err, exec.time = r$time, extra = extras[[i]])
  }

  # FIXME: Do we need this here? This function is for function evaluation,
  # we should not need to configure mlr in this function.
  # BB: there is the problem if we call an mlr learner in the target function.....
  # BB: still, this is bad style here
  configureMlr(on.learner.error = control$on.learner.error,
    show.learner.output = control$show.learner.output)

  extractSubList(res, "y")
}

