# Evaluates target fitness function on given set of points.
#
# @param opt.state [\code{OptState}]\cr
# @param xs [\code{list}]\cr
#   List of points.
# @param extras [\code{list}]\cr
#   List of extra information to be logged in \code{opt.path}.
# @param xs.times [\code{numeric}] \cr
#   A vector of the same length as \code{xs} giving the estimated times for each evaluation of \code{x}.
# @param xs.priorities [\code{numeric}] \cr
# @return [\code{numeric} | \code{matrix}] Numeric vector of y-vals or matrix
#   (for multi-criteria problems).
evalTargetFun.OptState = function(opt.state, xs, extras, xs.times = NULL, xs.priorities = NULL) {

  opt.problem = getOptStateOptProblem(opt.state)
  par.set = getOptProblemParSet(opt.problem)
  opt.path = getOptStateOptPath(opt.state)
  control = getOptProblemControl(opt.problem)
  oldopts = getOptProblemOldopts(opt.problem)

  # short names and so on
  ny = control$number.of.targets
  num.format = control$output.num.format
  num.format.string = paste("%s = ", num.format, sep = "")
  imputeY = control$impute.y.fun

    # trafo - but we only want to use the Trafo for function eval, not for logging
  xs.trafo = lapply(xs, trafoValue, par = par.set)

  # function to measure of fun call
  wrapFun = function(x) {
    st = proc.time()
    y = do.call(getOptProblemFun(opt.problem), insert(list(x = x), getOptProblemMoreArgs(opt.problem)))
    user.extras = list()
    # here we extract additional stuff which the user wants to log in the opt path
    if (hasAttributes(y, "extras")) {
      user.extras = attr(y, "extras")
      y = setAttribute(y, "extras", NULL)
    }
    st = proc.time() - st
    list(x = x, y = y, time = st[3], user.extras = user.extras)
  }

  # restore mlr configuration
  configureMlr(on.learner.error = oldopts[["ole"]], show.learner.output = oldopts[["slo"]])

  scheduleFunction = switch(control$schedule.method,
    smartParallelMap = evalScheduleSmartParallelMap,
    evalScheduleParallelMap
  )

  res = scheduleFunction(wrapFun = wrapFun, xs = xs.trafo, xs.times = xs.times, xs.priorities = xs.priorities, opt.state = opt.state)
  #extract the treated x variables
  xs = extractSubList(res, "x", simplify = FALSE)
  xs.trafo = lapply(xs, trafoValue, par = par.set)

  # do we have a valid y object?
  isYValid = function(y) {
    !is.error(y) && is.numeric(y) && (length(y) == ny) && !any(is.na(y) | is.nan(y) | is.infinite(y))
  }

  # loop evals and to some post-processing
  for (i in seq_along(res)) {
    r = res[[i]]; x = xs[[i]]; x.trafo = xs.trafo[[i]]; dob = asInteger(getOptStateLoop(opt.state))
    # y is now either error object or return val
    if (is.error(r)) {
      y = r; ytime = NA_real_; errmsg = r$message; user.extras = list()
    } else {
      y = r$y; ytime = r$time; errmsg = NA_character_; user.extras = r$user.extras
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
    showInfo(getOptProblemShowInfo(opt.problem), "[mbo] %i: %s : %s : %.1f secs%s", 
      dob, paramValueToString(par.set, x.trafo, num.format = num.format),
      collapse(sprintf(num.format.string, control$y.name, y2), ", "),
      ytime, ifelse(y.valid, "", " (imputed)")
    )

    # concatenate internal and user defined extras for logging in opt.path
    extras[[i]] = insert(extras[[i]], user.extras)

    # log to opt path - make sure to log the untrafo'd x-value!
    addOptPathEl(opt.path, x = x, y = y2, dob = dob,
      error.message = errmsg, exec.time = ytime, extra = extras[[i]])
  }

  # FIXME: See issue
  configureMlr(on.learner.error = control$on.learner.error,
    show.learner.output = control$show.learner.output)

  extractSubList(res, "y")
}
