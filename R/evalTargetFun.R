# Evaluates target fitness function on given set of points.
#
# @param opt.state
# @param xs: list of list of points to evaluate
# @param extras: list of extra stuff from getExtras, list of list of entries. same length as xs.
# @param xs.schedule.info [\code{list}] \cr
#   A list containing vectors of the same length as \code{xs} giving the estimated times, priorities and times.se for each evaluation of \code{x}.
# @return [\code{numeric} | \code{matrix}] Numeric vector of y-vals or matrix
#   (for multi-criteria problems).
#
# Does this:
# 1) trafo X points
# 2) evaluate the objective call, measure execution time
#    retrieve y vector(nr.of.targets) + "extras" attribute
#    eval is done with parallelMap, level=mlrMBO.feval
# 3) potentially impute y-values in case of problems, see error_handling.R
# 4) possibly log to console
# 5) log untrafoed x-points, evaluated y-values, passed extras and eval extras to  opt.path in opt.state

evalTargetFun.OptState = function(opt.state, xs, extras, xs.schedule.info = NULL) {
  opt.problem = getOptStateOptProblem(opt.state)
  par.set = getOptProblemParSet(opt.problem)
  opt.path = getOptStateOptPath(opt.state)
  control = getOptProblemControl(opt.problem)

  # short names and so on
  nevals = length(xs)
  ny = control$n.objectives
  num.format = control$output.num.format
  num.format.string = paste("%s = ", num.format, sep = "")
  imputeY = control$impute.y.fun

  # trafo X points
  # function to measure of fun call
  wrapFun = function(x) {
    st = proc.time()
    user.extras = list(exec.timestamp = as.integer(Sys.time()))
    y = do.call(getOptProblemFun(opt.problem), insert(list(x = x), getOptProblemMoreArgs(opt.problem)))    
    # here we extract additional stuff which the user wants to log in the opt path
    if (hasAttributes(y, "extras")) {
      user.extras = c(user.extras, attr(y, "extras"))
      y = setAttribute(y, "extras", NULL)
    }
    if (hasAttributes(y, "exec.time")) {
      st = attr(y, "exec.time")
    } else {
      st = (proc.time() - st)[3]
    }
    list(y = y, time = st, user.extras = user.extras)
  }

  scheduleFunction = switch(control$schedule.method,
    smartParallelMap = evalScheduleSmartParallelMap,
    asyn = evalScheduleParallelMap,
    scheduleKnapsack = evalScheduleKnapsack,
    evalScheduleParallelMap
  )
  
  # trafo - but we only want to use the Trafo for function eval, not for logging
  xs.trafo = lapply(xs, trafoValue, par = par.set)

  res = scheduleFunction(wrapFun = wrapFun, xs = xs, xs.trafo = xs.trafo, xs.schedule.info = xs.schedule.info, extras = extras, opt.state = opt.state)

  #extract the treated x variables
  xs = res$xs
  xs.trafo = res$xs.trafo
  extras = res$extras

  # do we have a valid y object?
  isYValid = function(y) {
    !is.error(y) && testNumeric(y, len = ny, any.missing = FALSE, finite = TRUE)
  }

  # loop evals and to some post-processing
  for (i in seq_along(res$funRes)) {
    r = res$funRes[[i]]; x = xs[[i]]; x.trafo = xs.trafo[[i]]; dob = res$dob
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

    prop.type = extras[[i]]$prop.type

    # showInfo - use the trafo'd value here!
    showInfo(getOptProblemShowInfo(opt.problem), "[mbo] %i: %s : %s : %.1f secs%s : %s",
      dob,
      paramValueToString(par.set, x.trafo, num.format = num.format),
      collapse(sprintf(num.format.string, control$y.name, y2), ", "),
      ytime,
      ifelse(y.valid, "", " (imputed)"),
      prop.type
    )

    # concatenate internal and user defined extras for logging in opt.path
    extras[[i]] = insert(extras[[i]], user.extras)

    # log to opt path - make sure to log the untrafo'd x-value!
    addOptPathEl(opt.path, x = x, y = y2, dob = dob,
      error.message = errmsg, exec.time = ytime, extra = extras[[i]])
  }

  extractSubList(res$funRes, "y")
}
